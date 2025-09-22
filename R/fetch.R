#' @title Fetch any Cramer API endpoint into a data frame (or sf)
#' @description
#' `fetch_df()` follows pagination automatically and normalizes both
#' plain JSON lists and GeoJSON (including FeatureCollections wrapped in
#' DRF `results`). If point coordinates are present, it returns an `sf`
#' tibble with EPSG:4326; otherwise a plain `tibble`.
#'
#' @param url Character. First page URL (DRF or non-paginated).
#' @param headers Named list of HTTP headers (e.g., list(Authorization = "Token xxx")).
#' @param as_sf Logical. If `TRUE` (default), try to return sf when geometry exists.
#' @return A `tibble` or an `sf` tibble.
#' @export

fetch <- function(url, headers = list(), as_sf = TRUE) {
  pages <- .fetch_pages(url, headers)
  .normalize_pages_to_df(pages, as_sf = as_sf)
}

# ---- internals -------------------------------------------------------------

# robust header adder without tidy-eval
.add_headers <- function(req, headers) {
  if (length(headers) > 0) {
    req <- do.call(httr2::req_headers, c(list(req), headers))
  }
  req
}

# robust query adder without tidy-eval
.add_query <- function(req, query) {
  if (length(query) > 0) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }
  req
}

# Does the URL already have a given query parameter name? (case-insensitive)
.has_query_param <- function(url, name) {
  # match ?name= or &name= or terminal ?name / &name
  patt <- paste0("(?i)([?&])", name, "(=|&|$)")
  grepl(patt, url, perl = TRUE)
}

.fetch_once <- function(url, headers = list(), labels = TRUE) {
  req <- httr2::request(url)
  req <- .add_headers(req, headers)
  
  # Add labels=1 by default unless caller disabled it or URL already has labels
  if (isTRUE(labels) && !.has_query_param(url, "labels")) {
    req <- .add_query(req, list(labels = "1"))
  }
  
  res <- httr2::req_perform(req)
  httr2::resp_check_status(res)
  httr2::resp_body_json(res, simplifyVector = FALSE)
}

# Follow DRF pagination if present.
# NOTE: never use $next (reserved word). Use [["next"]].
.fetch_pages <- function(url, headers = list(), labels = TRUE) {
  body <- .fetch_once(url, headers, labels = labels)
  # DRF-style page
  if (is.list(body) && !is.null(body[["results"]])) {
    pages <- list(body)
    nxt <- body[["next"]]
    while (!is.null(nxt) && is.character(nxt) && nzchar(nxt)) {
      # For safety, also ensure later pages carry labels unless they already have it.
      pg <- .fetch_once(nxt, headers, labels = labels)
      pages <- c(pages, list(pg))
      nxt <- pg[["next"]]
    }
    return(pages)
  }
  # single page (array/object or FeatureCollection)
  list(body)
}

# GeoJSON helpers
.is_feature <- function(x) {
  is.list(x) && identical(x[["type"]], "Feature") &&
    !is.null(x[["geometry"]]) && !is.null(x[["properties"]])
}

# Replace any NULLs in a (possibly nested) list with NA scalars
.nulls_to_na <- function(x) {
  if (!is.list(x)) return(if (is.null(x)) NA else x)
  purrr::modify(x, ~ if (is.null(.x)) NA else .x)
}

# Convert Feature list → tibble; attach sf if coordinates exist
.features_to_tbl <- function(features, as_sf = TRUE) {
  if (length(features) == 0) return(tibble::tibble())
  rows <- purrr::map(features, function(f) {
    if (!.is_feature(f)) return(NULL)

    id   <- f[["id"]]
    prop <- f[["properties"]]
    coords <- f[["geometry"]][["coordinates"]]

    prop <- .nulls_to_na(prop %||% list())
    tb   <- tibble::as_tibble(prop)
    tb$id <- as.character(id %||% NA_character_)

    # order id first
    tb <- dplyr::relocate(tb, id)

    # geometry (lon, lat)
    if (!is.null(coords) && length(coords) >= 2 &&
        is.numeric(coords[[1]]) && is.numeric(coords[[2]])) {
      tb$.lon <- coords[[1]]
      tb$.lat <- coords[[2]]
    }
    tb
  })
  out <- dplyr::bind_rows(rows)

  # If we have lon/lat and want sf, promote to sf and drop helper cols
  if (as_sf && all(c(".lon", ".lat") %in% names(out))) {
    sfc <- sf::st_sfc(purrr::pmap(out[, c(".lon", ".lat")], ~ sf::st_point(c(..1, ..2))), crs = 4326)
    out$geom <- sfc
    out <- dplyr::select(out, -".lon", -".lat")
    out <- sf::st_as_sf(out, sf_column_name = "geom", crs = 4326)
  }
  out
}

# Main normalizer
.normalize_pages_to_df <- function(pages, as_sf = TRUE) {
  # Case 1: DRF pages with plain objects in results
  if (length(pages) && is.list(pages[[1]]) && !is.null(pages[[1]][["results"]])) {
    # results can be:
    #   a) list of plain objects
    #   b) a single FeatureCollection
    #   c) list of Feature objects
    all_results <- purrr::map(pages, ~ .x[["results"]])

    # If results[[1]] is a FeatureCollection object
    if (is.list(all_results[[1]]) && !is.null(all_results[[1]][["type"]]) &&
        identical(all_results[[1]][["type"]], "FeatureCollection")) {
      feats <- purrr::map(all_results, ~ .x[["features"]] %||% list()) |> purrr::flatten()
      return(.features_to_tbl(feats, as_sf = as_sf))
    }

    # If results is a list of Features on each page
    items <- purrr::flatten(all_results)
    if (length(items) > 0 && all(purrr::map_lgl(items, .is_feature))) {
      return(.features_to_tbl(items, as_sf = as_sf))
    }

    # Otherwise: plain objects → data frame
    rows <- purrr::map(items, function(x) {
      tibble::as_tibble(.nulls_to_na(x))
    })
    return(dplyr::bind_rows(!!!rows))
  }

  # Case 2: Single-page FeatureCollection
  if (length(pages) == 1 &&
      is.list(pages[[1]]) &&
      identical(pages[[1]][["type"]], "FeatureCollection")) {
    feats <- pages[[1]][["features"]] %||% list()
    return(.features_to_tbl(feats, as_sf = as_sf))
  }

  # Case 3: Single-page array (Feature list or plain list)
  if (length(pages) == 1 && is.list(pages[[1]]) &&
      (is.null(names(pages[[1]])) || all(names(pages[[1]]) == ""))) {
    items <- pages[[1]]
    if (length(items) > 0 && all(purrr::map_lgl(items, .is_feature))) {
      return(.features_to_tbl(items, as_sf = as_sf))
    }
    rows <- purrr::map(items, tibble::as_tibble)
    return(dplyr::bind_rows(!!!rows))
  }

  # Case 4: Single object → tibble(1 row)
  tibble::as_tibble(pages[[1]])
}

`%||%` <- function(a, b) if (is.null(a)) b else a
