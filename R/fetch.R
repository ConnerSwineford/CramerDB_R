#' @title Fetch any Cramer API endpoint into a data frame (or sf)
#' @description
#' `fetch_df()` follows pagination automatically and normalizes both
#' plain JSON lists and GeoJSON (including FeatureCollections wrapped in
#' DRF `results`). If point coordinates are present, it returns an `sf`
#' tibble with EPSG:4326; otherwise a plain `tibble`.
#'
#' @param url Character. Full URL or relative path (e.g., "seine/event/" or "https://cramerdb.com/api/seine/event/").
#'   Relative paths are prepended with base_url.
#' @param headers Named list of HTTP headers (e.g., list(Authorization = "Token xxx")).
#' @param as_sf Logical. If `TRUE` (default), try to return sf when geometry exists.
#' @param base_url Character. Base API URL (default: "https://cramerdb.com/api/")
#' @return A `tibble` or an `sf` tibble.
#' @export

fetch <- function(url, headers = list(), as_sf = TRUE, base_url = "https://cramerdb.com/api/") {
  headers <- .auth_headers(headers)
  url <- .normalize_url(url, base_url)

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
  # Add Accept header to ensure JSON response
  req <- httr2::req_headers(req, Accept = "application/json")
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

    # Show progress if multiple pages
    if (!is.null(nxt) && is.character(nxt) && nzchar(nxt)) {
      page_num <- 2

      # Check if we can determine total from count
      total_count <- body[["count"]]
      page_size <- length(body[["results"]])

      if (.is_verbose()) {
        if (!is.null(total_count) && page_size > 0) {
          total_pages <- ceiling(total_count / page_size)
          cat(.gum_style("Fetching paginated data:", color = .gum_colors$primary),
              sprintf(" %d pages (~%d records)\n", total_pages, total_count))
        } else {
          cat(.gum_style("Fetching paginated data...", color = .gum_colors$primary), "\n")
        }
      }

      while (!is.null(nxt) && is.character(nxt) && nzchar(nxt)) {
        # Show progress for each page
        if (.is_verbose()) {
          if (!is.null(total_count) && page_size > 0) {
            .gum_progress(page_num, total_pages, "Progress")
          } else {
            cat(.gum_style(sprintf("  Fetching page %d...", page_num),
                           color = .gum_colors$muted), "\r")
            flush.console()
          }
        }

        # For safety, also ensure later pages carry labels unless they already have it.
        pg <- .fetch_once(nxt, headers, labels = labels)
        pages <- c(pages, list(pg))
        nxt <- pg[["next"]]
        page_num <- page_num + 1
      }

      if (.is_verbose()) {
        # Final newline if we didn't show progress bar
        if (is.null(total_count) || page_size == 0) {
          cat("\n")
        }

        .gum_success(sprintf("Fetched %d pages successfully", length(pages)))
      }
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

# Check if object has a geometry field (geom or geometry)
.has_geom_field <- function(x) {
  is.list(x) && ("geom" %in% names(x) || "geometry" %in% names(x))
}

# Convert objects with geom/geometry fields to sf DataFrame
.objects_with_geom_to_df <- function(objects, as_sf = TRUE) {
  if (length(objects) == 0) return(tibble::tibble())

  # Extract properties and geometries separately
  props_list <- purrr::map(objects, function(obj) {
    # Remove geometry fields from properties
    obj_copy <- obj
    geom_field <- if ("geom" %in% names(obj)) "geom" else "geometry"
    obj_copy[[geom_field]] <- NULL
    .nulls_to_na(obj_copy)
  })

  # Create data frame from properties
  df <- dplyr::bind_rows(purrr::map(props_list, tibble::as_tibble))

  # If as_sf is FALSE, return plain tibble
  if (!as_sf) {
    return(df)
  }

  # Convert geometries using sf
  tryCatch({
    # Build GeoJSON FeatureCollection
    geom_field <- if ("geom" %in% names(objects[[1]])) "geom" else "geometry"

    features <- purrr::map(objects, function(obj) {
      list(
        type = "Feature",
        geometry = obj[[geom_field]],
        properties = list()  # Properties already in df
      )
    })

    fc <- list(
      type = "FeatureCollection",
      features = features
    )

    # Convert to GeoJSON string
    geojson_str <- jsonlite::toJSON(fc, auto_unbox = TRUE, digits = NA)

    # Read with sf
    sf_obj <- sf::st_read(geojson_str, quiet = TRUE)

    # Extract just the geometry column
    geom_col <- sf::st_geometry(sf_obj)

    # Combine with properties DataFrame
    df$geometry <- geom_col
    result <- sf::st_as_sf(df, sf_column_name = "geometry")

    return(result)
  }, error = function(e) {
    # If sf conversion fails, return plain tibble
    warning("Could not convert to sf object: ", conditionMessage(e), call. = FALSE)
    return(df)
  })
}

# Replace NULLs and empty lists with NA; wrap multi-element vectors as list-columns
.nulls_to_na <- function(x) {
  if (!is.list(x)) return(if (is.null(x)) NA else x)
  purrr::imap(x, function(val, nm) {
    # NULL → NA
    if (is.null(val)) return(NA)
    # Empty list/vector → NA
    if (length(val) == 0) return(NA)
    # Multi-element list/vector → wrap in list() for tibble list-column
    if (is.list(val) || length(val) > 1) return(list(val))
    # Scalar stays as-is
    val
  })
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
    tryCatch({
      sfc <- sf::st_sfc(purrr::pmap(out[, c(".lon", ".lat")], ~ sf::st_point(c(..1, ..2))), crs = 4326)
      out$geom <- sfc
      out <- dplyr::select(out, -".lon", -".lat")
      out <- sf::st_as_sf(out, sf_column_name = "geom", crs = 4326)
    }, error = function(e) {
      # If sf conversion fails, keep as tibble with lon/lat columns
      warning("Could not convert to sf object: ", conditionMessage(e),
              "\nReturning tibble with .lon and .lat columns instead.", call. = FALSE)
      # out already exists from above, just return it with lon/lat
    })
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
    #   d) plain objects with 'geom' or 'geometry' fields
    all_results <- purrr::map(pages, ~ .x[["results"]])

    # If results[[1]] is a FeatureCollection object
    if (is.list(all_results[[1]]) && !is.null(all_results[[1]][["type"]]) &&
        identical(all_results[[1]][["type"]], "FeatureCollection")) {
      feats <- purrr::map(all_results, ~ .x[["features"]] %||% list()) |>
        purrr::flatten()
      return(.features_to_tbl(feats, as_sf = as_sf))
    }

    # If results is a list of Features on each page
    items <- purrr::flatten(all_results)
    if (length(items) > 0 && all(purrr::map_lgl(items, .is_feature))) {
      return(.features_to_tbl(items, as_sf = as_sf))
    }

    # Check if plain objects have geometry fields (geom or geometry)
    if (length(items) > 0 && .has_geom_field(items[[1]])) {
      return(.objects_with_geom_to_df(items, as_sf = as_sf))
    }

    # Otherwise: plain objects → data frame
    rows <- purrr::map(items, function(x) {
      tibble::as_tibble(.nulls_to_na(x))
    })

    # --- harmonize column types across pages ---------------------------
    # If a column is sometimes <character> and sometimes <logical>,
    # coerce the logical ones to character so bind_rows() is happy.
    all_cols <- unique(unlist(purrr::map(rows, names)))

    for (nm in all_cols) {
      classes <- purrr::map_chr(rows, function(df) {
        if (!nm %in% names(df)) return(NA_character_)
        class(df[[nm]])[1]
      })

      if ("character" %in% classes && "logical" %in% classes) {
        rows <- purrr::map(rows, function(df) {
          if (nm %in% names(df) && is.logical(df[[nm]])) {
            df[[nm]] <- as.character(df[[nm]])
          }
          df
        })
      }
    }

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

    # Check if plain objects have geometry fields
    if (length(items) > 0 && .has_geom_field(items[[1]])) {
      return(.objects_with_geom_to_df(items, as_sf = as_sf))
    }

    rows <- purrr::map(items, tibble::as_tibble)
    return(dplyr::bind_rows(!!!rows))
  }

  # Case 4: Single object → tibble(1 row)
  tibble::as_tibble(pages[[1]])
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Normalize URL: if relative path, prepend base_url
.normalize_url <- function(url, base_url = "https://cramerdb.com/api/") {
  # Check if URL is already absolute (has scheme)
  if (grepl("^https?://", url)) {
    return(url)
  }

  # Relative path - prepend base_url
  # Ensure base_url ends with /
  if (!grepl("/$", base_url)) {
    base_url <- paste0(base_url, "/")
  }

  # Remove leading slash from path if present
  url <- gsub("^/+", "", url)

  paste0(base_url, url)
}
