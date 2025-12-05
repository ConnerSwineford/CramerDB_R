#' @title List available API endpoints
#' @description
#' Navigate the CramerDB API tree structure by listing available endpoints.
#' Use `whoami()` to check authentication status.
#'
#' @param path Character. Optional API path segment (e.g., "seine", "biology").
#'   If NULL or missing, lists the root API endpoints.
#' @param base_url Character. Base API URL (default: "https://cramerdb.com/api/")
#' @param headers Named list of HTTP headers (e.g., list(Authorization = "Token xxx"))
#' @return Invisibly returns the parsed JSON response; prints formatted output
#' @export
#' @examples
#' \dontrun{
#' # List top-level endpoints
#' endpoints()
#'
#' # List endpoints for a specific section
#' endpoints("seine")
#' endpoints("biology")
#'
#' # Equivalent to:
#' endpoints("biology/some-sub-path")
#' }
endpoints <- function(path = NULL, base_url = "https://cramerdb.com/api/", headers = list()) {
  headers <- .auth_headers(headers)

  # Build the full URL
  url <- .build_endpoint_url(base_url, path)

  # Fetch the endpoint info
  tryCatch({
    req <- httr2::request(url)
    # Add Accept header to ensure JSON response
    req <- httr2::req_headers(req, Accept = "application/json")
    req <- .add_headers(req, headers)
    res <- httr2::req_perform(req)
    httr2::resp_check_status(res)
    body <- httr2::resp_body_json(res, simplifyVector = FALSE)

    # Determine endpoint structure
    # Root endpoints have "endpoints" key, sub-endpoints are flat objects
    endpoints <- if (!is.null(body[["endpoints"]])) {
      body[["endpoints"]]
    } else {
      # Filter out non-endpoint keys (authenticated, user, etc.)
      body[!names(body) %in% c("authenticated", "user")]
    }

    if (is.list(endpoints) && length(endpoints) > 0) {
      # Sort endpoints alphabetically
      endpoint_names <- names(endpoints)
      endpoint_names <- endpoint_names[order(endpoint_names)]

      # Display header with gum styling
      .gum_header(sprintf("Available endpoints at %s", url))

      # Display each endpoint with styling
      for (name in endpoint_names) {
        endpoint_url <- endpoints[[name]]

        # Style the endpoint name
        styled_name <- .gum_style(sprintf("  %-20s", name),
                                   color = .gum_colors$primary,
                                   bold = TRUE)

        # Display with muted URL
        styled_url <- .gum_style(endpoint_url, color = .gum_colors$muted)

        cat(styled_name, styled_url, "\n", sep = "")
      }
    } else {
      .gum_warning(sprintf("No endpoints available at %s", url))
    }

    cat("\n")
    invisible(body)

  }, error = function(e) {
    .gum_error(sprintf("Error fetching %s", url))
    cat("  ", conditionMessage(e), "\n", sep = "")
    invisible(NULL)
  })
}

#' @title Check authentication status
#' @description
#' Check if you are authenticated with the CramerDB API and see which user
#' you are logged in as.
#'
#' @param base_url Character. Base API URL (default: "https://cramerdb.com/api/")
#' @param headers Named list of HTTP headers (e.g., list(Authorization = "Token xxx"))
#' @return Invisibly returns the parsed JSON response; prints formatted output
#' @export
#' @examples
#' \dontrun{
#' # Check authentication status
#' whoami()
#'
#' # After setting token
#' set_token("your_token_here")
#' whoami()
#' }
whoami <- function(base_url = "https://cramerdb.com/api/", headers = list()) {
  headers <- .auth_headers(headers)

  # Fetch the root endpoint info
  tryCatch({
    req <- httr2::request(base_url)
    # Add Accept header to ensure JSON response
    req <- httr2::req_headers(req, Accept = "application/json")
    req <- .add_headers(req, headers)
    res <- httr2::req_perform(req)
    httr2::resp_check_status(res)
    body <- httr2::resp_body_json(res, simplifyVector = FALSE)

    # Show auth status with gum styling
    .gum_header("CramerDB Authentication Status")

    if (!is.null(body[["authenticated"]])) {
      is_authed <- isTRUE(body[["authenticated"]])

      # Display authentication status
      if (is_authed) {
        .gum_kv("Authenticated", .gum_style("YES", color = .gum_colors$success, bold = TRUE))

        if (!is.null(body[["user"]])) {
          .gum_kv("User", .gum_style(body[["user"]], color = .gum_colors$primary))
        }
      } else {
        .gum_kv("Authenticated", .gum_style("NO", color = .gum_colors$error, bold = TRUE))

        cat("\n")
        .gum_warning("Not authenticated")
        cat("  To authenticate, use: ", .gum_style("set_token('your_token_here')", color = .gum_colors$primary), "\n", sep = "")
      }
    } else {
      .gum_error("Unable to determine authentication status")
    }

    cat("\n")
    invisible(body)

  }, error = function(e) {
    .gum_error("Error checking authentication")
    cat("  ", conditionMessage(e), "\n", sep = "")
    invisible(NULL)
  })
}

# Internal helper to build endpoint URL from base + path
.build_endpoint_url <- function(base_url, path = NULL) {
  # Ensure base_url ends with /
  if (!grepl("/$", base_url)) {
    base_url <- paste0(base_url, "/")
  }

  # If no path provided, return base
  if (is.null(path) || !nzchar(path)) {
    return(base_url)
  }

  # Remove leading/trailing slashes from path
  path <- gsub("^/+|/+$", "", path)

  # Ensure path ends with /
  if (!grepl("/$", path)) {
    path <- paste0(path, "/")
  }

  paste0(base_url, path)
}

# Reuse helpers from fetch.R
.add_headers <- function(req, headers) {
  if (length(headers) > 0) {
    req <- do.call(httr2::req_headers, c(list(req), headers))
  }
  req
}
