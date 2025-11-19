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
#' list()
#'
#' # List endpoints for a specific section
#' list("seine")
#' list("biology")
#'
#' # Equivalent to:
#' list("biology/some-sub-path")
#' }
list <- function(path = NULL, base_url = "https://cramerdb.com/api/", headers = list()) {
  headers <- .auth_headers(headers)

  # Build the full URL
  url <- .build_endpoint_url(base_url, path)

  # Fetch the endpoint info
  tryCatch({
    req <- httr2::request(url)
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

      cat(sprintf("Available endpoints at %s:\n", url))
      for (name in endpoint_names) {
        endpoint_url <- endpoints[[name]]
        cat(sprintf("  %-20s %s\n", name, endpoint_url))
      }
    } else {
      cat(sprintf("No endpoints available at %s\n", url))
    }

    cat("\n")
    invisible(body)

  }, error = function(e) {
    cat(sprintf("Error fetching %s:\n", url))
    cat(sprintf("  %s\n", conditionMessage(e)))
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
    req <- .add_headers(req, headers)
    res <- httr2::req_perform(req)
    httr2::resp_check_status(res)
    body <- httr2::resp_body_json(res, simplifyVector = FALSE)

    # Show auth status
    cat("CramerDB Authentication Status\n")
    cat("==============================\n\n")

    if (!is.null(body[["authenticated"]])) {
      is_authed <- isTRUE(body[["authenticated"]])
      auth_status <- if (is_authed) "YES" else "NO"
      cat(sprintf("Authenticated: %s\n", auth_status))

      if (!is.null(body[["user"]])) {
        cat(sprintf("User: %s\n", body[["user"]]))
      }

      if (!is_authed) {
        cat("\nTo authenticate, use:\n")
        cat("  set_token('your_token_here')\n")
      }
    } else {
      cat("Unable to determine authentication status\n")
    }

    cat("\n")
    invisible(body)

  }, error = function(e) {
    cat(sprintf("Error checking authentication:\n"))
    cat(sprintf("  %s\n", conditionMessage(e)))
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
