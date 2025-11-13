# R/auth.R

#' Set the CramerDB API token for this R session
#'
#' This stores the token in an R option (`cramerdb.token`), so you don't have to
#' pass it on every call. You can also set the `CRAMERDB_TOKEN` environment
#' variable instead of using this function.
#'
#' @param token Character scalar token value (no "Token " prefix needed).
#' @examples
#' cramerdb::set_token("abcd1234...")
#' @export
set_token <- function(token) {
  if (!is.character(token) || length(token) != 1L || !nzchar(token)) {
    stop("set_token(): `token` must be a non-empty character scalar.", call. = FALSE)
  }
  options(cramerdb.token = token)
  invisible(token)
}

#' Get the currently configured CramerDB API token
#'
#' This first checks the `cramerdb.token` option (set via [set_token()]),
#' then falls back to the `CRAMERDB_TOKEN` environment variable if needed.
#'
#' @param error_if_missing Logical. If TRUE, error when no token is found.
#' @return The token string, or NULL if not set (and `error_if_missing = FALSE`).
#' @examples
#' cramerdb::get_token()
#' @export
get_token <- function(error_if_missing = FALSE) {
  token <- getOption("cramerdb.token", default = Sys.getenv("CRAMERDB_TOKEN", ""))
  
  if (!nzchar(token)) {
    if (isTRUE(error_if_missing)) {
      stop(
        "No CramerDB API token found.\n",
        "Use cramerdb::set_token('...') or set env var CRAMERDB_TOKEN.",
        call. = FALSE
      )
    }
    return(NULL)
  }
  
  token
}

# Internal helper: merge stored token into headers (if no Authorization supplied)
.auth_headers <- function(headers = list()) {
  # If user already supplied an Authorization header, don't touch it
  if ("Authorization" %in% names(headers)) {
    return(headers)
  }
  
  token <- get_token(error_if_missing = FALSE)
  if (is.null(token)) {
    # No token configured; just return original headers
    return(headers)
  }
  
  c(headers, list(Authorization = paste("Token", token)))
}
