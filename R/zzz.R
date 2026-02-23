# Package startup and cleanup hooks

.onAttach <- function(libname, pkgname) {
  # Show package info
  packageStartupMessage("cramerdb: R interface for the CramerDB API")

  # Check for gum and show helpful message
  if (!.has_gum()) {
    packageStartupMessage(
      "\nTip: Install 'gum' for beautiful styled output:\n",
      "  install_gum()\n",
      "\nGum adds colors, progress bars, and styled tables!"
    )
  } else {
    packageStartupMessage("\nBeautiful output enabled via gum CLI")
  }

  .check_for_updates()
}

.check_for_updates <- function() {
  tryCatch({
    current <- utils::packageVersion("cramerdb")
    req <- httr2::request(
      "https://raw.githubusercontent.com/ConnerSwineford/CramerDB_R/main/cramerdb/DESCRIPTION"
    )
    req <- httr2::req_timeout(req, 3)
    res <- httr2::req_perform(req)
    if (httr2::resp_status(res) == 200) {
      txt <- httr2::resp_body_string(res)
      m   <- regmatches(txt, regexpr("(?m)^Version:\\s*\\S+", txt, perl = TRUE))
      if (length(m) == 1) {
        latest <- package_version(trimws(sub("Version:\\s*", "", m)))
        if (latest > current) {
          packageStartupMessage(sprintf(
            "\nA new version of cramerdb is available: %s (installed: %s)",
            latest, current
          ))
          packageStartupMessage(
            "Update with: pak::pak(\"ConnerSwineford/CramerDb_R\")\n"
          )
        }
      }
    }
  }, error = function(e) invisible(NULL))
}
