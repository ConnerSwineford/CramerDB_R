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
}
