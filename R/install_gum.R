# Gum CLI Installation Helpers

#' Install gum CLI for beautiful terminal output
#'
#' This function automatically detects your operating system and installs
#' the gum CLI tool using the appropriate package manager. Gum enhances
#' the cramerdb package with colored output, progress bars, and styled tables.
#'
#' @param force Logical. If TRUE, reinstall even if gum is already installed.
#' @return Invisibly returns TRUE if installation succeeded, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Install gum automatically
#' install_gum()
#' }
install_gum <- function(force = FALSE) {
  # Check if already installed
  if (!force && .has_gum()) {
    message("Gum is already installed!")
    message("Run check_gum() to verify the installation.")
    return(invisible(TRUE))
  }

  os <- .detect_os()

  message("Installing gum CLI...")
  message(sprintf("Detected OS: %s", os$name))

  if (os$type == "macos") {
    .install_gum_macos()
  } else if (os$type == "linux") {
    .install_gum_linux(os$distro)
  } else if (os$type == "windows") {
    .install_gum_windows()
  } else {
    message("Unsupported operating system: ", os$name)
    message("Please install gum manually: https://github.com/charmbracelet/gum#installation")
    return(invisible(FALSE))
  }

  # Verify installation
  Sys.sleep(1) # Give system time to update PATH
  if (.has_gum()) {
    message("\nSuccess! Gum is now installed.")
    message("Restart your R session to see beautiful styled output.")
    return(invisible(TRUE))
  } else {
    message("\nGum installation may have succeeded, but is not yet in your PATH.")
    message("You may need to restart your terminal or R session.")
    message("Run check_gum() after restarting to verify.")
    return(invisible(FALSE))
  }
}

#' Check if gum is installed and working
#'
#' Verifies that the gum CLI tool is installed and accessible from R.
#' Displays the gum version if available.
#'
#' @return Invisibly returns TRUE if gum is available, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' check_gum()
#' }
check_gum <- function() {
  if (.has_gum()) {
    # Try to get version
    version <- tryCatch({
      system2("gum", "--version", stdout = TRUE, stderr = FALSE)
    }, error = function(e) "unknown")

    message("Gum is installed and ready!")
    if (length(version) > 0 && version != "unknown") {
      message("Version: ", version[1])
    }
    message("\nYou'll now see:")
    message("  - Styled azure blue headers")
    message("  - Real-time progress bars")
    message("  - Color-coded status messages")
    return(invisible(TRUE))
  } else {
    message("Gum is not installed.")
    message("\nTo install gum and enable beautiful output, run:")
    message("  install_gum()")
    message("\nOr install manually: https://github.com/charmbracelet/gum#installation")
    return(invisible(FALSE))
  }
}

# Internal helpers --------------------------------------------------------

#' Detect operating system
#' @return List with type, name, and distro (for Linux)
.detect_os <- function() {
  sys_info <- Sys.info()

  if (.Platform$OS.type == "windows") {
    return(list(
      type = "windows",
      name = "Windows",
      distro = NULL
    ))
  }

  if (sys_info["sysname"] == "Darwin") {
    return(list(
      type = "macos",
      name = "macOS",
      distro = NULL
    ))
  }

  if (sys_info["sysname"] == "Linux") {
    # Try to detect Linux distribution
    distro <- "unknown"

    # Check for common distro files
    if (file.exists("/etc/os-release")) {
      os_release <- readLines("/etc/os-release", warn = FALSE)
      id_line <- grep("^ID=", os_release, value = TRUE)
      if (length(id_line) > 0) {
        distro <- gsub("^ID=|\"", "", id_line[1])
      }
    } else if (file.exists("/etc/debian_version")) {
      distro <- "debian"
    } else if (file.exists("/etc/redhat-release")) {
      distro <- "rhel"
    }

    return(list(
      type = "linux",
      name = "Linux",
      distro = distro
    ))
  }

  return(list(
    type = "unknown",
    name = sys_info["sysname"],
    distro = NULL
  ))
}

#' Install gum on macOS
.install_gum_macos <- function() {
  message("\nInstalling via Homebrew...")

  # Check if brew is available
  if (nchar(Sys.which("brew")) == 0) {
    message("Homebrew is not installed.")
    message("Please install Homebrew first: https://brew.sh")
    message("Then run: brew install gum")
    return(invisible(FALSE))
  }

  # Install gum
  result <- system2("brew", c("install", "gum"),
                    stdout = TRUE, stderr = TRUE)

  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    message("Installation failed. Error:")
    message(paste(result, collapse = "\n"))
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

#' Install gum on Linux
#' @param distro Linux distribution identifier
.install_gum_linux <- function(distro) {
  if (distro %in% c("ubuntu", "debian", "linuxmint", "pop")) {
    .install_gum_debian()
  } else if (distro %in% c("fedora", "rhel", "centos", "rocky", "alma")) {
    .install_gum_rpm()
  } else if (distro == "arch") {
    .install_gum_arch()
  } else {
    message("\nCouldn't auto-detect package manager for: ", distro)
    message("Please install gum manually:")
    message("  Debian/Ubuntu: https://github.com/charmbracelet/gum#debian-ubuntu")
    message("  Fedora/RHEL:   https://github.com/charmbracelet/gum#fedora-rhel")
    message("  Arch Linux:    sudo pacman -S gum")
    message("  Other:         https://github.com/charmbracelet/gum#installation")
    return(invisible(FALSE))
  }
}

#' Install gum on Debian/Ubuntu
.install_gum_debian <- function() {
  message("\nInstalling via apt...")
  message("This will require sudo permissions.")

  commands <- c(
    "sudo mkdir -p /etc/apt/keyrings",
    "curl -fsSL https://repo.charm.sh/apt/gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/charm.gpg",
    "echo 'deb [signed-by=/etc/apt/keyrings/charm.gpg] https://repo.charm.sh/apt/ * *' | sudo tee /etc/apt/sources.list.d/charm.list",
    "sudo apt update",
    "sudo apt install -y gum"
  )

  for (cmd in commands) {
    message("Running: ", cmd)
    result <- system(cmd)
    if (result != 0) {
      message("\nCommand failed. You may need to run these commands manually:")
      message(paste(commands, collapse = "\n"))
      return(invisible(FALSE))
    }
  }

  invisible(TRUE)
}

#' Install gum on Fedora/RHEL
.install_gum_rpm <- function() {
  message("\nInstalling via yum/dnf...")
  message("This will require sudo permissions.")

  repo_config <- "[charm]
name=Charm
baseurl=https://repo.charm.sh/yum/
enabled=1
gpgcheck=1
gpgkey=https://repo.charm.sh/yum/gpg.key"

  # Create repo file
  repo_file <- tempfile(fileext = ".repo")
  writeLines(repo_config, repo_file)

  # Install
  cmd1 <- sprintf("sudo cp %s /etc/yum.repos.d/charm.repo", repo_file)
  cmd2 <- "sudo yum install -y gum"

  message("Running: ", cmd1)
  result1 <- system(cmd1)

  message("Running: ", cmd2)
  result2 <- system(cmd2)

  if (result1 != 0 || result2 != 0) {
    message("\nInstallation failed. Please run these commands manually:")
    message(cmd1)
    message(cmd2)
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

#' Install gum on Arch Linux
.install_gum_arch <- function() {
  message("\nInstalling via pacman...")
  message("This will require sudo permissions.")

  result <- system("sudo pacman -S --noconfirm gum")

  if (result != 0) {
    message("\nInstallation failed. Please run manually:")
    message("  sudo pacman -S gum")
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

#' Install gum on Windows
.install_gum_windows <- function() {
  message("\nAttempting to install via winget...")

  # Try winget first
  if (nchar(Sys.which("winget")) > 0) {
    result <- system2("winget", c("install", "charmbracelet.gum"),
                      stdout = TRUE, stderr = TRUE)

    if (is.null(attr(result, "status")) || attr(result, "status") == 0) {
      return(invisible(TRUE))
    }
  }

  # Try scoop as fallback
  message("Winget failed or not available. Trying scoop...")
  if (nchar(Sys.which("scoop")) > 0) {
    result <- system2("scoop", c("install", "charm-gum"),
                      stdout = TRUE, stderr = TRUE)

    if (is.null(attr(result, "status")) || attr(result, "status") == 0) {
      return(invisible(TRUE))
    }
  }

  # Both failed
  message("\nAutomatic installation failed.")
  message("Please install gum manually using one of these methods:")
  message("  1. Windows Package Manager: winget install charmbracelet.gum")
  message("  2. Scoop: scoop install charm-gum")
  message("  3. Manual download: https://github.com/charmbracelet/gum/releases")

  invisible(FALSE)
}
