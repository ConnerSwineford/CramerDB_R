# Gum CLI Integration Utilities
# Provides beautiful terminal output with graceful fallback when gum is not available

# Azure blue color theme
.gum_colors <- list(
  primary = "#007FFF",      # Azure blue
  success = "#00D084",      # Green
  warning = "#FFB800",      # Orange
  error = "#FF4444",        # Red
  muted = "#6C757D"         # Gray
)

#' Check if gum is available on the system
#' @return Logical indicating if gum CLI is installed
.has_gum <- function() {
  gum_path <- Sys.which("gum")
  return(nzchar(gum_path))
}

#' Execute gum command
#' @param args Character vector of gum arguments
#' @param input Optional input to pipe to gum
#' @return Output from gum command
.gum <- function(args, input = NULL) {
  if (!.has_gum()) {
    return(NULL)
  }

  if (!is.null(input)) {
    result <- system2("gum", args, input = input, stdout = TRUE, stderr = FALSE)
  } else {
    result <- system2("gum", args, stdout = TRUE, stderr = FALSE)
  }

  return(result)
}

#' Style text with gum
#' @param text Text to style
#' @param color Color (hex code or named color)
#' @param bold Whether to bold the text
#' @param italic Whether to italicize the text
#' @param underline Whether to underline the text
#' @return Styled text or plain text if gum unavailable
.gum_style <- function(text, color = NULL, bold = FALSE, italic = FALSE, underline = FALSE) {
  if (!.has_gum()) {
    return(text)
  }

  args <- c("style")

  if (!is.null(color)) {
    args <- c(args, "--foreground", color)
  }
  if (bold) {
    args <- c(args, "--bold")
  }
  if (italic) {
    args <- c(args, "--italic")
  }
  if (underline) {
    args <- c(args, "--underline")
  }

  args <- c(args, text)

  result <- .gum(args)
  if (is.null(result)) {
    return(text)
  }
  return(paste(result, collapse = "\n"))
}

#' Create a styled header with gum
#' @param text Header text
#' @param width Width of the header (default: 60)
#' @return Styled header
.gum_header <- function(text, width = 60) {
  if (!.has_gum()) {
    # Fallback: simple header with equals signs
    cat("\n", text, "\n")
    cat(paste(rep("=", nchar(text)), collapse = ""), "\n\n", sep = "")
    return(invisible(NULL))
  }

  # Create styled header with gum
  styled_text <- .gum_style(text, color = .gum_colors$primary, bold = TRUE)
  cat("\n", styled_text, "\n", sep = "")

  # Add separator line
  separator <- .gum_style(paste(rep("─", min(nchar(text), width)), collapse = ""),
                         color = .gum_colors$primary)
  cat(separator, "\n\n", sep = "")

  invisible(NULL)
}

#' Format key-value pair with gum styling
#' @param key Key name
#' @param value Value
#' @param key_color Color for key (default: primary)
#' @return Formatted output
.gum_kv <- function(key, value, key_color = .gum_colors$primary) {
  if (!.has_gum()) {
    cat(sprintf("  %-15s %s\n", paste0(key, ":"), value))
    return(invisible(NULL))
  }

  styled_key <- .gum_style(paste0(key, ":"), color = key_color, bold = TRUE)
  cat("  ", styled_key, " ", value, "\n", sep = "")

  invisible(NULL)
}

#' Create a spinner for long-running operations
#' @param title Spinner title/message
#' @return Process handle or NULL
.gum_spinner_start <- function(title = "Loading") {
  if (!.has_gum()) {
    cat(title, "...", sep = "")
    flush.console()
    return(NULL)
  }

  # Start spinner in background
  args <- c("spin", "--title", title, "--spinner", "dot",
            "--spinner.foreground", .gum_colors$primary,
            "--title.foreground", .gum_colors$primary,
            "--", "sleep", "999")

  # Start background process
  proc <- system2("gum", args, wait = FALSE, stdout = FALSE, stderr = FALSE)

  return(proc)
}

#' Stop a running spinner
#' @param proc Process handle from .gum_spinner_start
.gum_spinner_stop <- function(proc) {
  if (is.null(proc)) {
    cat(" done\n")
    return(invisible(NULL))
  }

  # Kill the spinner process
  tools::pskill(proc)

  invisible(NULL)
}

#' Show a success message
#' @param message Success message
.gum_success <- function(message) {
  if (!.has_gum()) {
    cat("✓", message, "\n")
    return(invisible(NULL))
  }

  styled <- .gum_style(paste("✓", message), color = .gum_colors$success, bold = TRUE)
  cat(styled, "\n", sep = "")

  invisible(NULL)
}

#' Show an error message
#' @param message Error message
.gum_error <- function(message) {
  if (!.has_gum()) {
    cat("✗", message, "\n")
    return(invisible(NULL))
  }

  styled <- .gum_style(paste("✗", message), color = .gum_colors$error, bold = TRUE)
  cat(styled, "\n", sep = "")

  invisible(NULL)
}

#' Show a warning message
#' @param message Warning message
.gum_warning <- function(message) {
  if (!.has_gum()) {
    cat("⚠", message, "\n")
    return(invisible(NULL))
  }

  styled <- .gum_style(paste("⚠", message), color = .gum_colors$warning, bold = TRUE)
  cat(styled, "\n", sep = "")

  invisible(NULL)
}

#' Create a progress bar for batch operations
#' @param current Current progress
#' @param total Total items
#' @param message Optional message
.gum_progress <- function(current, total, message = "") {
  if (!.has_gum()) {
    # Simple text progress
    pct <- round(100 * current / total)
    if (message != "") {
      cat(sprintf("\r%s [%d/%d] %d%%", message, current, total, pct))
    } else {
      cat(sprintf("\r[%d/%d] %d%%", current, total, pct))
    }
    flush.console()

    if (current == total) {
      cat("\n")
    }
    return(invisible(NULL))
  }

  # Use gum style for inline progress
  pct <- round(100 * current / total)
  bar_width <- 30
  filled <- round(bar_width * current / total)
  empty <- bar_width - filled

  bar <- paste0(
    .gum_style(paste(rep("█", filled), collapse = ""), color = .gum_colors$primary),
    paste(rep("░", empty), collapse = "")
  )

  if (message != "") {
    progress_text <- sprintf("\r%s [%d/%d] %s %d%%", message, current, total, bar, pct)
  } else {
    progress_text <- sprintf("\r[%d/%d] %s %d%%", current, total, bar, pct)
  }

  cat(progress_text)
  flush.console()

  if (current == total) {
    cat("\n")
  }

  invisible(NULL)
}

#' Format a table with gum styling
#' @param data Data frame or list of key-value pairs
#' @param header Optional header text
#' @return Formatted table
.gum_table <- function(data, header = NULL) {
  if (!.has_gum()) {
    # Fallback: simple formatting
    if (!is.null(header)) {
      cat("\n", header, "\n", sep = "")
    }

    if (is.data.frame(data)) {
      print(data)
    } else if (is.list(data)) {
      # Assume named list
      for (name in names(data)) {
        cat(sprintf("  %-20s %s\n", name, data[[name]]))
      }
    }

    return(invisible(NULL))
  }

  # Show header if provided
  if (!is.null(header)) {
    .gum_header(header)
  }

  # Format as table
  if (is.list(data) && !is.data.frame(data)) {
    # Named list - format as key-value pairs
    for (name in names(data)) {
      styled_name <- .gum_style(sprintf("  %-20s", name),
                                color = .gum_colors$primary,
                                bold = TRUE)
      cat(styled_name, data[[name]], "\n", sep = "")
    }
  }

  invisible(NULL)
}

#' Confirm an action interactively
#' @param prompt Confirmation prompt
#' @param default Default response ("yes" or "no")
#' @return Logical indicating user's choice
.gum_confirm <- function(prompt, default = "no") {
  if (!.has_gum()) {
    # Fallback: readline
    response <- readline(paste0(prompt, " (yes/no): "))
    return(tolower(response) %in% c("y", "yes"))
  }

  args <- c("confirm", prompt,
            "--default", if(default == "yes") "true" else "false",
            "--prompt.foreground", .gum_colors$primary)

  result <- system2("gum", args)

  # Exit code 0 means confirmed, anything else means no
  return(result == 0)
}

#' Choose from a list of options
#' @param options Character vector of options
#' @param prompt Optional prompt message
#' @param multiple Allow multiple selections
#' @return Selected option(s) or NULL if cancelled
.gum_choose <- function(options, prompt = "Choose an option:", multiple = FALSE) {
  if (!.has_gum()) {
    # Fallback: menu or readline
    cat(prompt, "\n")
    for (i in seq_along(options)) {
      cat(sprintf("  %d) %s\n", i, options[i]))
    }

    response <- readline("Enter number: ")
    idx <- suppressWarnings(as.integer(response))

    if (is.na(idx) || idx < 1 || idx > length(options)) {
      return(NULL)
    }

    return(options[idx])
  }

  # Use gum choose
  args <- c("choose")

  if (multiple) {
    args <- c(args, "--no-limit")
  }

  if (!is.null(prompt) && prompt != "") {
    args <- c(args, "--header", prompt)
  }

  args <- c(args,
            "--cursor.foreground", .gum_colors$primary,
            "--selected.foreground", .gum_colors$primary)

  result <- .gum(args, input = options)

  if (is.null(result) || length(result) == 0) {
    return(NULL)
  }

  return(result)
}

#' Filter/search through options
#' @param options Character vector of options to filter
#' @param prompt Optional prompt message
#' @return Selected option or NULL if cancelled
.gum_filter <- function(options, prompt = "Search:") {
  if (!.has_gum()) {
    # Fallback: just show all options and use menu
    return(.gum_choose(options, prompt = prompt, multiple = FALSE))
  }

  args <- c("filter",
            "--placeholder", prompt,
            "--indicator.foreground", .gum_colors$primary,
            "--match.foreground", .gum_colors$primary)

  result <- .gum(args, input = options)

  if (is.null(result) || length(result) == 0) {
    return(NULL)
  }

  return(result[1])
}
