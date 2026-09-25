#' Configure the SPM installation path
#'
#' Validates an SPM installation directory and sets the `SPM_PATH`
#' environment variable. When no path is supplied, the user is prompted
#' to enter the path interactively.
#'
#' The function verifies that the selected directory contains the
#' `canonical/avg152T2.nii` SPM template file before setting the path.
#' The path is also written to the user's `.Renviron` file so that
#' `SPM_PATH` is available in future R sessions.
#'
#' @param path Character. Full path to the SPM installation directory.
#'   If `NULL`, the user is prompted to enter the path.
#'
#' @return Invisibly returns the normalized SPM path.
#'
#' @examples
#' \dontrun{
#' # Interactively select an SPM installation
#' set_spm_path()
#'
#' # Provide the path directly
#' set_spm_path("/Users/me/Documents/MATLAB/spm")
#' }
#'
#' @export

set_spm_path <- function(path = NULL) {
  # Prompt for path when none is supplied
  if (is.null(path)) {
    if (!interactive()) {
      cli::cli_abort(
        "No {.envvar SPM_PATH} was provided and R is not running interactively."
      )
    }

    repeat {
      path <- readline(
        prompt = "Enter the full path to the SPM folder: "
      )

      if (nzchar(path)) {
        break
      }

      cli::cli_alert_warning("Please enter a path.")
    }
  }

  path <- path.expand(path)

  # Validate the SPM directory
  if (!dir.exists(path)) {
    cli::cli_abort(
      "The specified directory does not exist: {.path {path}}"
    )
  }

  template_file <- file.path(path, "canonical", "avg152T2.nii")

  if (!file.exists(template_file)) {
    cli::cli_abort(c(
      "Could not locate the SPM canonical template.",
      "x" = "Expected: {.path {template_file}}",
      "i" = "Are you sure this is the SPM installation directory?"
    ))
  }

  tryCatch(
    {
      renviron <- path.expand("~/.Renviron")
      # Read existing .Renviron if it exists
      lines <- if (file.exists(renviron)) {
        readLines(renviron, warn = FALSE)
      } else {
        character()
      }

      # Replace an existing SPM_PATH entry
      spm_path <- sprintf("SPM_PATH='%s'", path)

      has_spm_path <- grepl("^\\s*SPM_PATH\\s*=", lines)

      if (any(has_spm_path)) {
        lines[has_spm_path] <- spm_path
      } else {
        lines <- c(lines, spm_path)
      }

      writeLines(lines, renviron)

      cli::cli_alert_success(
        "SPM_PATH configured: {.path {path}} and saved to the {.path ~/.Renviron} file."
      )
    },
    error = function(e) {
      cli::cli_alert_info(
        c(
          "SPM_PATH could not be saved to {.path ~/.Renviron}.\n",
          "You can edit {.path ~/.Renviron} manually with {.code usethis::edit_r_environ()}."
        )
      )
    }
  )
  invisible(path)
}
