#' Internal: run a MATLAB expression on a session, capturing errors into the log
#'
#' Any new pipeline step (your `other_matlab_scripts()`, etc.) should call
#' this internally rather than `R.matlab::evaluate()` directly — it gives
#' every step the same try/catch + logging behavior for free.
#'
#' @param session A `matlab_session`.
#' @param step_name Character. Label for this step in the log.
#' @param cmd Character. MATLAB code to run (may be multi-line).
#' @return The `session`, invisibly, with a new log entry appended.
matlab_step <- function(session, step_name, cmd) {
  stopifnot(inherits(session, "matlab_session"))

  wrapped <- sprintf(
    "try\n%s\n  step_err = '';\ncatch ME\n  step_err = getReport(ME);\nend",
    cmd
  )
  R.matlab::evaluate(session$client, wrapped)
  err <- R.matlab::getVariable(session$client, "step_err")$step_err

  if (length(err) == 0) {
    err <- ""
  }

  if (nzchar(err)) {
    cli::cli_warn("{step_name} failed:\n{err}")
    status <- "error"
  } else {
    status <- "ok"
  }

  session$log[[length(session$log) + 1]] <- list(
    step = step_name,
    status = status,
    message = if (nzchar(err)) err else NA_character_
  )

  invisible(session)
}

# get_value <- function(value) {
#   tmp_mat_file <- tempfile(fileext = ".mat")
#   cmd <- sprintf("save('%s', '%s');", tmp_mat_file, value)
#   R.matlab::evaluate(session, cmd)
#   results <- R.matlab::readMat(tmp_mat_file)
#   unlink(tmp_mat_file)
#   return(results)
# }

sort_scans <- function(files) {
  ids <- sub("_.*$", "", basename(files))
  scans <- sub("^[^_]+_", "", sub("\\.nii$", "", basename(files)))
  data.frame(ids, scans, files)
}


identify_modality <- function(file, mri = c(), pet = c()) {
  mri <- paste0("mr|mri|t1|t2|flair", mri, collapse = "|")
  pet <- paste0("pet|pib|fdg|amyloid", pet, collapse = "|")
  name <- tolower(basename(file))

  if (grepl(pet, name)) {
    return("PET")
  } else if (grepl(mri, name)) {
    return("MRI")
  } else {
    return(NA_character_)
  }
}


replace_last_data_directory <- function(paths, from = "data", to) {
  purrr::map_chr(paths, function(path) {
    parts <- strsplit(path, "/", fixed = TRUE)[[1]]
    i <- tail(which(parts == from), 1)
    if (length(i) == 0) {
      return(path)
    }
    parts[i] <- paste(to, collapse = "/")
    paste(parts, collapse = "/")
  })
}
