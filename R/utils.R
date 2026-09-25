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


check_dirs <- function(datadir, outputdir) {
  if (missing(datadir) || length(datadir) != 1 || !nzchar(datadir)) {
    stop("Argument 'datadir' must be a single path.", call. = FALSE)
  }
  if (missing(outputdir) || length(outputdir) != 1 || !nzchar(outputdir)) {
    stop("Argument 'outputdir' must be a single path.", call. = FALSE)
  }
  if (!dir.exists(datadir)) {
    stop("datadir does not exist: ", datadir, call. = FALSE)
  }

  dat <- normalizePath(datadir, mustWork = TRUE)

  out <- normalizePath(outputdir, mustWork = FALSE)

  if (identical(dat, out) || startsWith(out, paste0(dat, .Platform$file.sep))) {
    stop(
      "outputdir must NOT equal or be a subdirectory of datadir.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

create_output_dirs <- function(outputdir) {
  outputdir <- normalizePath(outputdir, mustWork = FALSE)
  dirs <- list(
    data = file.path(outputdir, "data"),
    results = file.path(outputdir, "results")
  )
  stages <- c("centered", "coregister", "segmentation", "normalization")
  dirs$stages <- stats::setNames(file.path(dirs$results, stages), stages)

  purrr::walk(
    c(dirs$data, dirs$results, dirs$stages),
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
  dirs
}

check_file_range <- function(
  files,
  by = c("participant", "scan"),
  f0,
  f1
) {
  by <- match.arg(by)

  directories <- unique(dirname(files))

  if (f1 == 0) {
    if (by == "participant") {
      f1 <- length(directories)
    }

    if (by == "scan") {
      f1 <- min(
        purrr::map_int(directories, \(x) length(list.files(x))),
        na.rm = TRUE
      )
    }
  }

  if (f0 < 1 || f0 > length(files)) {
    stop("`f0` (", f0, ") is out of range 1:", length(files))
  }

  if (f1 < f0 || f1 > length(files)) {
    stop("`f1` (", f1, ") is out of range ", f0, ":", length(files))
  }

  files <- switch(
    by,
    participant = purrr::list_c(
      purrr::map(directories, \(x) {
        f <- list.files(x, full.names = TRUE)
      })
    ),
    scan = purrr::list_c(
      purrr::map(directories, \(x) {
        f <- list.files(x, full.names = TRUE)
        f[f0:f1]
      })
    )
  )

  return(files)
}


build_output_directory_key <- function(data_files, dirs) {
  if (length(data_files) == 0) {
    stop("No imaging files found in ", dirs$data, call. = FALSE)
  }

  raw <- normalizePath(data_files, mustWork = TRUE)
  stage <- function(name) dirs$stages[[name]]

  output_directory_key <- tibble::tibble(
    id = basename(dirname(raw)), # files are organized by participant
    filename = basename(raw),
    modality = purrr::map_chr(raw, identify_modality),
    raw = raw
  ) |>
    dplyr::mutate(
      centered = file.path(stage("centered"), id, filename),
      coregister = file.path(stage("coregister"), id, filename),
      segmentation = dplyr::if_else(
        modality == "MRI",
        file.path(stage("segmentation"), id, filename),
        NA_character_
      ),
      # derived from the MRI name instead of globbing the directory
      seg_sn_mat = dplyr::if_else(
        modality == "MRI",
        sub("\\.nii(\\.gz)?$", "_seg_sn.mat", segmentation),
        NA_character_
      ),
      normalization = file.path(stage("normalization"), id, filename),
      # SPM prepends "w" to normalized outputs
      normalized = file.path(dirname(normalization), paste0("w", filename))
    )

  validate_output_directory_key(output_directory_key)

  tidyr::pivot_wider(
    output_directory_key,
    names_from = modality,
    values_from = filename:normalized,
    names_repair = \(x) tolower(x)
  )
}

validate_output_directory_key <- function(manifest) {
  modality <- factor(manifest$modality, levels = c("MRI", "PET"))
  if (anyNA(modality)) {
    stop(
      "Unrecognized modality for: ",
      paste(manifest$raw[is.na(modality)], collapse = ", "),
      call. = FALSE
    )
  }

  counts <- table(manifest$id, modality)
  bad <- rownames(counts)[counts[, "MRI"] != 1 | counts[, "PET"] != 1]
  if (length(bad) > 0) {
    stop(
      "Each participant needs exactly one MRI and one PET. Check: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


copy_files <- function(from, to, overwrite = FALSE) {
  purrr::walk(dirname(to), \(x) {
    if (!dir.exists(x)) {
      dir.create(x, recursive = TRUE)
    }
  })
  invisible(
    file.copy(
      from = from,
      to = to,
      overwrite = overwrite
    )
  )
}
