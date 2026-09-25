#' Copy NIfTI imaging files into an organized directory structure
#'
#' Copies NIfTI files (`.nii` or `.nii.gz`) from a source directory into a
#' destination directory, organized either by participant (using a regex
#' `pattern` to extract a participant ID from each filename) or by scan type
#' (`mri`/`pet`, placed into a single subfolder).
#'
#' @param datadir Character. Path to the source directory containing imaging
#'   files.
#' @param output_datadir Character. Path to the destination directory.
#' @param f0 	Numeric (default = 1). File index to start with (default = 1).
#' Index refers to the filenames sorted in alphabetical order.
#' @param f1 Integer. Index of the last file to copy. If `0` (the default),
#'   all files from `f0` to the end of the listing are copied.
#' @param organize_files_by Character. How to organize copied files: `"participant"`
#'   (default) creates one subfolder per participant, derived from
#'   `pattern`; `"scan"` copies all files into a single subfolder named
#'   after `scan`.
#' @param overwrite Logical. Overwrite files that already exist at the
#'   destination? Default `FALSE`.
#'
#' @return Invisibly, a character vector of the destination paths that were
#'   copied (files that were skipped because they already existed and
#'   `overwrite = FALSE` are not included).
#'
#' @examples
#' \dontrun{
#' # Copy all files, organized into one folder per participant
#' copy_imaging_files(
#'   from    = "raw_scans",
#'   to      = "sorted_scans",
#'   by      = "participant",
#'   pattern = "^(sub-[0-9]+)"
#' )
#'
#' # Copy only the first 10 files, organized by scan type
#' copy_imaging_files(
#'   from = "raw_scans",
#'   to   = "sorted_scans",
#'   f1   = 10,
#'   by   = "scan",
#'   scan = "pet"
#' )
#' }
#'
#' @export

# Expects ID_SCAN--.nii

copy_imaging_files <- function(
  datadir,
  output_datadir,
  f0 = 1,
  f1 = 0,
  files_organized_by = c("scan", "participant"),
  organize_files_by = c("participant", "scan"),
  overwrite = FALSE
) {
  organize_files_by <- match.arg(organize_files_by)
  files_organized_by <- match.arg(files_organized_by)

  # --- input validation ----------------------------------------------------
  if (!dir.exists(datadir)) {
    stop("`datadir` directory does not exist: ", datadir)
  }

  # match .nii or .nii.gz, anchored to the end of the filename
  files <- list.files(
    path = datadir,
    pattern = "\\.nii(\\.gz)?$",
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(files) == 0) {
    warning("No .nii/.nii.gz files found in ", from)
    return(invisible(character(0)))
  }

  files <- check_file_range(files, by = files_organized_by, f0, f1)

  files_df <- tidyr::pivot_wider(
    sort_scans(files),
    names_from = "scans",
    values_from = "files"
  )

  exists <- character(0)

  copied <- character(0)

  if (organize_files_by == "participant") {
    persondirs <- file.path(output_datadir, unique(files_df$ids))
    purrr::walk(persondirs, \(x) {
      if (!dir.exists(x)) {
        dir.create(x, recursive = TRUE)
      }
    })

    for (i in seq_along(files_df$ids)) {
      for (n in 2:ncol(files_df)) {
        files_from <- files_df[i, n, drop = TRUE]
        files_to <- file.path(
          output_datadir,
          files_df$ids[i],
          basename(files_df[i, n, drop = TRUE])
        )
        for (f in seq_along(files_to)) {
          if (file.exists(files_to[f])) {
            exists <- c(exists, files_to[f])
          }
          if (!file.exists(files_to[f]) || overwrite) {
            ok <- file.copy(
              from = files_from[f],
              to = files_to[f],
              overwrite = overwrite
            )
            if (ok) copied <- c(copied, files_to[f])
          }
        }
      }
    }
  }

  if (organize_files_by == "scan") {
    scans <- colnames(files_df)[which(colnames(files_df) != "ids")]
    scandirs <- file.path(output_datadir, scans)
    purrr::walk(scandirs, \(x) {
      if (!dir.exists(x)) {
        dir.create(x, recursive = TRUE)
      }
    })

    for (n in 2:ncol(files_df)) {
      s <- colnames(files_df)[n]
      files_from <- files_df[[s]]
      files_to <- file.path(output_datadir, s, basename(files_df[[s]]))
      for (f in seq_along(files_to)) {
        if (file.exists(files_to[f])) {
          exists <- c(exists, files_to[f])
        }
        if (!file.exists(files_to[f]) || overwrite) {
          ok <- file.copy(
            from = files_from[f],
            to = files_to[f],
            overwrite = overwrite
          )
          if (ok) copied <- c(copied, files_to[f])
        }
      }
    }
  }

  invisible(
    list(
      files = sort(c(exists, copied)),
      copied = copied
    )
  )
}
