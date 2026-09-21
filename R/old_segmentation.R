#' Coregister one or more source images to a reference image
#'
#' @param session A `matlab_session`, already set up via `matlab_setup_spm()`.
#' @param ref Character. Path to the reference image, OR a vector the same
#'   length as `sources`.
#' @param sources Character vector of source image paths to coregister.
#' @return The `session`, invisibly, with one log entry per source file.
#' @export

matlab_old_segmentation <- function(
  session,
  spm_path,
  sources
) {
  spm_path <- '/Users/bhelsel/Documents/MATLAB/spm'
  tpm <- c("grey", "white", "csf")

  tpm <- setNames(
    file.path(spm_path, "toolbox", "OldSeg", sprintf("%s.nii", tpm)),
    tpm
  )

  for (i in seq_along(sources)) {
    cli::cli_inform(
      "[{i}/{length(sources)}] Segmenting {.file {basename(sources[i])}}"
    )
    session <- matlab_step(
      session,
      sprintf("segmentation:%s", basename(sources[i])),
      sprintf(
        "  old_segmentation('%s', '%s', '%s', '%s');",
        sources[i],
        tpm["grey"],
        tpm["white"],
        tpm["csf"]
      )
    )
  }

  invisible(session)
}
