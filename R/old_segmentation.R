#' Coregister one or more source images to a reference image
#'
#' @param session A `matlab_session`, already set up via `matlab_setup_spm()`.
#' @param sources Character vector of source mri image paths to segment.
#' @return The `session`, invisibly, with one log entry per source file.
#' @export

matlab_old_segmentation <- function(
  session,
  sources
) {
  old_seg <- file.path(Sys.getenv("SPM_PATH"), "toolbox", "OldSeg")
  matter <- c("grey", "white", "csf")
  old_seg_paths <- file.path(old_seg, sprintf("%s.nii", matter))
  tpm <- setNames(old_seg_paths, matter)

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
