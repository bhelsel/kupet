#' Coregister one or more source images to a reference image
#'
#' @param session A `matlab_session`, already set up via `matlab_setup_spm()`.
#' @param ref Character. Path to the reference image, OR a vector the same
#'   length as `sources`.
#' @param sources Character vector of source image paths to coregister.
#' @return The `session`, invisibly, with one log entry per source file.
#' @export

matlab_old_normalization <- function(
  session,
  mri,
  pet,
  seg_mat
) {
  stopifnot(length(mri) == length(pet))

  for (i in seq_along(mri)) {
    cli::cli_inform(
      "[{i}/{length(mri)}] Normalizing {.file {basename(mri[i])} and {.file {basename(pet[i])}}}"
    )

    session <- matlab_step(
      session,
      sprintf("normalizing: %s & %s", basename(mri[i]), basename(pet[i])),
      sprintf(
        "  old_normalization('%s', '%s', '%s');",
        seg_mat[i],
        mri[i],
        pet[i]
      )
    )
  }

  invisible(session)
}
