#' Coregister one or more source images to a reference image
#'
#' @param session A `matlab_session`, already set up via `matlab_setup_spm()`.
#' @param ref Character. Path to the reference image, OR a vector the same
#'   length as `sources`.
#' @param sources Character vector of source image paths to coregister.
#' @return The `session`, invisibly, with one log entry per source file.
#' @export

spm_coregister <- function(
  session,
  ref,
  sources
) {
  if (length(ref) == 1) {
    ref <- rep(ref, length(sources))
  }
  stopifnot(length(ref) == length(sources))

  for (i in seq_along(sources)) {
    cli::cli_inform(
      "[{i}/{length(sources)}] Coregistering {.file {basename(sources[i])}}"
    )
    session <- matlab_step(
      session,
      sprintf("coregister:%s", basename(sources[i])),
      sprintf("  coregister('%s', '%s');", ref[i], sources[i])
    )
  }

  invisible(session)
}
