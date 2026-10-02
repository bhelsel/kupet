spm_pet_smoothing <- function(session, pet) {
  for (i in seq_along(pet)) {
    cli::cli_inform(
      "[{i}/{length(pet)}] Smoothing {.file {basename(pet[i])}}"
    )

    session <- matlab_step(
      session,
      sprintf("smoothing: %s", basename(pet[i])),
      sprintf("  pet_smoothing('%s');", pet[i])
    )
  }

  invisible(session)
}
