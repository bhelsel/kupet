# devtools::load_all()
# directories <- "/Users/bhelsel/Desktop/ABCDS/Centiloid/ABC-DS-SCANS/ABC-DS"
# output_dir <- "/Users/bhelsel/Desktop/ABCDS/Centiloid/ABC-DS-SCANS/PET"
# spm_reslice_mean(list.files(directories, full.names = TRUE), output_dir, expected_files = 4)

spm_reslice_mean <- function(
  directories,
  output_dir,
  expected_files,
  scan = "PET"
) {
  session <- matlab_start_server()
  on.exit(matlab_close_server(session), add = TRUE) # register immediately
  session <- matlab_setup_spm(session, Sys.getenv("SPM_PATH"))

  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  for (i in seq_along(directories)) {
    cli::cli_inform(
      "[{i}/{length(directories)}] Realigning and Reslicing {.file {directories[i]}}"
    )

    files <- list.files(
      directories[i],
      pattern = "\\.nii$",
      full.names = TRUE,
      recursive = TRUE
    )

    files <- files[!grepl("^r", basename(files))]

    files <- normalizePath(files, mustWork = TRUE)

    stopifnot(length(files) == expected_files)

    output_file <- paste0(
      strsplit(basename(files[i]), "_")[[1]][1],
      "_",
      scan
    )

    session <- matlab_step(
      session,
      sprintf("reslicing: %s", output_file),
      sprintf(
        "  reslice_mean('%s', '%s', '%s');",
        paste0(files, collapse = "|"),
        output_dir,
        output_file
      )
    )
  }
  invisible(session)
}
