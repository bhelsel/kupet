process_centiloid <- function(
  datadir,
  outputdir,
  steps = 1:6,
  spm_path = Sys.getenv("SPM_PATH"),
  f0 = 1,
  f1 = 0,
  doParallel = TRUE,
  verbose = TRUE,
  ...
) {
  if (Sys.getenv("SPM_PATH") == "") {
    cli::cli_abort(c(
      "No {.envvar SPM_PATH} detected in your R environment.",
      "i" = "Run {.code set_spm_path()} to configure SPM.",
      "i" = "You can also provide the SPM directory manually:",
      " " = "{.code set_spm_path('/path/to/spm')}"
    ))
  }

  steps <- sort(unique(steps))
  stopifnot(all(steps %in% 1:6))
  check_dirs(datadir, outputdir)
  dirs <- create_output_dirs(outputdir)

  data_files <- copy_imaging_files(
    datadir = datadir,
    output_datadir = dirs$data,
    f0 = f0,
    f1 = f1,
    files_organized_by = "scan",
    organize_files_by = "participant"
  )

  output_key <- build_output_directory_key(data_files[["files"]], dirs)

  if (any(steps %in% 2:5)) {
    session <- matlab_start_server()
    on.exit(matlab_close_server(session), add = TRUE) # register immediately
    session <- matlab_setup_spm(session, Sys.getenv("SPM_PATH"))
  }

  if (doParallel) {
    future::plan(future::multisession, workers = future::availableCores() - 1)
  }

  # Stage 1: Calculate Center of Mass on PET Images
  if (any(steps == 1)) {
    center_of_mass <- get_center_of_mass(
      datadir = dirs$data,
      f0 = f0,
      f1 = f1,
      outputdir = dirs$stages[["centered"]],
      write = TRUE
    )
  }

  # Stage 2: Coregister MRI to Template
  if (any(steps == 2)) {
    copy_files(from = output_key$centered_mri, to = output_key$coregister_mri)

    avg152T1 <- file.path(Sys.getenv("SPM_PATH"), "canonical", "avg152T1.nii")

    session <- spm_coregister(
      session,
      ref = avg152T1,
      sources = output_key$coregister_mri
    )
  }

  # Stage 3: Coregister PET to MRI
  if (any(steps == 3)) {
    copy_files(from = output_key$centered_pet, to = output_key$coregister_pet)

    session <- spm_coregister(
      session,
      ref = output_key$coregister_mri,
      sources = output_key$coregister_pet
    )
  }

  # Mode 4: Apply segmentation to MRI images
  if (any(steps == 4)) {
    copy_files(
      from = output_key$coregister_mri,
      to = output_key$segmentation_mri
    )

    session <- matlab_old_segmentation(
      session,
      sources = output_key$segmentation_mri
    )
  }

  # Mode 5: Apply normalization to MRI and PET images
  if (any(steps == 5)) {
    copy_files(
      from = output_key$coregister_pet,
      to = output_key$normalization_pet
    )

    copy_files(
      from = output_key$segmentation_mri,
      to = output_key$normalization_mri
    )

    session <- matlab_old_normalization(
      session,
      mri = output_key$normalization_mri,
      pet = output_key$normalization_pet,
      seg_mat = output_key$seg_sn_mat_mri
    )
  }

  # Mode 6: Calculate Centiloid Values
  if (any(steps == 6)) {
    centiloids <- furrr::future_map2_dfr(
      output_key$normalized_pet,
      output_key$id,
      ~ get_centiloid(.x, .y)
    ) |>
      tidyr::pivot_wider(names_from = MASK, values_from = c(SUVR, CENTILOID))
  }

  return(centiloids)
}

# datadir <- "/Users/bhelsel/Desktop/Centiloid/GAAIN/AD-100-DATA"
# outputdir <- "/Users/bhelsel/Desktop/Centiloid/GAAIN/AD-100-PROC"
# devtools::load_all()
# process_centiloid(datadir, outputdir, steps = 6)

# datadir <- "/Users/bhelsel/Desktop/Centiloid/GAAIN/YC-0-DATA"
# outputdir <- "/Users/bhelsel//Desktop/Centiloid/GAAIN/YC-0-PROC"
# centiloids <- process_centiloid(datadir, outputdir, steps = 6, f0 = 1, f1 = 0)

# undebug(get_centiloid)
# get_centiloid("/Users/bhelsel/Desktop/Centiloid/GAAIN/YC-0-PROC/results/normalization/YC102/wYC102_PiB_5070.nii", id = "YC102")
# get_centiloid("/Users/bhelsel/Desktop/Centiloid/GAAIN/YC-0-PROC/results/normalization/YC129/wYC129_PiB_5070.nii", id = "YC129")

#' Next step is to test f0 and f1 and overwrite arguments.
#' Need to avoid segmentation if overwite is FALSE and there is a seg file as it is an expensive operation
#' Then, document and push to GitHub as initial version.
