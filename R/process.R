process_centiloid <- function(
  mode = 1:6,
  datadir,
  outputdir,
  studyname = c(),
  f0 = 1,
  f1 = 0,
  configfile,
  verbose = TRUE,
  ...
) {
  if (length(datadir) == 0) {
    stop("\nVariable datadir is not specified")
  }
  if (length(outputdir) == 0) {
    stop("\nVariable outputdir is not specified")
  }

  if (datadir == outputdir || grepl(paste(datadir, "/", sep = ""), outputdir)) {
    stop(paste0(
      "\nError: The file path specified by argument outputdir should ",
      "NOT equal or be a subdirectory of the path specified by argument datadir"
    ))
  }

  if (!dir.exists(outputdir)) {
    purrr::walk(
      file.path(outputdir, c("data", "results")),
      ~ dir.create(.x, recursive = TRUE)
    )
  }

  resultsdir <- file.path(outputdir, "results")

  output_datadir <- file.path(outputdir, "data")

  if (dir.exists(resultsdir)) {
    subfolders <- c("centered", "coregister", "segmentation", "normalization")
    purrr::walk(subfolders, \(x) {
      if (!dir.exists(file.path(resultsdir, x))) {
        dir.create(file.path(resultsdir, x))
      }
    })
  }

  data_files <- copy_imaging_files(
    datadir = datadir,
    output_datadir = output_datadir,
    f0 = f0,
    f1 = f1,
    organize_files_by = "participant",
    ...
  )

  # Mode 1: Calculate Center of Mass on PET Images
  centered_files <- replace_last_data_directory(
    data_files,
    to = "results/centered"
  )

  center_of_mass <- data.frame()

  for (f in seq_along(data_files)) {
    if (!dir.exists(dirname(centered_files[f]))) {
      dir.create(dirname(centered_files[f]))
    }
    invisible(file.copy(data_files[f], centered_files[f], overwrite = TRUE))
    com <- calculate_center_of_mass(centered_files[f], write = TRUE)
    center_of_mass <- rbind(
      center_of_mass,
      data.frame(
        id = basename(centered_files[f]),
        X = com[1],
        Y = com[2],
        Z = com[3]
      )
    )
  }

  session <- matlab_start_server()

  session <- matlab_setup_spm(
    session,
    spm_path = "/Users/bhelsel/Documents/MATLAB/spm"
  )

  # Mode 2: Coregister MRI to Template
  coregister_files <- replace_last_data_directory(
    data_files,
    to = "results/coregister"
  )

  for (f in seq_along(centered_files)) {
    if (!dir.exists(dirname(coregister_files[f]))) {
      dir.create(dirname(coregister_files[f]))
    }
    invisible(file.copy(
      centered_files[f],
      coregister_files[f],
      overwrite = TRUE
    ))
  }

  coregister_file_types <- purrr::map_chr(
    coregister_files,
    ~ identify_modality(.x)
  )

  session <- spm_coregister(
    session,
    ref = "/Users/bhelsel/Documents/MATLAB/spm/canonical/avg152T1.nii",
    sources = coregister_files[coregister_file_types == "MRI"]
  )

  # Mode 3: Coregister PET to MRI

  session <- spm_coregister(
    session,
    ref = coregister_files[coregister_file_types == "MRI"],
    sources = coregister_files[coregister_file_types == "PET"]
  )

  # Mode 4: Apply segmentation to MRI images

  segmentation_files <- replace_last_data_directory(
    data_files,
    to = "results/segmentation"
  )

  segment_file_types <- purrr::map_chr(
    segmentation_files,
    ~ identify_modality(.x)
  )

  coregister_mri_files <- coregister_files[coregister_file_types == "MRI"]

  segmentation_mri_files <- segmentation_files[segment_file_types == "MRI"]

  for (f in seq_along(segmentation_mri_files)) {
    if (!dir.exists(dirname(segmentation_mri_files[f]))) {
      dir.create(dirname(segmentation_mri_files[f]))
    }
    invisible(file.copy(
      coregister_mri_files[f],
      segmentation_mri_files[f],
      overwrite = TRUE
    ))
  }

  session <- matlab_old_segmentation(
    session,
    spm_path = "/Users/bhelsel/Documents/MATLAB/spm",
    sources = segmentation_mri_files
  )

  # Mode 5: Apply normalization to MRI and PET images

  normalized_files <- replace_last_data_directory(
    data_files,
    to = "results/normalization"
  )

  normalized_file_types <- purrr::map_chr(
    normalized_files,
    ~ identify_modality(.x)
  )

  normalized_mri_files <- normalized_files[normalized_file_types == "MRI"]

  normalized_pet_files <- normalized_files[normalized_file_types == "PET"]

  coregister_pet_files <- coregister_files[coregister_file_types == "PET"]

  segmentation_mat_files <- list.files(
    dirname(segmentation_mri_files),
    pattern = "seg_sn.mat",
    full.names = TRUE
  )

  for (f in seq_along(normalized_mri_files)) {
    if (!dir.exists(dirname(normalized_mri_files[f]))) {
      dir.create(dirname(normalized_mri_files[f]))
    }

    invisible(file.copy(
      segmentation_mri_files[f],
      normalized_mri_files[f],
      overwrite = TRUE
    ))

    invisible(file.copy(
      segmentation_mat_files[f],
      file.path(
        dirname(normalized_mri_files[f]),
        basename(segmentation_mat_files[f])
      ),
      overwrite = TRUE
    ))
  }

  for (f in seq_along(normalized_pet_files)) {
    if (!dir.exists(dirname(normalized_pet_files[f]))) {
      dir.create(dirname(normalized_pet_files[f]))
    }

    invisible(file.copy(
      coregister_pet_files[f],
      normalized_pet_files[f],
      overwrite = TRUE
    ))
  }

  session <- matlab_old_normalization(
    session,
    mri = normalized_mri_files,
    pet = normalized_pet_files
  )

  # Mode 6: Calculate Centiloid Values

  ids <- basename(dirname(normalized_pet_files))

  normalized_pet_files <- file.path(
    dirname(normalized_pet_files),
    paste0("w", basename(normalized_pet_files))
  )

  centiloids <- purrr::map2_dfr(
    normalized_pet_files,
    ids,
    ~ get_centiloid(.x, .y)
  ) |>
    tidyr::pivot_wider(names_from = MASK, values_from = c(SUVR, CENTILOID))

  on.exit(matlab_close_server(session))

  return(centiloids)
}
