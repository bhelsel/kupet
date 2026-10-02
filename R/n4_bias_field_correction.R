n4_bias_field_correction <- function(files, overwrite = FALSE, ...) {
  n4_bias_corrected_files <- furrr::future_walk(
    files,
    \(x) {
      if (!file.exists(x) || overwrite) {
        image <- ants$image_read(x)
        corrected_image <- ants$n4_bias_field_correction(image, ...)
        ants$image_write(corrected_image, filename = x)
      }
    },
    .options = furrr::furrr_options(seed = NULL)
  )
  NULL
}
