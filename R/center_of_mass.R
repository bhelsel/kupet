#' Set a NIfTI image's origin to its center of mass
#'
#' Computes an approximate center of mass for a NIfTI image — the centroid
#' of voxels whose intensity exceeds the image mean — and rewrites the
#' image's sform rows so that point becomes the new origin.
#'
#' @param path Character. Path to a NIfTI file (`.nii`/`.nii.gz`).
#' @param write Logical. If `TRUE` (default), overwrite `path` on disk with
#'   the updated header. If `FALSE`, the modified image object is returned
#'   without writing anything.
#'
#' @return Invisibly, the modified `niftiImage` object (see
#'   \code{\link[RNifti]{readNifti}}).
#'
#' @export

get_center_of_mass <- function(
  files,
  metadir = NULL,
  outputdir = NULL,
  f0 = 1,
  f1 = 0,
  overwrite = FALSE,
  write = TRUE
) {
  calculate_center_of_mass <- function(nifti) {
    idx <- which(nifti > mean(nifti[nifti > mean(nifti)]), arr.ind = TRUE)
    if (nrow(idx) == 0) {
      stop(
        "No voxels exceeded the intensity threshold; cannot compute center of mass."
      )
    }
    M <- RNifti::xform(nifti)
    M[1:3, 4] <- rowSums(M[1:3, ]) * -1
    com <- as.vector(
      M[1:3, ] %*% c(mean(idx[, 1]), mean(idx[, 2]), mean(idx[, 3]), 1)
    )

    Affine <- diag(4)
    Affine[1:3, 4] <- com
    M_new <- solve(Affine, M)
    nifti$srow_x <- M_new[1, ]
    nifti$srow_y <- M_new[2, ]
    nifti$srow_z <- M_new[3, ]

    names(com) <- c("X", "Y", "Z")

    return(list(data = nifti, center_of_mass = com))
  }

  if (write & is.null(outputdir)) {
    cli::cli_abort(
      "Must provide {.path outputdir} when {.code write = TRUE}"
    )
  }

  center_of_mass <- furrr::future_map_dfr(files, \(x) {
    VF <- RNifti::readNifti(x)
    center <- calculate_center_of_mass(VF)
    if (write) {
      RNifti::writeNifti(center$data, x)
    }
    return(center$center_of_mass)
  })

  center_of_mass <- data.frame(file = basename(files), center_of_mass)

  if (write) {
    com_file <- file.path(metadir, "center_of_mass.csv")
    if (!file.exists(com_file) || overwrite) {
      utils::write.csv(center_of_mass, file = com_file, row.names = FALSE)
    }
  }

  return(center_of_mass)
}
