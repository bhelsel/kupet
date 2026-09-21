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

calculate_center_of_mass <- function(path, write = TRUE) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  VF <- RNifti::readNifti(path)

  m <- mean(VF)
  avg <- mean(VF[VF > m])
  idx <- which(VF > avg, arr.ind = TRUE)

  if (nrow(idx) == 0) {
    stop(
      "No voxels exceeded the intensity threshold; cannot compute center of mass."
    )
  }

  x <- mean(idx[, 1])
  y <- mean(idx[, 2])
  z <- mean(idx[, 3])

  M <- RNifti::xform(VF)

  M[1:3, 4] <- rowSums(M[1:3, ]) * -1
  com <- as.vector(M[1:3, ] %*% c(x, y, z, 1))

  Affine <- diag(4)
  Affine[1:3, 4] <- com

  M_new <- solve(Affine, M)
  VF$srow_x <- M_new[1, ]
  VF$srow_y <- M_new[2, ]
  VF$srow_z <- M_new[3, ]

  if (write) {
    RNifti::writeNifti(VF, path)
  }

  return(com)
}
