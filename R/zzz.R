ants <- NULL

.onLoad <- function(libname, pkgname) {
  if (reticulate::py_module_available("ants")) {
    ants <<- reticulate::import("ants", delay_load = TRUE)
  }
}
