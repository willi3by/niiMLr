.onLoad <- function(libname, pkgname){
  reticulate::configure_environment(pkgname)
  tf_vis <<- reticulate::import("tf_keras_vis", delay_load = TRUE)
  art <<- reticulate::import("art", delay_load = TRUE)
}
