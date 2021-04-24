.onLoad <- function(libname, pkgname){
  reticulate::configure_environment(pkgname)
  tf_vis <<- reticulate::import("tf_keras_vis")
  art <<- reticulate::import("art")
}
