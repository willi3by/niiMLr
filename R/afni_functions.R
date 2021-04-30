#' Builds command to run in AFNI.
#'
#' @param afni_command
#' @param afni_opts
#'
#' @return
#' @export
#'
#' @examples
build_afni_cmd <- function(afni_command, afni_opts = NULL){

  if(Sys.info()['sysname'] == "Windows"){
  afni_cmd <- paste0('bash -c -i "', afni_command, " ",afni_opts, '"')
  } else{
    afni_cmd <- paste0(afni_command, " ",afni_opts, '"')
  }
  return(afni_cmd)
}

#' Runs afni command.
#'
#' @param afni_cmd
#'
#' @return
#' @export
#'
#' @examples
run_afni_command <- function(afni_cmd){

  system(afni_cmd, intern=TRUE)

}

#' Title
#'
#' @param image_path
#'
#' @return
#' @export
#'
#' @examples
adjust_image_path_windows <- function(image_path){
  linux_image_path <- paste0('/mnt/c', image_path)
  return(linux_image_path)
}

#' Resamples image to a particular grid resolution.
#'
#' @param in_file Absolute path to file to be reasampled
#' @param out_file Absolute path to save file destination.
#' @param new_dims Vector of new dimensions c(x,y,z)
#' @param extra_opts Extra options to feed to 3dresample (see docs for details).
#'
#' @return resampled image (also saves as nii).
#' @export
#'
#' @examples
resample_image <- function(in_file, out_file, new_dims, extra_opts = NULL){
  linux_in_file <- adjust_image_path_windows(in_file)
  linux_out_file <- adjust_image_path_windows(out_file)
  cmd <- build_afni_cmd("3dresample", afni_opts = paste("-prefix",
                                                        linux_out_file,
                                                        "-dxyz",
                                                        new_dims[1],
                                                        new_dims[2],
                                                        new_dims[3],
                                                        "-input",
                                                        linux_in_file,
                                                        "-overwrite")
                        )

  if(!is.null(extra_opts)){
    cmd <- paste(cmd, extra_opts)
  }
  run_afni_command(cmd)
  resampled_image <- neurobase::readnii(out_file)
  return(resampled_image@.Data)
}

#' Title
#'
#' @param in_file
#' @param out_file
#' @param reference_file
#' @param extra_opts
#'
#' @return
#' @export
#'
#' @examples
alline_image <- function(in_file, out_file, reference_file, extra_opts = NULL){

  linux_in_file <- adjust_image_path_windows(in_file)
  linux_out_file <- adjust_image_path_windows(out_file)
  linux_reference_file <- adjust_image_path_windows(reference_file)
  cmd <- build_afni_cmd("3dAllineate", afni_opts = paste("-prefix",
                                                         linux_out_file,
                                                         "-source",
                                                         linux_in_file,
                                                         "-base",
                                                         linux_reference_file,
                                                         "-overwrite"))

  if(!is.null(extra_opts)){
    cmd <- paste(cmd, extra_opts)
  }

  run_afni_command(cmd)
  allined_image <- neurobase::readnii(out_file)
  return(allined_image@.Data)

}

#' Title
#'
#' @param image_path
#'
#' @return
#' @export
#'
#' @examples
rgb_to_grayscale <- function(image_path){
  linux_path <- adjust_image_path_windows(image_path)
  afni_cmd <- build_afni_cmd('3dcalc', afni_opts = paste('-prefix',
                                                         linux_path,
                                                         '-a',
                                                         linux_path,
                                                         '-expr',
                                                         'a/255',
                                                         '-overwrite'))
  run_afni_command(afni_cmd)
  return("Image converted")
}

#' Title
#'
#' @param image_path
#'
#' @return
#' @export
#'
#' @examples
check_image_max <- function(image_path){

  linux_path <- adjust_image_path_windows(image_path)
  afni_cmd <- build_afni_cmd('3dinfo', afni_opts = paste('-max',
                                                         linux_path))
  cmd_output <- run_afni_command(afni_cmd) %>%
    as.list()
  idx <- which(lapply(cmd_output, function(x){grepl("^[[:digit:]]+", x)}) == TRUE)


  max_value <-  cmd_output[[idx]] %>%
    strsplit(., split='\\|') %>%
    unlist() %>%
    .[1] %>%
    as.numeric()

  return(max_value)

}

#' Title
#'
#' @param in_file
#' @param out_file
#'
#' @return
#' @export
#'
#' @examples
extract_highest_diffusion_shell <- function(in_file, out_file){
  nii <- neurobase::readnii(in_file)
  nii_data <- nii@.Data
  if(length(dim(nii_data)) == 4){
    nii_means <- apply(nii_data, 4, mean)
    idx <- which.min(nii_means)
    nii_final <- nii_data[,,,idx]
  } else {
    nii_final <- nii_data
  }
  new_nii <- oro.nifti::nifti(nii_final)
  new_nii <- neurobase::copyNIfTIHeader(nii, new_nii)
  oro.nifti::writeNIfTI(new_nii, filename = out_file)
  return(new_nii)
}
