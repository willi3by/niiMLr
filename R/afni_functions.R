#' Builds command to run in AFNI.
#'
#' @param afni_command
#' @param afni_opts
#'
#' @return AFNI command to run with system.
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
#' @return NA
#' @export
#'
#' @examples
run_afni_command <- function(afni_cmd){

  system(afni_cmd, intern=TRUE)

}

#' Add /mnt/c/ to path to make it compatible with WSL.
#'
#' @param image_path
#'
#' @return Adjusted path.
#' @export
#'
#' @examples
adjust_image_path_windows <- function(image_path){
  linux_image_path <- paste0('/mnt/c', image_path)
  return(linux_image_path)
}

#' Resample image to a particular grid resolution.
#'
#' @param in_file Absolute path to file to be reasampled
#' @param out_file Absolute path to save file destination.
#' @param new_dims Vector of new dimensions c(x,y,z)
#' @param extra_opts Extra options to feed to 3dresample (see afni docs for details).
#'
#' @return Resampled image (also saves as nii).
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

#' Aligns image to reference image.
#'
#' @param in_file
#' @param out_file
#' @param base_path
#' @param emask
#' @param extra_alline_opts
#' @param overwrite
#' @param reference_file
#'
#' @return
#' @export
#'
#' @examples
alline_image <- function(base_path ,in_file, out_file, reference_file, emask = NULL, extra_alline_opts = NULL, overwrite = TRUE){

  linux_in_file <- adjust_image_path_windows(in_file)
  linux_out_file <- adjust_image_path_windows(out_file)
  linux_reference_file <- adjust_image_path_windows(reference_file)

  cmd <- build_afni_cmd("3drefit", afni_opts = paste("-deoblique", linux_in_file))
  run_afni_command(cmd)

  cmd <- build_afni_cmd("3dCM", afni_opts = paste("-set",
                                                  "0 0 0",
                                                  linux_in_file))
  run_afni_command(cmd)

  if(!is.null(emask)){
    linux_emask <- adjust_image_path_windows(emask)
    cmd <- build_afni_cmd("3drefit", afni_opts = paste("-deoblique", linux_emask))
    run_afni_command(cmd)

    cmd <- build_afni_cmd("3dCM", afni_opts = paste("-set", "0 0 0", linux_emask))
    run_afni_command(cmd)
  }

  afni_opts <- paste("-prefix",
                     paste0('/mnt/c/',base_path, "/ref2subj.nii"),
                     "-base",
                     linux_in_file,
                     "-source",
                     linux_reference_file,
                     "-1Dmatrix_save",
                     paste0('/mnt/c/',base_path, "/ref2subj.aff.1D"))

  if(!is.null(emask)){
    afni_opts <- paste(afni_opts, "-emask", linux_emask)
  }
  if(!is.null(extra_alline_opts)){
    afni_opts <- paste(afni_opts, extra_alline_opts)
  }
  if(overwrite == TRUE){
    afni_opts <- paste(afni_opts, "-overwrite")
  }

  cmd <- build_afni_cmd("3dAllineate", afni_opts = afni_opts)
  run_afni_command(cmd)

  cmd <- build_afni_cmd('cat_matvec', afni_opts = paste(paste0('/mnt/c/', base_path, "/ref2subj.aff.1D"),
                                                        "-I >",
                                                        paste0('/mnt/c/', base_path, "/subj2ref.aff.1D"))
  )
  run_afni_command(cmd)

  cmd <- build_afni_cmd("3dAllineate", afni_opts = paste("-prefix",
                                                         linux_out_file,
                                                         "-source",
                                                         linux_in_file,
                                                         "-1Dmatrix_apply",
                                                         paste0('/mnt/c/', base_path, "/subj2ref.aff.1D"),
                                                         "-master",
                                                         linux_reference_file,
                                                         "-overwrite"))


  run_afni_command(cmd)
  allined_image <- neurobase::readnii(out_file)

  if(!is.null(emask)){
    linux_emask <- adjust_image_path_windows(emask)
    emask_out <- substr(emask, 1, nchar(emask)-4)
    emask_out <- paste0(emask_out, "_allined.nii")
    linux_emask_out <- adjust_image_path_windows(emask_out)
    cmd <- build_afni_cmd("3dAllineate", afni_opts = paste("-prefix",
                                                           linux_emask_out,
                                                           "-source",
                                                           linux_emask,
                                                           "-1Dmatrix_apply",
                                                           paste0('/mnt/c/', base_path, "/subj2ref.aff.1D"),
                                                           "-master",
                                                           linux_reference_file,
                                                           "-overwrite"))

  }
  run_afni_command(cmd)
  return(allined_image@.Data)

}

#' Convert RGB image to grayscale.
#'
#' @param image_path
#'
#' @return NA
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

#' Get max value of nii.
#'
#' @param image_path
#'
#' @return image max.
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

#' Extract highest shell of diffusion dataset.
#'
#' @param in_file
#' @param out_file
#'
#' @return Nii object of highest shell image.
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
