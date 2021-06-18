#' Title
#'
#' @param path_to_parent_folder
#' @param mask_substring
#'
#' @return
#' @export
#'
#' @examples
get_mask_list <- function(path_to_parent_folder, mask_substring){

  mask_files <- list.files(path_to_parent_folder, pattern = mask_substring,
                           full.names = T, recursive = T)
  return(mask_files)
}

#' Title
#'
#' @param in_file
#'
#' @return
#' @export
#'
#' @examples
extract_pixdims <- function(in_file){
  nii <- neurobase::readnii(in_file)
  pixdim <- nii@pixdim[2:4]
  return(pixdim)
}
