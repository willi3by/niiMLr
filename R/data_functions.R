get_mask_list <- function(path_to_parent_folder, mask_substring){

  mask_files <- list.files(path_to_parent_folder, pattern = mask_substring,
                           full.names = T, recursive = T)
  return(mask_files)
}
