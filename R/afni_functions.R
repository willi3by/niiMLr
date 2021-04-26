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

#' Title
#'
#' @param afni_cmd
#'
#' @return
#' @export
#'
#' @examples
run_afni_command <- function(afni_cmd){

  system(afni_cmd)

}
