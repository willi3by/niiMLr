build_afni_cmd <- function(afni_command, afni_opts = NULL){

  afni_cmd <- paste0('bash -c -i "', afni_command, " ",afni_opts, '"')
  return(afni_cmd)
}

run_afni_command <- function(afni_cmd){

  system(afni_cmd)

}
