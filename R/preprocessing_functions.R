#' Resampling fn.
#'
#' @param data
#' @param scale_factor
#'
#' @return
#' @export
#'
#' @examples
resample_image <- function(data, target_dims=NULL, scale_factor=NULL){
  data_dims <- dim(data)[1:3]
  if(!is.null(target_dims) & !is.null(scale_factor)){
    stop("Cannot use both target dims and scale factor")
  }
  if(!is.null(scale_factor)){
  new_data_dims <- data_dims %/% scale_factor
  scales <- new_data_dims/data_dims
  }
  if(!is.null(target_dims)){
    scales <- target_dims/data_dims
  }
  nifti_data <- oro.nifti::nifti(data)
  rescaled_nii <- RNiftyReg::rescale(nifti_data, scales)
  rescaled_data <- oro.nifti::nii2oro(rescaled_nii)@.Data
  return(rescaled_data)
}

#' Scale fn.
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
scale_image <- function(data){
  return(array(scale(data, center=F), dim = dim(data)))
}


#' Flip fn.
#'
#' @param data
#' @param flipdim
#'
#' @return
#' @export
#'
#' @examples
flip_data <- function(data, flipdim){
  return(nat::flip(data, flipdim=flipdim))
}

#' Repeat fn.
#'
#' @param data
#' @param n
#'
#' @return
#' @export
#'
#' @examples
repeat_data <- function(data, n){
  return(rep(data, n))
}

#' Add dim fn.
#'
#' @param data
#' @param last
#'
#' @return
#' @export
#'
#' @examples
add_dim <- function(data, last=TRUE){
  if(last==TRUE){
    dim(data) <- c(dim(data),1)
  } else{
    dim(data) <- c(1, dim(data))
  }
  return(data)
}

#' Function to chunk image for fitting into memory.
#'
#' @return
#' @export
#'
#' @examples
chunk_image <- function(num_chunks, chunk_size){

}
