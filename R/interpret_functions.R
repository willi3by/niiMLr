#' Builds model modifier for GradCam++
#'
#' @param m
#'
#' @return model_modifier
#'
#' @examples
model_modifier <- function(m){
  reticulate::py_set_attr(m$layers[[length(m$layers)]], "activation",
                          tensorflow::tf$keras$activations$linear)
}


#' 3dGradCAM++
#'
#' @param test_sample Example to use for grad cam.
#' @param ref_img Path to image used as reference for preprocessing.
#' @param write_imgs If not NULL, where to write nifti for cam and orig imgs.
#'
#' @return Aligned gradcam and orig image as nii objects.
#' @export
#'
#' @examples
gradcam_pp <- function(test_sample, ref_img, write_imgs = NULL){

  loss = function(output) {tensorflow::tf$keras$backend$mean(output)}
  ref_img <- readNIfTI(ref_img)
  dim(test_sample) <- c(1, dim(test_sample), 1)
  gradcam <- tf_vis$gradcam$GradcamPlusPlus(model, model_modifier, clone=F)
  cam <- gradcam(loss, test_sample, penultimate_layer=-1L)
  cam <- tf_vis$utils$normalize(cam)
  cam_arr <- array(0, dim=dim(ref_img)[1:3])
  cam_arr[,,] <- cam*10
  cam_nim <- nifti(cam_arr)
  cam_nim <- copyNIfTIHeader(img = ref_img, arr = cam_nim)
  orig <- test_sample
  dim(orig) <- c(dim(orig)[1:3])
  orig_nim <- nifti(orig)
  orig_nim <- copyNIfTIHeader(img = ref_img, arr = orig_nim)
  if(!is.null(write_imgs)){
    writeNIfTI(cam_nim, filename = paste0(write_imgs, i, '_cam'))
    writeNIfTI(orig_nim, filename = paste0(write_imgs, i, "_orig"))
  }
  return(list(orig = orig_nim, cam = cam_nim))
}
