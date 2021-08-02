#' Function to define dice loss for networks
#'
#' @param y_true
#' @param y_pred
#'
#' @return
#' @export
#'
#' @examples
dice_loss <- function(y_true, y_pred){

  numerator <- 2 * keras::k_sum(y_true * y_pred, axis=c(1,2,3))
  denominator <- keras::k_sum(y_true + y_pred, axis=c(1,2,3))

  return(keras::k_reshape(1-numerator / denominator, c(-1,1,1)))
}

jaccard_loss <- function(y_true, y_pred, smooth=100){

  intersection <- keras::k_sum(keras::k_abs(y_true * y_pred), axis=-1)
  sum_ = keras::k_sum(keras::k_abs(y_true) + keras::k_abs(y_pred), axis=-1)
  jac = (intersection + smooth) / (sum_ - intersection + smooth)
  return((1-jac)*smooth)
}
