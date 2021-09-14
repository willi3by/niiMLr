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

#' Jaccard loss, better for unbalanced datasets.
#'
#' @param y_true
#' @param y_pred
#' @param smooth
#'
#' @return
#' @export
#'
#' @examples
jaccard_loss <- function(y_true, y_pred){
  y_true <- keras::k_cast(y_true, tensorflow::tf$float32)
  intersection <- keras::k_sum(keras::k_abs(y_true * y_pred), axis=-1L)
  sum_ <- keras::k_sum(keras::k_abs(y_true) + keras::k_abs(y_pred), axis=-1L)
  jac <- (intersection) / (sum_ - intersection)
  jac_loss <- 1-jac
  return(jac_loss)
}
