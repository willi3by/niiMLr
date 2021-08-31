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

  K <- keras::backend()
  intersection <- K$sum(K$abs(y_true * y_pred), axis=-1L)
  sum_ <- K$sum(K$abs(y_true) + K$abs(y_pred), axis=-1L)
  jac <- (intersection) / (sum_ - intersection)
  jac_loss <- 1-jac
  return(jac_loss)
}
