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

  numerator <- 2 * tensorflow::tf$reduce_sum(y_true * y_pred, axis=c(1,2,3))
  denominator <- tensorflow::tf$reduce_sum(y_true + y_pred, axis=c(1,2,3))

  return(tensorflow::tf$reshape(1-numerator / denominator, c(-1,1,1)))
}
