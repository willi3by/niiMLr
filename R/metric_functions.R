# mean_iou <- custom_metric("mean_iou", function(y_true, y_pred, num_classes=2){
#   return(tensorflow::tf$keras$metrics$MeanIoU(num_classes=2))
# })

#'
#'
#' @export
dice_fn <- function(y_true, y_pred, ...) {
  dice()(..., y_true, y_pred)
}


#' Dice coefficient metric
#'
#' @param y_true
#' @param y_pred
#' @param smooth
#'
#' @return
#' @export
#' @examples
dice <- function() keras::custom_metric("dice", function(y_true, y_pred) {
  y_true_f <- keras::k_flatten(y_true)
  y_pred_f <- keras::k_flatten(y_pred)
  intersection <- keras::k_sum(y_true_f * y_pred_f)
  (2 * intersection) / (keras::k_sum(y_true_f) + keras::k_sum(y_pred_f))
})
