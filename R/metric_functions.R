# mean_iou <- custom_metric("mean_iou", function(y_true, y_pred, num_classes=2){
#   return(tensorflow::tf$keras$metrics$MeanIoU(num_classes=2))
# })

#' Dice coefficient metric
#'
#' @param y_true
#' @param y_pred
#' @param smooth
#'
#' @return
#' @export
#'
#' @examples
dice <- keras::custom_metric("dice", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- keras::k_flatten(y_true)
  y_pred_f <- keras::k_flatten(y_pred)
  intersection <- keras::k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (keras::k_sum(y_true_f) + keras::k_sum(y_pred_f) + smooth)
})
