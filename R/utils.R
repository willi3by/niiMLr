#' Set model parameters.
#'
#' @param input_shape 4D (x,y,z,channel) vector for input dimensions.
#' @param num_classes Number of classes used for classification.
#' @param loss Loss function (in quotes, e.g. "binary_crossentropy")
#' @param activation Activation function (in quotes, e.g. "sigmoid")
#' @param optimizer Optimizer as a keras function (e.g. keras::optimizer_adam())
#' @param metrics Evaluation metrics; if more than 1, then make list.
#' @param ... Any additional parameters for specific models.
#'
#' @return List of model parameters.
#' @export
#'
#' @examples
set_model_params <- function(input_shape,
                             num_classes,
                             loss,
                             activation,
                             optimizer,
                             metrics,
                             ...)
{
  model_params <- list(input_shape = input_shape, num_classes = num_classes,
              loss = loss, activation = activation,
              optimizer = optimizer, metrics = metrics)

  additional_params <- list(...)
  model_params <- append(model_params, additional_params)
  return(model_params)
}
