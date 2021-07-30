#' Builds ResNet 50
#'
#' @param model_params List of model parameters.
#' @param numResNet Which resnet to use (ResNet50, ResNet101, ResNet152)
#'
#' @return ResNet model
#' @export
#'
#' @examples
build_ResNet <- function(numResNet, model_params){

  filter_list <<- switch (numResNet,
    "ResNet50" = c(rep(64,3), rep(128, 4), rep(256,6), rep(512,3)),
    "ResNet101" = c(rep(64,3), rep(128, 4), rep(256,23), rep(512,3)),
    "ResNet152" = c(rep(64,3), rep(128, 8), rep(256,36), rep(512,3))
  )

  model <- keras::keras_model_sequential() %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(7,7,7), strides = c(2,2,2), padding = 'same',
                  use_bias = F, input_shape = model_params$input_shape) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_relu() %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides = c(2,2,2), padding = 'same')

  prev_filters <<- 64
  for(i in seq_along(filter_list)){
    i <<- i
    if(filter_list[i] == prev_filters){strides <<- 1} else {strides <<- 2}
    model %>% residual_layer(filters = filter_list[i], strides = strides)
    prev_filters <<- filter_list[i]
  }

  model <- model %>%
    keras::layer_global_average_pooling_3d() %>%
    keras::layer_flatten() %>%
    keras::layer_dropout(0.3) %>%
    keras::layer_dense(units = model_params$num_classes,
                       activation = model_params$activation)

  model <- model %>% keras::compile(loss=model_params$loss,
                    optimizer = model_params$optimizer,
                    metrics = model_params$metrics)


  return(model)
}

#' Build ResNet-50
#'
#' @param model_params
#'
#' @return
#' @export
#'
#' @examples
build_ResNet50 <- function(model_params){
  return(build_ResNet("ResNet50", model_params))
}

#' Build ResNet-101
#'
#' @param model_params
#'
#' @return
#' @export
#'
#' @examples
build_ResNet101 <- function(model_params){
  return(build_ResNet("ResNet101", model_params))
}

#' Build ResNet-152
#'
#' @param model_params
#'
#' @return
#' @export
#'
#' @examples
build_ResNet152 <- function(model_params){
  return(build_ResNet("ResNet152", model_params))
}
