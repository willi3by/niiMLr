#' @export
#
residual_layer <- function(filters, strides, ...) {
  filters <<- filters
  strides <<- strides
  custom_residual_layer()(..., filters, strides)
}



custom_residual_layer <- function() keras::Layer(
  classname = "ResidualUnit",
  initialize = function(filters=1, strides=1){
    super()$`__init__`()
    self$main_layers = list()
    self$skip_layers = list()
    self$filters = filters
    self$strides = strides
  },
  build = function(input_shape){
    self$main_layers <- list(
      keras::layer_conv_3d(filters = self$filters, kernel_size = c(3,3,3), strides = self$strides,
                    padding = 'same', use_bias = F),
      keras::layer_batch_normalization(),
      keras::layer_activation_relu(),
      keras::layer_conv_3d(filters = self$filters, kernel_size = c(3,3,3), strides = 1,
                    padding = 'same', use_bias = F),
      keras::layer_batch_normalization()
    )

    self$skip_layers <- list()
    if(self$strides > 1){
      self$skip_layers <- list(
        keras::layer_conv_3d(filters = self$filters, kernel_size = c(1,1,1), strides = self$strides,
                      padding = 'same', use_bias = F),
        keras::layer_batch_normalization()
      )
    }
  },
  call = function(inputs, ...){
    Z <- inputs
    for(i in c((seq_along(self$main_layers))-1)){
      Z <- self$main_layers[[i]](Z)
    }

    skip_Z <- inputs
    for(i in c((seq_along(self$skip_layers))-1)){
      skip_Z <- self$skip_layers[[i]](skip_Z)
    }
    return(keras::activation_relu(Z + skip_Z))
  }
)

