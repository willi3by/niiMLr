#' Function to build the DeepMedic Network from scratch so it can be customized
#'
#' @param model_params input shape should be a list of two where the first entry
#' is the shape of the input to path 1 and the second is the shape for path 2
#'
#' @return
#' @export
#'
#' @examples
build_DeepMedic <- function(model_params){
  input_path_1 <- keras::layer_input(shape=model_params$input_shape[[1]], name="input_path_1")
  input_path_2 <- keras::layer_input(shape=model_params$input_shape[[2]], name="input_path_2")

  path_1 <- input_path_1 %>%
    keras::layer_conv_3d(filters = 30, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 30, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 50, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 50, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal())

  path_2 <- input_path_2 %>%
    keras::layer_conv_3d(filters = 30, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 30, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 50, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_conv_3d(filters = 50, kernel_size = 3,
                         activation = "relu",
                         kernel_initializer = keras::initializer_he_normal()) %>%
    keras::layer_upsampling_3d(size=c(2,2,2))



}
