#' Function to build the DeepMedic Network from scratch so it can be customized
#'
#' @param model_params all 3 spatial dimensions of input shape must be even
#'
#' @return
#' @export
#'
#' @examples
build_DeepMedic <- function(model_params){
  input_path_1 <- keras::layer_input(shape=model_params$input_shape, name="input_path_1")
  input_shape_2 <- c((model_params$input_shape[1:3]/2+8), model_params$input_shape[4])
  input_path_2 <- keras::layer_input(shape=input_shape_2, name="input_path_2")

  path_1 <- input_path_1 %>%
    keras::layer_conv_3d(filters = 30, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_1_conv_1") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 30, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_1_conv_2") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_1_conv_3") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_1_conv_4") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_1_conv_5") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_1_conv_6") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 50, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_1_conv_7") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 50, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_1_conv_8") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02)

  path_2 <- input_path_2 %>%
    keras::layer_conv_3d(filters = 30, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_2_conv_1") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 30, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_2_conv_2") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_2_conv_3") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_2_conv_4") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_2_conv_5") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 40, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_2_conv_6") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 50, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_2_conv_7") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_conv_3d(filters = 50, kernel_size = 3,
                         kernel_initializer = keras::initializer_he_normal(),
                         kernel_regularizer = keras::regularizer_l1_l2(l1=0.00001, l2=0.0001),
                         name = "path_2_conv_8") %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_batch_normalization() %>%
    keras::layer_spatial_dropout_3d(rate=0.02) %>%
    keras::layer_upsampling_3d(size=c(2,2,2))

concat_layer <- keras::layer_add(list(path_1, path_2))

main_output <- concat_layer %>%
  keras::layer_dense(units = 150, activation="relu") %>%
  keras::layer_dense(units = 150, activation="relu") %>%
  keras::layer_dense(units = model_params$num_classes,
                     activation = model_params$activation) %>%
  keras::layer_zero_padding_3d(padding = c(8,8,8), name = "main_output")

model <- keras::keras_model(inputs = c(input_path_1, input_path_2), outputs = main_output)
model %>% keras::compile(optimizer = model_params$optimizer,
                  loss = model_params$loss,
                  metrics = model_params$metrics)
}
