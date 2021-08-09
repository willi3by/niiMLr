#' Build Residual Y-Net
#'
#' @param model_params Model params list, must add batch size when making list.
#' Also, x,y,z dims of input_shape must be even.
#'
#' @return
#' @export
#'
#' @examples
build_YResNet <- function(model_params){

  inputs <- keras::layer_input(name = "inputs", batch_shape = c(model_params$batch_size, model_params$input_shape))

  conv1 <- inputs %>%
    keras::layer_conv_3d(name = "conv1", filters = 64, kernel_size = c(7,7,7), strides = c(2,2,1), padding = "same", use_bias = F) %>%
    keras::layer_batch_normalization(name="conv1_batchnorm") %>%
    keras::layer_activation_relu(name = "conv1_relu")

  pool1 <- conv1 %>%
    keras::layer_max_pooling_3d(name = "pool1", pool_size = c(3,3,3), strides = 1, padding = "same")

  conv2 <- pool1 %>%
    residual_layer(name = "conv2", filters=128, strides=2)

  pool2 <- conv2 %>%
    keras::layer_max_pooling_3d(name = "pool2", pool_size = c(3,3,3), strides=1, padding = "same")

  conv3 <- pool2 %>%
    residual_layer(name = "conv3", filter=256, strides = 2)

  pool3 <- conv3 %>%
    keras::layer_max_pooling_3d(name = "pool3", pool_size = c(3,3,3), strides = 1, padding = "same")

  conv4 <- pool3 %>%
    residual_layer(name = "conv4", filters = 512, strides = 2)

  pool4 <- conv4 %>%
    keras::layer_max_pooling_3d(name = "pool4", pool_size = c(3,3,3), strides = 1, padding = "same")

  conv5 <- pool4 %>%
    residual_layer(name = "conv5", filters = 1024, strides = 2)

  drop5 <- conv5 %>%
    keras::layer_alpha_dropout(0.5, name = "drop5")

  up6 <- drop5 %>%
    residual_layer_decode(name = "up6", filters = 512, strides = 2)
  merge6 <- keras::layer_concatenate(name = "merge6", list(conv4, up6), axis = 4)

  conv6 <- merge6 %>%
    residual_layer(name = "conv6", filters = 512, strides = 2) %>%
    keras::layer_upsampling_3d(name = "conv6_upsampling", size=c(2,2,2))

  up7 <- conv6 %>%
    residual_layer_decode(name = "up7", filters = 256, strides = 2)
  merge7 <- keras::layer_concatenate(name = "merge7", list(conv3, up7), axis=4)

  conv7 <- merge7 %>%
    residual_layer(name = "conv7", filters = 256, strides = 2) %>%
    keras::layer_upsampling_3d(name = "conv7_upsampling", size = c(2,2,2))

  up8 <- conv7 %>%
    residual_layer_decode(name = "up8", filters = 128, strides = 2)
  merge8 <- keras::layer_concatenate(list(conv2, up8), axis = 4, name = "merge8")

  conv8 <- merge8 %>%
    residual_layer(name = "conv8", filters = 128, strides = 2) %>%
    keras::layer_upsampling_3d(size = c(2,2,2))

  up9 <- conv8 %>%
    residual_layer_decode(name = "up9", filters = 64, strides = 2)
  merge9 <- keras::layer_concatenate(list(conv1, up9), name = "merge9")

  conv9 <- merge9 %>%
    residual_layer(name = "conv9", filters = 64, strides = 2) %>%
    keras::layer_upsampling_3d(name = "conv9_upsampling", size = c(2,2,2))

  conv10 <- conv9 %>%
    keras::layer_upsampling_3d(name = "conv10_upsampling", size = c(2,2,1)) %>%
    keras::layer_conv_3d(name = "conv10", filters = 1,kernel_size = 1, strides = 1, activation = model_params$activation)

  y_bottom <- drop5 %>%
    keras::layer_max_pooling_3d(name = "y_pool11", pool_size = c(3,3,3), strides = 1, padding = "same") %>%
    # number filters here?
    residual_layer(name = "y_conv12", filters=128, strides=2) %>%

    keras::layer_max_pooling_3d(name = "y_pool12", pool_size = c(3,3,3), strides = 1, padding = "same") %>%
    # number filters here?
    residual_layer(name = "y_conv13", filters=128, strides=2) %>%

    # pool_size, strides, padding?
    keras::layer_average_pooling_3d(name = "y_average_pool", pool_size = c(3,3,3), strides = 1, padding = "same") %>%

    keras::layer_dense(64, name = "y_dense1") %>%

    keras::layer_dense(model_params$num_classes, name = "y_dense2")

  # Operands could not be broadcast together with shapes (512, 512, 512, 1) (4, 4, 8, 3)
  # mulitplied <- keras::layer_multiply(c(conv10, y_bottom))

  uncompiled_model <- keras::keras_model(
    inputs,
    outputs = c(conv10, y_bottom)
  )

  compiled_model <- uncompiled_model %>%
    keras::compile(
      optimizer = model_params$optimizer,
      loss = model_params$loss
    )

  return(compiled_model)

}
