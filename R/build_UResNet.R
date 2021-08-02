<<<<<<< Updated upstream
#' Build Residual U-Net
#'
#' @param model_params Model params list, must add batch size when making list.
#' Also, x,y,z dims of input_shape must be even and dividends must be even too.
#'
#' @return
#' @export
#'
#' @examples
build_UResNet <- function(model_params){

  inputs <- keras::layer_input(batch_shape = c(model_params$batch_size, model_params$input_shape))

  conv1 <- inputs %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(7,7,7), strides = c(2,2,1), padding = "same", use_bias = F) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_relu()

  pool1 <- conv1 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides = 1, padding = "same")

  conv2 <- pool1 %>%
    residual_layer(filters=128, strides=2)

  pool2 <- conv2 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides=1, padding = "same")

  conv3 <- pool2 %>%
    residual_layer(filter=256, strides = 2)

  pool3 <- conv3 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides = 1, padding = "same")

  conv4 <- pool3 %>%
    residual_layer(filters = 512, strides = 2)

  pool4 <- conv4 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides = 1, padding = "same")

  conv5 <- pool4 %>%
    residual_layer(filters = 1024, strides = 2)

  drop5 <- conv5 %>%
    keras::layer_alpha_dropout(0.5)

  up6 <- drop5 %>%
    residual_layer_decode(filters = 512, strides = 2)
  merge6 <- keras::layer_concatenate(list(conv4, up6), axis = 4)

  conv6 <- merge6 %>%
    residual_layer(filters = 512, strides = 2) %>%
    keras::layer_upsampling_3d(size=c(2,2,2))

  up7 <- conv6 %>%
    residual_layer_decode(filters = 256, strides = 2)
  merge7 <- keras::layer_concatenate(list(conv3, up7), axis=4)

  conv7 <- merge7 %>%
    residual_layer(filters = 256, strides = 2) %>%
    keras::layer_upsampling_3d(size = c(2,2,2))

  up8 <- conv7 %>%
    residual_layer_decode(filters = 128, strides = 2)
  merge8 <- keras::layer_concatenate(list(conv2, up8), axis = 4)

  conv8 <- merge8 %>%
    residual_layer(filters = 128, strides = 2) %>%
    keras::layer_upsampling_3d(size = c(2,2,2))

  up9 <- conv8 %>%
    residual_layer_decode(filters = 64, strides = 2)
  merge9 <- keras::layer_concatenate(list(conv1, up9))

  conv9 <- merge9 %>%
    residual_layer(filters = 64, strides = 2) %>%
    keras::layer_upsampling_3d(size = c(2,2,2))

  conv10 <- conv9 %>%
    keras::layer_upsampling_3d(size = c(2,2,1)) %>%
    keras::layer_conv_3d(filters = 1,kernel_size = 1, strides = 1, activation = model_params$activation)

  model <- keras::keras_model(inputs, conv10) %>%
    keras::compile(optimizer = model_params$optimizer, loss = model_params$loss)

  return(model)

}
=======
#' Build Residual U-Net
#'
#' @param model_params Model params list, must add batch size when making list.
#' Also, x,y,z dims of input_shape must be even and dividends must be even too.
#'
#' @return
#' @export
#'
#' @examples
build_UResNet <- function(model_params){

  inputs <- keras::layer_input(batch_shape = c(model_params$batch_size, model_params$input_shape))

  conv1 <- inputs %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(7,7,7), strides = c(2,2,1), padding = "same", use_bias = F) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_relu()

  pool1 <- conv1 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides = 1, padding = "same")

  conv2 <- pool1 %>%
    residual_layer(filters=128, strides=2)

  pool2 <- conv2 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides=1, padding = "same")

  conv3 <- pool2 %>%
    residual_layer(filter=256, strides = 2)

  pool3 <- conv3 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides = 1, padding = "same")

  conv4 <- pool3 %>%
    residual_layer(filters = 512, strides = 2)

  pool4 <- conv4 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides = 1, padding = "same")

  conv5 <- pool4 %>%
    residual_layer(filters = 1024, strides = 2)

  drop5 <- conv5 %>%
    keras::layer_alpha_dropout(0.5)

  up6 <- drop5 %>%
    residual_layer_decode(filters = 512, strides = 2)
  merge6 <- keras::layer_concatenate(list(conv4, up6), axis = 4)

  conv6 <- merge6 %>%
    residual_layer(filters = 512, strides = 2) %>%
    keras::layer_upsampling_3d(size=c(2,2,2))

  up7 <- conv6 %>%
    residual_layer_decode(filters = 256, strides = 2)
  merge7 <- keras::layer_concatenate(list(conv3, up7), axis=4)

  conv7 <- merge7 %>%
    residual_layer(filters = 256, strides = 2) %>%
    keras::layer_upsampling_3d(size = c(2,2,2))

  up8 <- conv7 %>%
    residual_layer_decode(filters = 128, strides = 2)
  merge8 <- keras::layer_concatenate(list(conv2, up8), axis = 4)

  conv8 <- merge8 %>%
    residual_layer(filters = 128, strides = 2) %>%
    keras::layer_upsampling_3d(size = c(2,2,2))

  up9 <- conv8 %>%
    residual_layer_decode(filters = 64, strides = 2)
  merge9 <- keras::layer_concatenate(list(conv1, up9))

  conv9 <- merge9 %>%
    residual_layer(filters = 64, strides = 2) %>%
    keras::layer_upsampling_3d(size = c(2,2,2))

  conv10 <- conv9 %>%
    keras::layer_upsampling_3d(size = c(2,2,1)) %>%
    keras::layer_conv_3d(filters = 1,kernel_size = 1, strides = 1, activation = model_params$activation)

  model <- keras::keras_model(inputs, conv10) %>%
    keras::compile(optimizer = model_params$optimizer, loss = model_params$loss)

  return(model)

}
#' Build Residual U-Net
#'
#' @param model_params Model params list, must add batch size when making list.
#' Also, x,y,z dims of input_shape must be even.
#'
#' @return
#' @export
#'
#' @examples
build_UResNet <- function(model_params){

  inputs <- keras::layer_input(batch_shape = c(model_params$batch_size, model_params$input_shape))

  conv1 <- inputs %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(7,7,7), strides = c(2,2,1), padding = "same", use_bias = F) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_relu()

  pool1 <- conv1 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides = 1, padding = "same")

  conv2 <- pool1 %>%
    residual_layer(filters=128, strides=2)

  pool2 <- conv2 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides=1, padding = "same")

  conv3 <- pool2 %>%
    residual_layer(filter=256, strides = 2)

  pool3 <- conv3 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides = 1, padding = "same")

  conv4 <- pool3 %>%
    residual_layer(filters = 512, strides = 2)

  pool4 <- conv4 %>%
    keras::layer_max_pooling_3d(pool_size = c(3,3,3), strides = 1, padding = "same")

  conv5 <- pool4 %>%
    residual_layer(filters = 1024, strides = 2)

  drop5 <- conv5 %>%
    keras::layer_alpha_dropout(0.5)

  up6 <- drop5 %>%
    residual_layer_decode(filters = 512, strides = 2)
  merge6 <- keras::layer_concatenate(list(conv4, up6), axis = 4)

  conv6 <- merge6 %>%
    residual_layer(filters = 512, strides = 2) %>%
    keras::layer_upsampling_3d(size=c(2,2,2))

  up7 <- conv6 %>%
    residual_layer_decode(filters = 256, strides = 2)
  merge7 <- keras::layer_concatenate(list(conv3, up7), axis=4)

  conv7 <- merge7 %>%
    residual_layer(filters = 256, strides = 2) %>%
    keras::layer_upsampling_3d(size = c(2,2,2))

  up8 <- conv7 %>%
    residual_layer_decode(filters = 128, strides = 2)
  merge8 <- keras::layer_concatenate(list(conv2, up8), axis = 4)

  conv8 <- merge8 %>%
    residual_layer(filters = 128, strides = 2) %>%
    keras::layer_upsampling_3d(size = c(2,2,2))

  up9 <- conv8 %>%
    residual_layer_decode(filters = 64, strides = 2)
  merge9 <- keras::layer_concatenate(list(conv1, up9))

  conv9 <- merge9 %>%
    residual_layer(filters = 64, strides = 2) %>%
    keras::layer_upsampling_3d(size = c(2,2,2))

  conv10 <- conv9 %>%
    keras::layer_upsampling_3d(size = c(2,2,1)) %>%
    keras::layer_conv_3d(filters = 1,kernel_size = 1, strides = 1, activation = model_params$activation)

  model <- keras::keras_model(inputs, conv10) %>%
    keras::compile(optimizer = model_params$optimizer, loss = model_params$loss)

  return(model)

}
>>>>>>> Stashed changes
