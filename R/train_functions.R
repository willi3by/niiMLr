train_model <- function(model, x_train, y_train, model_weights=NULL, epochs, batch_size, validation_data = NULL, validation_split = NULL,
                        callbacks = NULL, class_weight = NULL, steps_per_epoch = NULL, validation_steps = NULL){
  model$build(input_shape = x_train$shape)
  if(!is.null(model_weights)){
    model %>%
      load_model_weights_hdf5(model_weights)
  }
  model_history <- model %>% fit(x_train, y_train, epochs=epochs, batch_size=batch_size, validation_data = validation_data,
                                 validation_split = validation_split, callbacks = callbacks, class_weight = class_weight,
                                 steps_per_epoch = steps_per_epoch, validation_steps = validation_steps)
  return(model_history)
}
