#' Standard training function.
#'
#' @param model
#' @param x_train
#' @param y_train
#' @param model_weights
#' @param epochs
#' @param batch_size
#' @param validation_data
#' @param validation_split
#' @param callbacks
#' @param class_weight
#' @param steps_per_epoch
#' @param validation_steps
#'
#' @return
#' @export
#'
#' @examples
train_model_standard <- function(model, x_train, y_train, model_weights=NULL, epochs, batch_size, validation_data = NULL, validation_split = NULL,
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

#' Training with K-fold cross-validation.
#'
#' @param model_building_fn
#' @param model_params
#' @param n_epochs
#' @param batch_size
#' @param x_train
#' @param y_train
#' @param n_folds
#'
#' @return
#' @export
#'
#' @examples
train_model_kfold <- function(model_building_fn, model_params, n_epochs, batch_size, x_train, y_train, n_folds){

  folds <- caret::createFolds(y_train, k = n_folds)
  acc_per_fold <- list()
  loss_per_fold <- list()
  for(k in 1:length(folds)){

    model <- model_building_fn

    x_train_fold <- x_train[-folds[[i]]]
    y_train_fold <- y_train[-folds[[i]]]
    x_val_fold <- x_train[folds[[i]]]
    y_val_fold <- y_train[folds[[i]]]

    model_history <- model %>%
      fit(x_train_fold, y_train_fold, epochs = n_epochs, batch_size = batch_size, callbacks = list(
        keras::callback_model_checkpoint(filepath = paste("./best_model_Fold_", k, ".h5")),
        keras::callback_tensorboard(log_dir = paste0("./Fold_", i, "_logs/")),
        keras::callback_reduce_lr_on_plateau()
      ))

    model_scores <- model %>%
      keras::evaluate(x_val_fold, y_val_fold)

    print(paste0("Score for fold ", i, ": ", model$metrics_names[1], " of ", model_scores[1]))
    print(paste0("Score for fold ", i, ": ", model$metrics_names[2], " of ", model_scores[2]*100, "%"))

    acc_per_fold <- append(acc_per_fold, model_scores[2]*100)
    loss_per_fold <- append(loss_per_fold, model_scores[1])
  }
  print("Score per fold")
  for(i in 1:length(acc_per_fold)){
    print('-------------------------------------------------------------------')
    print(paste0("Fold ", i, "Loss: ", loss_per_fold[i], " - Accuracy: ", acc_per_fold[i], "%"))
  }
  print('-------------------------------------------------------------------')
  print('Average scores for all folds:')
  print(paste0("Accuracy: ", mean(unlist(acc_per_fold)), " +/- ", sd(unlist(acc_per_fold))))
  print(paste0("Loss: ", mean(unlist(loss_per_fold))))
  print('-------------------------------------------------------------------')

}
