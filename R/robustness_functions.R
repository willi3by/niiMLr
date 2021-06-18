#' Clever score on l2 norm for binary classification with sigmoid activation.
#'
#' @param path_to_weights Absolute path to model weights.
#' @param model_params List of model parameters.
#' @param model_build_fn Function to build model.
#' @param x_test Examples on which to calculate score.
#'
#' @return Lit of clever score for each test example.
#' @export
#'
#' @examples
binary_clever_score <- function(model_build_fn, model_params, path_to_weights,
                                x_test){
  #Must disable eager execution in tensorflow and rebuild models to run art functions.
  tensorflow::tf$compat$v1$disable_eager_execution()

  source_model <- model_build_fn(model_params)
  source_model <- source_model %>%
    keras::load_model_weights_hdf5(path_to_weights)

  model_params$activation = "softmax"
  model_params$num_classes = 2

  model <- model_build_fn(model_params)

  for(w in 1:(length(model$layers)-1)){
    source_weights <- source_model$layers[[w]]$get_weights()
    model$layers[[w]]$set_weights(source_weights)
  }
  classifier <- art$estimators$classification$KerasClassifier(model=model)
  all_scores <- list()
  for(i in 1:dim(x_test)[1]){
    x_single_sample <- x_test[i,,,,]
    dim(x_single_sample) <- c(dim(x_single_sample), 1)
    clever_score <- art$metrics$clever_u(classifier, x_single_sample,
                                         nb_batches = 50L, batch_size = 6L,
                                         radius = 10L, norm = 2)
    all_scores <- append(all_scores, clever_score)
  }
  return(all_scores)

}
