# load('/Users/willi3by/Desktop/ISC_Class_Net/all_isc_class_subjects.R')
# load('/Users/willi3by/Desktop/ISC_Class_Net/isc_class_df.R')
#
# test_set_df <- splitstackshape::stratified(isc_class_subject_df, "class", 0.20, keep.rownames = TRUE)
# sorted_test_set_df <- test_set_df[order(as.numeric(test_set_df$rn)),]
# test_dataset <- lapply(all_isc_class_subjects, function(x){x[as.numeric(sorted_test_set_df$rn)]})
# pre_train_set_df <- isc_class_subject_df[-as.numeric(sorted_test_set_df$rn),]
# pre_train_dataset <- lapply(all_isc_class_subjects, function(x){x[-as.numeric(sorted_test_set_df$rn)]})
#
# rownames(pre_train_set_df) <- NULL
# val_set_df <- splitstackshape::stratified(pre_train_set_df, "class", 0.20, keep.rownames = TRUE)
# sorted_val_set_df <- val_set_df[order(as.numeric(val_set_df$rn)),]
# val_dataset <- lapply(pre_train_dataset, function(x){x[as.numeric(sorted_val_set_df$rn)]})
# train_df <- pre_train_set_df[-as.numeric(sorted_val_set_df$rn),]
# train_dataset <- lapply(pre_train_dataset, function(x){x[-as.numeric(sorted_val_set_df$rn)]})
#

load('/Users/willi3by/Desktop/ISC_Class_Net_Files_for_Abdulla/isc_train_dataset.R')
load('/Users/willi3by/Desktop/ISC_Class_Net_Files_for_Abdulla/isc_val_dataset.R')

model_params <- set_model_params(input_shape = c(103,122,32,1), num_classes = 2,
                                loss = "binary_crossentropy", activation = "sigmoid",
                                optimizer = keras::optimizer_adam(), metrics = c("accuracy"))

model <- build_ResNet50(model_params)

results <- model %>%
  fit()



