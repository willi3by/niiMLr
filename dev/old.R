dataset <- load_tfrecord_dataset('/Users/willi3by/PycharmProjects/APRISE_GCAS_T1/data/train.tfrecord',
                                 x_shape = as.integer(c(256,256,32,1)),
                                 x_type=tf$float32, y_shape=as.integer(c(1)),
                                 y_type = tf$int64)

batched_data <- dataset %>% dataset_map(binarize_y) %>% dataset_batch(12)
iter <- make_iterator_one_shot(batched_data)
next_batch <- iterator_get_next(iter)
x_train <- next_batch[[1]]
y_train <- next_batch[[2]]

# model %>% keras::compile(optimizer = optimizer_adam(lr = 0.0001), loss = tfaddons::loss_sigmoid_focal_crossentropy(),
#                          metrics = c('accuracy', tfaddons::metric_fbetascore(2, average = 'micro')))
