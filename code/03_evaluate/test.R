library(tensorflow)
library(keras3)
keras::install_keras()

c(c(x_train, y_train), c(x_test, y_test)) %<-% keras::dataset_mnist()
x_train <- x_train / 255
x_test <-  x_test / 255

model <- keras_model_sequential(input_shape = c(28, 28)) %>%
  layer_flatten() %>%
  layer_dense(128, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(10)

summary(model)

model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = list("accuracy")
)

predictions <- predict(model, x_train[1:2, , ])
