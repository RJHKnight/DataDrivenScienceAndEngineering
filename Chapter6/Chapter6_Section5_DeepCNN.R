library(keras)
library(imager)
source("Misc/Utils.R")

if (!exists("mnist"))
{
  mnist <- dataset_mnist()
}

num_images_train <- dim(mnist$train$x)[1]
num_images_test <- dim(mnist$test$x)[1]

par(mfrow = c(5, 4), mar = rep(1, 4))
# Plot 20 at random from the training set
for (i in 1:20)
{
  this_id <- sample(1:num_images_train, 1)
  plot(get_one_image(mnist$train$x[this_id, ,], n_row = 28), axes = FALSE, main = mnist$train$y[this_id])
}

# Data Preparation -----------------------------------------------------

# Training and testing sets
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Tensor inputs (n,x,y,z)
img_rows <- 28
img_cols <- 28

x_train <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
x_test <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, 1)

# Transform RGB values into [0,1] range
x_train <- x_train / 255
x_test <- x_test / 255

# 1 hot encode out the 10 output classes
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)


# Model -------------------------------------------------------------------

# Define model
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = 5, activation = 'relu', input_shape = input_shape) %>%
  layer_max_pooling_2d(pool_size = 2, strides = 2) %>%
  layer_flatten() %>% 
  layer_dense(units = 10, activation = 'softmax')


# Compile model
model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_sgd(),
  metrics = c('accuracy')
)

# Train model - a CUDA GPU makes this a lot faster (< 1 sec per epoch)
# reduce the epochs if you are training on a CPU.
model %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 50,
  validation_split = 0.2
)

scores <- model %>% evaluate(
  x_test, y_test, verbose = 0
)

# Output metrics
cat('Test loss:', scores[[1]], '\n')
cat('Test accuracy:', scores[[2]], '\n')


prediction <- model %>% 
  predict(x_test)

prediction_digit <- apply(prediction, 1, which.max) - 1


par(mfrow = c(5, 4), mar = rep(1, 4))
# Plot 20 at random from the test set with prediction
for (i in 1:20)
{
  this_id <- sample(1:num_images_test, 1)
  plot(get_one_image(mnist$test$x[this_id, ,], n_row = 28), 
       axes = FALSE, 
       main = paste("Actual =", mnist$test$y[this_id], "Prediction =", prediction_digit[this_id])
  )
}