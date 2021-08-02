library(keras)
library(rmatio)
library(tidyverse)
library(imager)
library(patchwork)

use_session_with_seed(1)

dog_wav <- read.mat("Data/dogData_w.mat")$dog_wave
cat_wav <- read.mat("Data/catData_w.mat")$cat_wave

dog_cat <- cbind(dog_wav, cat_wav)

# Split into test and train, with 1 hot encoding for y
train_x <- dog_cat[,c(1:40, 81:120)]
train_y <- c(rep(0, 40), rep(1, 40))
train_y <- to_categorical(train_y)
             

test_x <- dog_cat[,c(41:80, 121:160)]
test_y <- train_y

# Note: If you haven't installed keras / python yet, you can do it with:
# keras::install_keras()

# I don't know the exact network structure and hyperparams for the matlab "patternnet",
# but this one behaves reasonably similar
model <- keras_model_sequential()

model %>% 
  layer_dense(units = 10, activation = "relu", input_shape = nrow(train_x)) %>% 
  layer_dense(units = 2, activation = "sigmoid")
  

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = 'rmsprop',
  metrics = c('accuracy')
)

model %>% fit(
  t(train_x), train_y,
  epochs = 100,
  validation_split = 0.2
)

# Plots

plot_one <- function(x, title)
{
  data.frame(x) %>% 
    mutate(X1 = if_else(X1 > X2, 1, 0)) %>% 
    mutate(X2 = if_else(X1 == 0, 1, 0)) %>% 
    mutate(id = 1:n()) %>% 
    pivot_longer(-id, names_to = "type", values_to = "pct") %>% 
    ggplot(aes(id, pct, fill = type)) +
    geom_col() + 
    theme_bw() +
    geom_vline(xintercept = 40, linetype = "dashed") + 
    ggtitle(title)
}

p1 <- plot_one(predict(model, t(train_x)), "In Sample")
p2 <- plot_one(predict(model, t(test_x)), "Out of Sample")

p1 / p2
