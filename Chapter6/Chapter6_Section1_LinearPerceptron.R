library(rmatio)
library(MASS)
library(patchwork)
library(glmnet)
library(imager)
source("Misc/Utils.R")

dog_wav <- read.mat("Data/dogData_w.mat")$dog_wave
cat_wav <- read.mat("Data/catData_w.mat")$cat_wave

dog_cat <- cbind(dog_wav, cat_wav)

train_x <- dog_cat[,c(1:60, 81:140)]
train_y <- c(rep(1, 60), rep(-1, 60))

test_x <- dog_cat[,c(61:80, 141:160)]
test_y <- c(rep(1, 20), rep(-1, 20))

# Least squares regularisation using pseudo-inverse
a_l2 <- train_y %*% ginv(train_x)
test_predict <- sign(a_l2 %*% test_x)
l2_df <- data.frame(prediction = t(test_predict), id = 1:length(test_predict), type = "l2")

# Add a L1 penalty with LASSO
a_l1 <- glmnet(t(train_x), train_y, alpha = 1, lambda = 0.1, intercept = FALSE)
test_predict <- sign(predict(a_l1, t(test_x)))
l1_df <- data.frame(prediction = as.numeric(test_predict), id = 1:length(test_predict), type = "l1")

# Plot of predictions
pred_plot <- ggplot(rbind(l2_df, l1_df), aes(id, prediction, fill = type)) + 
  geom_col() + 
  geom_vline(xintercept = 20, linetype = "dashed") + 
  facet_wrap(~ type, nrow = 1) + 
  theme_bw() + 
  theme(legend.position = "none") +
  ggtitle("Predicted vs position")


# Note: Drop first element of coef. from lasso fit as intercept is returned, even if it is not in the fit.
coef_df <- rbind(data.frame(pixel = 1:nrow(dog_cat), coef = t(a_l2), type = "l2"),
                 data.frame(pixel = 1:nrow(dog_cat), coef = as.numeric(coef(a_l1))[-1], type = "l1")) 

coef_plot <- ggplot(coef_df, aes(pixel, coef, colour = type)) + 
  geom_line() + 
  facet_wrap(~ type, nrow = 1) + 
  theme_bw() + 
  theme(legend.position = "none") +
  ggtitle("Weight vs. pixel position")

pred_plot / coef_plot

# Plot of weightings as images.
par(mfrow = c(1, 2))

plot(get_one_image(a_l2, n_row = 32), axes = FALSE, main = "L2")
plot(get_one_image(coef(a_l1)[-1], n_row = 32), axes = FALSE, main = "L1")

