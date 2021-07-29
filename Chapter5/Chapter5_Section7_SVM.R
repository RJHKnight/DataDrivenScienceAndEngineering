library(tidyverse)
library(rgl)
library(rmatio)
library(e1071)



# Plots -------------------------------------------------------------------

# Overlapping parabola
n <- 300

parabola_df <- rbind(
  data.frame(x = (1.5 * rnorm(n))-1.5, sign = 1,  type = "1"),
  data.frame(x = (1.5 * rnorm(n))+1.5, sign = -1, type = "2")
  )

parabola_df <- parabola_df %>% 
  group_by(type) %>% 
  mutate(y = (1.2 * rnorm(n)) + (sign *((x + sign*1.5))^2) - sign*7) %>% 
  ungroup()
  

ggplot(parabola_df, aes(x, y, colour = type)) + geom_point() + theme_bw()

parabola_df <- mutate(parabola_df, z = x^2*y)

# Separation on the z-axis
plot3d(parabola_df$x, parabola_df$y, parabola_df$z, col = parabola_df$type)


# 2 Ellipsis with varying radius
r <- 7 + rnorm(n)
theta <- 2 * pi * runif(n)

ellipse_df <- rbind(
  data.frame(x = rnorm(n), y = rnorm(n), type = "1"),
  data.frame(x = r * cos(theta), y = r * sin(theta), type = "2")
  )

# Add the higher dimensional feature - x^2 + y^2 - or the radius in polar coordinates.
ellipse_df <- mutate(ellipse_df, z = x^2 + y^2)

ggplot(ellipse_df, aes(x, y, colour = type)) + geom_point() + theme_bw()

# Separation on the z-axis
plot3d(ellipse_df$x, ellipse_df$y, ellipse_df$z, col = ellipse_df$type)
planes3d(a = 0, b = 0, c = -1, d = 14, alpha = 0.3)

# SVM ---------------------------------------------------------------------

dog_wav <- read.mat("Data/dogData_w.mat")$dog_wave
cat_wav <- read.mat("Data/catData_w.mat")$cat_wave

cat_dog <- cbind(dog_wav, cat_wav)
svd_res <- svd(cat_dog - rowMeans(cat_dog))

# Take the first 20 entries of V as features
cat_dog_df <- data.frame(svd_res$v[, 1:20]) %>% 
  mutate(class = c(rep("dog", ncol(dog_wav)), rep("cat", ncol(cat_wav)))) %>% 
  mutate(class = as.factor(class))

cat_dog_df_train <- cat_dog_df[c(1:60, 81:140),]
cat_dog_df_test <- cat_dog_df[c(61:80, 141:160),]

# Linear kernel
svm_fit_1 <- svm(class ~ ., data = cat_dog_df_train, kernel = "linear", type = "C")
sum(predict(svm_fit_1, cat_dog_df_test) == cat_dog_df_test$class) / nrow(cat_dog_df_test)

# Radial kernel
svm_fit_2 <- svm(class ~ ., data = cat_dog_df_train, kernel = "radial", type = "C")
sum(predict(svm_fit_2, cat_dog_df_test) == cat_dog_df_test$class) / nrow(cat_dog_df_test)
