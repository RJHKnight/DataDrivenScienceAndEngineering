library(tidyverse)
library(rmatio)
library(patchwork)
library(gghighlight)
source("Misc/Utils.R")

# Load
dog_wav <- read.mat("Data/dogData_w.mat")$dog_wave
cat_wav <- read.mat("Data/catData_w.mat")$cat_wave

cat_dog <- cbind(dog_wav, cat_wav)
svd_res <- svd(cat_dog - rowMeans(cat_dog))

cat_dog_df <- data.frame(pc2   = svd_res$v[,2],
                         pc4   = svd_res$v[,4],
                         class = c(rep("dog", ncol(dog_wav)), rep("cat", ncol(cat_wav))))

cat_dog_df_train <- cat_dog_df[c(1:60, 81:140),]
cat_dog_df_test <- cat_dog_df[c(61:80, 141:160),]

lda_res <- lda(class ~ pc2 + pc4, data = cat_dog_df_train)
lda_means <- lda_res$means
lda_m <- (lda_means[1, ] + lda_means[2,]) / 2
lda_s <- (lda_means[1, 1] - lda_means[2, 1]) / (lda_means[1, 2] - lda_means[2, 2])
lda_i <- lda_m[2] - lda_s * lda_m[1]

p1 <- ggplot(cat_dog_df_train, aes(pc2, pc4, colour = class)) + 
  geom_point() +
  geom_abline(intercept = lda_i, slope = lda_s, linetype = "dashed") + 
  ggtitle("Training")


oos_pred <- predict(lda_res, newdata = cat_dog_df_test)
cat_dog_df_test$prediction <- oos_pred$class

p2 <- ggplot(cat_dog_df_test, aes(pc2, pc4, colour = prediction)) + 
  geom_point() +
  ggtitle("Testing")

p1 / p2

# Accuracy
sum(cat_dog_df_test$class == cat_dog_df_test$prediction) / nrow(cat_dog_df_test)

# Plot of projection of test data onto the 1st (and only) linear discriminant
ld1_df <- data.frame(ld1 = oos_pred$x, prediction = oos_pred$class, actual = cat_dog_df_test$class) %>% 
  arrange(LD1) %>% 
  mutate(n = 1:n())

ggplot(ld1_df, aes(n, LD1, colour = prediction)) + 
  geom_point(aes(shape = actual)) + 
  theme_bw() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 20) +  
  gghighlight(prediction != actual, label_key = actual, use_group_by = FALSE) +
  ggtitle("OOS Prediction in LD1 space.")


# Cross-validation

accuracy_df <- NULL

for (i in 1:100)
{
  this_train <- c(sample(1:80, 60), sample(81:160, 60))
  this_test  <- which(!1:160 %in% this_train)
  
  cat_dog_df_train <- cat_dog_df[this_train,]
  cat_dog_df_test <- cat_dog_df[this_test,]
  
  lda_res <- lda(class ~ pc2 + pc4, data = cat_dog_df_train)
  oos_pred <- predict(lda_res, newdata = cat_dog_df_test)
  
  this_accuracy <- sum(cat_dog_df_test$class == oos_pred$class) / nrow(cat_dog_df_test)
  
  accuracy_df <- rbind(accuracy_df, data.frame(run_num = i, accuracy = this_accuracy))
}

ggplot(accuracy_df, aes(run_num, accuracy)) + 
  geom_col(fill = "blue", alpha = 0.6) +
  geom_hline(yintercept = mean(accuracy_df$accuracy), colour = "red", linetype = "dashed") + 
  scale_y_continuous(labels = scales::percent) + 
  theme_bw()
               
