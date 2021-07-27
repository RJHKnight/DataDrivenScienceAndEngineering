library(tidyverse)
library(rmatio)
library(mclust)
library(imager)
library(mvtnorm)
source("Misc/Utils.R")

# Load
dog_wav <- read.mat("Data/dogData_w.mat")$dog_wave
cat_wav <- read.mat("Data/catData_w.mat")$cat_wave
 
cat_dog <- cbind(dog_wav, cat_wav)
svd_res <- svd(cat_dog - rowMeans(cat_dog))

# Choose the 2nd and 4th columns of 'v'
key_projections <- svd_res$v[,c(2,4)]


# Gaussian Mixture with 2 clusters using EM Algorithm
gmm_model <- Mclust(key_projections, G = 2, modelNames = "VVV")

key_projections_df <- data.frame(x= key_projections[,1], y = key_projections[,2]) %>% 
  mutate(actual = if_else(row_number() <= 80, "Dog", "Cat"))

# Contours of the distribution
data_grid <- expand.grid(x = seq(min(key_projections_df$x), max(key_projections_df$x), length.out = 100),
                         y = seq(min(key_projections_df$y), max(key_projections_df$y), length.out = 100))

cluster_1 <- data_grid %>% 
  mutate(prob =  dmvnorm(data_grid, mean = gmm_model$parameters$mean[1,], sigma = gmm_model$parameters$variance$sigma[,,1])) %>% 
  mutate(cluster = "1")

cluster_2 <- data_grid %>% 
  mutate(prob =  dmvnorm(data_grid, mean = gmm_model$parameters$mean[2,], sigma = gmm_model$parameters$variance$sigma[,,2])) %>% 
  mutate(cluster = "2")


ggplot(key_projections_df, aes(x, y, colour = actual)) + 
  geom_point() + 
  geom_contour(aes(x, y, z = prob), colour = "blue", data = cluster_1, bins = 5) + 
  geom_contour(aes(x, y, z = prob), colour = "red", data = cluster_2, bins = 5) + 
  theme_bw()
  


plot(mclustBIC(key_projections, modelNames = "VVV", G = 1:20))

gmm_model_extreme <- Mclust(key_projections, G = 15, modelNames = "VVV")

key_projections_df$cluster <- gmm_model_extreme$classification
ggplot(key_projections_df, aes(x, y, colour = as.factor(cluster))) + 
  geom_point()


# Look at some animals in the same cluster
dog <- read.mat("Data/dogData.mat")$dog
cat <- read.mat("Data/catData.mat")$cat

dog_cat_raw <- cbind(dog, cat)

ids <- which(gmm_model_extreme$classification == 11)
par(mfrow = c(3, 3))

for (i in ids[1:9])
{
  plot(get_one_image(dog_cat_raw[,i], n_row = 64), axes = FALSE)
}

