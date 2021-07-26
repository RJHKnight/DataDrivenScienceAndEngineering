library(rmatio)
library(imager)
library(tidyverse)
library(rgl)
source("Misc/Utils.R")

# Load
dog <- read.mat("Data/dogData.mat")$dog
cat <- read.mat("Data/catData.mat")$cat
dog_wav <- read.mat("Data/dogData_w.mat")$dog_wave
cat_wav <- read.mat("Data/catData_w.mat")$cat_wave

# SVD
cat_dog <- cbind(dog, cat)
cat_dog_wave <- cbind(dog_wav, cat_wav)

svd_res <- svd(cat_dog - rowMeans(cat_dog))
svd_res_wav <- svd(cat_dog_wave - rowMeans(cat_dog_wave))


# Raw Data ----------------------------------------------------------------

par(mfrow = c(2, 2))

# Plot of eigenfaces
for (i in 1:4)
{
  plot(get_one_image(svd_res$u[,i], n_row = 64), axes = FALSE)
}

# Plot of V
v_df <- data.frame(svd_res$v[,1:4]) %>% 
  mutate(i = 1:n()) %>% 
  pivot_longer(-i, names_to = "id", values_to = "energy", names_prefix = "X") 

ggplot(v_df, aes(i, energy, fill = id)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~ id, ncol = 1) + 
  geom_vline(xintercept = 80, linetype = "dashed")

# Distribution of First 4 V's
v_df %>% 
  mutate(type = if_else(i > 80, "cat", "dog")) %>% 
  ggplot(aes(energy, fill = type)) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~ id)


plot3d(
  x = svd_res$v[,1],
  y = svd_res$v[,2],
  z = svd_res$v[,3],
  col = c(rep("red", 80), rep("blue", 80)),
  type = "s", radius = 0.01,
  main = "Projection of raw images into feature space"
)


# Wavelet Transformed Data ------------------------------------------------

# Plot of transformed data
for (i in 1:4)
{
  plot(get_one_image(cat_dog_wave[,i], n_row = 32), axes = FALSE)
}

# Plot of eigenfaces
for (i in 1:4)
{
  plot(get_one_image(svd_res_wav$u[,i], n_row = 32), axes = FALSE)
}

v_wav_df <- data.frame(svd_res_wav$v[,1:4]) %>% 
  mutate(i = 1:n()) %>% 
  pivot_longer(-i, names_to = "id", values_to = "energy", names_prefix = "X") 

ggplot(v_wav_df, aes(i, energy, fill = id)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~ id, ncol = 1) + 
  geom_vline(xintercept = 80, linetype = "dashed")


v_comb <- rbind(
  mutate(v_df, data = "raw"),
  mutate(v_wav_df, data = "wavelet")
)

v_comb %>% 
  mutate(type = if_else(i > 80, "cat", "dog")) %>% 
  ggplot(aes(energy, fill = type)) + 
  geom_density(alpha = 0.2) + 
  facet_grid(id ~ data)


plot3d(
  x = svd_res_wav$v[,1],
  y = svd_res_wav$v[,2],
  z = svd_res_wav$v[,3],
  col = c(rep("red", 80), rep("blue", 80)),
  type = "s", radius = 0.01,
  main = "Projection of raw images into feature space"
)
