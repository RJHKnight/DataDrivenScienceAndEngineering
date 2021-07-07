library(rmatio)
library(imager)
library(RSpectra)
library(tidyverse)
library(gghighlight)
library(rsvd)

source("./Misc/Optimal Hard Threshold.R")
source("./Misc/Utils.R")

all_faces <- read.mat("Data/allFaces.mat")

# Extract meta data
num_faces <- all_faces$nfaces
num_persons <- length(num_faces)

training_faces <- all_faces$faces[, 1:sum(num_faces[1:36])]
avg_face <- rowMeans(training_faces)

# De-mean
training_faces <- training_faces - avg_face

# Full SVD takes a very long time, to speed it up I will just compute for the first 800 singular values.
svd_decomp <- svds(training_faces, k = 800)
svd_decomp_2 <- rsvd(training_faces)



beta <- ncol(training_faces) / nrow(training_faces)
threshold <- optimal_SVHT_coef(beta) * median(svd_decomp$d)
cut_off <- max(which(svd_decomp$d >= threshold))

# Plot singular values
singlular_values_df <- data.frame(s = svd_decomp$d) %>% 
  mutate(j = 1:n()) %>% 
  mutate(type = case_when(
    s >= threshold ~ "Above Threshold",
    TRUE           ~ "Below Threshold"))

ggplot(singlular_values_df, aes(j, s)) +
  geom_line() + 
  geom_point(aes(colour = type)) + 
  geom_hline(yintercept = threshold)


# Plot faces

par(mfrow = c(2, 2), mar = rep(1, 4))

for (i in c(100, cut_off, 700))
{
  plot(get_one_image(svd_decomp$u[,i]), axes = FALSE)
}


