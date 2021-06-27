library(rmatio)
library(imager)
library(RSpectra)
library(MASS)
library(tidyverse)

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

# Create image based on a long vector of greyscale values
get_one_image <- function(x)
{
  x_mat <- matrix(x, nrow = 192)
  return (as.cimg(t(x_mat)))
}

# Plot Average Face
plot(get_one_image(avg_face))

# Plot First Eigenface
plot(get_one_image(svd_decomp$u[,1]))

# Plot reconstruction of face 37
test_face <- all_faces$faces[, 1 + sum(num_faces[1:36])]


# demean test face
test_face <- test_face - avg_face

par(mfrow = c(3, 2))

plot(get_one_image(test_face), main = "Original", axes = FALSE)

for (r in c(25,50,100, 400, 800))
{
  u_r <- svd_decomp$u[, 1:r]
  recon_face <- avg_face +  u_r %*% (t(u_r) %*% test_face)
  plot(get_one_image(recon_face), main = paste("Rank", r), axes = FALSE)
}


# Projection of person 2 and 7 onto PC5 and PC6

get_one_person_all_angles_demean <- function(person_number)
{
  all_faces$faces[, (1+sum(num_faces[1:person_number-1])):sum(num_faces[1:person_number])] - avg_face
}

person_two <- get_one_person_all_angles_demean(2)
person_seven <- get_one_person_all_angles_demean(7)

projection_df <- rbind(
  data.frame(t(t(svd_decomp$u[,5:6]) %*% person_two), person = "two"),
  data.frame(t(t(svd_decomp$u[,5:6]) %*% person_seven), person = "seven")
)

ggplot(projection_df, aes(X1, X2, colour = person)) + 
  geom_point()