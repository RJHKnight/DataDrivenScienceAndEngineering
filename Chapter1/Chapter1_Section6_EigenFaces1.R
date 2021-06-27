library(rmatio)
library(imager)

all_faces <- read.mat("Data/allFaces.mat")

# Extract meta data
num_faces <- all_faces$nfaces
num_persons <- length(num_faces)

get_one_image <- function(person, angle)
{
  index <- sum(num_faces[1:person-1]) + angle
  this_data <- all_faces$faces[,index]
  this_data <- matrix(this_data, nrow = all_faces$n, byrow = FALSE)
  
  return (as.cimg(t(this_data)))
}

# Plot image 1 for the first 36 people
par(mfrow = c(6, 6), mar = rep(1, 4))

for (i in 1:36)
{
  plot(get_one_image(i,1), axes = FALSE)
}

# Plot all angles for the first person
par(mfrow = c(8, 8), mar = rep(1, 4))

for (i in 1:num_faces[1])
{
  plot(get_one_image(1,i), axes = FALSE)
}


