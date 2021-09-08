library(rmatio)
library(imager)

source("./Misc/Utils.R")

all_faces <- read.mat("Data/allFaces.mat")

# Extract meta data
num_faces <- all_faces$nfaces
num_persons <- length(num_faces)

# Plot image 1 for the first 36 people
par(mfrow = c(6, 6), mar = rep(1, 4))

extract_one <- function(person_number, photo_number)
{
  index <- (sum(num_faces[0:(person_number-1)])) + photo_number
  return (get_one_image(all_faces$faces[,index]))
}

for (i in 1:36)
{
  plot(extract_one(i,1), axes = FALSE)
}

# Plot all angles for the first person
par(mfrow = c(8, 8), mar = rep(1, 4))

for (i in 1:num_faces[1])
{
  plot(extract_one(1,i), axes = FALSE)
}


