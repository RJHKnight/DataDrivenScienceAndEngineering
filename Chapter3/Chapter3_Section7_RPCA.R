library(rmatio)
source("Misc/Utils.R")
source("Misc/RPCA.R")

faces_mat <- read.mat("Data/allFaces.mat")

# First person - all angles.
x <- faces_mat$faces[,1:faces_mat$nfaces[1]]

rpca_res <- rpca(x)

sample_index <- c(3,4,14,15,17,18,19,20,21,32,43)

for (i in sample_index)
{
  par(mfrow = c(2, 2), mar = rep(1, 4), ask = TRUE)
  
  plot(get_one_image(x[,i]), main = "Original", axes = FALSE)
  plot(get_one_image(rpca_res$l[,i]), main = "De-noised", axes = FALSE)
  plot(get_one_image(rpca_res$s[,i]), main = "Noise", axes = FALSE)
}
