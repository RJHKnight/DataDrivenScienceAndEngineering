library(tidyverse)
library(dendextend)
source("Misc/Utils.R")

n <- 150
train_index <- 1:100
test_index  <- 101:150

# Group 1: Random ellipse centered at (0,0)
x_1 <- rnorm(n)
y_1 <- 0.5 * rnorm(n)

# Group 2: Random ellipse centered at (1,-2) and rotated by theta
x_2 <- rnorm(n) + 1
y_2 <- (0.2 * rnorm(n)) - 2

theta <- pi/4

# Rotation matrix
rot <- matrix(c(cos(theta), -sin(theta),
                sin(theta), cos(theta)),
              nrow = 2, byrow = TRUE)

xy_2 <- rot %*% rbind(x_2, y_2)
x_2 <- xy_2[1,]
y_2 <- xy_2[2,]

xy_df <- rbind(
  data.frame(x = x_1, y = y_1, class = "1"),
  data.frame(x = x_2, y = y_2, class = "2")
)

dist_xy <- stats::dist(dplyr::select(xy_df, x, y), method = "euclidean")

clust_res <- hclust(dist_xy, method = "average")

dend <- as.dendrogram(clust_res)

# High threshold
threshold = 0.85 * max(clust_res$height)
dend <- colour_branches(dend, h =threshold)
plot(stats::dendrapply(dend, no_label_dend))

# Low threshold
threshold = 0.25 * max(clust_res$height)
dend <- colour_branches(dend, h =threshold)
plot(stats::dendrapply(dend, no_label_dend))

