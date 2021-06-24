library(ggplot2)


# Generate Data -----------------------------------------------------------

# Center of data
x_c <- c(2,1)
# Principle axis
sig <- c(2,0.5)

# Rotation angle
theta <- pi/3

# Rotation matrix
R <- matrix(c(cos(theta), -sin(theta), 
             sin(theta), cos(theta)), 
             ncol = 2, byrow = TRUE)

n <- 10000

rand_matrix <- matrix(rnorm(2 * n), nrow = 2)
X <- R %*% diag(sig) %*% rand_matrix + diag(x_c)%*% matrix(1, 2, n) 


# Plot of Raw Data --------------------------------------------------------

raw_data <- data.frame(t(X))
ggplot(raw_data, aes(X1, X2)) + 
  geom_point()


# SVD Decomp to find Principle Comps --------------------------------------

row_means <- rowMeans(X)
B <- X - row_means 
svd_results <- svd(B/sqrt(n))

# Data frames for the extra info on the plots

get_conf_int <- function(d, U, multiple, row_means)
{
  theta <- seq(0,1, 0.01) * 2 * pi
  x_std <- U %*% diag(d) %*% matrix(c(cos(theta), sin(theta)), nrow = 2, byrow = TRUE)
  x_std <- multiple * x_std + matrix(row_means, ncol = length(theta), nrow = 2)
  res <- data.frame(t(x_std))
  res$multiple <- paste(multiple, "std.")
  
  return (res)
}

conf_int <- rbind(
  get_conf_int(svd_results$d, svd_results$u, 1, row_means),
  get_conf_int(svd_results$d, svd_results$u, 2, row_means),
  get_conf_int(svd_results$d, svd_results$u, 3, row_means)
)

# Principle components
pc <- svd_results$u %*% diag(svd_results$d)

ggplot(raw_data, aes(X1, X2)) + 
  geom_point(alpha = 0.2) + 
  geom_point(aes(colour = multiple), alpha = 1, data = conf_int) +
  # PC1
  annotate("segment", x = row_means[1], xend = pc[1,1] + row_means[1], y = row_means[2], yend = pc[2,1] + row_means[2], colour = "purple", size=1, arrow=arrow()) + 
  annotate("segment", x = row_means[1], xend = pc[1,2] + row_means[1], y = row_means[2], yend = pc[2,2] + row_means[2], colour = "purple", size=1, arrow=arrow())
