library(rgl)
source("Misc/Utils.R")


# Rotation Matrix ---------------------------------------------------------

theta <- c(pi/15, -pi/9, -pi/20)  # Rotations in x,y,z
sigma <- diag(c(3, 1, 0.5))       # Scale in x,y,z

# Rotation about x-axis
r_x <- matrix(c(1, 0, 0,
                0, cos(theta[1]), -sin(theta[1]),
                0, sin(theta[1]), cos(theta[1])),
              ncol = 3, byrow = TRUE)


# Rotation about y-axis
r_y <- matrix(c(cos(theta[2]), 0, sin(theta[2]),
                0, 1, 0,
                -sin(theta[2]),0, cos(theta[2])),
              ncol = 3, byrow = TRUE)

# Rotation about z-axis
r_z <- matrix(c(cos(theta[3]), -sin(theta[3]), 0,
                sin(theta[3]), cos(theta[3]), 0,
                0, 0, 1),
              ncol = 3, byrow = TRUE)

# Rotation and Scaling
X <- r_z %*% r_y %*% r_x %*% sigma                   

res <- sphere()
x <- res$x
y <- res$y
z <- res$z

# Transformations ---------------------------------------------------------

x_r = 0* res$x
y_r = 0* res$y
z_r = 0* res$z


for (i in 1:nrow(x))
{
  for (j in 1:ncol(x))
  {
    this_vector <- c(x[i,j], y[i,j], z[i,j])
    this_vector_r <- X %*% this_vector
    
    x_r[i,j] <- this_vector_r[1]
    y_r[i,j] <- this_vector_r[2]
    z_r[i,j] <- this_vector_r[3]
  }
}


# Plotting ----------------------------------------------------------------

open3d()
mfrow3d(1, 2, byrow = FALSE, sharedMouse = TRUE)

# Plot our unit sphere.
next3d()
persp3d(res$x, res$y, res$z,
        xlim = c(-3,3),
        ylim = c(-3,3),
        zlim = c(-3,3),
        col = "red")

next3d()
persp3d(x_r, y_r, z_r,
        xlim = c(-3,3),
        ylim = c(-3,3),
        zlim = c(-3,3), 
        col = "lightblue")
