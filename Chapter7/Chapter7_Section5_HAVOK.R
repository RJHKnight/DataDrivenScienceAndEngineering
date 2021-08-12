library(deSolve)
source("Misc/Utils.R")

params <- c(
  rho = 28,
  sigma = 10,
  beta = 8/3)

n <- 3
x_0 <- c(-8, 8, 27)
names(x_0) <- c("x", "y", "z")
dt <- 0.01
t_span <- seq(dt, 50, by = dt)

this_lorenz <- function(t, state, parms)
{
  with(as.list(c(state, parms)),
       {
         return (list(c(
           sigma * (y - x),
           x * (rho - z) - y,
           x * y - beta * z
         )))
       })
}

this_traj <- ode(x_0, t_span, this_lorenz, params, method = "ode45", atol = 1e-12, rtol = 1e-12)

# Add in timeshifted - x variable only!
stack_max <- 10
rank <- 10
n_row <- nrow(this_traj)

h <- matrix(NA, nrow = stack_max, ncol = n_row - stack_max)

for (i in 1:stack_max)
{
  end_index <- n_row - stack_max -1 + i
  h[i,] <- this_traj[i:end_index,2]
}


# SVD of time delay coords
svd_res <- svd(h)

# Calculate derivatives using 4th order central difference method
nv <- nrow(svd_res$v)
dv <- matrix(NA, nrow = nv-5, rank)
v <- svd_res$v

for (i in 3:(nv-3))
{
  for (k in 1:rank)
  {
    dv[i-2,k] = (1/(12*dt))*(-v[i+2,k]+8*v[i+1,k]-8*v[i-1,k]+v[i-2,k]);
  }
}

v <- v[3:(nrow(v)-3),]

# HAVOK Regression on time delay
x_i <- MASS::ginv(t(v)%*%v)%*%t(v)%*%dv


a = x_i[1:rank-1,1:rank-1]
b = t(tail(x_i[,1:rank-1],1))
