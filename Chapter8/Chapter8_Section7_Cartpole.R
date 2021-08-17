library(animation)
library(QZ)
library(control)
library(pracma)
source("Misc/CartPendulum.R")

m <- 1
M <- 5
l <- 2
g <- -10
d <- 1

# Pendulum up
b <- 1

A <- matrix(c(
  0,          1,                0, 0,
  0,       -d/M,          b*m*g/M, 0,
  0,          0,                0, 1,
  0, -b*d/(M*l), -b*(m+M)*g/(M*l), 0
  ), ncol = 4, byrow = TRUE
)

B <- matrix(c(0, 1/M, 0, b*1/(M*l)), ncol = 1)

geigen(A)
Rank(ctrb(A, B))

# Simulate system with no feedback
x_0 <- c(-1,0, pi+0.1, 0)

res <- ode45(function(t, x) pend_cart(t,x,m,M,l,g,d,0), 0, 15, x_0)

for (i in 1:length(res$t))
{
  dev.hold()
  draw_cart_pendulum(res$y[i,], m, M, l)
  ani.pause()
}


# Now use place to define a K matrix so our system has the desired eigenvalues

p <- matrix(c(-1.1, -1.2, -1.3, -1.4), ncol = 1)
#k <- place(A,B,p)
k <- c(-1.716, -6.6357, 156.932, 61.0714)
eigen(A - B %*% k)
w_r <- c(3,0,pi,0)

u <- function(x)
{
  -k %*% (x-w_r)
}

res <- ode45(function(t, x) pend_cart(t,x,m,M,l,g,d,u(x)), 0, 15, x_0)

for (i in 1:length(res$t))
{
  dev.hold()
  draw_cart_pendulum(res$y[i,], m, M, l)
  ani.pause()
}

