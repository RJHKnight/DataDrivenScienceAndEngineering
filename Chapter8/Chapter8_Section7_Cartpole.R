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

B <- c(0, 1/M, 0, b*1/(M*l))

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