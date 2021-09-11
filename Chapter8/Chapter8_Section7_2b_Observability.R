library(control)
library(pracma)
library(tidyverse)
library(slicotr)
source("Misc/Gramian.R")

m <- 1
M <- 5
l <- 2
g <- -10
d <- 1

# Pendulum down
b <- -1

A <- matrix(c(
  0,          1,                0, 0,
  0,       -d/M,          b*m*g/M, 0,
  0,          0,                0, 1,
  0, -b*d/(M*l), -b*(m+M)*g/(M*l), 0
), ncol = 4, byrow = TRUE)

B <- matrix(c(0, 1/M, 0, b*1/(M*l)), ncol = 1)

# Only observable if x is measured due to translational invariance.

C_full <- diag(4)

test_one <- function(this_c)
{
  this_c <- matrix(this_c, nrow = 1)
  rank <- Rank(obsv(A, this_c))
  det <- det(obsv(A, this_c))
  
  return (data.frame(c = paste(this_c, collapse = ","), rank = rank, det = det))
}

map_dfr(array_branch(C_full, 1), test_one)


# And if we omit x
A <- A[-1, -1]
B <- B[-1]

C_full <- diag(3)

map_dfr(array_branch(C_full, 1), test_one)

det(obs_gram(A, C))
