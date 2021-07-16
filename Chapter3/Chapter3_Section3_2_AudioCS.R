library(tidyverse)
library(CVXR)
library(dtt)
source("Misc/Utils.R")

# Generate 2 tone audio signal
n <- 4096
t <- seq(0,1, length.out = n)
x <- cos(2 * 97 * pi * t) + cos(2 * 777 * pi * t)
x_t <- fftshift(fft(x))
psd <- Re(x_t * Conj(x_t) / n)

# Randomly sample from x
p <- 128
perm <- sample(1:n, p)
y <- x[perm]

# Warning: this is slow...
theta <- dct(diag(1, nrow = n, ncol = n)[perm,])

# L1 minimisation using CVX
s_var <- Variable(n)
objective <- Minimize(norm(s_var, "1"))
constraint_1 <- theta %*% s_var == y
problem <- Problem(objective, constraints = c(constraint_1))
result <- solve(problem)
s <- result$getValue(s_var)

