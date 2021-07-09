library(tidyverse)
library(patchwork)

# Create triangle function ------------------------------------------------

dx <- 0.001
l <- pi
x <- seq(-1 + dx, 1, by = dx) * l
n <- length(x)
n_quart <- floor(n/4)
n_half <- floor(n/2)

# Start with all values at 0
f <- 0 * x
# Linearly increasing from n/4 -> n/2 between 0:1
f[n_quart:n_half] <- 4 * (1:(n_quart+1)) / n
# Linearly decreasing from n/2 -> 3n/4 between 0:1
f[(n_half+1):((3 * n_quart))] <- 1 - (4 * (0:(n_quart-1))) / n


# Fourier Decomp ----------------------------------------------------------

k_max <- 7

a_0 <- sum(f) * dx

fourier_decomp <- data.frame(x = x, ffs = f, iteration = "actual")
ffs <- a_0 / 2

for (k in 1:k_max)
{
  # Projection of f onto cos/sin pi*k
  a_k <- sum(f * cos(pi * k * x / l)) * dx
  b_k <- sum(f * sin(pi * k * x / l)) * dx
  
  ffs <- ffs + a_k * cos(k * pi * x/l) + b_k * sin(k * pi * x/l)
  this_fourier_decomp <- data.frame(x = x, ffs = ffs, iteration = k)
  fourier_decomp <- rbind(fourier_decomp, this_fourier_decomp)
}

fourier_decomp %>% 
  mutate(iteration = as.factor(iteration)) %>% 
  mutate(is_actual = iteration == "actual") %>% 
  ggplot(aes(x, ffs, colour = iteration, linetype = is_actual)) + 
  geom_line() +
  scale_linetype_manual(values=c("dashed", "solid")) + 
  theme_bw()


# Amplitude Plot ----------------------------------------------------------

k_max <- 100

vector_norm <- function(x)
{
  sqrt(sum(x^2))
}

# Set up variables to hold coefficients and error at each step
a <- rep(NA, k_max+1)
err <- rep(NA, k_max+1)

# 0 step
ffs <- a_0 / 2
a[1] <- a_0 / 2
err[1] <- vector_norm(f - ffs)
norm_f <- vector_norm(f)

for (k in 1:k_max)
{
  a[k+1] <- sum(f * cos(pi * k * x/l)) * dx
  b_k <- sum(f * sin(pi * k * x/l)) * dx
  ffs <- ffs + a[k+1] * cos(k * pi * x/l) + b_k * sin(k * pi * x /l)
  err[k+1] <- vector_norm(f - ffs) / norm_f
}

res <- data.frame(a_k = a, error = err) %>% 
  mutate(k = 1:n())

# Amplitude plot
p1 <- ggplot(res, aes(k, a)) + 
  geom_line() + 
  scale_y_log10() + 
  theme_bw()

p2 <- ggplot(res, aes(k, error)) + 
  geom_line() + 
  scale_y_log10() + 
  theme_bw()

p1 / p2




