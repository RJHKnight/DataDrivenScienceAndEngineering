library(tidyverse)
library(patchwork)

# Create top hat function ------------------------------------------------

l <- 10
dx <- l/1024
x <- seq(dx, l, by = dx)
n <- length(x)
n_quart <- floor(n/4)

# Start with all values at 0
f <- 0 * x
# Set to 1 between n/4 and 3n/4
f[n_quart:(3*n_quart)] <- 1

k_max <- 512

a_0 <- sum(f) * dx * 2/l

fourier_decomp <- data.frame(x = x, ffs = f, iteration = "actual")
ffs <- a_0 / 2

for (k in 1:k_max)
{
  # Projection of f onto cos/sin pi*k
  a_k <- sum(f * cos(2 * pi * k * x / l)) * dx * 2/l
  b_k <- sum(f * sin(2 * pi * k * x / l)) * dx * 2/l
  
  ffs <- ffs + a_k * cos(2 * k * pi * x/l) + b_k * sin(k * pi * x/l)
  this_fourier_decomp <- data.frame(x = x, ffs = ffs, iteration = k)
  fourier_decomp <- rbind(fourier_decomp, this_fourier_decomp)
}

# Select points

p1 <- fourier_decomp %>% 
  filter(iteration %in% c("actual", "100")) %>% 
  mutate(iteration = as.factor(iteration)) %>% 
  mutate(is_actual = iteration == "actual") %>% 
  ggplot(aes(x, ffs, colour = iteration, linetype = is_actual)) + 
  geom_line() +
  scale_linetype_manual(values=c("dashed", "solid")) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("100 iterations")

p2 <- fourier_decomp %>% 
  filter(iteration %in% c("actual", "512")) %>% 
  mutate(iteration = as.factor(iteration)) %>% 
  mutate(is_actual = iteration == "actual") %>% 
  ggplot(aes(x, ffs, colour = iteration, linetype = is_actual)) + 
  geom_line() +
  scale_linetype_manual(values=c("dashed", "solid")) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("512 (n/2) iterations")


# All points
p3 <- fourier_decomp %>% 
  filter(iteration %in% c("actual", "10", "50", "100", "250", "500")) %>% 
  mutate(iteration = as.factor(iteration)) %>% 
  mutate(is_actual = iteration == "actual") %>% 
  ggplot(aes(x, ffs, colour = iteration, linetype = is_actual)) + 
  geom_line() +
  scale_linetype_manual(values=c("dashed", "solid")) + 
  theme_bw()


(p1 + p2) / p3
