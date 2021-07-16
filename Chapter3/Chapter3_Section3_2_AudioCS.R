library(tidyverse)
library(CVXR)
library(dtt)
library(patchwork)
source("Misc/Utils.R")

# Generate 2 tone audio signal
n <- 4096
t <- seq(0,1, length.out = n)
x <- cos(2 * 97 * pi * t) + cos(2 * 777 * pi * t)
x_t <- fft(x)
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
constraint_1 <- theta %*% s_var - y == 0
problem <- Problem(objective, constraints = c(constraint_1))
result <- solve(problem)
s <- result$getValue(s_var)
x_recon <- n/2 * dct(t(s), inverted = TRUE)[1,]

x_t_recon <- fft(x_recon)
psd_recon <- Re(x_t_recon * Conj(x_t_recon) / n)

# Plotting
freq = n/(n)*(0:n)   #create the x-axis of frequencies in Hz
l = 1:floor(n/2)     #only plot the first half of freqs

psd_df <- rbind(
  data.frame(freq = freq[l], psd = psd[l], type = "original"),
  data.frame(freq = freq[l], psd = psd_recon[l], type = "recovered")
)

# Power spectrum plot
p1 <- ggplot(psd_df, aes(freq, psd, col = type)) + 
  geom_line() + 
  facet_wrap(~ type) + 
  theme(legend.position = "none")


# Raw x plot (subset)
x_df <- rbind(
  data.frame(t, x = x, type = "original"),
  data.frame(t, x = x_recon, type = "recovered")
)

x_df_highlight <- x_df[perm,]

p2 <- ggplot(x_df, aes(t, x)) + 
  geom_line() + 
  geom_point(data = x_df_highlight, colour = "red") + 
  scale_x_continuous(limits = c(t[1024],t[1200])) + 
  facet_wrap(~ type)

p1 / p2
