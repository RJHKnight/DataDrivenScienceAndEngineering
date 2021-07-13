library(tidyverse)
source("Misc/Utils.R")

n <- 64
l <- 30
dx <- l/n
x <- seq(-l/2, l/2-dx, by = dx)

# True function - cos function with gaussian envelope
f <- cos(x) * exp(-x^2/25)

# True analytical derivative by chain rule
df <- -(sin(x) * exp(-x^2/25) + 2/25 * x * f)

# Finite difference method
df_fd <- rep(NA, length(df))

for (kappa in 1:(length(df)-1))
{
  df_fd[kappa] <- (f[kappa+1] - f[kappa]) / dx
}

df_fd[length(df_fd)] <- df_fd[length(df_fd)-1]


# Spectral derivative
f_hat <- fft(f)
kappa <- (2*pi/l) * seq(-n/2, (n/2)-1)
kappa <- fftshift(kappa)
df_hat <- 1i * kappa * f_hat
df_fft <- Re(fft(df_hat, inverse = TRUE)) / n

# Plotting
res_df <- rbind(
  data.frame(x = x, f = df,     type = "Analytical Derivative"),
  data.frame(x = x, f = df_fd,  type = "Finite Difference"),
  data.frame(x = x, f = df_fft, type = "Spectral Derivative")
)

ggplot(res_df, aes(x, f, colour = type)) + 
  geom_line()