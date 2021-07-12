library(tidyverse)
library(patchwork)

dt <- 0.001
t <- seq(0,1, by = dt)

# Clean signal is sum of 2 sin waves of 50hz and 120hz
f_clean <- sin(2*pi*50*t) + sin(2*pi*120*t)

# Add noise
f <- f_clean + 2.5 * rnorm(length(t))

signal_df <- rbind(
  data.frame(t = t, f = f, type = "noisy"),
  data.frame(t = t, f = f_clean, type = "clean"))


n <- length(t)
f_hat <- fft(f)
psd <- Re(f_hat * Conj(f_hat) / n)
freq <- (1/(dt*n)) * seq(0, n-1)

# Use the PSD to filter out noise
f_hat[which(psd < 100)] <- 0

# Inverse fourier to recover the cleaned signal - note the 'n' which is different to matlab.
f_filt <- fft(f_hat, inverse = TRUE) / n


# Plots

# Clean and noisy
p1 <- ggplot(signal_df, aes(t, f, colour = type)) + 
  geom_line() + 
  ggtitle("Original and Noisy") + 
  theme_bw()

# Filtered
p2 <- data.frame(t = t, f = Re(f_filt)) %>% 
  ggplot(aes(t, f)) + 
  geom_line() + 
  ggtitle("Filtered") + 
  theme_bw()

# Power spectrum
p3 <- data.frame(freq = freq, psd = psd) %>% 
  head(500) %>% 
  ggplot(aes(freq, psd)) + 
  geom_line() + 
  ylab("Power") + 
  xlab("Frequency") + 
  geom_hline(yintercept = 100) + 
  ggtitle("Power Spectrum") + 
  theme_bw()

p1 / p2 / p3
