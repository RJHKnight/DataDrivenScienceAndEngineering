library(tidyverse)

t <- seq(0, 10, by=0.1)

# Reference speed
w_r <- 60 + 0*t
# Disturbance (hills)
d <- 10 * sin(pi * t)

# Predicted response
a_model <- 1
# Actual response
a_true <- 0.5

# Open loop - fixed "U" based on model
u_ol <- w_r / a_model
y_ol <- a_true * u_ol + d

# Add in feedback with gain = 50
k <- 50
y_cl <- a_true * k/ (1 + a_true*k) * w_r + d / (1 + a_true*k)

y_df <- data.frame(t = t, openloop = y_ol, closedloop = y_cl, reference = w_r) %>% 
  pivot_longer(-t, names_to = "type", values_to = "y")


ggplot(y_df, aes(t, y, colour = type)) + 
  geom_line() +
  geom_line(data = data.frame(t = t, y = d, type = "disturbance"), linetype = "dashed", colour = "black") + 
  theme_bw() + 
  xlab("Time") + 
  ylab("Speed")
 

