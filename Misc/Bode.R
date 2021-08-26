library(tidyverse)
library(patchwork)

plot_bode <- function(x)
{
  df <- data.frame(w = x$w, magnitude = x$mag, phase = x$phase)
  
  mag_plot <- ggplot(df, aes(w, magnitude)) + 
    geom_line(colour = "#3f96ce") + 
    ylab("Magnitude [dB]") +
    xlab("Frequency") + 
    scale_x_log10() +
    theme_bw()
  
  phase_plot <- ggplot(df, aes(w, phase)) + 
    geom_line(colour = "#3f96ce") + 
    ylab("Phase (degrees)") +
    xlab("Frequency") + 
    theme_bw() +
    scale_x_log10() 
  
  return ((mag_plot / phase_plot) + plot_annotation(title = 'Bode Diagram'))
}

plot_impulse <- function(x)
{
  x_df <- data.frame(t = x$t, y = x$y[1,])
  
  ggplot(x_df, aes(t, y)) + 
    geom_line(colour = "#3f96ce") + 
    xlab("Time") + 
    ylab("y") +
    theme_bw()
}

plot_step <- function(x)
{
  plot_impulse(x)
}

