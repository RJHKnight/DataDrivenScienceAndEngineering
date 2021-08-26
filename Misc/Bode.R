library(tidyverse)
library(patchwork)

plot_bode <- function(...)
{
  names <- names(list(...))
  
  if (is.null(names))
  {
    names <- "S"
  }
  
  raw <- list(...)
  
  df <- NULL
  
  for (i in 1:length(raw))
  {
    df <- rbind(df, data.frame(w = raw[[i]]$w, magnitude = raw[[i]]$mag, phase = raw[[i]]$phase, name = names[i]))
  }
  
  mag_plot <- ggplot(df, aes(w, magnitude, colour = name)) + 
    geom_line() + 
    ylab("Magnitude [dB]") +
    xlab("Frequency") + 
    scale_x_log10() +
    theme_bw()
  
  phase_plot <- ggplot(df, aes(w, phase, colour = name)) + 
    geom_line() + 
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

