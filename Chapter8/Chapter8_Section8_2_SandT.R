library(control)
library(tidyverse)
source("Misc/Bode.R")

L <- tf(1, c(0,1,0))
S <- TF("1/(1+L)")
T <- TF("L/(1+L)")

plot_bode(T=bode(T), L=bode(L), S=bode(S))
