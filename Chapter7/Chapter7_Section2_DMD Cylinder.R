library(tidyverse)
library(patchwork)
source("Misc/DynamicModeDecomposition.R")
source("Misc/Utils.R")

vortall <- readRDS("Data/vortall.RDS")
x <- vortall[,1:(ncol(vortall)-1)]
x_prime <- vortall[,-1]

dmd_res <- dmd(x, x_prime, 21)

p1 <- plot_cylinder(Re(dmd_res$phi[,2]))
p2 <- plot_cylinder(Re(dmd_res$phi[,10]))

# Plot mode 2 and 10.
p1 / p2
