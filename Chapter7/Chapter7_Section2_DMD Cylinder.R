library(tidyverse)
source("Misc/DynamicModeDecomposition.R")
source("Misc/Utils.R")

vortall <- readRDS("Data/vortall.RDS")
x <- vortall[,1:(ncol(vortall)-1)]
x_prime <- vortall[,-1]

dmd_res <- dmd(x, x_prime, 21)

plot_cylinder(Im(dmd_res$phi[,2]))
plot_cylinder(Re(dmd_res$phi[,2]))
