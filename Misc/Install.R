# Core Packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("MASS")) install.packages("MASS")
if (!require("deSolve")) install.packages("deSolve")
if (!require("pracma")) install.packages("pracma")
if (!require("tuneR")) install.packages("tuneR")
if (!require("wavelets")) install.packages("wavelets")
if (!require("CVXR")) install.packages("CVXR")
if (!require("dtt")) install.packages("dtt")
if (!require("glmnet")) install.packages("glmnet")
if (!require("RSpectra")) install.packages("RSpectra")
if (!require("pracma")) install.packages("pracma")
if (!require("tseries")) install.packages("tseries")
if (!require("mclust")) install.packages("mclust")
if (!require("e1071")) install.packages("e1071")
if (!require("signal")) install.packages("signal")

# Keras requires a python install - see keras::install_keras() for more details.
if (!require("keras")) install.packages("keras")

# Plotting and Visualisation
if (!require("imager")) install.packages("imager")
if (!require("patchwork")) install.packages("patchwork")
if (!require("gganimate")) install.packages("gganimate")
if (!require("rgl")) install.packages("rgl")
if (!require("dendextend")) install.packages("dendextend")

# Matlab
if (!require("rmatio")) install.packages("rmatio")
if (!require("QZ"))install.packages("QZ")

# Cartpole
if (!require("animation")) install.packages("animation")
if (!require("control")) install.packages("control")
if (!require("plotrix")) install.packages("plotrix")
if (!require("scales")) install.packages("scales")

# SLICOT - this requires a FORTRAN compiler
if (!require("devtools")) install.packages("devtools")
if (!require("animation")) devtools::install_github("rjhknight/slicotr")

if (!require("GA")) install.packages("GA")