library(tidyverse)
library(wavelets)
library(imager)
source("Misc/Utils.R")

A <- load.image("data/dog.jpg")
B <- as.matrix(grayscale(A))

# Wavelet decomposition - 2 level
n <- 2
w <- "d2"

res <- dwt(B, n.levels = 2, filter = w)
