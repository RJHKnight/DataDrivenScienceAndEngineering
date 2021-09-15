library(control)
library(tidyverse)
library(patchwork)
library(rmatio)
source("Misc/ERA.R")
source("Misc/OKID.R")


# Setup -------------------------------------------------------------------

sys_full <- rmatio::read.mat("DATA/testSys_ABCD.mat")

t <- 0:((r*5)+1)
sys_full <- ss(sys_full$A, sys_full$B, sys_full$C, sys_full$D, 1)


# impulse and lsim from the Control blows up, so here
# is a simple version for discrete systems.

my_lsim <- function(sys, u, t_in)
{
  y <- matrix(0, nrow = length(t_in), ncol = 2)
  x <- matrix(0, nrow = ncol(sys$A), 1)
  
  for (k in 1 : nrow(u))
  {
    y[k,] <- t((sys$C %*% x)  +  sys$D %*% u[k,])
    x <- (sys$A %*% x)  + (sys$B %*% u[k,])
  }
  
  return (y)
}

my_impulse <- function(sys, t_in)
{
  u_impulse_1 <- matrix(0, ncol = 2, nrow = length(t_in))
  u_impulse_2 <- matrix(0, ncol = 2, nrow = length(t_in))
  u_impulse_1[1,1] <- 1
  u_impulse_2[1,2] <- 1
  
  y_1 <- my_lsim(sys, u_impulse_1, t_in)
  y_2 <- my_lsim(sys, u_impulse_2, t_in)
  
  return (list(y1 = y_1, y2 = y_2))
}


# Impulse Response of full system -----------------------------------------

y_full <- my_impulse(sys_full, t)

# Reorder to be size p x q x m
yy <- aperm(array(c(y_full$y1, y_full$y2), dim = c(nrow(y_full$y1), ncol(y_full$y1), 2)), 
            c(2,3,1))


# Compute ERA from Impulse Response ---------------------------------------

r <- 10
num_inputs <- ncol(sys_full$B)
num_outputs <- nrow(sys_full$C)

mco <- floor((nrow(y_full$y1)-1)/2)

era_res <- ERA(yy,mco,mco,num_inputs,num_outputs,r)

sys_era <- ss(era_res$a_r, era_res$b_r, era_res$c_r, era_res$d_r, -1)


impulse_era <- my_impulse(sys_era, t)



# Compute OKID then ERA from Random input ---------------------------------

u_random <- matrix(rnorm(num_inputs * 200), nrow = num_inputs)
y_random <-  t(my_lsim(sys_full, t(u_random), 0:199))

H <- OKID(y_random, u_random, r)
mco <- floor((dim(H)[3]-1)/2)
era_res <- ERA(H,mco,mco,num_inputs,num_outputs,r)
sys_era_okid <- ss(era_res$a_r, era_res$b_r, era_res$c_r, era_res$d_r, -1)

impulse_era_okid <- my_impulse(sys_era_okid, t)

# Plotting ----------------------------------------------------------------

res_df <- rbind(
  data.frame(x1 = y_full$y1[,1], x2 = y_full$y1[,2], input = 1, system = "full"),
  data.frame(x1 = y_full$y2[,1], x2 = y_full$y2[,2], input = 2, system = "full"),
  data.frame(x1 = impulse_era$y1[,1], x2 = impulse_era$y1[,2], input = 1, system = "ERA"),
  data.frame(x1 = impulse_era$y2[,1], x2 = impulse_era$y2[,2], input = 2, system = "ERA"),
  data.frame(x1 = impulse_era_okid$y1[,1], x2 = impulse_era_okid$y1[,2], input = 1, system = "ERA_OKID"),
  data.frame(x1 = impulse_era_okid$y2[,1], x2 = impulse_era_okid$y2[,2], input = 2, system = "ERA_OKID")
)

# Add t
res_df_long <- res_df %>% 
  group_by(input, system) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  pivot_longer(contains("x"), "state", "value")

# Plot
ggplot(res_df_long, aes(t, value, colour = system)) + 
  geom_step() + 
  facet_grid(state ~ input) + 
  theme_bw()
