library(control)
library(GA)

set.seed(1)

pid_test <- function(param, return_results = FALSE)
{
  K = TF(paste0(param[1] ,"+", param[2], "/s + ", param[3], "*s / (1+ 0.001 *s)"))
  loop <- series(K, G)
  closed_loop <- feedback(loop, 1)
  t <- seq(0, 20, dt)
  
  res <- step(closed_loop, t)
  
  u <- lsim(K, 1-res$y[1,], t)$y
  Q <- 1
  R <- 0.001
  J <- dt*sum(Q*(1-res$y[1,])^2 + R*u^2)
  
  return (-J)
  
}

dt <- 0.001
G <- TF("1/(s*(s*s+s+1))")
pop_size <- 25
max_generations <- 25

GA <- ga(type = "real-valued", fitness =  pid_test,
         lower = c(0, 0, 0), upper = c(1, 1, 1), 
         popSize = pop_size, maxiter = max_generations, 
         pmutation = 0.15)

solution <- GA@solution
GA@fitnessValue

# Plot best solution
K = TF(paste0(solution[1] ,"+", solution[2], "/s + ", solution[3], "*s / (1+ 0.001 *s)"))
loop <- series(K, G)
closed_loop <- feedback(loop, 1)
t <- seq(0, 20, dt)

res <- step(closed_loop, t)

ggplot2::qplot(x = res$t, y = res$y[1,], main = "GA Solution")
