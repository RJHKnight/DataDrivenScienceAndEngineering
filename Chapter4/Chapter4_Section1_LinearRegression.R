
x <- 1:10
y <- c(0.2,0.5,0.3,3.5,1,1.5,1.8,2,2.3,2.2)

# Infinity norm - max error
fit_one <- function(x_0, x, y)
{
  max(abs(x_0[1] * x + x_0[2] - y))
}

# L1 norm - sum abs error
fit_two <- function(x_0, x, y)
{
  sum(abs(x_0[1]*x + x_0[2] - y))
}

# L2 norm - sum of squares of error
fit_three <- function(x_0, x, y)
{
  sum((x_0[1]*x + x_0[2] - y)^2)
}

p_1 <- optim(c(1,1), function(par) fit_one(par, x, y), method = "BFGS")
p_2 <- optim(c(1,1), function(par) fit_two(par, x, y), method = "BFGS")
p_3 <- optim(c(1,1), function(par) fit_three(par, x, y), method = "BFGS")

# Remove outlier
y2 <- y
y2[4] <- 0.7

p_4 <- optim(c(1,1), function(par) fit_one(par, x, y2), method = "BFGS")
p_5 <- optim(c(1,1), function(par) fit_two(par, x, y2), method = "BFGS")
p_6 <- optim(c(1,1), function(par) fit_three(par, x, y2), method = "BFGS")


par(mfrow = c(2, 1))

plot(x, y, main = "With Outlier")
lines(x, p_1$par[1]*x + p_1$par[2], col = "red")
lines(x, p_2$par[1]*x + p_2$par[2], col = "green")
lines(x, p_3$par[1]*x + p_3$par[2], col = "blue")

plot(x, y2, main = "Without Outlier")
lines(x, p_1$par[1]*x + p_4$par[2], col = "red")
lines(x, p_2$par[1]*x + p_5$par[2], col = "green")
lines(x, p_3$par[1]*x + p_6$par[2], col = "blue")