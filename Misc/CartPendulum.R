library(graphics)
library(plotrix)
library(scales)

draw_cart_pendulum <- function(state, m, M, L)
{
  x <- state[1]
  th <- state[3]
  
  # Dimensions of cart, wheel and weight
  w <- sqrt(M/5)           # Cart width
  h <- 0.5 * sqrt(M/5)     # Cart height
  w_r <- 0.15              # Wheel radium
  m_r <- 0.2 * sqrt(m)     # Mass radius
  
  # Positions
  y <- w_r/2 + h/2         # Top of the cart
  pend_x <- x + L*sin(th)  # x position of top of pendulum
  pend_y <- y - L*cos(th)  # y position of top of pendulum
  
  # Background
  plot(c(-5, 5), c(-0.1,1), type = "n", asp = 1)
  abline(0,0)
  
  # Cart
  rect(x-w/2,y-h/2, x+w/2, y+h/2, col = alpha("blue", 0.5))  
  
  # Wheels
  draw.circle(x-.65*(w/2), w_r, radius = w_r, col = "grey")
  draw.circle(x+.65*(w/2), w_r, radius = w_r, col = "grey")
  
  # Pendulum
  lines(c(x, pend_x), c(y, pend_y), lwd = 3)
  draw.circle(pend_x, pend_y, m_r, col = alpha("red", 0.5))
}

# # Simulate Lorenz attractor.
# lorenz <- function(t, state, parms)
# {
#   with(as.list(c(state, parms)),
#        {
#          return (list(c(
#            sigma * (y - x),
#            (rho * x) - (x * z) - y,
#            x * y - beta * z
#          )))
#        })
# }


# Simulate Pendulum on a cart
# State:
# x, d_x, th, d_th
# Parms:
# M, L, g, d, u(x)
pend_cart <- function(t,y,m,M,L,g,d,u)
{
  s_x <- sin(y[3])
  c_x <- cos(y[3])
  D <- m * L * L * (M+m*(1-c_x^2))
  
  return (
    matrix(c(
      y[2],
      (1/D)*(-m^2*L^2*g*c_x*s_x + m*L^2*(m*L*y[4]^2*s_x - d*y[2])) + m*L*L*(1/D)*u,
      y[4],
      (1/D)*((m+M)*m*g*L*s_x - m*L*c_x*(m*L*y[4]^2*s_x - d*y[2])) - m*L*c_x*(1/D)*u
    ), 4,1, byrow = TRUE)
  )
}