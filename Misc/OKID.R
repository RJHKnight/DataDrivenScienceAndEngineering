library(MASS)

# Port of Matlab code by Steve Brunton, November 2010
# OKID code, based on 1991 NASA TM-104069 by Juang, Phan, Horta and Longman
# inputs: y (sampled output), u (sampled input), r (effective system order)
# outputs: H (Markov parameters), M (Observer gain)
#
# lowercase u,y indicate sampled data
# double uppercase UU, YY indicate bold-faced quantities in paper
# single uppercase U, Y indicate script quantities in paper
OKID <- function(y, u, r)
{
  # Step 0: Dimensions
  y_shape <- dim(y)
  PP <- y_shape[1]
  MM <- y_shape[2]
  
  u_shape <- dim(u)
  QQ <- u_shape[1]
  lu <- u_shape[2]
  
  if (!(MM==lu))
  {
    stop("L and Lu need to be the same length.")
  }
  
  # Step 1: Choose p
  LL <- r * 5
  
  # Step 2: Form matrices Y and V as shown in Eq. (7), solve for observer Markov parameters, Ybar
  V <- matrix(0, nrow = QQ + (QQ + PP)*LL, MM)
  
  for (i in 1:MM)
  {
    V[1:QQ, i] = u[1:QQ, i]
  }
  
  for (i in 2:(LL+1))
  {
    for (j in 1:(MM+1-i))
    {
      v_temp <- c(u[,j], y[,j])
      v_index <- (QQ+(i-2)*(QQ+PP)+1) : (QQ+(i-1)*(QQ+PP))
      V[v_index, i+j-1] <- v_temp
    }
  }
  
  Y_bar <- y %*% ginv(V, 1e-3)
  
  # Step 3: Isolate Markov Parameters H and observer gain M
  D <- Y_bar[, 1:QQ]
  
  Y_bar_1 <- array(NA, dim = c(PP, QQ, LL))
  Y_bar_2 <- array(NA, dim = c(PP, QQ, LL))
  
  for (i in 1:LL)
  {
    Y_bar_1[1:PP, 1:QQ, i] <- Y_bar[,(QQ+1+(QQ+PP)*(i-1)):(QQ+(QQ+PP)*(i-1)+QQ)]
    Y_bar_2[1:PP, 1:QQ, i] <- Y_bar[,(QQ+1+(QQ+PP)*(i-1)+QQ):(QQ+(QQ+PP)*i)]
  }
  
  Y <- array(NA, dim = c(PP, QQ, LL))
  
  Y[,,1] <- Y_bar_1[,,1] + Y_bar_2[,,1] %*% D
  
  for (k in 2:LL)
  {
    Y[,,k] <- Y_bar_1[,,k] + Y_bar_2[,,k] %*% D
    
    for (i in 1:(k-1))
    {
      Y[,,k] = Y[,,k] + Y_bar_2[,,i] %*% Y[,,k-i]
    }
  }
  
  H <- array(NA, dim = c(PP, QQ, LL+1))
  H[,,1] <- D
  
  for (k in (2:(LL+1)))
  {
    H[,,k] <- Y[,,k-1]
  }
  
  return (H)
}