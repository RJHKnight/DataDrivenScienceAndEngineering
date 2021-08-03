# Unit sphere in cartesian coordinates
sphere <- function()
{
  lat <- matrix(seq(90, -90, len = 50) * pi/180, 50, 50, byrow = TRUE)
  long <- matrix(seq(-180, 180, len = 50) * pi/180, 50, 50)
  
  r <- 1 
  x <- r*cos(lat)*cos(long)
  y <- r*cos(lat)*sin(long)
  z <- r*sin(lat)
  
  return (list(x = x, y = y, z = z))
}

# Equivalent to imshow in matlab
plot_matrix <- function(x, x_val = NA, y_val = NA)
{
  x_df <- data.frame(x) %>% 
    mutate(y = 1:n()) %>% 
    pivot_longer(names_to = "x", names_prefix = "X", values_to = "z", -y) %>% 
    mutate(x = as.integer(x))
  
  if (any(!is.na(x_val)))
  {
    x_df$x <- rep(x_val, ncol(x))
  }
  
  if (any(!is.na(y_val)))
  {
    x_df$y <- rep(y_val, each = nrow(x))
  }
  
  ggplot(x_df, aes(x,y)) + 
    geom_raster(aes(fill = z))  + 
    theme_bw() + 
    scale_fill_viridis_c()
}

plot_sv <- function(d)
{
  singular_values <- data.frame(s = d) %>% 
    mutate(j = 1:n()) 
  
  ggplot(singular_values, aes(j, s)) + 
    geom_point() + 
    geom_line() + 
    scale_y_log10()
}


# Create image based on a long vector of greyscale values
get_one_image <- function(x, n_row = 192)
{
  x_mat <- matrix(x, nrow = n_row)
  return (as.cimg(t(x_mat)))
}


# Rearrange the output of an FFT so the 0 frequency is in the center
fftshift <- function(x) 
{
  if (is.vector(x))
    return (fftshift_vector(x))
  
  # First by rows, then by cols
  x <- flip_up_down(x)
  return (flip_left_right(x))
}

ifftshift <- function(x)
{
  if (is.vector(x))
    return (fftshift_vector(x, inverse = TRUE))
  
  # First by cols, then by rows
  x <- flip_left_right(x, inverse = TRUE)
  return(flip_up_down(x, inverse = TRUE))
}

flip_up_down <- function(x, inverse = FALSE) 
{
  n <- nrow(x)
  mid_point <- ifelse(inverse, floor(n/2), ceiling(n/2))
  return(rbind(x[((mid_point+1):n),], x[(1:mid_point), ]))
}

flip_left_right <- function(x, inverse = FALSE) 
{
  n <- ncol(x)
  mid_point <- ifelse(inverse, floor(n/2), ceiling(n/2))
  return(cbind(x[,((mid_point+1):n)], x[, 1:mid_point]))
}

fftshift_vector <- function(x, inverse = FALSE)
{
  n <- length(x)
  mid_point <- ifelse(inverse, floor(n/2), ceiling(n/2))
  
  return (c(x[(mid_point+1):n], x[1:mid_point]))
}

ml_divide <- function(A, B)
{
  MASS::ginv(t(A)%*%A)%*%t(A)%*%B
}

no_label_dend <- function(x) 
{
  if (stats::is.leaf(x)) 
  {
    attr(x, "label") <- NULL 
  }
  
  return(x)
}

# Very naively handle the situation where xp / yp is out of the range of x,y
my_interp2 <- function(x,y,Z, xp, yp, method = c("linear", "nearest", "constant"))
{
  xp = max(min(x), min(xp, max(x)))
  yp = max(min(y), min(yp, max(y)))
  
  return (interp2(x, y, Z, xp, yp, method = method))
}
