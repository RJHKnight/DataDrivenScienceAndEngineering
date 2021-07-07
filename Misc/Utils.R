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
plot_matrix <- function(x)
{
  x_df <- data.frame(x) %>% 
    mutate(y = 1:n()) %>% 
    pivot_longer(names_to = "x", names_prefix = "X", values_to = "z", -y) %>% 
    mutate(x = as.integer(x))
  
  ggplot(x_df, aes(x,y, fill = z)) + 
    geom_raster()  + 
    theme_bw() + 
    scale_fill_viridis_c()
}


# Create image based on a long vector of greyscale values
get_one_image <- function(x)
{
  x_mat <- matrix(x, nrow = 192)
  return (as.cimg(t(x_mat)))
}