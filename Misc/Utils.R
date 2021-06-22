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
