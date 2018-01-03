## Coordinate conversions
## Erin Keleske

## Function that takes a data.frame with a list of x and y coordinates and translates them to distance
## in kilometers from geographic center [0,0]. 

km.coord <- function(dataset){
  
  lon0 <- mean(range(dataset$x))
  lat0 <- mean(range(dataset$y))
  
  rx <- 6371 * acos(sin(lat0 *pi/180)^2 + cos(lat0*pi/180)^2 * cos((lon0+.5)*pi/180 - (lon0-.5)*pi/180))
  ry <- 6371 * acos(sin((lat0 -.5)*pi/180) *  sin((lat0+.5)*pi/180) + cos((lat0-.5)*pi/180) * cos((lat0+.5)*pi/180))
  
  dataset$x <-(dataset$x-lon0)*rx
  dataset$y <-(dataset$y-lat0)*ry
  
  return(dataset)
  
}
  