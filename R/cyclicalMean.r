# Cyclical mean
# 'Cyclical mean'

#' Cyclical mean
#'
#' Calculate the mean of a set of cyclical values, e.g. calendar months, day of year, angles. The code transform the cycle to cartesian vectors, calculates summed vector, and get the cycle of the sum vector.
#' 
#' @param x vector of cyclical values
#' @param steps number of steps in the cycle
#' 
#' @return collapsed string
#' @export
cyclicalMean = function(x, steps=360){
  radial_frac = 2*pi/steps
  x_angle = radial_frac * x
  vec_x = cos(x_angle)
  vec_y = sin(x_angle)
  vec_x_mean = mean(vec_x)
  vec_y_mean = mean(vec_y)
  a_mean = atan2(vec_y_mean, vec_x_mean)
  x_mean = steps * a_mean / (2 * pi)
  x_mean = ifelse(x_mean < 0, steps+x_mean, x_mean)
  x_mean
}

