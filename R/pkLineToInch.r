# function to convert number of lines to inches
# 'Convert lines to inches'

#' Convert lines to inches
#'
#' Function to convert device lines to inches
#' 
#' @param x Number of lines
#' @param cm Return centimeter instead (logical)
#' @return Number of inches 
#' @export
pkLineToInch <- function(x=1, cm=F){
  inches <- par('cin')[2] * par('cex') * par('lheight') * x
  return(ifelse(cm, inches*2.54, inches))
}