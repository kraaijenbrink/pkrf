# function to convert number of lines to inches
# 'Convert lines to inches'

#' Convert lines to inches
#'
#' Function to convert device lines to inches
#' 
#' @param x Number of lines.
#' @return Number of inches 
#' @export
linesToInches <- function(x=1){
  par('cin')[2] * par('cex') * par('lheight') * x
}