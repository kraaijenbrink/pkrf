# labeler function to create proper scientific notation
# philip kraaijenbrink

#' Clean scientific notation labeler
#'
#' Convert numbers into a scientific notation expression that uses \out{<i>2.4 x 10<sup>-3</sup></i>} format
#' @param x Numeric vector.
#' @param ... Any other argument passed to \code{scales::scientific()}
#' 
#' @return Expression vector
#' @export

scientific10 <- function(x, ...){
  
  # get scientific notation from scales function
  s <- scales::scientific(x, ...)

  # get separate components
  number   <- sapply(strsplit(s,'e'), function(x) x[1])
  expbit   <- as.integer(sapply(strsplit(s,'e'), function(x) x[2]))
  # expsign  <- substr(expbit,0,1)
  # exponent <- as.integer(substr(expbit,2,1e3))
  
  # contruct expressions
  formatted <- parse(text=paste0(number,'%*%10^',expbit))
  return(formatted)
}