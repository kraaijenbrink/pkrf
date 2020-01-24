# labeler function to create proper scientific notation
# philip kraaijenbrink

#' Clean scientific notation labeler
#'
#' Convert numbers into a scientific notation expression that uses \out{<i>2.4 x 10<sup>-3</sup></i>} format
#' @param x Numeric vector.
#' @param ... Any other argument passed to \code{base::format()}
#' 
#' @return Expression vector
#' @export
scientific10 <- function(x, ...){
  s         <- format(x,scientific=T,...)
  number    <- sapply(strsplit(s,'e'), function(x) x[1])
  expbit    <- as.character(as.integer(sapply(strsplit(s,'e'), function(x) x[2])))
  formatted <- parse(text=paste0(number,'%*%10^',expbit), keep.source=T)
  return(formatted)
}
