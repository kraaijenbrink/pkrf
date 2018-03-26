# function to make a color transparant
# 'Make color transparant'

#' Make color transparant
#'
#' Make color transparant
#' 
#' @param x (vector of) color(s)
#' @param alpha transparency value
#' @return transparant (vector of) color(s)
#' @export
makeTransparent = function(colors=c('red','green','blue'), alpha=0.5) {
  
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  
  cols    <- col2rgb(col=colors, alpha=FALSE)
  alphacols <- sapply(1:ncol(cols), function(i)
    sprintf('#%s%s%s%s',
            format(as.hexmode(cols[1,i]), width=2),
            format(as.hexmode(cols[2,i]), width=2),
            format(as.hexmode(cols[3,i]), width=2),
            format(as.hexmode(floor(255*alpha)), width=2)
            )
    )
  return(alphacols) 
  }