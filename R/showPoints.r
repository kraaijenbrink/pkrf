# Show point symbols
# 'Show point symbols'

#' Show point symbols
#'
#' Show a plot with available point symbols and their index in external plot window
#' 
#' @export
showPoints = function() {
  x11(bg='#FFFFFF', height=2, width=12)
  op <- par(mar=rep(0.5,4))
  plot(NA, xlim=c(0,26), ylim=c(0,1), axes=F, xlab='', ylab='')
  points(1:25,rep(0.6,25),pch=1:25, cex=2)
  text(1:25,rep(0.4,25),label=1:25, font=2)
  par <- op
}


