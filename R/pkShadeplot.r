# Plot shaded lines for data series (after plot has been initiated)


#' Plot shaded polygons
#' 
#' Plot shaded polygons between any number of input lines
#' 
#' @param x Data frame with x values
#' @param y Data frame with y values
#' @param clr vector with colors
#' @param ... Other parameters parsed to polygon function
#'
#' @return Shaded areas in current plot
#' @export
plotRibbons <- function(x,y,clr,...){
  for (i in 1:(ncol(x)-1)){
    polygon(c(x[,i],rev(x[,i+1])),
            c(y[,i],rev(y[,i+1])),
              col=clr[i],
            ...)
  }
}
  
  
  
#' @rdname plotRibbons
#' @export
pkShadeplot <- plotRibbons