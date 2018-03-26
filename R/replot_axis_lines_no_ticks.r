# replot all outer axis lines with no ticks or labels
# requires existing plot

#' Replot axis lines
#'
#' Overplot the axis lines without any labels or ticks.
#' Useful when lines or polygons were plotted over the original axes.
#' 
#' @return NULL
#' @export
overplotAxes <- function(...){
  l <- par('usr')
  axis(1,at=l[1:2],labels=NA,tck=0,...)
  axis(2,at=l[3:4],labels=NA,tck=0,...)
  axis(3,at=l[1:2],labels=NA,tck=0,...)
  axis(4,at=l[3:4],labels=NA,tck=0,...)
  return(NULL)
}
  
  
