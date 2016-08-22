# function to plot rasters over RDP connection
# 'Remote Raster Plot'

#' Raster plot over RDP
#'
#' Function to plot rasters correctly over an remote desktop protocol connection
#' 
#' @param x Raster as defined by the \code{"raster"} package.
#' @param ... Other parameters parsed to the raster plot function.
#'
#' @return None
#' @export
rrplot <- function(x,...){
  plot(x,useRaster=F,...)
}