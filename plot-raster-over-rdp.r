# function to plot rasters over RDP connection
# 'Remote Raster Plot'

#' Raster plot over RDP
#'
#' Function to plot rasters correctly over an Windows remote desktop protocol connection
#' 
#' @param x Raster as defined by the \code{"raster"} package.
#' @param ... Other parameters parsed to the raster plot function.
#'
#' @return Plot of the raster that is correctly displayed
#' @export
rrplot <- function(x,...){
  plot(x,useRaster=F,...)
}