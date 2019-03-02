# Base plot of raster, but with cut limits
# 'Plot raster with limits

#' Plot raster with limits
#'
#' Base plot of raster, with inlusion of lower and higher plot limits
#' 
#' @param x raster
#' @param lcut Minimum value to plot, all below will be similarly coloured. Set to \code{NA} to disable.
#' @param hcut Maximum value to plot, all above will be similarly coloured. Set to \code{NA} to disable.
#' @param setNA Value in raster to set as \code{NA} in output. Set to \code{NA} to disable.
#' @param useimage Use \code{image.plot} to make the plot rather than \code{plot.raster} (logical).
#' 
#' @return no function return
#' @export
pkRastplot = function(x, lcut=NA, hcut=NA, setNA=NA, useimage=F, ...) {
  
  require(raster)
  
  # perfom cutting
  if ((!is.na(lcut)) & (!is.na(lcut))){
    x[x<lcut] <- lcut
    x[x>hcut] <- hcut
    zlimits <- c(lcut,hcut)
    
  }else if (!is.na(lcut)){
    x[x<lcut] <- lcut
    zlimits   <- c(lcut, cellStats(x,max))
  }
  else if (!is.na(hcut)){
    x[x>hcut] <- hcut
    zlimits   <- c(cellStats(x,min), hcut)
  }else{
    zlimits <- cellStats(x, range)
  }
  
  # set NA
  if (!is.na(setNA)){
    x[x==setNA] <- NA
  }
  
  # plot the image
  if (useimage){
    image(x, zlim=zlimits, ...)
  }else{
    plot(x, zlim=zlimits, ...)
  }
  
}