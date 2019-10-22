# function to create a custom heatmap using polygons


#' Create heat map
#' 
#' Function for a simple heat map with custom limits in base r plotting. The function is a wrapper for the image function
#'
#' @param mat matrix with values to create heatmap for
#' @param xrange the x range in which to plot the grid (vector length 2)
#' @param yrange the y range in which to plot the grid (vector length 2)
#' @param xlimit the x limits of the plot (vector length 2)
#' @param ylimit the y limits of the plot (vector length 2)
#' @param colramp the colors to use for plotting (vector)
#' @param maketicks draw axis ticks and tick marks (logical)
#' @param makebox draw box line around plot (logical)
#' @param axislwd linewidth to use for ticks and box (numeric)
#' @param locut low threshold on raster values (numeric)
#' @param hicut high threshold on raster values (numeric)
#' @param nacut values outside cutrange are set to NA instead of cut value (logical)
#' @param ... other parameters passed to image/plot/axis functions
#'
#' @return plot
#' @export
heatGrid <- function(mat=NULL, xrange=NULL, yrange=NULL, xlimit=NULL, ylimit=NULL,
                     colramp=topo.colors(200), maketicks=T, makebox=T, axislwd=0.5,
                     locut=NULL, hicut=NULL, nacut=F,...){
  
  # setup default data
  testn    <- 25
  trigo    <- cos(seq(0,2*pi,length.out=testn))
  testdata <- do.call(cbind,lapply(1:length(trigo), function(i) abs(rev(sort(rnorm(testn,trigo[i],0.5))))))
  # testdata <- ifelse(testdata>0.2,NA,testdata)
  if (is.null(mat)){mat <- testdata}
  
  
  # seq axes sequencing
  if (is.null(xrange)){xrange <- c(1,ncol(mat))}
  if (is.null(yrange)){yrange <- c(1,nrow(mat))}
  xseq <- seq(xrange[1],xrange[2],length.out=ncol(mat))
  yseq <- seq(yrange[1],yrange[2],length.out=nrow(mat))
  
  # transpose matrix for image function
  pdat <- t(mat[nrow(mat):1,])
  
  # cut values outside range
  if (!is.null(locut)){
    if (nacut){
      pdat[pdat<=locut] <- NA
    }else{
      pdat[pdat<=locut] <- locut
    }
  }
  if (!is.null(hicut)){
    if (nacut){
        pdat[pdat>=hicut] <- NA
    }else{
      pdat[pdat>=hicut] <- hicut
    }
  }
  
  # set zlimit
  zlimit <- range(mat)
  if (!is.null(locut)){zlimit[1] <- locut}
  if (!is.null(hicut)){zlimit[2] <- hicut}
  
  # get exact border coordinates
  if (is.null(xlimit)){xlimit <- c(xseq[1]-0.5*diff(xseq[1:2]), tail(xseq,1)+0.5*diff(xseq[1:2]))}
  if (is.null(ylimit)){ylimit <- c(yseq[1]-0.5*diff(yseq[1:2]), tail(yseq,1)+0.5*diff(yseq[1:2]))}
  image(xseq,yseq, pdat, col=colramp, axes=F, xaxs='i', yaxs='i',
        xlim=xlimit, ylim=ylimit, zlim=zlimit,
        ...)
  
  if (maketicks){
    axis(1, at=pretty(xseq), lwd=axislwd, ...)
    axis(2, at=pretty(yseq), lwd=axislwd, ...)
  }
  if (makebox){
    box(lwd=axislwd)
  }

}




#' @rdname heatGrid
#' @export
pkHeatmap <- heatGrid