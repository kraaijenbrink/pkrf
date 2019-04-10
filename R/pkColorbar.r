# Plot a color bar that keept layout function intact
# 'Plot color bar'

#' Plot color bar
#'
#' Plot color bar
#' 
#' @param breaks vector of break values
#' @param colors vector of color
#' @param barlength relative length of the bar
#' @param barlwd line width for color bar
#' @param axislab axis label for the colorbar
#' @param axislabline spacing for the axis label
#' @param locut should the lower limit represent a slanted cutoff? (boolean)
#' @param hicut should the higher limit represent a slanted cutoff? (boolean)
#' @param slantwidth width of the slanted cutoff as a fraction of the total bar width (numeric)
#' @param smallticks plot small unlabeled ticks for each break (boolean)
#' @param axisbreaks labelled break values to use in the colorbar axis
#' @param axisbreaklab labels for the axisbreaks 
#' @param tck1 length of small ticks
#' @param tck1 length of large ticks
#' @param axiscex cex for tick label
#' @param labelcex cex for axis label
#' @param pmgp temporary par 'mgp' to use
#' @param pmar temporary par 'mar' to use
#' @param ... other arguments passed to axis function
#' @return plotted colorbar
#' @export
pkColorbar <- function(breaks       = seq(0,1,0.1),
                       colors       = gray.colors(length(breaks)-1),
                       barlength    = 0.5,
                       barlwd       = 0.5,
                       axislab      = 'Colorscale',
                       axislabline  = 2.25,
                       locut        = F,
                       hicut        = F,
                       slantwidth   = 0.1, 
                       smallticks   = F,
                       axisbreaks   = breaks,
                       axisbreaklab = axisbreaks,
                       tck1         = -0.015,
                       tck2         = -0.03,
                       axiscex      = 0.8,
                       labelcex     = 1.0,
                       pmgp         = c(2.5,0.66,0),
                       pmar         = c(3,0.5,0.5,0.5),
                       ...
                       ){

# set and store par
op <- par(mgp=pmgp, mar=pmar, xpd=NA)

bspan <- diff(range(breaks))
if (locut){
  breaks <- c(breaks[1]-bspan*slantwidth, breaks)
}
if (hicut){
  breaks <- c(breaks, tail(breaks,1)+bspan*slantwidth)
}


# get polygon coordinates
px.outer <- c(breaks,rev(breaks))
py.outer <- c(rep(0,length(breaks)), rep(1,length(breaks)))
if(locut){py.outer[c(1,length(py.outer))] <- 0.5}
if(hicut){py.outer[c(length(breaks),length(breaks)+1)] <- 0.5}

# get polygons for each break
polysplit <- c(rep(1:(length(breaks)-1),each=2), rev(rep(1:(length(breaks)-1),each=2)))
pxy <- lapply(1:(length(breaks)-1), function(i){
  ir <- length(px.outer)-i+1
  list(x=px.outer[c(i,i+1,ir-1,ir)],
       y=py.outer[c(i,i+1,ir-1,ir)])
})

# set xlimits of plot to scale the barlength
barlength <- barlength/diff(range(breaks))
xlimits <- c(mean(range(breaks)) - 0.5*(1/barlength),
             mean(range(breaks)) + 0.5*(1/barlength))

# set ylimits
ylimits <- c(0.2,0.8)

# make plot
plot(NA, axes=F,xlab='',ylab='',xlim=xlimits,ylim=c(0,1),xaxs='i',yaxs='i')
out <- sapply(1:length(pxy), function(i){
  polygon(pxy[[i]]$x,pxy[[i]]$y, col=colors[i], border=NA)
})
polygon(px.outer,py.outer, lwd=barlwd)

# create axis
axisticks <- breaks
if(locut){axisticks <- axisticks[-1]}
if(hicut){axisticks <- axisticks[-length(axisticks)]}
if(smallticks){axis(1,at=axisticks, label=NA, tck=tck1, lwd=barlwd)}
axis(1,at=axisbreaks, labels=axisbreaklab, tck=tck2, cex.axis=axiscex, lwd=barlwd, ...)
mtext(axislab,1, line=axislabline,cex=labelcex)

# restore par
par(op)

return(invisible())
}
