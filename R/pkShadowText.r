# Plot text labels with a different color background (taken from TeachingDemos package)
# philip kraaijenbrink


#' Plot shadowed text labels
#'
#' Plot text labels with a different color shadow or faked outline. Function taken from \code{TeachingDemos} package.
#' @param x x position
#' @param y y position
#' @param labels text labels
#' @param col foreground color
#' @param bg shadow/outline color
#' @param theta angle at which to plot the shadow
#' @param r distance of the shadow
#' @param ... any other arguments for text function
#' 
#' @return Vector with hex colors strings. 
#' @export

pkShadowText <- function(x, y=NULL, labels, col='white', bg='black',
                       theta=seq(pi/4, 2*pi, length.out=8), r=0.1, ... ) {
  
  xy <- xy.coords(x,y)
  xo <- r*strwidth('A')
  yo <- r*strheight('A')
  
  for (i in theta) {
    text(xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
  }
  text(xy$x, xy$y, labels, col=col, ... ) }
