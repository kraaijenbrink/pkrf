# Plot shaded lines for data series (after plot has been initiated)


shadeplot <- function(x,y,clr,...){
  for (i in 1:(ncol(x)-1)){
    polygon(c(x[,i],rev(x[,i+1])),
            c(y[,i],rev(y[,i+1])),
              col=clr[i],
            ...)
  }
}
  
  
  
  