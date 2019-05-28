#' R colors and their names
#'
#' Show a PDF with all R colors and their names for reference.
#' @export
#'
#' @examples
#' colorList()
colorList <- function(){
  require(colorRamps, quietly=T)
  
  dstpdf <- file.path(tempdir(),'colorlist.pdf')
  pdf(dstpdf,width=14.57, height=8.2, onefile=TRUE)
  
  kol <- colors()
  kol <- kol[!grepl('grey',kol)]

  rows = 50
  cols = 11
  m      <- matrix(1:(rows*cols), nc=cols, nr=rows, byrow=F)

  kol <- c(rep(NA,rows*cols - length(kol)), kol)
  
  transm   <- t(m)[,rows:1]
  transkol <- kol[order(t(transm))]
  txtcolor <- ifelse(colMeans(col2rgb(transkol))<100, "white", "black")
  
  op     <- par(mar=c(0,0,0,0))
  image(1:cols, 1:rows, transm, col=kol, axes=FALSE, ann=FALSE)
  text(as.numeric(col(m)), as.numeric(row(m)), transkol, cex=.7, col=txtcolor, adj=c(0.5,0.5))
  par(op)
  
  dev.off()
  browseURL(dstpdf)
}
