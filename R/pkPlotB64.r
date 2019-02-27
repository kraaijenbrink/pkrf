# function to plot image as png and load is as b64 encoded


#' Plot byte64
#' 
#' Plot image to temporary png and load as Byte64 data
#'
#' @param pfun a function that produces a plot
#' @param ... other parameters passed to <code>pfun</code>
#' @param prefix prefix to apply to the byte64 string, useful to generate full HTML img src
#' @param suffix suffix to apply to the byte64 string, useful to generate full HTML img src
#' @param width PNG width (px)
#' @param height PNG width (px)
#' @param res PNG nominal resolution (ppi)
#' 
#' @return string with byte64 data
#' @export
pkB64 <- function(pfun, ..., width=200, height=200, res=100, prefix='<img src="data:image/png;base64, ', suffix='">'){
  tmpfn <- tempfile('subplot_',fileext='.png')
  png(tmpfn, width=width, height=height, res=res)
  pfun(...)
  dev.off()
  b64 <- paste0(prefix, base64enc::base64encode(tmpfn), suffix)
  return(b64)
}