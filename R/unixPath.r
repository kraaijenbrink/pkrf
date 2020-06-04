# Base plot of raster, but with cut limits
# 'Plot raster with limits

#' Plot raster with limits
#'
#' Get unix paths equivalents of Windows paths, for example for linux subsystem or cygwin.
#' 
#' @param path path to be transformed
#' @param reverse convert from windows to unix (\code{FALSE}) or from unix to windows (\code{TRUE})
#' @param mountpoint mountpoint of the windows drive, usually \code{'/mnt'} for linux subsystem and \code{'/cygdrive'} for cygwin
#' 
#' @return transformed path
#' @export
unixPath <- function(path, reverse=FALSE, mountpoint='/mnt'){
  if (reverse){
    winpath   <- sub('/',':/',sub(sprintf("^%s/",mountpoint),'',path))
    paste0(toupper(substr(winpath, 1, 1)), substr(winpath, 2, nchar(winpath)))
  }else{
    splitpath <- str_split(normalizePath(path,winslash='/', mustWork=F),":")
    sapply(splitpath, function(p) sprintf('%s/%s%s',mountpoint,tolower(p[1]), p[2]))
  }
}
