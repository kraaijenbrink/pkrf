#' Convert Windows to Unix paths
#'
#' Get unix paths equivalents of Windows paths, for example for linux subsystem or cygwin. Function likely only applicable under Windows.
#' 
#' @param path Path to be transformed.
#' @param reverse Convert from windows to unix (\code{FALSE}) or from unix to windows (\code{TRUE}). When converting from unix, a full path must be provided. Converting to unix can be performed with relative paths.
#' @param mountpoint Mountpoint of the windows drive, usually \code{'/mnt'} for linux subsystem and \code{'/cygdrive'} for cygwin.
#' 
#' @return transformed path
#' @export
unixPath <- function(path, reverse=FALSE, mountpoint='/mnt'){
  if (reverse){
    winpath   <- sub('/',':/',sub(sprintf("^%s/",mountpoint),'',path))
    paste0(toupper(substr(winpath, 1, 1)), substr(winpath, 2, nchar(winpath)))
  }else{
    splitpath <- strsplit(normalizePath(path,winslash='/', mustWork=F),":")
    sapply(splitpath, function(p) sprintf('%s/%s%s',mountpoint,tolower(p[1]), p[2]))
  }
}
