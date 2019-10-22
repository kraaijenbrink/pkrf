# Function to calculate a shade or a tint of a color
# 'Shade or tint'

#' Shade or tint
#'
#' Shade or tint
#' 
#' @param x (vector of) color(s) with no alpha
#' @param opt "tint" (lighten) or "shade" (darken)
#' @return altered (vector of) color(s)
#' @export
tint = function(colors=c('red','green','blue'), opt='shade', factor=0.5) {
  if(!(opt %in% c('shade','tint'))){stop("Option must be either 'shade' or 'tint'")}
  cols    <- col2rgb(col=colors, alpha=FALSE)
  if (opt=='shade'){
    outcol = cols*(1-factor) / 255
  }else if(opt=='tint'){
    outcol = (cols+(255-cols)*factor) / 255
  }
  return(rgb(red=outcol[1,],green=outcol[2,],blue=outcol[3,]))
}



#' @rdname tint
#' @export
pkTintShade <- tint