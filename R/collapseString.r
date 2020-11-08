# Collapse string
# 'Collapse string'

#' Collapse string
#'
#' Collapse vector of strings into single string with delimiters (and quotes)
#' 
#' @param x (vector of strings
#' @param delimiter character(s) used as delimiter
#' @param quotes add quotes to each string
#' @return collapsed string
#' @export
collapseString = function(x, delimiter=',',quotes=F) {
  if (quotes){
    paste0('"',paste(x, collapse=paste0('"',delimiter,'"')),'"')
  }else{
    paste(x, collapse=delimiter)
  }
}

