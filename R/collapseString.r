# Collapse string
# 'Collapse string'

#' Collapse string
#'
#' Collapse vector of strings into single string with delimiters (and quotes)
#' 
#' @param x (vector of strings
#' @param delimiter character(s) used as delimiter
#' @param add_quotes add quotes to each string
#' @return collapsed string
#' @export
collapseString = function(x, delimiter=',',add_quotes=F) {
  if (add_quotes){
    paste0('"',paste(x, collapse=paste0('"',delimiter,'"')),'"')
  }else{
    paste(x, collapse=delimiter)
  }
}

