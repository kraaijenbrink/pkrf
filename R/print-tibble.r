#' Print all rows
#'
#' Print all rows of a tibble
#'
#' @param tbl Input tibble
#' @export
pinf <- function(tbl){
  print(tbl,n=Inf)
}

#' Print more rows
#'
#' Print 200 rows of a tibble
#'
#' @param tbl Input tibble
#' @export
pmore <- function(tbl){
  print(tbl,n=200)
}