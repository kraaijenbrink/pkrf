# mountain hydrology markdown report template
# philip kraaijenbrink


#' Markdown report template
#'
#' Report for R markdown with custom css and header and footers in mountainhydrology style.
#' @export

mh_report = function(...) {
  
  mhcss  = system.file("rmarkdown/styles/mh-report_styles.css", package="pkrf")
  footer = system.file("rmarkdown/styles/mnthydro-footer.html", package="pkrf")
  rmarkdown::html_document(...,css=mhcss, includes = rmarkdown::includes(after_body=footer))
  
}