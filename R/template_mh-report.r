# mountain hydrology markdown report template
# philip kraaijenbrink


#' Markdown template
#'
#' Report for R markdoiwn with custom css and header and footers in mountainhydrology style.
#' @export

mh_report = function(...) {
  
  # locations of resource files in the package
  pkg_resource = function(...) {
    system.file(..., package = "pkrf")
  }
  
  css    = pkg_resource("rmarkdown/styles/mh-report_styles.css")
  header = pkg_resource("rmarkdown/styles/mnthydro-header.html")
  footer = pkg_resource("rmarkdown/styles/mnthydro-footer.html")
  
  # call the base html_document function
  rmarkdown::html_document(...,
                           css=css,
                           includes = rmarkdown::includes(before_body=header, after_body=footer)
                           )
}