# mountain hydrology markdown report template
# philip kraaijenbrink


#' Markdown slides template
#'
#' IOslides for R markdown with custom css in mountainhydrology style.
#' @export

mh_slides = function(...) {
  
  mhcss  = system.file("rmarkdown/styles/mh-slides_styles.css", package="pkrf")
  mhlogo = system.file("rmarkdown/styles/mh-logo-regular-250x79.png", package="pkrf")
  rmarkdown::ioslides_presentation(..., css=mhcss, logo=mhlogo)

}