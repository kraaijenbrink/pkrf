#' Cheat sheets for various topics
#'
#' Display a pdf cheat sheet for various R topics and packages
#'
#' @param topic Topic to display cheat sheet for. Defaults to 'topics', which will provide a list of available topics.
#' @export
#'
#' @examples
#' sheet('topics')
#' sheet('ggplot2')
#' sheet('colors')
#' 
sheet <- function(topic='topics'){
  
  topic <- tolower(topic)
  
  topics <- list(
    colors=NA,
    `base-r`='https://github.com/rstudio/cheatsheets/raw/main/base-r.pdf',
    `advanced-r`='https://www.rstudio.com/wp-content/uploads/2016/02/advancedR.pdf',
    forcats='https://github.com/rstudio/cheatsheets/raw/main/factors.pdf',
    keras='https://github.com/rstudio/cheatsheets/raw/main/keras.pdf',
    lubridate='https://github.com/rstudio/cheatsheets/raw/main/lubridate.pdf',
    stringr='https://github.com/rstudio/cheatsheets/raw/main/strings.pdf',
    purrr='https://github.com/rstudio/cheatsheets/raw/main/purrr.pdf',
    dplyr='https://github.com/rstudio/cheatsheets/raw/main/data-transformation.pdf',
    rmarkdown='https://github.com/rstudio/cheatsheets/raw/main/rmarkdown-2.0.pdf',
    rstudio='https://github.com/rstudio/cheatsheets/raw/main/rstudio-ide.pdf',
    shiny='https://github.com/rstudio/cheatsheets/raw/main/shiny.pdf',
    ggplot2='https://github.com/rstudio/cheatsheets/raw/main/data-visualization.pdf',
    devtools='https://github.com/rstudio/cheatsheets/raw/main/package-development.pdf',
    caret='https://github.com/rstudio/cheatsheets/raw/main/caret.pdf',
    leaflet='https://github.com/rstudio/cheatsheets/raw/main/leaflet.pdf',
    regex='https://github.com/rstudio/cheatsheets/raw/main/regex.pdf',
    sf='https://github.com/rstudio/cheatsheets/raw/main/sf.pdf',
    reticulate='https://github.com/rstudio/cheatsheets/raw/main/reticulate.pdf',
    rlang='https://github.com/rstudio/cheatsheets/raw/main/tidyeval.pdf',
    readr='https://github.com/rstudio/cheatsheets/raw/main/data-import.pdf',
    cdo='https://code.mpimet.mpg.de/projects/cdo/embedded/cdo_refcard.pdf',
    latex='https://wch.github.io/latexsheet/latexsheet-a4.pdf'
  )
  
  srcurl <- switch(topic,
                colors=NA,
                `base-r`='https://github.com/rstudio/cheatsheets/raw/main/base-r.pdf',
                `advanced-r`='https://www.rstudio.com/wp-content/uploads/2016/02/advancedR.pdf',
                forcats='https://github.com/rstudio/cheatsheets/raw/main/factors.pdf',
                keras='https://github.com/rstudio/cheatsheets/raw/main/keras.pdf',
                lubridate='https://github.com/rstudio/cheatsheets/raw/main/lubridate.pdf',
                stringr='https://github.com/rstudio/cheatsheets/raw/main/strings.pdf',
                purrr='https://github.com/rstudio/cheatsheets/raw/main/purrr.pdf',
                dplyr='https://github.com/rstudio/cheatsheets/raw/main/data-transformation.pdf',
                rmarkdown='https://github.com/rstudio/cheatsheets/raw/main/rmarkdown-2.0.pdf',
                rstudio='https://github.com/rstudio/cheatsheets/raw/main/rstudio-ide.pdf',
                shiny='https://github.com/rstudio/cheatsheets/raw/main/shiny.pdf',
                ggplot2='https://github.com/rstudio/cheatsheets/raw/main/data-visualization-2.1.pdf',
                devtools='https://github.com/rstudio/cheatsheets/raw/main/package-development.pdf',
                caret='https://github.com/rstudio/cheatsheets/raw/main/caret.pdf',
                leaflet='https://github.com/rstudio/cheatsheets/raw/main/leaflet.pdf',
                regex='https://github.com/rstudio/cheatsheets/raw/main/regex.pdf',
                sf='https://github.com/rstudio/cheatsheets/raw/main/sf.pdf',
                reticulate='https://github.com/rstudio/cheatsheets/raw/main/reticulate.pdf',
                rlang='https://github.com/rstudio/cheatsheets/raw/main/tidyeval.pdf',
                readr='https://github.com/rstudio/cheatsheets/raw/main/data-import.pdf',
                cdo='https://code.mpimet.mpg.de/projects/cdo/embedded/cdo_refcard.pdf',
                latex='https://wch.github.io/latexsheet/latexsheet-a4.pdf'
  )
  
  available <- sort(names(topics))
  if (topic == 'topics'){
    cat(sprintf('\nSelect a topic to display. Available topics are:\n\n- "%s"\n\n',paste(available, collapse='"\n- "')))
  }else if (topic == 'colors'){
    pkrf::colorList()
  }else if (topic %in% names(topics)){
    dstfile <- tempfile(pattern='cheatsheet_',fileext='.pdf')
    download.file(srcurl, dstfile, quiet=T, mode='wb')
    browseURL(dstfile)
  }else{
    stop(sprintf('Sorry, this topic is not available. Available topics are:\n\n- "%s"\n\n',paste(available, collapse='"\n- "')))
  }
  
}