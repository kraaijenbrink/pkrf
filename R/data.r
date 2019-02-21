#' Global warming data.
#'
#' A dataset containing global warming data for 110 different climate models and/or scenarios from
#' \href{https://esgf-node.llnl.gov/projects/cmip5/}{CMIP5}.
#' Additionally the results of a large glacier model for the high mountains of Asia (HMA) are included
#' (\href{https://www.nature.com/articles/nature23878}{Kraaijenbrink et al., 2017}).
#' \cr\cr
#' Temperature delta are provided between pre-industrial (PI, 1851-1880) and end of century (EOC, 2071-2100).
#' \cr\cr
#' Glacier volume change is provided in percent between 2005 and EOC.\cr
#' Total glacier mass in the region is 4754 Gt.
#'
#' @format A data frame with 110 rows and 7 variables:
#' \describe{
#'   \item{\code{gcm}}{General Circulation Model.}
#'   \item{\code{rcp}}{Representative Concentration Pathway.}
#'   \item{\code{dt_glob}}{Global temperature rise between PI and EOC.}
#'   \item{\code{dt_land}}{Temperature rise between PI and EOC for world's land masses.}
#'   \item{\code{dt_hma}}{Temperature rise for only the glaciers in HMA}
#'   \item{\code{dv_hma}}{EOC loss of glacier volume, given in percentage of their 2005 volume.}
#'   \item{\code{deg15scen}}{Models of the +1.5 °C scenario, i.e. 1.5 ± 0.1 °C at the EOC.}
#' }
#' @source \href{https://www.nature.com/articles/nature23878}{Kraaijenbrink et al., 2017}
"warming"

#' Glacier data.
#'
#' A dataset with glacier summaries for the high mountains of Asia on a 1x1 degree grid.
#' Data is limited to glaciers larger than 0.4 square km.
#'
#' @format A data frame with 303 rows and 14 variables:
#' \describe{
#'   \item{\code{longitude}}{Cell centroid longitude}
#'   \item{\code{latitude}}{Cell centroid latitude}
#'   \item{\code{region}}{RGI region at cell centroid.}
#'   \item{\code{count}}{Number of glaciers in cell}
#'   \item{\code{precip}}{Mean annual precipitation from ERA-Interim for 1996-2015 (mm)}
#'   \item{\code{ela}}{Mean equilibrium line altitude (ELA)(m)}
#'   \item{\code{area_total}}{Total glacier area (km2)}
#'   \item{\code{area_ela}}{Glacier area below the ELA (km2)}
#'   \item{\code{area_debris}}{Debris covered glacier area (km2)}
#'   \item{\code{area_debris_ela}}{Debris covered glacier area below the ELA (km2)}
#'   \item{\code{mass_total}}{Total glacier mass (Gt)}
#'   \item{\code{mass_ela}}{Glacier mass below the ELA (Gt)}
#'   \item{\code{mass_debris}}{Debris covered glacier mass (Gt)}
#'   \item{\code{mass_debris_ela}}{Debris covered glacier mass below the ELA (Gt)}
#' }
#' @source \href{https://www.nature.com/articles/nature23878}{Kraaijenbrink et al., 2017}
"glaciers"

