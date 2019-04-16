

#' Custom ggplot2 theme
#'
#' @import ggplot2
#' @return
#' @export
#'
#' @examples
pkTheme <- function(base_size = 11,
                    base_family = "",
                    base_line_size = base_size / 22,
                    base_rect_size = base_size / 22,
                    alwd = 0.5){
  theme_bw(base_size = base_size, base_family = base_family, base_line_size = base_line_size) %+replace%
    theme(
      
      
      
      # main labels
      plot.title           = element_text(color='black', face = "bold", hjust = 0, margin=margin(0,0,6,0), size=rel(1.1)),
      plot.subtitle        = element_text(face = "plain", hjust=0, size=rel(1.1), margin=margin(0,0,6,0)),
      plot.caption         = element_text(face = "italic", hjust=1, size=rel(0.8)),
    
      # plot panel
      panel.border         = element_rect(fill=NA, color="black", size = rel(alwd)),
      panel.grid.major     = element_line(linetype = "solid", size=rel(alwd)),   
      panel.grid.minor     = element_line(linetype = "solid", size=rel(alwd)),   
    
      
      # axis stuff
      axis.title           = element_text(color='black', size = rel(1.1)),
      axis.title.x         = element_text(margin=margin(12,0,0,0)),
      axis.title.y         = element_text(angle = 90, margin=margin(0,12,0,0)),
      axis.text            = element_text(color='black', size = rel(0.85)),
      axis.text.x          = element_text(margin=margin(4,0,0,0)),
      axis.text.y          = element_text(margin=margin(0,4,0,0), angle=90),
      axis.ticks           = element_line(lineend='round', size = rel(alwd)),
      axis.ticks.length    = unit(.15, "cm"),

      # legend
      legend.justification = 'top',
      legend.margin        = margin(t=0,b=6),
      legend.title         = element_text(colour="black", size=rel(0.85), face='bold'),
      legend.text          = element_text(colour="black", size=rel(0.85)),
      
    )
}
  