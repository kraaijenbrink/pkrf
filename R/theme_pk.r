#' Custom ggplot2 theme PK style
#'
#' @import ggplot2
#' @param base_size base point size
#' @param base_family base font family
#' @param base_line_size = base line size
#' @param base_rect_size = base rect size
#' @param grid draw no (\cpde{0}), only major ((\cpde{1})) or major and minor grid lines (\cpde{2})
#' @param yrot rotate y axis tick labelsl 90 degrees (boolean)
#' @param strip Use gray facet strip with white text ("dark") or transparent strip with black text ("light")
#' @return \code{ggplot2} theme opbject
#' @export
theme_pk <- function(base_size = 10,
                     base_family = "",
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22,
                     alwd = 0.5,
                     grid = F,
                     yrot = F,
                     strip = 'light'){
  outtheme <- theme_bw(base_size = base_size, base_family = base_family,
                       base_line_size = base_line_size) %+replace%
    theme(

      # main labels
      plot.title           = element_text(color='black', face = "bold", hjust = 0, margin=margin(0,0,4,0), size=rel(1.1)),
      plot.subtitle        = element_text(face = "plain", hjust=0, size=rel(1.0), margin=margin(0,0,6,0)),
      plot.caption         = element_text(face = "plain", hjust=0, size=rel(0.7), margin=margin(t=2)),
    
      # plot panel
      panel.border         = element_rect(fill=NA, color="black", size = rel(alwd*2)),  # use double lwd to fix cutoff caused by clip='on'
      panel.grid.major     = element_blank(),   
      panel.grid.minor     = element_blank(), 

      # axis stuff
      axis.title           = element_text(color='black', size = rel(1.0)),
      axis.title.x         = element_text(margin=margin(9,0,0,0)),
      axis.title.y         = element_text(angle=90, margin=margin(0,9,0,0)),
      axis.text            = element_text(color='black', size = rel(0.8)),
      axis.text.x          = element_text(margin=margin(4,0,0,0)),
      axis.text.y          = element_text(margin=margin(0,4,0,0), angle=0),
      axis.ticks           = element_line(lineend='round', size = rel(alwd)),
      axis.ticks.length    = unit(.15, "cm"),

      # legend
      legend.justification = 'top',
      legend.margin        = margin(t=0,b=4),
      legend.title         = element_text(colour="black", size=rel(0.8), face='bold', hjust=0),
      legend.text          = element_text(colour="black", size=rel(0.8) , hjust=0),
      legend.key.size      = unit(base_size*1.2, "pt"),
      
      # facet strips
      strip.background=element_rect(fill='gray40',colour='black'),
      strip.text=element_text(colour='white', face='bold', hjust=0, vjust=1, margin=margin(4,4,4,4), size=rel(0.8))
    )

   # conditional changes
  if (grid==1){
    outtheme <- outtheme %+replace% theme(
      panel.grid.major     = element_line(linetype = "solid", size=rel(alwd), colour="gray90")
    )
  }
  if (grid==2){
    outtheme <- outtheme %+replace% theme(
      panel.grid.major     = element_line(linetype = "solid", size=rel(alwd), colour="gray90"),
      panel.grid.minor     = element_line(linetype = "solid", size=rel(alwd*0.65), colour="gray90")
    )
  }
  if (yrot){
    outtheme <- outtheme %+replace% theme(
      axis.text.y          = element_text(margin=margin(0,4,0,0), angle=90) 
    )
  }
  if (strip=='light'){
    outtheme <- outtheme %+replace% theme(
      strip.background=element_rect(fill='#FFFFFF00', colour=NA),
      strip.text=element_text(colour='black', face='bold', hjust=0, vjust=1, margin=margin(4,4,4,0), size=rel(0.8)) 
    )
  }
  
  return(outtheme)
}