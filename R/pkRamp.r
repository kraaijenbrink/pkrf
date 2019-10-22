# custom color ramp generator
# philip kraaijenbrink


#' Load preset color ramps
#'
#' Load color ramps from preset
#' @param name Name of the color ramp (string).
#' @param number Number of output colors desired (integer).
#' @param reversed Should the ramp be reversed (logical).
#' @param show Show a plot of all available color ramps (logical).
#' 
#' @return Vector with hex colors strings. 
#' @export

ramp <- function(name='parula', number=100, reversed=F, show=F){
  
  namelist <- c('Jet','Parula','Viridis','Inferno','Magma','Plasma','Cividis','ElevAG','WtSpec','RwB','PwG','OwP','Spectral',
                'ElevNat1','ElevNat2','RwBsoft','RwBpale','Taupe', 'Blues','Greens','BW','MHblues','MHramp')
  if (sum(tolower(namelist) %in% tolower(name))==0){
    message <- stop(paste0('Color ramp not available. Choose any of:\n',paste(sort(namelist),collapse='\n')))
  }
  
  getPal <- function(name){
    switch(which(tolower(namelist) %in% tolower(name)),
           c("0000FF","0080FF","00FFFF","80FF80","FFFF00","FF8000","FF0000","800000"),
           c("352A87","0567DF","108ED0","1BAEB4","6BBC85","C4BB5E","F7CB33","F9FB0E"),
           c("440154","482878","3E4A89","31688E","26828E","1F9E89","35B779","6DCD59","B4DE2C","FDE725"),
           c("000004","1B0C42","4B0C6B","781C6D","A52C60","CF4446","ED6925","FB9A06","F7D03C","FCFFA4"),
           c("000004","180F3E","451077","721F81","9F2F7F","CD4071","F1605D","FD9567","FEC98D","FCFDBF"),
           c("0D0887","47039F","7301A8","9C179E","BD3786","D8576B","ED7953","FA9E3B","FDC926","F0F921"),
           c("00204D","00336F","39486B","575C6D","707173","8A8779","A69D75","C4B56C","E4CF5B","FFEA46") ,
           c("AFF0E9","FFFFB3","008040","FCBA03","800000","69300D","ABABAB","FFFCFF"),
           c("FFFFFF","B7E8FF","ACD88C","DDE241","E83535","380000"),
           c("67001F","B2182B","D6604D","F4A582","FDDBC7","F7F7F7","D1E5F0","92C5DE","4393C3","2166AC","053061"),
           c("40004B","762A83","9970AB","C2A5CF","E7D4E8","F7F7F7","D9F0D3","A6DBA0","5AAE61","1B7837","00441B"),
           c("7F3B08","B35806","E08214","FDB863","FEE0B6","F7F7F7","D8DAEB","B2ABD2","8073AC","542788","2D004B"),
           c("9E0142","D53E4F","F46D43","FDAE61","FEE08B","FFFFBF","E6F598","ABDDA4","66C2A5","3288BD","5E4FA2"),
           c('566314','9ba864','c5cc90','a59a7f','5b4242'),
           c('75a05b','e5d9a7','fcc575','baa395','e2e2e2'),
           c('93021b','e28f76','F4F4F4','8c95ad','2b2d42'),
           c("934855","e2b7aa","F4F4F4","9ca0ad","353642"),
           c('22223b','4a4e69','9a8c98','c9ada7','f2e9e4'),
           c('f7fbff','deebf7','c6dbef','9ecae1','6baed6','4292c6','2171b5','08519c','08306b'),
           c('f7fcf5','e5f5e0','c7e9c0','a1d99b','74c476','41ab5d','238b45','006d2c','00441b'),
           c('000000','ffffff'),
           c('ffffff','232365'),
           c('FFCD00','c1ae5e','6b6b8c','232365')
    )
  }

  
  if (show){ # make plot of available ramps
    x11(bg='#2d2d2d')
    op  <- par(mar=c(0.5,6,0.5,1))
    all <- do.call(rbind,lapply(namelist, function(x) colorRampPalette(paste0('#',getPal(x)))(number)))
    plot(NA,axes=F,xlab='',ylab='',ylim=c(0,length(namelist)+1), xlim=c(1,number), xaxs='i', yaxs='i')
    for (i in 1:length(namelist)){
      for (j in 1:number){
        irev <-  (length(namelist):1)[i]
        polygon(x=c(j-1,j,j,j-1), y=c(irev-0.4,irev-0.4,irev+0.4,irev+0.4), border=NA, col=all[i,j])
      }
    }
    mtext(namelist, side=2, line=0.5, at=length(namelist):1,padj=0.5,adj=1,las=2, cex=1.1, col='white')
    par(op)
    warning('No color ramp output when "show=TRUE", just a plot with available color ramps')
    
  }else{ # generate output
    colpal   <- getPal(name)
    if (reversed){colpal <- rev(colpal)}
    outcolors <- colorRampPalette(paste0('#',colpal))(number)
    return(outcolors)
  }
  

}



#' @rdname ramp
#' @export
pkRamp <- ramp

