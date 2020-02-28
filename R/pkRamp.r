# custom color ramp generator
# philip kraaijenbrink


#' Load preset color ramps and palettes
#'
#' Load color ramps and palettes from presets. Identical to \code{pkrf::pal() besides some argument defaults.}
#' @param name Name of the color ramp (string).
#' @param number Number of output colors desired (integer). Defaults to number of colors in the pallete for qualitative type and to 100 for sequential and diverging types.
#' @param reversed Reverse the output color order (logical).
#' @param random Randomize output color order (logical).
#' @param center Relative position of the center of the color ramp (numeric, 0-1). Useful to change centerpoint of diverging scales.
#' @param show Show a plot of all available color ramps (logical).
#' @return Vector with hex colors strings. 
#' @export
ramp <- function(name='parula', number=NULL, reversed=F, random=F, center=0.5, show=F){
  
  coldat <- tibble::tribble(
    ~name, ~type, ~source, ~colors,
    "Jet", 'Sequential', 'MATLAB', c("0000FF","0080FF","00FFFF","80FF80","FFFF00","FF8000","FF0000","800000"),
    "Parula", 'Sequential', 'MATLAB', c("352A87","0567DF","108ED0","1BAEB4","6BBC85","C4BB5E","F7CB33","F9FB0E"),
    "Viridis", 'Sequential', 'Matplotlib', c("440154","482878","3E4A89","31688E","26828E","1F9E89","35B779","6DCD59","B4DE2C","FDE725"),
    "Inferno", 'Sequential', 'Matplotlib', c("000004","1B0C42","4B0C6B","781C6D","A52C60","CF4446","ED6925","FB9A06","F7D03C","FCFFA4"),
    "Magma", 'Sequential', 'Matplotlib', c("000004","180F3E","451077","721F81","9F2F7F","CD4071","F1605D","FD9567","FEC98D","FCFDBF"),
    "Plasma", 'Sequential', 'Matplotlib', c("0D0887","47039F","7301A8","9C179E","BD3786","D8576B","ED7953","FA9E3B","FDC926","F0F921"),
    "Cividis", 'Sequential', 'Matplotlib', c("00204D","00336F","39486B","575C6D","707173","8A8779","A69D75","C4B56C","E4CF5B","FFEA46") ,
    "ElevAG", 'Sequential', 'ArcGIS', c("AFF0E9","FFFFB3","008040","FCBA03","800000","69300D","ABABAB","FFFCFF"),
    "WtSpec", 'Sequential', 'Custom', c("FFFFFF","B7E8FF","ACD88C","DDE241","E83535","380000"),
    "Spectral", 'Sequential', 'ColorBrewer', c("9E0142","D53E4F","F46D43","FDAE61","FEE08B","FFFFBF","E6F598","ABDDA4","66C2A5","3288BD","5E4FA2"),
    "ElevNat1", 'Sequential', 'Custom', c('566314','9ba864','c5cc90','a59a7f','5b4242'),
    "ElevNat2", 'Sequential', 'Custom', c('75a05b','e5d9a7','fcc575','baa395','e2e2e2'),
    "Taupe", 'Sequential', 'Custom', c('22223b','4a4e69','9a8c98','c9ada7','f2e9e4'),
    "Blues", 'Sequential', 'ColorBrewer', c('f7fbff','deebf7','c6dbef','9ecae1','6baed6','4292c6','2171b5','08519c','08306b'),
    "Greens", 'Sequential', 'ColorBrewer', c('f7fcf5','e5f5e0','c7e9c0','a1d99b','74c476','41ab5d','238b45','006d2c','00441b'),
    "BW", 'Sequential', 'ColorBrewer', c('000000','ffffff'),
    "MHblues",  'Sequential', 'Custom', c('ffffff','232365'),
    "MHramp", 'Sequential', 'Custom', c('FFCD00','c1ae5e','6b6b8c','232365'),
    
    "RwB", 'Diverging', 'ColorBrewer', c("67001F","B2182B","D6604D","F4A582","FDDBC7","F7F7F7","D1E5F0","92C5DE","4393C3","2166AC","053061"),
    "RwBsoft", 'Diverging', 'Custom', c('93021b','e28f76','F4F4F4','8c95ad','2b2d42'),
    "RwBpale", 'Diverging', 'Custom', c("934855","e2b7aa","F4F4F4","9ca0ad","353642"),
    "RyG", 'Diverging', 'ColorBrewer', c("A50026","D73027","F46D43","FDAE61","FEE08B","FFFFBF","D9EF8B","A6D96A","66BD63","1A9850","006837"),
    "PwG", 'Diverging', 'ColorBrewer', c("40004B","762A83","9970AB","C2A5CF","E7D4E8","F7F7F7","D9F0D3","A6DBA0","5AAE61","1B7837","00441B"),
    "OwP", 'Diverging', 'ColorBrewer', c("7F3B08","B35806","E08214","FDB863","FEE0B6","F7F7F7","D8DAEB","B2ABD2","8073AC","542788","2D004B"),
    
    "MntHydro",'Qualitative','Custom', c('232365','FFCD00','AAAAAA','7b942e','deebf6','202030','FFECA2','454545','dfebbd','9b7f0a','1E5E37'),
    "UUmain",'Qualitative','Utrecht University', c('FFCD00','C00A35','000000','094D8E','36A2C9','791D72','9A0000','F08000','FEF200','419702'),
    "UUalt",'Qualitative','Utrecht University', c('FFE6AB','F3965E','AA1555','6E3B23','24A793','5287C6','001240','5B2182'),
    "Set1",'Qualitative','ColorBrewer',  c("E41A1C","377EB8","4DAF4A","984EA3","FF7F00","FFFF33","A65628","F781BF","999999"),
    "Set2",'Qualitative','ColorBrewer', c("66C2A5","FC8D62","8DA0CB","E78AC3","A6D854","FFD92F","E5C494","B3B3B3"),
    "Set3",'Qualitative','ColorBrewer', c("8DD3C7","FFFFB3","BEBADA","FB8072","80B1D3","FDB462","B3DE69","FCCDE5","D9D9D9","BC80BD","CCEBC5"),
    "Pastel1",'Qualitative','ColorBrewer', c("FBB4AE","B3CDE3","CCEBC5","DECBE4","FED9A6","FFFFCC","E5D8BD","FDDAEC","F2F2F2"),
    "Pastel2",'Qualitative','ColorBrewer', c("B3E2CD","FDCDAC","CBD5E8","F4CAE4","E6F5C9","FFF2AE","F1E2CC","CCCCCC"),
    "Accent",'Qualitative','ColorBrewer',  c("7FC97F","BEAED4","FDC086","FFFF99","386CB0","F0027F","BF5B17","666666"),
    "Dark",'Qualitative','ColorBrewer', c("1B9E77","D95F02","7570B3","E7298A","66A61E","E6AB02","A6761D","666666"),
    "Google",'Qualitative','Internet',  c('008744','0057e7','d62d20','ffa700','000000'),
    "Raven",'Qualitative','Internet', c('0e1a40','222f5b','5d5d5d','946b2d','000000'),
    "Muted",'Qualitative','Internet', c('2e4045','83adb5','c7bbc9','5e3c58','bfb5b2'),
    "Cappuccino",'Qualitative','Internet', c('4b3832','854442','E5DBCF','3c2f2f','be9b7b'),
    "Pale1",'Qualitative','Internet', c('c4b2c2','b38d97','d5aca9','ebcfb2','c5baaf'),
    "Water",'Qualitative','Internet', c('b9d6f2','084c61','061a40','0353a4','2d93ad'),
    "Skin",'Qualitative','Internet', c('d8e2dc','ffe5d9','ffcad4','f4acb7','9d8189'),
    "NS",'Qualitative','Internet', c('06aed5','086788','f0c808','f9e39f','dd1c1a'),
    "Legendary",'Qualitative','Internet', c('5bc0eb','fde74c','9bc53d','e55934','fa7921'),
    "WarmWall",'Qualitative','Internet', c('d1cebe','e07a5f','3d405b','81b29a','f2cc8f'),
    "Oasis",'Qualitative','Internet', c('000000','839788','eee0cb','baa898','bfd7ea'),
    "Earthly",'Qualitative','Internet', c('537d99','def2c8','bbc451','9a9b58','8f451f'),
    "A3",'Qualitative','Internet', c('1c3144','d00000','ffba08','a2aebb','3f88c5'),
    "AfterCold",'Qualitative','Internet', c('171219','225560','edf060','f0803c','310d20'),
    "Swoop",'Qualitative','Internet', c('dd7373','3b3561','ead94c','d1d1d1','51a3a3'),
    "BurbleTeal",'Qualitative','Internet', c('2d3047','93b7be','e0ca3c','a799b7','048a81'),
  )
  
  # function to extract correct color from table
  getPal <- function(name){
    coldat$colors[[which(tolower(coldat$name) %in% tolower(name))]]
  }
  # function to extract type for color ramp
  getType <- function(name){
    coldat$type[[which(tolower(coldat$name) %in% tolower(name))]]
  }
  
  # force default number
  if (is.null(number)){
    if(getType(name)=='Qualitative'){
      number <- length(getPal(name))
    }else{
      number <- 100
    }
  }
  
  # force a hash at beginning of the hexcol
  coldat$colors <- lapply(coldat$colors, function(x) tolower(paste0('#',gsub('#','',x))))
  
  # Warning message in case of unknown color name
  if (sum(tolower(coldat$name) %in% tolower(name))==0){
    message <- stop(paste0('Color name not available. Choose any of:\n',paste(sort(coldat$name),collapse=', '),
                           '\n\n','Use ramp(show=TRUE) to plot all color ramps/palettes, types and names.'))
  }
  
  
  # Make plot of available colors
  if (show){ # make plot of available ramps
    x11(bg='#2d2d2d', height=7, width=11.3)
    
    if(getType(name)!='Qualitative'){number <- 100}
    
    # split the classes, sort alphabetically, and merge back
    plotdat        <- split(coldat,coldat$type)[c(1,3,2)]
    plotdat        <- do.call(rbind,lapply(plotdat, function(x) x[order(x$name),]))
    
    # add blank row at type change
    typechange <- which(plotdat$type[-1] != plotdat$type[-nrow(plotdat)])
    plotdat <- rbind(plotdat[1:typechange[1],],
                     c(NA,NA,NA,NA),
                     plotdat[(typechange[1]+1):typechange[2],],
                     c(NA,NA,NA,NA),
                     plotdat[(typechange[2]+1):nrow(plotdat),]
                     )
    rampdat <- plotdat[plotdat$type!='Qualitative',]
    rampdat <- rampdat[-nrow(rampdat),]
    rampdat$colors <- lapply(rampdat$colors, function(x){
      if (length(x)<2){
        return(rep(NA,number))
      }else{
        return(colorRampPalette(x)(number))
      }
    })
    paldat  <- plotdat[plotdat$type=='Qualitative',]
    paldat  <- paldat[-c(1:2),]
    
    # make plot
    op  <- par(mar=c(0.5,7,0.5,7), mfcol=c(1,2))
    barheight <- 0.6
    namecex <- 0.9
    typecex <- 1.5
    sourcex <- 0.65
    
    # ramp plot ==========
    plot(NA,axes=F,xlab='',ylab='',ylim=c(0,nrow(rampdat)+1), xlim=c(0,number), xaxs='i', yaxs='i')
    for (i in 1:nrow(rampdat)){
      ncolors=number
      for (j in 1:ncolors){
        irev <-  (length(rampdat$name):1)[i]
        polygon(x=c(j-1,j,j,j-1), y=c(rep(irev-0.5*barheight,2),rep(irev+0.5*barheight,2)), border=NA, col=rampdat$colors[[i]][j])
      }
      if (!is.na(plotdat$name[i])){
        polygon(x=c(0,100,100,0),y=c(rep(irev-0.5*barheight,2),rep(irev+0.5*barheight,2)), border='black', col=NA, lwd=0.5)
      }
    }
    mtext(rampdat$name, side=2, line=0.5, at=nrow(rampdat):1,padj=0.5,adj=1,las=2, cex=namecex, col='white', font=2)
    mtext(rampdat$source, side=4, line=0.5, at=nrow(rampdat):1,padj=0.5,adj=0,las=2, cex=sourcex, col='white')
    
    # add class labels
    classpos   <- sapply(split(rev(seq(nrow(rampdat))),rampdat$type), mean)
    mtext(names(classpos), side=2, line=5, cex=typecex, at=classpos, col='white',font=2)
    
  
    # pal plot =========
    plot(NA,axes=F,xlab='',ylab='',ylim=c(0,nrow(paldat)+1), xlim=c(0,1), xaxs='i', yaxs='i')
    for (i in 1:nrow(paldat)){
      ncolors <- length(paldat$colors[[i]])
      delta   <- 1/ncolors
      for (j in 1:ncolors){
        p <- j/ncolors
        irev <-  (length(paldat$name):1)[i]
        polygon(x=c(p-delta,p,p,p-delta), y=c(rep(irev-0.5*barheight,2),rep(irev+0.5*barheight,2)), border=NA, col=paldat$colors[[i]][j])
      }
      if (!is.na(plotdat$name[i])){
        polygon(x=c(0,1,1,0),y=c(rep(irev-0.5*barheight,2),rep(irev+0.5*barheight,2)), border='black', col=NA, lwd=0.5)
      }
    }
    mtext(paldat$name, side=2, line=0.5, at=nrow(paldat):1,padj=0.5,adj=1,las=2, cex=namecex, col='white', font=2)
    mtext(paldat$source, side=4, line=0.5, at=nrow(paldat):1,padj=0.5,adj=0,las=2, cex=sourcex, col='gray80')
    
    # add class labels
    classpos   <- sapply(split(rev(seq(nrow(paldat))),paldat$type), mean)
    mtext(names(classpos), side=2, line=5, cex=typecex, at=classpos, col='white', font=2)
    
    
    par(op)
    warning('No color output when "show=TRUE", only an external plot with available ramps and palettes')
    
    
    
  }else{ # generate output
    
    colpal   <- getPal(name)
    coltype  <- getType(name)
    
    if (reversed){colpal <- rev(colpal)}
    if (random){colpal   <- sample(colpal)}
    if (center!=0.5){
      if (coltype == 'Qualitative'){stop('Center should be 0.5 for qualitative palettes.')}
      censcl  <- center*1e3 + 1
      colvec  <- colorRampPalette(colpal)(1e4+1)
      sampind <- approx(y=c(0,500,1000), x=c(0,censcl,1000), xout=0:1000)$y
      sampind <- as.integer(sampind * 10 +1)
      colpal  <- colvec[sampind]
    }
    if (coltype=='Qualitative'){
      if (number>length(colpal)){
        outcolors <- rep(colpal,ceiling(number/length(colpal)))[1:number]
        warning(paste0('The qualitative ',toupper(name),' palette has only ',length(colpal), ' colors. Recycling colors up to desired number.'))
      }else{
        outcolors <- colpal[1:number]
      }
    }else{
      outcolors <- colorRampPalette(colpal)(number)
    }
    return(outcolors)
  }
}

#' @rdname ramp
#' @export
pkRamp <- ramp





#' Load preset color palettes and ramps
#'
#' Load color ramps and palettes from presets. Identical to \code{pkrf::ramp()} besides default pallete.
#' @param name Name of the color ramp (string).
#' @param ... Other arguments passed to \code{pkrf::ramp()}
#' @return Vector with hex colors strings. 
#' @export
pal <- function(name='MntHydro', ...){
  pkrf::ramp(name, ...)
}

#' @rdname pal
#' @export
pkPal <- pal