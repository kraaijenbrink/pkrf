# custom color ramp generator
# philip kraaijenbrink


#' Load preset color palettes
#'
#' Load color palettes from preset
#' @param name Name of the palette (case-insensitive string).
#' @param number Number of output colors desired (integer). Set to 0 to get all colors available in the palette.
#' @param reversed Reverse palette order (logical).
#' @param reversed Randomize palette order (logical).
#' @param show Show a plot of all available color palettes (logical).
#' 
#' @return Vector with hex colors strings. 
#' @export
pal <- function(name='MntHydro', number=0, reversed=F, random=F, show=F){
  
  namelist <- c('MntHydro','UUmain','UUalt',
                'Set1','Set2','Set3','Pastel1','Pastel2','Accent','Dark', 'Google','Raven','Muted','Cappuccino',
                'Pale1','Water','Skin','NS','Legendary','WarmWall', 'Oasis','Earthly','A3','AfterCold','Swoop',
                'BurbleTeal')
  if (sum(tolower(namelist) %in% tolower(name))==0){
    message <- stop(paste0('Color palette not available. Choose any of:\n',paste(sort(namelist),collapse='\n')))
  }
  
  getPal <- function(name){
    switch(which(tolower(namelist) %in% tolower(name)),
                     c('232365','FFCD00','AAAAAA','7b942e','deebf6','202030','FFECA2','454545','dfebbd','9b7f0a','1E5E37'),
                     c('FFCD00','C00A35','000000','094D8E','36A2C9','791D72','9A0000','F08000','FEF200','419702'),
                     c('FFE6AB','F3965E','AA1555','6E3B23','24A793','5287C6','001240','5B2182'),
                     c("E41A1C","377EB8","4DAF4A","984EA3","FF7F00","FFFF33","A65628","F781BF","999999"),
                     c("66C2A5","FC8D62","8DA0CB","E78AC3","A6D854","FFD92F","E5C494","B3B3B3"),
                     c("8DD3C7","FFFFB3","BEBADA","FB8072","80B1D3","FDB462","B3DE69","FCCDE5","D9D9D9","BC80BD","CCEBC5"),
                     c("FBB4AE","B3CDE3","CCEBC5","DECBE4","FED9A6","FFFFCC","E5D8BD","FDDAEC","F2F2F2"),
                     c("B3E2CD","FDCDAC","CBD5E8","F4CAE4","E6F5C9","FFF2AE","F1E2CC","CCCCCC"),
                     c("7FC97F","BEAED4","FDC086","FFFF99","386CB0","F0027F","BF5B17","666666"),
                     c("1B9E77","D95F02","7570B3","E7298A","66A61E","E6AB02","A6761D","666666"),
                     c('008744','0057e7','d62d20','ffa700','000000'),
                     c('0e1a40','222f5b','5d5d5d','946b2d','000000'),
                     c('2e4045','83adb5','c7bbc9','5e3c58','bfb5b2'),
                     c('4b3832','854442','E5DBCF','3c2f2f','be9b7b'),
                     c('c4b2c2','b38d97','d5aca9','ebcfb2','c5baaf'),
                     c('b9d6f2','084c61','061a40','0353a4','2d93ad'),
                     c('d8e2dc','ffe5d9','ffcad4','f4acb7','9d8189'),
                     c('06aed5','086788','f0c808','f9e39f','dd1c1a'),
                     c('5bc0eb','fde74c','9bc53d','e55934','fa7921'),
                     c('d1cebe','e07a5f','3d405b','81b29a','f2cc8f'),
                     c('000000','839788','eee0cb','baa898','bfd7ea'),
                     c('537d99','def2c8','bbc451','9a9b58','8f451f'),
                     c('1c3144','d00000','ffba08','a2aebb','3f88c5'),
                     c('171219','225560','edf060','f0803c','310d20'),
                     c('dd7373','3b3561','ead94c','d1d1d1','51a3a3'),
                     c('2d3047','93b7be','e0ca3c','a799b7','048a81')
    )
  }
  
  
  if (show){ # make plot of available ramps
    
    spac <- 0.33
    all <- lapply(namelist, function(x) (paste0('#',getPal(x))))
    x11(bg='#2d2d2d')
    op  <- par(mar=c(0.5,8,0.5,1))
    plot(NA,axes=F,xlab='',ylab='',ylim=c(0,length(namelist)+1), xlim=c(0,1), xaxs='i', yaxs='i')
    for (i in 1:length(namelist)){
      ncolor <- length(all[[i]])
      delta <- 1/ncolor
      for (j in 1:ncolor){
        p <- j/ncolor
        irev <-  (length(namelist):1)[i]
        polygon(x=c(p-delta,p,p,p-delta), y=c(irev-spac,irev-spac,irev+spac,irev+spac), border=NA, lwd=0.5, col=all[[i]][j])
      }
    }
    mtext(namelist, side=2, line=0.5, at=length(namelist):1,padj=0.5,adj=1,las=2, col='white', font=1, cex=1.1)
    par(op)
    warning('No color palette output when "show=TRUE", just a plot with available color palettes')
  
  
  }else{
    colpal <- getPal(name)
    if (reversed){colpal <- rev(colpal)}
    if (random){colpal   <- sample(colpal)}
    if (number>length(colpal)){
      outcolors <- paste0('#',colpal)
      warning(paste0('The ',name,' palette has only ',length(colpal), ' colors. Returning vector with only those.'))
    }else if (number==0){
      outcolors <- paste0('#',colpal)
    }else{
      outcolors <- paste0('#',colpal)[1:number]
    }
    return(outcolors)
  }
}



#' @rdname pal
#' @export
pkPal <- pal