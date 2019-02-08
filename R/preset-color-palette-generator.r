# custom color ramp generator
# philip kraaijenbrink


#' Load preset color palettes
#'
#' Load color palettes from preset
#' @param name Name of the palette (case-insensitive string).
#' @param number Number of output colors desired (integer). 
#' @param reversed Reverse palette order (logical).
#' @param reversed Randomize palette order (logical).
#'
#' @return Vector with hex colors strings. 
#' @export
pkPal <- function(name='Set1', number=0, reversed=F, random=F){
  
  namelist <- c('Set1','Set2','Set3','Pastel1','Pastel2','Accent','Dark', 'Google','Raven','Muted','Cappuccino',
                'Pale1','Water','Skin','NS','Legendary','WarmWall', 'Oasis','Earthly','A3','AfterCold','Swoop',
                'BurbleTeal')
  if (sum(namelist %in% name)){
    message <- paste0('Color palette not available. Choose any of:\n',paste(sort(namelist),collapse=', '))
  }
  colpal   <- switch(which(tolower(namelist) %in% tolower(name)),
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

