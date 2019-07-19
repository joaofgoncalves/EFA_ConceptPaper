

library(raster)

getLmResid <- function(x, y=2001:2018, na.rm=TRUE){
  
  if(any(is.na(x)|is.nan(x)|is.null(x))){
    return(NA)
  }
  DF <- data.frame(x = x, y = y)
  mod <- lm(x~y, data = DF)
  return(residuals(mod))
}

dirsToParse <- list.dirs("./DATA/RASTER/EFA_ANNUAL", recursive = FALSE)

pb <- txtProgressBar(1,length(dirsToParse),style=3)

i <- 0
for(Dir in dirsToParse){
  
  i <- i + 1
  varName <- basename(Dir)
  
  rstFilePath <- list.files(Dir, pattern = ".tif$", full.names = TRUE)
  rstStack <- stack(rstFilePath)
  
  rstAvg <- calc(rstStack, mean)
  rstStd <- calc(rstStack, sd)
  rstCoV <- rstStd / rstAvg
    
  detrdRst <- calc(rstStack, getLmResid)
  
  detrdRstAvg <- calc(rstStack, mean)
  detrdRstStd <- calc(rstStack, sd)
  detrdRstCoV <- detrdRstAvg / detrdRstStd
  
  
  writeRaster(rstAvg, paste("./DATA/RASTER/EFA_MULTI_YR/",varName,"_MultiYrAvg.tif",sep=""))
  writeRaster(rstAvg, paste("./DATA/RASTER/EFA_MULTI_YR/",varName,"_MultiYrStd.tif",sep=""))
  writeRaster(rstAvg, paste("./DATA/RASTER/EFA_MULTI_YR/",varName,"_MultiYrCoV.tif",sep=""))
  
  writeRaster(detrdRstAvg, paste("./DATA/RASTER/EFA_MULTI_YR/",varName,"_MultiYrAvg_detrd.tif",sep=""))
  writeRaster(detrdRstStd, paste("./DATA/RASTER/EFA_MULTI_YR/",varName,"_MultiYrStd_detrd.tif",sep=""))
  writeRaster(detrdRstCoV, paste("./DATA/RASTER/EFA_MULTI_YR/",varName,"_MultiYrCoV_detrd.tif",sep=""))
  
  
  setTxtProgressBar(pb, i)
}



