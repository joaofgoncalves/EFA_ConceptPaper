

library(raster)

getLmResid <- function(x, y=2001:2018, na.rm=TRUE){
  if(na.rm){
    if(any(is.na(x)|is.nan(x)|is.null(x))){
      return(rep(NA,length(x)))
    } 
  }
  DF <- data.frame(x = x, y = y)
  mod <- try(lm(x~y, data = DF), silent = TRUE)
  if(inherits(mod, "try-error")){
    return(rep(NA,length(x)))  
  }else{
    return(residuals(mod))
  }
}

## -------------------------------------------------------------------------------------------- ##

dirsToParse <- list.dirs("./DATA/RASTER/EFA_ANNUAL_v2", recursive = FALSE)

pb <- txtProgressBar(1,length(dirsToParse),style=3)

i <- 0

for(Dir in dirsToParse){
  
  i <- i + 1
  varName <- basename(Dir)
  
  rstFilePath <- list.files(Dir, pattern = ".tif$", full.names = TRUE)
  rstStack <- stack(rstFilePath)
  
  rstAvg <- calc(rstStack, mean, na.rm=TRUE)
  rstStd <- calc(rstStack, sd, na.rm=TRUE)
  rstCoV <- rstStd / rstAvg
  
  writeRaster(rstAvg, paste("./DATA/RASTER/EFA_MULTI_YEAR/",varName,
                            "_MultiYr_Avg_01_18_v1.tif",sep=""))
  writeRaster(rstStd, paste("./DATA/RASTER/EFA_MULTI_YEAR/",varName,
                            "_MultiYr_Std_01_18_v1.tif",sep=""))
  writeRaster(rstCoV, paste("./DATA/RASTER/EFA_MULTI_YEAR/",varName,
                            "_MultiYr_CoV_01_18_v1.tif",sep=""))
  
  # De-trend annual EFA's based on linear regression

  detrdRst <- calc(rstStack, getLmResid)

  message("-> De-trend done for variable: ", varName, "...... \n\n")
  
  detrdRstAvg <- calc(detrdRst, mean, na.rm=TRUE)
  detrdRstStd <- calc(detrdRst, sd, na.rm=TRUE)
  detrdRstCoV <- detrdRstStd / detrdRstAvg 

  writeRaster(detrdRstAvg, paste("./DATA/RASTER/EFA_MULTI_YEAR/",varName,
                                 "_MultiYr_Avg_01_18_v1_detrd.tif",sep=""))
  writeRaster(detrdRstStd, paste("./DATA/RASTER/EFA_MULTI_YEAR/",varName,
                                 "_MultiYr_Std_01_18_v1_detrd.tif",sep=""))
  writeRaster(detrdRstCoV, paste("./DATA/RASTER/EFA_MULTI_YEAR/",varName,
                                 "_MultiYr_CoV_01_18_v1_detrd.tif",sep=""))

  setTxtProgressBar(pb, i)
}



