

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

doDetrending <- FALSE

outDetrendFiles <- "./DATA/RASTER/EFA_MULTI_YEAR/detrd"
outFiles <- "./DATA/RASTER/EFA_MULTI_YEAR"
fileSuffix <- "01_18"

## ---------------------------------------------------------------------- #

i <- 0
pb <- txtProgressBar(1,length(dirsToParse),style=3)

for(Dir in dirsToParse){
  
  i <- i + 1
  
  # Variable name taken from the base dir
  varName <- basename(Dir)
  
  # List files in the path
  rstFilePath <- list.files(Dir, pattern = ".tif$", full.names = TRUE)
  
  # Ignore empty dirs
  if(length(rstFilePath)==0){
    cat("\n\n[",Dir,"] Empty directory! Skipping ........ \n\n")
    next
  }
  
  # Ignore Dmx - day of maximum layers
  if(grepl("_Dmx",varName)){
    cat("\n\n[",varName,"] Skipping variable day of maximum value! ....... \n\n")
    next
  }
  
  # Make raster stack
  rstStack <- stack(rstFilePath)
  
  # Generate output file paths
  #
  fnOut1 <- paste(outFiles, "/", varName, "_MultiYr_Avg_",fileSuffix,"_v1.tif",sep="")
  fnOut2 <- paste(outFiles, "/", varName, "_MultiYr_Std_",fileSuffix,"_v1.tif",sep="")
  fnOut3 <- paste(outFiles, "/", varName, "_MultiYr_CoV_",fileSuffix,"_v1.tif",sep="")
  
  if(doDetrending){
    # De-trended files
    fnOut4 <- paste(outDetrendFiles, "/",varName, "_MultiYr_Avg_",fileSuffix,"_v1_detrd.tif",sep="")
    fnOut5 <- paste(outDetrendFiles, "/",varName, "_MultiYr_Std_",fileSuffix,"_v1_detrd.tif",sep="")
    fnOut6 <- paste(outDetrendFiles, "/",varName, "_MultiYr_CoV_",fileSuffix,"_v1_detrd.tif",sep="")
  }
  

  # Calculate non-detrended multi-year EFA's
  #
  if(file.exists(fnOut1)){
    message("\n\n-> Skipping...",fnOut1,"...\n\n")
    next
  }else{
    rstAvg <- calc(rstStack, mean, na.rm=TRUE)
    writeRaster(rstAvg, fnOut1)
  }
  
  if(file.exists(fnOut2)){
    message("\n\n-> Skipping...",fnOut2,"...\n\n")
    next
  }else{
    rstStd <- calc(rstStack, sd, na.rm=TRUE)
    writeRaster(rstStd, fnOut2)
  }
  
  if(file.exists(fnOut3)){
    message("\n\n-> Skipping...",fnOut3,"...\n\n")
    next
  }else{
    rstCoV <- rstStd / rstAvg
    writeRaster(rstCoV, fnOut3)
  }
  
  if(doDetrending){
    
    # Check if any of the detrended files does not exist
    #
    #
    if(any(!file.exists(c(fnOut4, fnOut5, fnOut6)))){
      # De-trend annual EFA's based on linear regression
      detrdRst <- calc(rstStack, getLmResid)
      message("\n\n-> De-trend done for variable: ", varName, "...... \n\n")
    }

    if(file.exists(fnOut4)){
      message("\n\n-> Skipping...",fnOut4,"...\n\n")
      next
    }else{
      detrdRstAvg <- calc(detrdRst, mean, na.rm=TRUE)
      writeRaster(detrdRstAvg, fnOut4)
    }

    if(file.exists(fnOut5)){
      message("\n\n-> Skipping...",fnOut5,"...\n\n")
      next
    }else{
      detrdRstStd <- calc(detrdRst, sd, na.rm=TRUE)
      writeRaster(detrdRstStd, fnOut5)
    }

    if(file.exists(fnOut6)){
      message("\n\n-> Skipping...",fnOut6,"...\n\n")
      next
    }else{
      detrdRstCoV <- detrdRstStd / detrdRstAvg
      writeRaster(detrdRstCoV, fnOut6)
    }
  }
  
  setTxtProgressBar(pb, i)
}



