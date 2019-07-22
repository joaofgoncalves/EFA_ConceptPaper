
library(raster)



binarizeNoData <- function(x){
  ifelse(any(is.na(x))|any(is.nan(x))|any(is.null(x))|any(is.infinite(x)), NA, 1)
}

dirsToParse <- list.dirs("./DATA/RASTER/EFA_ANNUAL", recursive = FALSE)

pb <- txtProgressBar(1,length(dirsToParse),style=3)

i <- 0
for(Dir in dirsToParse){
  
  i <- i + 1
  varName <- basename(Dir)
  
  fnOut <- paste("./DATA/RASTER/Masks/mask_",varName,".tif",sep="")
  if(file.exists(fnOut)){
    cat("\nSkipping!\n\n")
    next
  }
    
  
  rstFilePath <- list.files(Dir, pattern = ".tif$", full.names = TRUE)
  rstStack <- stack(rstFilePath)
  
  DF <- values(rstStack)
  
  maskValues <- apply(DF, 1, binarizeNoData)
  
  newRst <- rstStack[[1]]
  values(newRst) <- maskValues
  
  writeRaster(newRst, fnOut)
  setTxtProgressBar(pb, i)
}


