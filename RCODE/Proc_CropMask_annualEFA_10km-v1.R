

library(raster)
library(gdalUtils)
library(sp)

# getMaskValues<-function(x, na.rm=TRUE){
#   ifelse(all(is.na(x)|is.nan(x)),NA,1)
# }
# 
## -------------------------------------------------------------------------------------------- ##
# 
# Generate file mask
# 
# fl <- list.files("./DATA/RASTER/Masks",full.names = TRUE)[c(2,10,14)]
# 
# r1 <- raster(fl[1])
# r2 <- raster(fl[2])
# r3 <- raster(fl[3])
# 
# r1 <- resample(r1, r3, method="ngb")
# r2 <- resample(r2, r3, method="ngb")
# 
# rstMasks <- stack(r1,r2,r3)
# 
# rstMask <- calc(rstMasks, getMaskValues)
# 
# plot(rstMask)
# 
# writeRaster(rstMask,"./DATA/RASTER/Masks/EFA_10km_mask.tif")
#
## -------------------------------------------------------------------------------------------- ##

rstMask <- raster("./DATA/RASTER/Masks/EFA_10km_mask.tif")

outFolder <- "./DATA/RASTER/EFA_ANNUAL_v2"

dirsToParse <- list.dirs("./DATA/RASTER/EFA_ANNUAL", recursive = FALSE)

dirsToParse <- dirsToParse[18:22]

yrs <- 2001:2018

pb <- txtProgressBar(1, length(dirsToParse)*length(yrs), style = 3)


i <- 0
k <- 0

for(Dir in dirsToParse){
  
  i <- i + 1
  varName <- basename(Dir)
  outDir <- paste(outFolder,"/",varName,sep="")
  
  if(!dir.exists(outDir)){
    dir.create(outDir)
  }
  
  rstFilePath <- list.files(Dir, pattern = ".tif$", full.names = TRUE)
  rstStack <- stack(rstFilePath)
  
  
  ## WRITING RASTER DATA TO YEARLY FILES -------------------------------
  ##
  ##
  nl <- nlayers(rstStack)
  for(nlr in 1:nl){
    
    rstInProc <- rstStack[[nlr]]
    yr <- yrs[nlr]
    k <- k + 1
    
    outFn <- paste(outDir,"/",varName,"_",yr,"_v1_20190722.tif",sep="")
    
    if(file.exists(outFn)){
      cat("\n\n-> File already exists! Skipping ............ \n\n")
      next
    }
    
    ## CROP RASTER DATASET ----------------------------------------------
    ##
    ##
    if(!compareRaster(rstMask, rstStack, stopiffalse = FALSE)){
      
      cat("\n\n-> Cropping raster dataset ....",varName,"[",yr,"] ....")
      rstInProc <- crop(rstInProc, extent(rstMask))
      cat("done.\n\n")
    }
    
    ## MASK RASTER STACK DATASET ----------------------------------------- 
    ##
    ##
    cat("\n\n-> Masking raster dataset ....",varName,"[",yr,"] ....")
    maskedRst <- mask(rstInProc, rstMask)
    cat("done.\n\n")
    
    #outFn <- paste(outDir,"/",varName,"_",yr,"_v1_20190722.tif",sep="")
    writeRaster(maskedRst, outFn)
    
    setTxtProgressBar(pb, k)
  }
}




