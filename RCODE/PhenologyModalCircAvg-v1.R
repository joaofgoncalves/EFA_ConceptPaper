

library(raster)
library(circular)
library(hydroTSM)

dirsToParse <- list.dirs("./DATA/RASTER/EFA_ANNUAL_v2", recursive = FALSE)

dirsToParse <- dirsToParse[grepl("_Dmx",dirsToParse)]

yrs <- 2001:2018

deg2rad <- function(x) x*(pi/180)

rad2deg <- function(x) (x*180)/pi

days2deg <- function(x, ndays) (x*360)/ndays

rad2days <- function(x,ndays) (x*ndays)/(2*pi)

circularAverage <- function(x, na.rm=TRUE){
  
  if(all(is.na(x))){
    return(NA)
  }
  if(na.rm){
    x<-na.omit(x)
  }
  xc <- suppressWarnings(circular(x, 
                                  type = 'angles', 
                                  units = 'radians', 
                                  rotation = 'clock'))
  xc_avg <- mean.circular(xc, na.rm = TRUE)
  return(xc_avg)
}


circAvg <- function (x,na.rm=TRUE){
  if(all(is.na(x))){
    return(NA)
  }
  if(na.rm){
    x<-na.omit(x)
  }
  sinr <- sum(sin(x))
  cosr <- sum(cos(x))
  circmean <- atan2(sinr, cosr)
  if(circmean<0) 
    circmean <- circmean + (2*pi)
  return(circmean)
}


i <- 0
#pb <- txtProgressBar(1,length(dirsToParse),style=3)

for(Dir in dirsToParse){
  
  i <- i + 1
  
  # Variable name taken from the base dir
  varName <- basename(Dir)
  
  # List files in the path
  rstFilePath <- list.files(Dir, pattern = ".tif$", full.names = TRUE)
  
  rst <- stack(rstFilePath)
  #rstMode <- modal(rst)
  #rstDF <- na.omit(values(rst))

  for(j in 1:nlayers(rst)){
    
    # Convert days to degrees
    ndays   <- length(diy(yrs[j]))
    dmx_rad_tmp <- deg2rad(days2deg(rst[[j]], ndays))
    
    if(j == 1){
        dmx_rad <- dmx_rad_tmp
    }else{
      dmx_rad <- stack(dmx_rad, dmx_rad_tmp)
    }
  }
  
  #dmxradStack <- na.omit(values(dmx_rad))
  dmx_circ_avg <- calc(dmx_rad, circAvg)
  dmx_avg_days <- rad2days(dmx_circ_avg, 365)
  dmx_avg_deg  <- rad2deg(dmx_circ_avg)
  
  writeRaster(dmx_circ_avg, paste("./DATA/RASTER/EFA_MULTI_YEAR/",varName,"_rad_circAvg.tif",sep=""))
  writeRaster(dmx_avg_days, paste("./DATA/RASTER/EFA_MULTI_YEAR/",varName,"_days_circAvg.tif",sep=""))
  writeRaster(dmx_avg_deg,  paste("./DATA/RASTER/EFA_MULTI_YEAR/",varName,"_deg_circAvg.tif",sep=""))

    
}


