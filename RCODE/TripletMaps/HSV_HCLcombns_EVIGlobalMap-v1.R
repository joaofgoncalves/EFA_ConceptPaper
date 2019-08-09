
library(raster)
library(magrittr)

EVI_dmx_deg <- raster("D:/MyDocs/Projects/EFA_ConceptPaper/DATA/RASTER/EFA_MULTI_YEAR/EVI_Dmx_deg_circAvg.tif")

EVI_avg <- raster("D:/MyDocs/Projects/EFA_ConceptPaper/DATA/RASTER/EFA_MULTI_YEAR/EVI_Avg_MultiYr_Avg_01_18_v1.tif")

EVI_std <- raster("D:/MyDocs/Projects/EFA_ConceptPaper/DATA/RASTER/EFA_MULTI_YEAR/EVI_Std_MultiYr_Avg_01_18_v1.tif")

# EVI_dmx_deg <- resample(EVI_dmx_deg, EVI_avg, method="ngb")
# writeRaster(EVI_dmx_deg,"D:/MyDocs/Projects/EFA_ConceptPaper/DATA/RASTER/EFA_MULTI_YEAR/EVI_Dmx_deg_circAvg.tif",overwrite=TRUE)

EVI_avg <- EVI_avg + -1*cellStats(EVI_avg,"min")
EVI_avg <- EVI_avg / cellStats(EVI_avg,"max")

EVI_std <- EVI_std / cellStats(EVI_std,"max")

EVI_dmx_deg <- EVI_dmx_deg / 360


#' Create ternary maps based on the HSV or HCL color space
#' 
#' Creates a map based on HSV or HCL color space and returns an image stack 
#' with three bands containing the HSV/HCL colors converted back to RGB 
#' to easier plotting
#' 
#' @param H Hue raster usually representing angles in the HSV color wheel and within 
#' the [0,1] interval
#' @param S Saturation raster for HSV and within [0,1] interval
#' @param V Value raster for HSV and within [0,1] interval
#' 
#' @param hue Hue raster usually representing angles in the HCL color wheel and within 
#' the [0,360] interval
#' @param chroma Chroma raster for HCL and within [0,100] interval
#' @param luminance Luminance raster for HCL and within [0,100] interval
#' 
#' @param colorspace Color space to use. Either 'HSV' (default) or 'HCL'
#' @param rescale Re-scale values of S and V or chroma and luminance to 0 - 1 range? 
#' Rescaling is performed as: (x-min(x)) / (max(x)-min(x))? (default: FALSE)
#' @param hueUnits Units of angle in the H (HSV) or hue (HCL) component? 
#' Either 'deg' (degrees) or 'rad' (radians)
#' @param verbose Print progress messages? (default: TRUE)
#' @param filename A filename to which write the output RGB raster stack (default: NULL).
#' @param ... Extra parameters to pass in writeRaster
#' 
#' @return A RasterStack object with three bands (for R,G, and B values in the 0-255 range)
#' 

ternaryMaps <- function(H=NULL, S=NULL, V=NULL, hue=NULL, chroma=NULL, luminance=NULL, colorspace="HSV", 
                        rescale=FALSE, hueUnits="deg", verbose=TRUE, filename=NULL, ...){
  
  require(raster)
  
  if(colorspace=="HSV"){
    if(any(c(is.null(H), is.null(S), is.null(V)))){
      stop("H, S and V must be non-null! Please check input arguments.")
    }
    c1 <- H
    c2 <- S
    c3 <- V
  }
  if(colorspace=="HCL"){
    if(any(c(is.null(hue), is.null(chroma), is.null(luminance)))){
      stop("hue, chroma and luminance must be non-null! Please check input arguments.")
    }
    c1 <- hue
    c2 <- chroma
    c3 <- luminance
  }
  
  if(!inherits(c1,"RasterLayer") || !inherits(c2,"RasterLayer") || !inherits(c3,"RasterLayer")){
    stop("H/S/V or hue/chroma/luminance must be RasterLayer objects!")
  }
  
  if(!compareRaster(c1,c2,c3,stopiffalse = FALSE)){
    stop("Rasters in H/S/V or hue/chroma/luminance are different! Please check input arguments.")
  }
  
  if(rescale){
    
    if(verbose) message("Standardizing raster data....")
    
    rescale01 <- function(x, ...){(x - cellStats(x, 'min', ...)) / (cellStats(x, 'max', ...) - cellStats(x, 'min', ...))}
    #c1 <- rescale01(c1)
    if(hueUnits=="deg"){
      c1 <- c1 / 360
    }else if(hueUnits=="rad"){
      c1 <- c1 / (2*pi)
    }else{
      stop("Invalid units in hueUnits! Either 'deg' (degrees) or 'rad' (radians) options are valid.")
    }
    c2 <- rescale01(c2)
    c3 <- rescale01(c3)
    
    if(verbose) message("Finished!")
  }
  
  
  if(verbose) message("Stacking and loading raster data....")
  rstDF <- values(stack(c1,c2,c3))
  if(verbose) message("Finished!")
  
  
  if(verbose) message("Calculating colors....")
  ind <- complete.cases(rstDF)
  
  if(colorspace=="HSV"){
    # Generate HSV color codes
    colVec <- hsv(h = rstDF[ind,1], s = rstDF[ind,2], v = rstDF[ind,3])
    # Convert color code to RGB and transpose the color matrix from wide to long
    rgbCol <- t(col2rgb(colVec)) 
  }
  
  if(colorspace=="HCL"){
    # Generate HCL color codes
    colVec <- hcl(h = rstDF[ind,1]*360, c = rstDF[ind,2]*100, l = rstDF[ind,3]*100)
    # Convert color code to RGB and transpose the color matrix from wide to long
    rgbCol <- t(col2rgb(colVec))
  }
  if(verbose) message("Finished!")
  
  
  if(verbose) message("Attributing colors to the output RGB raster....")

  r <- c1
  values(r) <- NA
  values(r)[ind] <- rgbCol[,1] # red component
  
  g <- c1
  values(g) <- NA
  values(g)[ind] <- rgbCol[,2] # green component
  
  b <- c1
  values(b) <- NA
  values(b)[ind] <- rgbCol[,3] # blue component
  
  rstOut <- stack(r,g,b)
  
  if(verbose) message("Finished!")
  
  if(!is.null(filename)){
    if(verbose) message("Writing raster data to file....")
    
    writeRaster(rstOut, filename = filename, ...)
    
    if(verbose) message("Finished!")
  }
  return(rstOut)
}


rgbMap <- ternaryMaps(H=EVI_dmx_deg, S=EVI_std, V=EVI_avg, colorspace="HSV", rescale = TRUE,
                      filename="./DATA/RASTER/RGB_EFA/RGB_EVI_H-Dmx_S-Std_V-Avg_HSV_v3.tif", 
                      overwrite=TRUE)
                        
plotRGB(rgbMap, stretch="hist")

