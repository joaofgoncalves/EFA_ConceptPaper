
library(raster)
library(sp)
library(rgdal)
library(readr)
library(dplyr)
library(biomod2plus)
library(sf)

## -------------------------------------------------------------------------------------- ##

setwd("D:/MyDocs/Projects/EFA_ConceptPaper/OUT/MODS/R1")

Dirs <- list.dirs(recursive = FALSE)

spNames <- basename(Dirs)



rstEnsBySp <- list()
rstBinBySp <- list()

i <- 0
for(sp in spNames){
  
  i <- i + 1
  
  fn <- list.files(Dirs[i], recursive = TRUE, pattern=".tif$", full.names = TRUE)
  
  fn_ens <- (fn[grepl("_ensemble.tif", fn)])[1]
  fn_bin_tss <- (fn[grepl("_ensemble_TSSbin.tif", fn)])[1]
  
  for(j in 1:length(fn_bin_tss)){
    
    if(j==1){
      rstStackBin <- stack(fn_bin_tss[j])[[1]]
      rstStackEns <- stack(fn_ens[j])[[1]]
      
    }else{
      rstStackBin <- stack(rstStackBin, stack(fn_bin_tss[j])[[1]])
      rstStackEns <- stack(rstStackEns, stack(fn_ens[j])[[1]])
      
    }
  }
  
  rstBinBySp[[sp]] <- rstStackBin
  rstEnsBySp[[sp]] <- rstStackEns
  
  cat("--> Finished loading results for species:",sp,"\n\n")
}


rstAvg <- rstEnsBySp[["Uncia.uncia"]]
rstBin <- rstBinBySp[["Uncia.uncia"]]

rstAvg[rstBin==0] <- NA

mean(na.omit(values(rstAvg)))

round((147.5 / 797.2)*100, 1)
round((83.5 / 797.2)*100, 1)


rstAvg <- rstEnsBySp[["Grus.americana"]]
rstBin <- rstBinBySp[["Grus.americana"]]

rstAvg[rstBin==0] <- NA

mean(na.omit(values(rstAvg)))

round((40.25/881.2)*100, 1)
round((39/881.2)*100, 1)

## -------------------------------------------------------------------------------------- ##



setwd("D:/MyDocs/Projects/EFA_ConceptPaper")

baseFolder <- "D:/MyDocs/Projects/EFA_ConceptPaper/OUT/MODS/R1_MOD_PREDS"

fl <- list.files("D:/MyDocs/Projects/EFA_ConceptPaper/OUT/MODS/R1_MOD_PREDS", 
                 pattern="_SenSlope.tif$",full.names = TRUE)


for(i in 1:length(fl)){
  
  sp <- spNames[i]
  speciesName <- gsub("\\ +|\\.","_",sp)
  
  # Make raster stack per species
  rstStack <- stack(fl[i])
  
  # Remove non-suitable pixels from the Sen-slope map
  # Uses the average model 2001-2018
  rstStack[rstBinBySp[[sp]] == 0] <- NA

  fname <- paste(baseFolder,"/",sp, "_HS_Ensemble_SenSlope_SuitAreaOnly.tif", sep="")
  fname1 <- paste(baseFolder,"/",sp, "_HS_Ensemble_SenSlope_pval005.tif", sep="")
  fname2 <- paste(baseFolder,"/",sp, "_HS_Ensemble_SenSlope_pval01.tif", sep="")
  outFolder1 <- paste(baseFolder,"/",sp, "_SenSlopePval", sep="")

  # p-value at 0.05  
  rstPval_005 <- rstStack[[2]]
  rstPval_005[rstPval_005 > 0.05] <- NA
  rstPval_005[rstPval_005 <= 0.05] <- 1
  
  # p-value at 0.1
  rstPval_01 <- rstStack[[2]]
  rstPval_01[rstPval_01 > 0.1] <- NA
  rstPval_01[rstPval_01 <= 0.1] <- 1
  
  # Convert statistically significant data to points and polygons
  # at alpha = {0.05, 0.1}
  pols005 <- rasterToPolygons(rstPval_005, n=4, dissolve=TRUE)
  points005 <- rasterToPoints(rstPval_005, spatial = TRUE)
  
  pols01 <- rasterToPolygons(rstPval_01, n=4, dissolve=TRUE)
  points01 <- rasterToPoints(rstPval_01, spatial = TRUE)
  
  
  # Write data to files
  writeRaster(rstStack, fname, overwrite = TRUE)
  writeRaster(rstPval_005, fname1, overwrite = TRUE)
  writeRaster(rstPval_01, fname2, overwrite = TRUE)
  
  writeOGR(pols005, dsn = outFolder1, layer = paste(speciesName,"_polyg_p005",sep=""), 
           driver = "ESRI Shapefile")
  writeOGR(points005, dsn = outFolder1, layer = paste(speciesName,"_pts_p005",sep=""), 
           driver = "ESRI Shapefile")
  
  writeOGR(pols01, dsn = outFolder1, layer = paste(speciesName,"_polyg_p01",sep=""), 
           driver = "ESRI Shapefile")
  writeOGR(points01, dsn = outFolder1, layer = paste(speciesName,"_pts_p01",sep=""), 
           driver = "ESRI Shapefile")
  
}



