
library(raster)
library(trend)
library(diptest)
library(parallel)
library(doParallel)
library(foreach)


## Functions to calculate the Theil-Sen trend slope and the p-value ----------------------

senSlope <- function(x,...) sens.slope(x,...)$estimates
senSlopePval <- function(x,...) sens.slope(x,...)$p.value

senTrendSlope <- function(x, na.rm = TRUE, ...){
  
  
  
  if(all(is.na(x) | is.nan(x))){
    return(c(NA,NA))
  }else{
    if(na.rm){
      x <- na.omit(x)
    }
  }
  x <- as.numeric(x)
  sens <- try(suppressWarnings(sens.slope(x,...)))
  
  if(inherits(sens,"try-error")){
    return(c(NA,NA))
  }else{
    return(as.numeric(c(sens[["estimates"]], sens[["p.value"]])))
  }
}

multiModalTest <- function(x, na.rm=TRUE, ...){
  
  if(all(is.na(x) | is.nan(x))){
    return(c(NA,NA))
  }else{
    if(na.rm){
      x <- na.omit(x)
    }
  }
  x <- as.numeric(x)
  testRes <- try(suppressWarnings(dip.test(x, ...)))
  
  if(inherits(testRes,"try-error")){
    return(c(NA,NA))
  }else{
    return(as.numeric(c(testRes[["statistic"]], 
                        testRes[["p.value"]])))
  }
} 


## Load data from raster files -------------------------------------------------------

setwd("D:/MyDocs/Projects/EFA_ConceptPaper/OUT/MODS/R1")


Dirs <- list.dirs(recursive = FALSE)

spNames <- basename(Dirs)

rstEnsBySp <- list()
rstBinBySp <- list()

i <- 0
for(sp in spNames){
  
  i <- i + 1
  
  fn <- list.files(Dirs[i], recursive = TRUE, pattern=".tif$", full.names = TRUE)
  
  fn_ens <- (fn[grepl("_ensemble.tif", fn)])[-1]
  fn_bin_tss <- (fn[grepl("_ensemble_TSSbin.tif", fn)])[-1]
  
  for(j in 1:length(fn_ens)){
    
    if(j==1){
      rstStackEns <- stack(fn_ens[j])[[1]]
      rstStackBin <- stack(fn_bin_tss[j])[[1]]
    }else{
      rstStackEns <- stack(rstStackEns, stack(fn_ens[j])[[1]])
      rstStackBin <- stack(rstStackBin, stack(fn_bin_tss[j])[[1]])
    }
  }
  
  rstEnsBySp[[sp]] <- rstStackEns
  rstBinBySp[[sp]] <- rstStackBin
  cat("--> Finished loading results for species:",sp,"\n\n")
}



## Summarize results for all years ---------------------------------------------------


setwd("D:/MyDocs/Projects/EFA_ConceptPaper/OUT/MODS/R1_MOD_PREDS")


for(sp in spNames){
  
  cat("## ----------------------------------------------------------------------------##\n")
  cat("## Processing species:",sp,"\n")
  cat("## ----------------------------------------------------------------------------##\n\n")
  
  rstEns <- rstEnsBySp[[sp]]
  rstBin <- rstBinBySp[[sp]]
  
  
  binSum <- calc(rstBin, sum)
  writeRaster(binSum, paste(sp,"_HS_Bin_SumPreds.tif",sep=""), overwrite=TRUE)
  message("Finished calculating the sum for 0-1 predictions!")
  
  binSumPerc <- (binSum / nlayers(rstBin)) * 100
  writeRaster(binSumPerc, paste(sp,"_HS_Bin_SumPredsPerc.tif",sep=""), overwrite=TRUE)
  message("Finished calculating the % sum for 0-1 predictions!")
  
  binMode <- modal(rstBin)
  writeRaster(binMode, paste(sp,"_HS_Bin_ModalClass.tif",sep=""), overwrite=TRUE)
  message("Finished calculating the mode of 0-1 predictions!")
  
  
  
  ensAvg <- calc(rstEns, mean)
  writeRaster(ensAvg, paste(sp,"_HS_Ensemble_Avg.tif",sep=""), overwrite=TRUE)
  message("Finished calculating the average!")
  
  ensStd <- calc(rstEns, sd)
  writeRaster(ensStd, paste(sp,"_HS_Ensemble_Std.tif",sep=""), overwrite=TRUE)
  message("Finished calculating the standard-deviation!")
  
  ensCov <- ensStd / ensAvg
  writeRaster(ensCov, paste(sp,"_HS_Ensemble_CoV.tif",sep=""), overwrite=TRUE)
  message("Finished calculating the Coeff. of variation!")
  
  ensTrendSlope <- calc(rstEns, senTrendSlope)
  writeRaster(ensTrendSlope, paste(sp,"_HS_Ensemble_SenSlope.tif",sep=""), overwrite=TRUE)
  message("Finished calculating the Sen-slope!")

  ensMultimodalStat <- calc(rstEns, multiModalTest)
  writeRaster(ensMultimodalStat, paste(sp,"_HS_Ensemble_MultimodalTest.tif",sep=""), overwrite=TRUE)
  message("Finished calculating the Multimodal test stat!")

  cat("\n--> Finished processing species:",sp,"\n\n")
}



