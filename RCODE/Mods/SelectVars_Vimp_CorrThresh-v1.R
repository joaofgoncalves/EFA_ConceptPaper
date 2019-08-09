

library(raster)
library(ggplot2)
library(biomod2)
library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(sf)
library(sp)
library(biomod2plus)
library(magrittr)

selectBestVarsByCor <- function(varImpDF, rstDF, 
                                 vnamesCol = "cnames",
                                 vimpCol   = "vimpAVG",
                                 thresh    = 0.75,
                                 method    = "spearman",
                                 sortIt    = FALSE){
  
  
  if(sortIt){
    varImpDF <- varImpDF %>% arrange(desc(.[[vimpCol]]))
  }
  
  for(k in 2:nrow(varImpDF)){
    
    if(k==2){
      varsToKeep <- as.character(varImpDF[1,vnamesCol])
    }
    
    # Variable name to test
    varToTest <- as.character(varImpDF[k,vnamesCol])
    
    # Corelation vector
    cmat <- cor(rstDF[,varsToKeep], rstDF[,varToTest], method=method)
    
    if(is.null(dim(cmat))){
      corVals <- cmat
    }else{
      ncols <- length(varsToKeep) + 1 
      corVals <- cmat[,1]
      #print(cmat %>% round(2))
      #print(corVals)
    }
    
    if(all(abs(corVals) < thresh)){
      varsToKeep <- c(varsToKeep, varToTest)
      cat("-> [",k,"] Keeping variable:", varToTest,"( max. correlation = ",max(corVals %>% round(2)),")\n\n")
    }
  }
  
  attr(varsToKeep,'thresh') <- thresh
  attr(varsToKeep,'method') <- method
  return(varsToKeep)
}



fl <- list.files("D:/MyDocs/Projects/EFA_ConceptPaper/DATA/RASTER/EFA_MULTI_YEAR", pattern=".tif$", full.names = TRUE)
fl <- fl[grepl("_MultiYr_Avg",fl)]

varNames <- gsub(".tif|_01_18_v1|_MultiYr_Avg","",basename(fl))

current <- stack(fl)
names(current) <- varNames

gbifData <- read_csv("D:/MyDocs/Projects/EFA_ConceptPaper/DATA/TABLES/spData/GBIF_DATA_AllSpecies-v1.csv")

colnames(gbifData)

spNames <- unique(gbifData$species)
spNames <- spNames[-c(1,8)]




setwd("D:/MyDocs/Projects/EFA_ConceptPaper/OUT/MODS/R0")

Dirs <- list.dirs(recursive = FALSE)

selVarsBySpecies <- list()

i <- 0
for(Dir in Dirs){
  i <- i+1
  
  spName <- spNames[i]
  sp <- gsub("\\ +",".",spName)
  
  spRecords <- gbifData %>% filter(species == spName)
  
  x1 <- min(spRecords[,"decimalLongitude"])
  x2 <- max(spRecords[,"decimalLongitude"])
  y1 <- min(spRecords[,"decimalLatitude"])
  y2 <- max(spRecords[,"decimalLatitude"])
  
  ext <- SpatialPolygons(list(Polygons(
    list(Polygon(coords = list(matrix(c(x1,y1, x1,y2, x2,y2, x2,y1, x1,y1),
                                      nrow = 5, ncol = 2, byrow = TRUE)))),
    ID = 1)))
  
  #plot(current[[1]])
  #plot(ext, add=TRUE)
  
  rstCrop <- crop(current, ext)
  #plot(rstCrop[[1]])
  #rstMask <- mask(rstCrop, ext)
  
  rstSubsetDF <- values(rstCrop) %>% na.omit
  #head(rstSubsetDF)
  
  varImpDF <- read.csv(paste("./",sp,"/",sp,"_varImportance.csv",sep=""), stringsAsFactors = FALSE)
  
  selVars <- selectBestVarsByCor(varImpDF, rstSubsetDF, 
                      thresh    = 0.75,
                      method    = "spearman",
                      sortIt    = FALSE)
  print(sp)
  print(selVars)
  selVarsBySpecies[[sp]] <- selVars
  
}

saveRDS(selVarsBySpecies, "../../selVarsBySpecies-List-v1.RData")




