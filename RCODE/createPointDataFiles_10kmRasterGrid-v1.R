
library(raster)
library(sp)
library(rgdal)
library(readr)
library(dplyr)
library(biomod2plus)
library(sf)

setwd("D:/MyDocs/Projects/EFA_ConceptPaper/OUT/SpeciesPointData")


fl <- list.files("D:/MyDocs/Projects/EFA_ConceptPaper/DATA/RASTER/EFA_MULTI_YEAR", 
                 pattern=".tif$", full.names = TRUE)
fl <- fl[grepl("_MultiYr_Avg",fl)]
varNames <- gsub(".tif|_01_18_v1|_MultiYr_Avg","",basename(fl))

current <- stack(fl)

gbifData <- read_csv("D:/MyDocs/Projects/EFA_ConceptPaper/DATA/TABLES/spData/GBIF_DATA_AllSpecies-v1.csv")
colnames(gbifData)
spNames <- unique(gbifData$species)
spNames <- spNames[-c(1,8)]



for(spName in spNames){
  
  # Subset species occurrences data
  gbifData_tmp <- gbifData %>% filter(species %in% spName)
  
  spRecordsPt <- SpatialPointsDataFrame(gbifData_tmp[,c("decimalLongitude","decimalLatitude")],
                                             proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                             data = gbifData_tmp)
  
  # Remove duplicates
  spRecordsPtFilt <- getUniqueRasterXYCoords(current, spRecordsPt, spatial = TRUE)
  spRecordsPtFilt <- as(spRecordsPtFilt,"SpatialPointsDataFrame")
  
  speciesName <- gsub("\\ +","_",spName)
  spRecordsPtFilt@data <- data.frame(ID = 1:length(spRecordsPtFilt), species=speciesName)
  spRecordsPtFilt <- st_as_sf(spRecordsPtFilt)
  
  # Write data to file
  #writeOGR(spRecordsPtFilt, dsn = "./OUT/SpeciesPointData/", layer = speciesName, driver = "ESRI Shapefile")
  write_sf(spRecordsPtFilt, dsn = speciesName, layer = speciesName, driver = "ESRI Shapefile")
}


