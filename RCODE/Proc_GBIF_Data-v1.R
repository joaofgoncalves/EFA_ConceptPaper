
library(dplyr)
library(readr)

setwd("C:/Users/JG/Desktop/GBIF_Data")

fl <- list.files(pattern=".csv$")

i <- 0
for(f in fl){
  i <- i + 1
  TMP <- readr::read_delim(f, progress = TRUE,delim = "\t")
  if(i==1){
    spData <- TMP
  }else{
    spData <- rbind(spData, TMP)
  }
  
}

colnames(spData)

spDataSel <- spData %>% select("species","decimalLatitude","decimalLongitude",
                               "coordinateUncertaintyInMeters", "year" ) %>% 
  filter(year >= 2000, !is.na(decimalLatitude), !is.na(decimalLongitude))


spDataSel %>% group_by(species) %>% summarize(numrec=n())

readr::write_csv(spDataSel,"./GBIF_DATA_AllSpecies.csv")
