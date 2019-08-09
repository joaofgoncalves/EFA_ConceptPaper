
library(stringr)

setwd("D:/MyDocs/Projects/EFA_ConceptPaper/DATA/RASTER/EFA_ANNUAL_v2/__RENAME")

for(yr in 2001:2017){
  
  fn_from <- list.files(getwd(),
             pattern=paste("_",yr,"_",sep=""), recursive = TRUE)
  
  fn_to <- str_replace(fn_from, 
                       paste("_",yr,"_v1_",sep=""),
                       paste("_",yr+1,"_v1a_",sep=""))
  
  file.rename(fn_from, fn_to)
  
}

fn_from <- list.files(getwd(),
                      pattern=paste("_",2002,"_",sep=""), recursive = TRUE)

fn_to <- str_replace(fn_from, 
                     paste("_2002_",sep=""),
                     paste("_2001_",sep=""))

file.copy(fn_from, fn_to)

