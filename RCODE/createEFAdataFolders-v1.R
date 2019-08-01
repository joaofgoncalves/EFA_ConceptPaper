
# Create base folders for each variable

vars <- c("Albedo","ET","EVI","LST","TCTbri","TCTgrn","TCTwet","FPAR","LAI","GPP")

inds <- c("Min","Max","Avg","Std","Spg","Wnt","Dmx")

baseFolder <- "D:/MyDocs/Projects/EFA_ConceptPaper/DATA/RASTER/EFA_ANNUAL"

for(v in vars){
  for(i in inds){
    
    fname <- paste(baseFolder,"/",paste(v,i,sep="_"),sep="")
    print(fname)
    
    if(!dir.exists(fname)){
      dir.create(fname)
    }
  }
}
