
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

setwd("D:/MyDocs/Projects/EFA_ConceptPaper/OUT/MODS/R0")

gbifData <- read_csv("D:/MyDocs/Projects/EFA_ConceptPaper/DATA/TABLES/spData/GBIF_DATA_AllSpecies-v1.csv")

colnames(gbifData)

spNames <- unique(gbifData$species)
spNames <- spNames[-c(1,2,8)]

fl <- list.files("D:/MyDocs/Projects/EFA_ConceptPaper/DATA/RASTER/EFA_MULTI_YEAR", pattern=".tif$", full.names = TRUE)
fl <- fl[grepl("_MultiYr_Avg",fl)]

varNames <- gsub(".tif|_01_18_v1|_MultiYr_Avg","",basename(fl))

current <- stack(fl)
names(current) <- varNames

plot(current[[1]])


gbifData %>% group_by(species) %>% summarize(nsp=n()) %>% arrange(desc(nsp))


for(i in 1:length(spNames)){
  
  spName <- spNames[i]
  sp <- gsub("\\ +",".",spName)
  
  spRecords <- gbifData %>% filter(species == spName)
  
  spRecordsPt <- SpatialPointsDataFrame(spRecords[,c("decimalLongitude","decimalLatitude")],
                                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                        data = spRecords)
  
  # x1 <- min(spRecords[,"decimalLongitude"])
  # x2 <- max(spRecords[,"decimalLongitude"])
  # y1 <- min(spRecords[,"decimalLatitude"])
  # y2 <- max(spRecords[,"decimalLatitude"])
  # 
  # ext <- SpatialPolygons(list(Polygons(
  #   list(Polygon(coords = list(matrix(c(x1,y1, x1,y2, x2,y2, x2,y1, x1,y1), 
  #                                     nrow = 5, ncol = 2, byrow = TRUE)))),
  #   ID = 1)))
  # plot(ext, add=TRUE)
  
  # Remove duplicates
  spRecordsPtFilt <- getUniqueRasterXYCoords(current, spRecordsPt, spatial = TRUE)
  
  Np <- length(spRecordsPtFilt)
  
  
  # Run biomod2 data formatting
  myBiomodData <- BIOMOD_FormatingData(resp.var = spRecordsPtFilt,
                                       expl.var = current,
                                       resp.name = spName,
                                       PA.nb.rep = 5,
                                       PA.nb.absences = biomod2plus:::powerFunPAnumberCalc(Np),
                                       PA.strategy = 'random')
  
  myBiomodOptions <- BIOMOD_ModelingOptions(GAM = list(k = 4),
                                            MAXENT.Phillips = list(threshold=FALSE,
                                                                   hinge=FALSE,
                                                                   path_to_maxent.jar="D:/MyDocs/temp"),
                                            GBM = list(n.trees = 1000))
  
  
  ## -------------------------------------------------------------------------------------- ##
  ## Calibrate models ----
  ## -------------------------------------------------------------------------------------- ##
  #
  myBiomodModelOut <- BIOMOD_Modeling(
    myBiomodData,
    # models = c('GLM','GAM','CTA','ANN', 'GBM',
    #            'FDA','MARS','RF','MAXENT.Phillips', 
    #            'MAXENT.Tsuruoka'),
    models = "RF",
    models.options = myBiomodOptions,
    NbRunEval = 20,
    DataSplit = 70,
    Prevalence = 0.5,
    VarImport = 5,
    models.eval.meth = c('TSS','ROC','KAPPA'),
    SaveObj = TRUE,
    rescal.all.models = FALSE,
    do.full.models = FALSE)
  
  
  # Get model evaluation values
  myBiomodModelEval <- get_evaluations(myBiomodModelOut)
  
  # Print ROC scores
  print(myBiomodModelEval["ROC","Testing.data",,,])
  
  # Get boxplot stats
  print(fivenum(as.numeric(myBiomodModelEval["ROC","Testing.data",,,])))
  
  # Save evaluation metrics from the arrays
  evalDF.ROC <- as.data.frame(myBiomodModelEval["ROC","Testing.data",,,])
  evalDF.TSS <- as.data.frame(myBiomodModelEval["TSS","Testing.data",,,])
  evalDF.KAPPA <- as.data.frame(myBiomodModelEval["KAPPA","Testing.data",,,])
  
  write.csv(evalDF.ROC, file = paste(getwd(),"/",sp,"/",sp,"_evalDF_ROC.csv",sep=""))
  write.csv(evalDF.TSS, file = paste(getwd(),"/",sp,"/",sp,"_evalDF_TSS.csv",sep=""))
  write.csv(evalDF.KAPPA, file = paste(getwd(),"/",sp,"/",sp,"_evalDF_KAPPA.csv",sep=""))
  
  
  varImportance <- get_variables_importance(myBiomodModelOut)
  varImportanceByVariableAVG <- apply(varImportance,1, mean, na.rm=TRUE)
  varImportanceByVariableSTD <- apply(varImportance,1, sd, na.rm=TRUE)
  vimpDF <- data.frame(cnames=names(varImportanceByVariableAVG),
                       vimpAVG = varImportanceByVariableAVG, 
                       varImpSTD=varImportanceByVariableSTD) %>% 
    arrange(desc(vimpAVG))
  
  write.csv(vimpDF, file = paste(getwd(),"/",sp,"/",sp,"_varImportance.csv",sep=""))
  
  
  ## -------------------------------------------------------------------------------------- ##
  ## Perform ensemble modelling ----
  ## -------------------------------------------------------------------------------------- ##
  
  # Ensemble all partial models
  # selMods <- twoStepBestModelSelection(myBiomodModelOut, 
  #                                      evalMetric = "TSS", 
  #                                      nrBestAlgos = 7, 
  #                                      bestAlgoFun = stats::median, 
  #                                      topFraction = 0.10)
  # 
  # myBiomodEM <- BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut,
  #                                       #chosen.models = 'all',
  #                                       #chosen.models = selMods,
  #                                       em.by = 'all',
  #                                       #eval.metric = c('TSS'),
  #                                       #eval.metric.quality.threshold = quantileThresh,
  #                                       prob.mean = TRUE,
  #                                       prob.cv = FALSE,
  #                                       prob.ci = FALSE,
  #                                       prob.ci.alpha = 0.05,
  #                                       prob.median = FALSE,
  #                                       committee.averaging = FALSE,
  #                                       prob.mean.weight = FALSE,
  #                                       prob.mean.weight.decay = 'proportional')
  # 
  # 
  # # Get evaluation scores for the Ensemble Modelling stage
  # emEvalDF <- as.data.frame(get_evaluations(myBiomodEM))
  # write.csv(emEvalDF, file = paste(getwd(),"/",sp,"/",sp,"_EnsMod_evalDF_AllMetrics.csv",sep=""))
  # 
}


## -------------------------------------------------------------------------------------- ##
## Obtain spatiotemporal projections ----
## -------------------------------------------------------------------------------------- ##

# # Models to consider in the ensemble and projection
# modelsToUse <- get_kept_models(myBiomodEM, 1)
# 
# projName <- 'present'
# 
# # Obtain spatiotemporal projections
# myBiomodProj <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
#                                   new.env = calRstStack,
#                                   proj.name = projName, ## Name of the projection from above variable proj.name
#                                   selected.models = modelsToUse,
#                                   filtered.meth = NULL,
#                                   binary.meth = NULL,
#                                   compress = 'gzip',
#                                   clamping.mask = TRUE,
#                                   output.format = '.grd',
#                                   do.stack = TRUE)
# 
# 
# # Perform the ensembling of projections
# myBiomodEF <- BIOMOD_EnsembleForecasting(projection.output = myBiomodProj,
#                                          binary.meth = c('TSS','ROC','KAPPA'),
#                                          EM.output = myBiomodEM,
#                                          output.format = '.grd')
# 
# # Convert all output raster files to GeoTIFF
# inFolder <- paste(getwd(),"/",sp,"/proj_",projName,sep="")
# outFolder <- paste(inFolder,"/","GeoTIFF", sep="")
# dir.create(outFolder)
# 
# convertToGeoTIFF(inFolder, outFolder)
# 
# save.image(file=paste(sp,"ModObjects.RData",sep="_"))


