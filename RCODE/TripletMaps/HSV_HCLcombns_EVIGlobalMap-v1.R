
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

rst <- stack(EVI_dmx_deg,EVI_std,EVI_avg)

rstDF <- values(rst)
ind <- complete.cases(rstDF)

#hsvCol <- hsv(h = rstDF[ind,1]/360, s = rstDF[ind,2], v = rstDF[ind,3])
hsvCol <- hcl(h = rstDF[ind,1], c = rstDF[ind,2]*100, l = rstDF[ind,3]*100)
head(hsvCol)

rgbCol <- t(col2rgb(hsvCol))
head(rgbCol)

rstDF_rgb <- rstDF
rstDF_rgb[ind, ] <- rgbCol
rstDF_rgb[!ind, ] <- NA

r <- EVI_avg
values(r) <- rstDF_rgb[,1]
g <- EVI_avg
values(g) <- rstDF_rgb[,2]
b <- EVI_avg
values(b) <- rstDF_rgb[,3]

plotRGB(stack(r,g,b))

writeRaster(stack(r,g,b), "./DATA/RASTER/RGB_EFA/RGB_EVI_Dmx_Std_Avg_HCL_v1.tif")

