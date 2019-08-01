

library(raster)
library(rasterVis)
library(sf)
library(ggplot2)
library(viridis)
library(ggthemes)
library(dplyr)
library(rasterVis)
library(ggtern)


setwd("D:/MyDocs/Projects/EFA_ConceptPaper")

evi_avg <- raster("./DATA/RASTER/EFA_MULTI_YEAR/EVI_Avg_MultiYr_Avg_01_18_v1.tif")
#evi_dmx <- raster("./DATA/RASTER/EFA_ANNUAL_v2/EVI_Dmx/EVI_Dmx_2018_v1_20190722.tif")
evi_dmx <- raster("./DATA/RASTER/EFA_MULTI_YEAR/EVI_Wnt_MultiYr_Avg_01_18_v1.tif")
evi_std <- raster("./DATA/RASTER/EFA_MULTI_YEAR/EVI_Std_MultiYr_Avg_01_18_v1.tif")
alb_avg <- raster("./DATA/RASTER/EFA_MULTI_YEAR/Albedo_Avg_MultiYr_Avg_01_18_v1.tif")
lst_avg <- raster("./DATA/RASTER/EFA_MULTI_YEAR/LST_Avg_MultiYr_Avg_01_18_v1.tif")
et_avg <- raster("./DATA/RASTER/EFA_MULTI_YEAR/ET_Avg_MultiYr_Avg_01_18_v1.tif")


rst<-stack(evi_avg, evi_std, evi_dmx, alb_avg, lst_avg, et_avg)
writeRaster(rst,filename = "./DATA/RASTER/EFA_MULTI_YEAR/EVI_Alb_LST_ET_MultiYr.tif")

# Transforms
evi_std <- evi_std / cellStats(evi_std, stat = "max")
evi_avg <- evi_avg + (-1*cellStats(evi_avg,stat = "min"))
evi_avg <- evi_avg / cellStats(evi_avg,stat = "max")

#evi_dmx <- evi_dmx / 365


p1 <- levelplot(evi_dmx, par.settings = magmaTheme, margin=FALSE)
plot(p1)

#df <- values(stack(alb_avg,evi_avg,lst_avg)) %>% na.omit
#cor(df) %>% round(2)

df <- values(stack(lst_avg,evi_avg,et_avg)) %>% na.omit
cor(df, method="spearman") %>% round(2)

plotRGB(stack(evi_std,evi_avg,evi_dmx), scale=1, maxpixels=1E6, stretch='lin')


rstStack <- stack(lst_avg, evi_avg, alb_avg)
  
rstStack_proj <- projectRaster(rstStack,  crs="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")



png(filename = "./OUT/RGBplot_LST_EVI_Alb-lin--v3.png", width = 3000, height = 2500)
plotRGB(rstStack_proj, scale=1, maxpixels=1E6, stretch='lin')
#plotRGB(stack(alb_avg,evi_avg,lst_avg), scale=1, maxpixels=1E6, stretch='lin')
dev.off()

png(filename = "./OUT/RGBplot_LST_EVI_Alb-hist-v2.png", width = 3000, height = 2500)
plotRGB(rstStack_proj, scale=1, maxpixels=1E6, stretch='hist')
#plotRGB(stack(alb_avg,evi_avg,lst_avg), scale=1, maxpixels=1E6, stretch='hist')
dev.off()



png(filename = "./OUT/RGBplot_ET_EVI_Alb-lin-v1.png", width = 3000, height = 2500)
plotRGB(stack(lst_avg,evi_avg,et_avg), scale=1, maxpixels=1E6, stretch='lin')
dev.off()

png(filename = "./OUT/RGBplot_ET_EVI_Alb-hist-v1.png", width = 3000, height = 2500)
plotRGB(stack(lst_avg,evi_avg,et_avg), scale=1, maxpixels=1E6, stretch='hist')
dev.off()



rdf <- as.data.frame(cbind(coordinates(evi_avg), 
                           values(stack(evi_std,evi_avg,evi_dmx))))
colnames(rdf) <- c("x","y","std","avg","dmx")

ind <- complete.cases(rdf)
rdf <- rdf %>% mutate(col_hsv=NA, red=NA, green=NA, blue=NA)
rdf[ind,"col_hsv"] <- hsv(rdf$dmx[ind], rdf$avg[ind], 1)

rgb_col <- as.data.frame(t(col2rgb(rdf[ind,"col_hsv"])))
rdf[ind, c("red","green","blue")] <- rgb_col


#plot(evi_avg, col=rdf$col)
# ggplot(data=rgb_col,aes(red,green,blue)) +
#   coord_tern() +
#   geom_point(color=rdf[ind,"col_hsv"]) + 
#   labs(x="X",y="Y",z="Z",title="Title")


red <- evi_avg
values(red) <- rdf$red

green <- evi_avg
values(green) <- rdf$green

blue <- evi_avg
values(blue) <- rdf$blue

r.rgb <- stack(red, green, blue)

plotRGB(r.rgb)








