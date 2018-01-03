## Startup document 

## Libraries
library(akima)
library(dismo)
library(dplyr)
library(gdalUtils)
library(ggjoy)
library(ggplot2)
library(jsonlite)
library(lattice)
library(mapdata)
library(maps)
library(maptools)
library(matrixStats)
library(MASS)
library(mosaic)
library(mosaicData)
library(randomForest)
library(raster)
library(rasterVis)
library(readr)
library(rgdal)
library(rgeos)
library(rpart)
library(rpart.plot)
library(SDMTools)
library(sp)
library(spatial.tools)
library(yaImpute)
library(zoo)

## Define map colors
temp.col <- colorRampPalette(rev(brewer.pal(5, "RdYlBu")))(1000)
min.temp = -30
max.temp = 40

vel.col <- colorRampPalette(rev(brewer.pal(11,"RdYlGn")))(1000)
min.vel <- -600
max.vel <- 300

pew.col <- "firebrick2"
tee.col <- "orange1"
mco.col <- "yellow2"
mce.col <- "springgreen4"
pwe.col <- "green2"
lgm.col <- "mediumblue"
hol.col <- "turquoise2"
pir.col <- "purple3"
cur.col <- "orchid2"
fut.col <- "black"

biome.col <- c("darkorchid","red", "yellow", "darkgreen", "darkgrey", "darkorange", "maroon1", "mediumblue", "cyan", "chartreuse1", "pink")

biome.col2 <- c("darkorchid","red", "yellow", "darkgreen", "darkgrey", "darkorange", "maroon1", "mediumblue", "cyan", "chartreuse1", "pink", "tan", "seagreen2", "black")
