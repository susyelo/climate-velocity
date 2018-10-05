# libraries ---------------------------------------------------------------
library(raster)
library(rgdal)
library(rgeos)



# data --------------------------------------------------------------------
# Read bioclim variables 
setwd("./data/Spatial_data/Bioclim_v2/")
brk <- do.call(brick, lapply(list.files(path = "./", pattern = "*tif"), raster))
setwd("../../../")


# ****************************************
# 1. Olson and Geiger classification ----
# *****************************************

# Biome polygons
olson_biomes <- shapefile("./data/Spatial_data/wwf_olson/tnc_terr_ecoregions.shp")

# Crop the new world ------------------------------------------------------
inx<-which(olson_biomes$WWF_REALM2=="Nearctic" | olson_biomes$WWF_REALM2=="Neotropic")
biome_NW<-olson_biomes[inx,]


# 1.2 Aggregating biomes levels  -------------------------
# Ensure shapefile row.names and polygon IDs are sensible
row.names(biome_NW) <- row.names(biome_NW@data)
biome_NW <- spChFIDs(biome_NW, row.names(biome_NW))

# Create new variable with the names we are using in BIEN 
biome_NW$biomes<-biome_NW$WWF_MHTNAM
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Temperate Broadleaf and Mixed Forests")]<-"Cold"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Tropical and Subtropical Coniferous Forests")]<-"Dry"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Temperate Grasslands, Savannas and Shrublands")]<-"Cold"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Tropical and Subtropical Dry Broadleaf Forests")]<-"Dry"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Deserts and Xeric Shrublands")]<-"Xeric"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Tropical and Subtropical Moist Broadleaf Forests")]<-"Moist"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Flooded Grasslands and Savannas")]<-"Dry"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Tropical and Subtropical Grasslands, Savannas and Shrublands")]<-"Dry"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Montane Grasslands and Shrublands")]<-"Cold"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Mediterranean Forests, Woodlands and Scrub")]<-"Dry"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Temperate Conifer Forests")]<-"Cold"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Boreal Forests/Taiga")]<-"Cold"
biome_NW$biomes[which(biome_NW$WWF_MHTNAM=="Tundra")]<-"Cold"

## Change Chaco and Caatinga classification
biome_NW$biomes[grep("Caatinga",biome_NW$ECO_NAME)]<-"Dry"
biome_NW$biomes[grep("Chaco",biome_NW$ECO_NAME)]<-"Xeric"


# Extract the data I want (the larger geography)
biome_poly <- gUnaryUnion(biome_NW, id = biome_NW@data$biomes)

# And add the data back in
lu<-row.names(biome_poly)
lu <- as.data.frame(lu)
colnames(lu) <- "biomes" 
row.names(lu)<-lu$biomes

biome_poly <- SpatialPolygonsDataFrame(biome_poly, lu)

# Filter some biomes ------------------------------------------------------
exclude_biomes<-c("Inland Water","Rock and Ice","Mangroves")
biome_poly_sub<-biome_poly[which(biome_poly$biomes%in%exclude_biomes==FALSE),]
biome_poly_sub$biomes<-droplevels(biome_poly_sub$biomes)

spplot(biome_poly_sub)

# Crop raster and polygons -------------------------------------------------------
e <- extent(-175,-22,-56,74)

biome_poly<-crop(biome_poly_sub,e)
bioclim_ly<- do.call(crop, c(brk,e))

names(bioclim_ly)<-paste("bio",1:19,sep="")

# Rasterize polygon -------------------------------------------------------
## Rasterize Biomes shapefile 100 km^2
r_ref <- bioclim_ly$bio1
r_ref[] <- 1:ncell(r_ref)

## Rasterize the biomes
# https://cran.r-project.org/web/packages/fasterize/vignettes/using-fasterize.html
biomes_ras <- rasterize(biome_poly, r_ref)

r.pts <- as.data.frame(rasterToPoints(biomes_ras))
bioclim_pts <- lapply(as.list(bioclim_ly), rasterToPoints)
names(bioclim_pts)<-names(bioclim_ly)


# Combining all together --------------------------------------------------
bio_clim_df <- Reduce(function(x, y) merge(x, y, all=TRUE), bioclim_pts)
Biomes_clim_df<-merge(bio_clim_df,r.pts)

colnames(Biomes_clim_df)[22]<-"Biomes"


# Writing csv file --------------------------------------------------------
saveRDS(Biomes_clim_df,"./data/01_Biome_class.rds")

# ****************************************
# 2. Koppen and Geiger classification ----
# *****************************************
Koppen_Geiger <- shapefile("./data/Spatial_data/1976-2000_GIS/1976-2000.shp")

# Crop the new world ------------------------------------------------------
e <- extent(-175,-22,-56,74)
KP_NW<-crop(Koppen_Geiger, e)

clim_class <- read.csv("data/Spatial_data/1976-2000_GIS/Clim_class.csv")
KP_NW$GRIDCODE <- clim_class$Class[as.factor(KP_NW$GRIDCODE)]


## 2.1 Aggregating climatic levels ----
KP_NW$GRIDCODE <- as.character(KP_NW$GRIDCODE)

## Wet forest
clim_get1 <- "Af|Am"
KP_NW$GRIDCODE[grep(clim_get1,KP_NW$GRIDCODE)] <- "AWet"

## Dry forest
clim_get2 <- "As|Aw"
KP_NW$GRIDCODE[grep(clim_get2,KP_NW$GRIDCODE)] <- "ADry"

# Xeric 
KP_NW$GRIDCODE[grep("B",KP_NW$GRIDCODE)] <- "B"

# Temperate and Cold
clim_get3 <- "^C|^D"
KP_NW$GRIDCODE[grep(clim_get3,KP_NW$GRIDCODE)] <- "CD"



# Extract the data I want (the larger geography)
KP_poly <- gUnaryUnion(KP_NW, id = KP_NW@data$GRIDCODE)

# And add the data back in
lu<-row.names(KP_poly)
lu <- as.data.frame(lu)
colnames(lu) <- "KP_class" 
row.names(lu)<-lu$KP_class

KP_poly <- SpatialPolygonsDataFrame(KP_poly, lu)
spplot(KP_poly)

