library(leaflet)
library(raster)
library(rgdal)
library(rnaturalearth)

bathy<-readOGR(paste0(getpath4data(),"GIS/LakeChamplain_Shapefile/LakeChamplain.shp"))
lcgroupmap=c("lake",rep("island", 84562))
bathy2 <- raster(paste0(getpath4data(),"GIS/LCbathy.tif"))

world <- ne_countries(scale = "medium", returnclass = "sf")
bathy <- spTransform(bathy,crs(world))
bathy2 <- projectRaster(bathy2, crs = crs(world))

bathy2_raster_aggregate <- aggregate(bathy2, fact=5)

# #000b70 = blue
# #159112 = green
# #fced4b = yellow
# #f23c24 = red

pal <- colorNumeric(c("#000b70", "#67c7f7", "#159112", "#fced4b", "#f23c24"), values(bathy2_raster_aggregate),
                    na.color = "transparent")

leaflet() %>% addTiles() %>% 
  addRasterImage(bathy2_raster_aggregate, colors = pal, opacity = 0.8)

raster_LC_leaflet <- projectRasterForLeaflet(bathy2_raster_aggregate, method = "ngb")

writeRaster(raster_LC_leaflet, filename = "LakeChamp_v0.3/data/raster_LC_leaflet", format = "GTiff", overwrite = TRUE)
writeRaster(bathy2_raster_aggregate, filename = "LakeChamp_v0.3/data/raster_LC", format = "GTiff", overwrite = TRUE)
