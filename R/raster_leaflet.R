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

bathy2.raster.aggregate <- aggregate(bathy2, fact=5)

# #000b70 = blue
# #159112 = green
# #fced4b = yellow
# #f23c24 = red

pal <- colorNumeric(c("#000b70", "#67c7f7", "#159112", "#fced4b", "#f23c24"), values(bathy2.raster.aggregate),
                    na.color = "transparent")

leaflet() %>% addTiles() %>% 
  addRasterImage(bathy2.raster.aggregate, colors = pal, opacity = 0.8)

raster.LC.leaflet <- projectRasterForLeaflet(bathy2.raster.aggregate, method = "ngb")

writeRaster(raster.LC.leaflet, filename = "raster.LC.leaflet", format = "GTiff", overwrite = TRUE)
