library(leaflet)
library(raster)

bathy2.raster.aggregate <- aggregate(bathy2, fact=8)

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(bathy2.raster.aggregate),
                    na.color = "transparent")

leaflet() %>% addTiles() %>% 
  addRasterImage(bathy2.raster.aggregate, colors = pal, opacity = 0.8)
