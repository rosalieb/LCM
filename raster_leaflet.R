library(leaflet)
library(raster)

colr <- colorRampPalette(brewer.pal(11, 'RdYlBu'))

leaflet() %>% addTiles() %>% 
  addRasterImage(bathy2, colors = colr, opacity = 0.8)
