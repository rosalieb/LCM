library(leaflet)

boatIcon <- makeIcon(
  iconUrl = "https://www.materialui.co/materialIcons/maps/directions_boat_black_192x192.png",
  iconWidth = 20, iconHeight = 20)

xIcon <- makeIcon(
  iconUrl = "https://cdn4.iconfinder.com/data/icons/defaulticon/icons/png/256x256/cancel.png",
  iconWidth = 20, iconHeight = 20)

# LCMcoord = rbind(LCMcoord,
                # c("Burlington", as.numeric(-73.212074), as.numeric(44.475883)))
# str(LCMcoord)
# LCMcoord[-16,3] = as.numeric(LCMcoord[,3])+1
m <- leaflet(LCMcoord) %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(data = LCMcoord, icon = boatIcon, lat = as.numeric(LCMcoord$LLatitude), 
             lng = as.numeric(LCMcoord$LLongitude), 
             popup = LCMcoord$LStation) %>%
  addMarkers(data = LCMcoord, icon = xIcon, lat = as.numeric(LCMcoord$TLatitude), 
             lng = as.numeric(LCMcoord$TLongitude),
             popup = LCMcoord$TStation)

m  # Print the map

