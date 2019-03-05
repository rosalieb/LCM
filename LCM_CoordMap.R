library(leaflet)

boatIcon <- makeIcon(
  iconUrl = "https://www.materialui.co/materialIcons/maps/directions_boat_black_192x192.png",
  iconWidth = 20, iconHeight = 20)

# LCMcoord = rbind(LCMcoord,
                # c("Burlington", as.numeric(-73.212074), as.numeric(44.475883)))
# str(LCMcoord)
# LCMcoord[-16,3] = as.numeric(LCMcoord[,3])+1
m <- leaflet(LCMcoord) %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(icon = boatIcon, lat = as.numeric(LCMcoord$Latitude), lng = as.numeric(LCMcoord$Longitude), 
             popup = LCMcoord$Station) #, weight = 3, radius = 40
m  # Print the map
