# library(devtools)
# install_github("USGS-R/dataRetrieval")
library(dataRetrieval)
vignette("dataRetrieval",package = "dataRetrieval")

siteNo <- "05427718"
pCode <- "00060"
start.date <- "2014-10-01"
end.date <- "2015-09-30"

yahara <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)
names(attributes(yahara))
head(attributes(yahara))

url <- attr(yahara, "url")
url
library(ggplot2)
ts <- ggplot(data = yahara,
             aes(dateTime, Flow_Inst)) +
  geom_line()
ts


pCode <- c("00662","00665")
phWI <- readNWISdata(stateCd="WI", parameterCd=pCode,
                     service="site", seriesCatalogOutput=TRUE)

library(dplyr)
phWI.1 <- filter(phWI, parm_cd %in% pCode) %>%
  filter(count_nu > 300) %>%
  mutate(period = as.Date(end_date) - as.Date(begin_date)) %>%
  filter(period > 15*365)
library(leaflet)
leaflet(data=phWI.1) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~dec_long_va,~dec_lat_va,
                   color = "red", radius=3, stroke=FALSE,
                   fillOpacity = 0.8, opacity = 0.8,
                   popup=~station_nm)
?leaflet
