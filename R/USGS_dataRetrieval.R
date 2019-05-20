# library(devtools)
# install_github("USGS-R/dataRetrieval")
library(dataRetrieval)
vignette("dataRetrieval",package = "dataRetrieval")

siteNo <- "05427718"
siteNo <- "503387" #Lake Champlain STORET for station 02
siteNo <- "04294620" #Lake Champlain near Grand Isle
siteNo <- "04294500" #Lake Champlain near Burlington According to site mapper https://maps.waterdata.usgs.gov/mapper/
siteNo <- "04150408" # site 9 according to STORET DATABASE https://www.waterqualitydata.us/portal/#within=50&lat=44.47653735690138&long=-73.22058662436186&mimeType=csv
siteNo <- "1VTDECWQ-500449" # site 07 according to STORET DATABASE https://www.waterqualitydata.us/portal/#within=50&lat=44.47653735690138&long=-73.22058662436186&mimeType=csv
siteNo <- "1VTDECWQ-503519" # site 25

pCode <- "00010" #sample data Temperature
start.date <- "1996-10-01"
end.date <- "2015-09-30"

readNWISsite(siteNo)

LC_grandIsle <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)
names(attributes(LC_grandIsle))
head(attributes(LC_grandIsle))

#Other option:
#the website https://www.waterqualitydata.us/portal/#within=50&lat=44.47653735690138&long=-73.22058662436186&siteType=Lake%2C%20Reservoir%2C%20Impoundment&organization=1VTDECWQ&siteid=1VTDECWQ-503535&siteid=1VTDECWQ-500449&providers=STORET&mimeType=csv
#gives an URL at the end, where we can add the sites we're interested in
startDate <- "01-01-1990" #a very early date
endDate   <- format(Sys.Date(), "%m-%d-%Y") #today
listSites <- paste0("1VTDECWQ-",c("503387", #02
                                  "503288", #04
                                  "500449", #07
                                  "500451", #09
                                  "503506", #16
                                  "500458", #19
                                  "500459", #21
                                  "503519", #25
                                  "500468", #33
                                  "503485", #34
                                  "500470", #36
                                  "503488", #40
                                  "503535", #46
                                  "503515", #50
                                  "500476" #51
                                  ))
url <- paste0("https://www.waterqualitydata.us/portal/#within=50&lat=44.47653735690138&long=-73.22058662436186&siteType=Lake%2C%20Reservoir%2C%20Impoundment&",
              "organization=1VTDECWQ&siteid=",
              paste(listSites,sep="", collapse = "&siteid="),
              "&startDateLo=",startDate,
              "&startDateHi=",endDate,
              "&providers=STORET&mimeType=csv&sorted=yes")
url



url <- attr(yahara, "url")
url
library(ggplot2)
ts <- ggplot(data = yahara,
             aes(dateTime, X_00010_00000)) +
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
