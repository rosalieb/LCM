#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Toggle switch between datasets dt_annual and dt_day?

# dt = data; df = data frame; pl = plot; map = maps; prm = parameters; txt = text/paragraphs

library(shiny)
library(ggplot2)
library(gridExtra)
library(DT)
library(yaml)
library(shinydashboard)
library(leaflet)
# names(tags) # list of tags recognized by shiny.
library(htmlTable)
library(shinyWidgets)
library(raster) # to read the raster layer
library(timevis)
library(RColorBrewer)

# Introducing the dataframe dt_annual, which contains annual averages of the data we're working with.
dt_annual <- total_year
dt_annual$year <- as.numeric(substring(dt_annual[,1],1,4))
str(dt_annual$VisitDate)
dt_annual <- dt_annual[order(dt_annual$year,decreasing = F),]
dt_annual$VisitDate <- paste0(dt_annual$VisitDate,"-01-01") # I'm adding this because we need a format Date for the plots
#dt_annual <- round(dt_annual, digits = 2)

# Introducing the dataframe dt_day, which contains the original data collected weekly. 
dt_day <- total
dt_day$year <- as.numeric(substring(dt_day[,1],1,4))
str(dt_day$VisitDate)
dt_day <- dt_day[order(dt_day$year,decreasing = F),]
dt_day$VisitDate <- paste0(as.Date(dt_day$VisitDate))
#dt_day <- round(dt_day, digits = 2)

# Cleaning up the datasets some here for later on. 
dt_annual$StationID[dt_annual$Station == "2"] <- "02"
dt_annual$StationID[dt_annual$Station == "4"] <- "04"
dt_annual$StationID[dt_annual$Station == "7"] <- "07"
dt_annual$StationID[dt_annual$Station == "9"] <- "09"

dt_day$StationID[dt_day$Station == "2"] <- "02"
dt_day$StationID[dt_day$Station == "4"] <- "04"
dt_day$StationID[dt_day$Station == "7"] <- "07"
dt_day$StationID[dt_day$Station == "9"] <- "09"

# Create an overall dataset that will later be subset
dt_all <- rbind(cbind(dt_annual,"type"=1),cbind(dt_day,"type"=2))

# Creating a subset of the data so we are only focusing on particular stations in our analyses. 
stations_metadata <- read.delim(paste0(getpath4data(),"/LCM_bio_PeteStangel/Plankton data stations.txt"))
lake_stations_subset <- stations_metadata[stations_metadata$StationID %in% dt_annual$StationID,]
trib_stations_subset <- stations_metadata[stations_metadata$StationID %in% LCMcoord$TStation,]
stations_metadata_subset <- rbind(lake_stations_subset, trib_stations_subset)

# Creating the X icon for the map here because there isn't a good X glyphicon.
xIcon <- makeIcon(
  iconUrl = "https://cdn4.iconfinder.com/data/icons/defaulticon/icons/png/256x256/cancel.png",
  iconWidth = 20, iconHeight = 20)

# Read the raster layer for Lake Champlain bathymetry 
# raster_LC <- raster("LakeChamp_v0.3/data/raster_LC.tif")
# raster_LC_leaflet <- projectRasterForLeaflet(raster_LC, method = "ngb")

# Reading in the parameter datafile to be placed in the "Parameter descriptions" tab. 
parameters_info <- read.csv(paste0(getpath4data(),"LCM Parameter Descriptions.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")

# tweaks, a list object to set up multicols for checkboxGroupInput
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                 height: 150px;
                                 -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 5;    /* Firefox */ 
                                 column-count: 5; 
                                 -moz-column-fill: auto;
                                 -column-fill: auto;
                                 } 
                                 ")) 
  ))

# Creating an object in hopes that it will be useful for being able to switch between the daily and annual datasets...
annual_or_daily <- div(style="display: inline-block;vertical-align:top; width: 150px;", radioButtons(
  inputId = "data_toggle", label = "Annual or daily data", choices = list("Annual data" = 1, "Daily data" = 2)
))

#dt_out <- ifelse(data_toggle_plots == 1, dt_annual, dt_day)
dt_out <- dt_day

# Timeline data frame
timeline_data <- data.frame(
  content = c("65 million rainbow smelt stocked in Lake Champlain", "Lake trout populations disappeared", "Lake whitefish fishery closes after annual harvests of 60k fish", "Alewife first appear in LC basin in Green Pond, NY",
              "Summer water clarity in Main Lake increases by over a meter", "August surface temperatures increase by 1.6-3.8 C", "Salmonid stocking program begins", "Comprehensive fish inventory of Lake Champlain conducted", 
              "Canadian commercial fishery for walleye closes", "Lake Champlain Fish and Wildlife Management Cooperative organized", "Harvest of lake whitefish in Missisquoi Bay decreases from 13,214 kg to 35 kg",
              "Sustained stocking program begins for reestablishment of lake trout fishery", "Strategic Plan for the Development of Salmonid Fisheries in Lake Champlain implemented", "Reduction in daily creel limit in Vermont",
              "Walleye stocking efforts first initiated", "Beginning of the expermental program to control sea lamprey", "Lake Champlain Basin Program created", "8-year forage fish evaluation", "Zebra mussels first discovered in Lake Champlain",
              "Alewife first appeared in Lake St. Catharine, VT", "Long-term control program for sea lamprey created", "Commercial fishery for american eel repealed", "Use of bait fish in Vermont restricted to 16 native species",
              "Alewife found in Missisquoi Bay", "Alewife first collected", "Commercial fishery for lake whitefish in Quebec ends", "Lake-wide hydroacoustic survey added", "Zebra mussel veliger monitoring for Main Lake, Cumberland Bay, and Isle La Motte ends",
              "Alewife become abundant in catches", "Major die-off of YOY alewife in Inland Sea and South Lake in late winter months"),
  start = c("1900", "1900", "1912", "1960", "1964", "1964", "1970", "1971", "1971", "1972", "1972", "1973", "1977", "1978", "1986", "1990", "1990", "1990", "1993", "1997", "2002",
            "2002", "2002", "2002", "2003", "2004", "2005", "2005", "2007", "2008"),
  end = c(NA, NA, NA, NA, "2019", "2019", NA, "1977", NA, NA, "2004", NA, NA, NA, NA, NA, NA, "1998", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  group = c("Stocking", "Fish population dynamics", "Regulations", "Fish population dynamics", "Notable changes in chemical and biological parameters", "Notable changes in chemical and biological parameters",
            "Groups/committees/programs", "Groups/committees/programs", "Regulations", "Groups/committees/programs", "Fish population dynamics", "Groups/committees/programs", "Groups/committees/programs",
            "Regulations", "Stocking", "Groups/committees/programs", "Groups/committees/programs", "Groups/committees/programs", "Species invasions", "Species invasions", "Groups/committees/programs", "Regulations",
            "Regulations", "Species invasions", "Fish population dynamics", "Regulations", "Groups/committees/programs", "Groups/committees/programs", "Fish population dynamics", "Fish population dynamics"),
  groups = data.frame(id = "timeline", content = c("Stocking", "Fish population dynamics", "Regulations", "Species invasions", "Groups/committees/programs", "Notable changes in chemical and biological parameters"))
) 

# Define UI for slider demo app ----
ui <- dashboardPage(
  #define color
  skin = "black",
  # App title ----
  #embedment of logo is not working:
  dashboardHeader(tags$li(class = "dropdown", div(style="display: inline-block; vertical-align:middle; width:60px;",
                                                  actionBttn("home", "", icon("home"), 
                                                             size = "lg", style = "gradient",
                                                             color = "success"))),
                  tags$li(class = "dropdown", tags$a(href="https://www.uvm.edu/rsenr/rubensteinlab", 
                                                     icon("desktop"))),
                  tags$li(class = "dropdown", tags$a(href="https://twitter.com/RosalieBruel", 
                                                     icon("twitter"))),
                  tags$li(class = "dropdown", tags$a(href="https://rosalieb.github.io/rosaliebruelweb/index.html",
                                                     icon("user"))),
                  tags$li(class = "dropdown", tags$a(href="https://www.instagram.com/rubensteinlaboratory/",
                                                     icon("instagram")))
  ),
  
  # Sidebar layout with input and output definitions ----
  dashboardSidebar(
    #you can edit the width of the sidebar here
    #width = 200,
    sidebarMenu(
      id = "main_sidebar",
      menuItem(
        "About this project", 
        tabName = "home", 
        icon = icon("question")
      ),
      menuItem(
        "Parameter descriptions", 
        tabName = "parameters_info", 
        icon = icon("list-alt")
      ),
      menuItem(
        "Maps", 
        tabName = "m_lake", 
        icon = icon("map"), #icon("globe"),
        menuSubItem("General map", tabName = "m_lake", icon = icon("globe")),
        menuSubItem("Density map", tabName = "d_dens_map", icon = icon("map-marker"))
      ),
      menuItem(
        "Data", 
        tabName = "data", 
        icon = icon("bar-chart"),
        menuSubItem("Charts", tabName = "d_chart", icon = icon("line-chart")),
        menuSubItem("Table", tabName = "d_table", icon = icon("table")),
        menuSubItem("Stats", tabName = "d_stats", icon = icon("percent")),
        prettyRadioButtons(inputId = "data_toggle",
                           label = "Annual or daily dataset", choices = list("Annual data" = 1, "Daily data" = 2),
                           inline = T),
        sliderInput("range", "Years selected",
                    min = min(dt_out$year,na.rm=FALSE), max = max(dt_out$year,na.rm=FALSE),
                    value = c(2000,2012),sep = "")),
      menuItem(
        "Historic data/information", 
        tabName = "history", 
        icon = icon("book")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        #Render an output text
        htmlOutput("mytext1")
      ),
      tabItem(
        tabName = "m_lake",
        htmlOutput("mymap1help"),
        leafletOutput("mymap1", height = 900, width = 660)
      ),
      tabItem(
        tabName = "parameters_info",
        box(
          title = "Chemical and biological parameters",
          width = "100%",
          height = "100%",
          collapsible = TRUE,
          htmlOutput("parameter_table_help"),
          DT::dataTableOutput("parameters_table")
        )
      ),
      tabItem(
        tabName = "d_chart",
        title = "Instruction: Select data to plot",
        box(
          title = "Instructions",
          width = "100%", 
          height = "100%",
          # Render an output text
          htmlOutput("Instructions_plot_1"),
          # Checkbox for parameters to plot
          div(style="display: inline-block;vertical-align:top; width: 150px;", dropdownButton(
            label = "Parameters to plot", status = "default", width = 80, circle = FALSE,
            div(style='max-height: 40vh; overflow-y: auto;', checkboxGroupInput("parameters_toshow", "Check any parameters:",
                                                                                colnames(dt_out))))),
          # Checkbox for stations to plot
          div(style="display: inline-block;vertical-align:top; width: 150px;", dropdownButton(
            label = "Stations to plot", status = "default", width = 80, circle = FALSE,
            div(style='max-height: 40vh; overflow-y: auto;', checkboxGroupInput("stations_toshow", "Check any stations:",
                                                                                sort(unique(dt_out$StationID)), selected = sort(unique(dt_out$StationID)), inline = FALSE))))
        ),
        box(
          title = "Dot charts",
          collapsible = TRUE,
          width = "100%",
          height = "100%",
          plotOutput("myplot1")
        ),
        box(
          title = "Line charts",
          collapsible = TRUE,
          width = "100%",
          height = "100%",
          plotOutput("myplot2")
        ),
        box(
          title = "Box plots",
          collapsible = TRUE,
          width = "100%",
          height = "100%",
          htmlOutput("info_plot_3"),
          plotOutput("myplot3")
        )
      ),
      tabItem(
        tabName = "d_table",
        htmlOutput("mytable1help"),
        DT::dataTableOutput("mytable1")
      ),
      tabItem(
        tabName = "d_stats",
        tabsetPanel(type = "tabs", 
                    tabPanel("Correlation",
                             # All htmlOutput is written toward the end of the script
                             htmlOutput("Stats1.1"), 
                             htmlOutput("Stats1.2"),
                             # Need a different parameter dropdown menu
                             div(style="display: inline-block;vertical-align:top; width: 150px;", 
                                 dropdownButton(label = "Parameters to plot", status = "default", width = 80, circle = FALSE, 
                                                checkboxGroupInput("parameters_toshow2", "Check any boxes:", colnames(dt_out)))),
                             htmlOutput("Stats1.3"),
                             sidebarPanel(sliderInput("size_slider", label = "Image size", min = 200, max = 700, value = 200)), uiOutput("corr_plot")),
                    tabPanel("Basic stats", htmlOutput("Stats2.1"), 
                             div(style="display: inline-block;vertical-align:top; width: 150px;", dropdownButton(
                               label = "Parameters to plot", status = "default", width = 80, circle = FALSE, 
                               checkboxGroupInput("parameters_toshow3", "Check any boxes:", colnames(dt_out)))),
                             div(style="display: inline-block;vertical-align:top; width: 150px;", dropdownButton(
                               label = "Stations to plot", status = "default", width = 80, circle = FALSE, 
                               checkboxGroupInput("stations_toshow2", "Check any boxes:", sort(unique(dt_out$StationID)), selected = sort(unique(dt_out$StationID)), inline = FALSE))),
                             htmlOutput("Stats2.2")))
      ),
      tabItem(
        tabName = "d_dens_map",
        htmlOutput("mymap2help"),
        div(style="display: inline-block;vertical-align:top; width: 200px;", dropdownButton(
          label = "Parameters to plot", status = "default", width = 80, circle = FALSE,
          div(style='max-height: 40vh; overflow-y: auto;', radioButtons("parameters_toshow4", "Check any parameters:", colnames(dt_out[,3:37]), selected = "Total Phosphorus")))),
        div(style="display: inline-block;vertical-align:top; width: 400px;", sliderInput("range_map", "Years selected",
                    min = min(dt_out$year,na.rm=FALSE), max = max(dt_out$year,na.rm=FALSE),
                    value = c(2000,2012),sep = "")), 
        htmlOutput("mymap2break"),
        leafletOutput("mymap2", height = 900, width = 660)
      ),
      
      tabItem(
        tabName = "history",
        #Render an output text
        htmlOutput("timeline_text"),
        checkboxGroupInput("timeline_checkbox", "Check which groups to display:", sort(unique(timeline_data$group)), selected = c("Fish population dynamics", "Groups/committees/programs"), inline = FALSE),
        timevisOutput("timeline")
      )
    )
  )
)

# Server ####
server <- function(input, output, session) {
  
  # Text for the home page
  output$mytext1 <- renderUI({ 
    header <- "<h2>Lake Champlain</h2>"
    myparagraph1 <- "Spanning a length of 190 km from Whitehall, NY to its outlet at the Richelieu River in Québec, Canada, Lake Champlain covers 113,000 hectares and is estimated to hold roughly 25 trillion liters. Average lake depth is 19.5 meters (64.5 feet), with the greatest lake depth of 122 meters (400 feet). A majority of the water that enters Lake Champlain runs through its basin, which covers over 21,000 square kilometers. Over half of the basin is in Vermont, about a third in New York, and less than a tenth in the Province of Québec. The water retention time varies by lake segment, ranging between two months in the South Lake to about 3 years in the Main Lake. <br/> <br/>"
    header2 <- "<h3> Lake Champlain Long-Term Water Quality and Biological Monitoring Program (LTMP) </h3>"
    myparagraph2 <- "Data have been collected through the Lake Champlain Long-Term Water Quality and Biological Monitoring Program since 1992, with one of the original purposes to provide hydrodynamic, eutrophication, and food web models for the lake, although their purpose changed in 1995 (Vermont Department of Environmental Conservation, & New York State Department of Environmental Conservation, 2017). Data were collected by the Vermont Department of Environmental Conservation and the New York State Department of Environmental Conservation, with the support of the Lake Champlain Basin Program. Fifteen lake and twenty-two tributary stations have been sampled throughout the years, although some biological and chemical measurements were only collected during more recent years along with the acquisition of newer equipment. Data are stored in a database and available on request for research, management, consulting, and learning purposes. <br/> <br/>"
    linkToSite <- "To visit the Lake Champlain Basin Program's website, click <a href = 'http://www.lcbp.org/'>here</a>. <br/>"
    linkToData <- "To retrieve the data on Lake Champlain used for this project, click <a href = 'https://dec.vermont.gov/watershed/lakes-ponds/monitor/lake-champlain'>here</a>.</br></br>"
    final_note <- "<u><b>Please note that this app is a permanent work-in-progress.</u></b>"
    HTML(paste(header, myparagraph1, header2, myparagraph2, linkToSite, linkToData, final_note))
  })
  
  # Timeline
  output$timeline <- renderTimevis({
    timevis(timeline_data[timeline_data$group %in% input$timeline_checkbox,], options = list(height = "600px")) %>%
      setWindow("timeline", start = "1970", end = "2019") 
  })
  
  # Timeline text
  output$timeline_text <- renderUI({
    timeline_title <- paste0("<h3>Important recent events occuring in and around Lake Champlain</h3>")
    timeline_instructions <- paste0("You can expand the app window, zoom in and out, and drag the timeline to get a better view of important events that involve the health of Lake Champlain. Important dates as long ago as 1900 and as recent as 2008 are included in the timeline. <b>Please note that this timeline, like the rest of the app, is a permanent work-in-progress and we're gradually adding information.</b></br></br>")
    HTML(paste(timeline_title, timeline_instructions))
  })
  
  # Brings user back to home page when they click the 'home' icon
  observeEvent(input$home, {
    updateTabItems(session, "main_sidebar", "home")
  })
  
  # Parameters table 
  output$parameters_table = DT::renderDataTable({
    DT::datatable(parameters_info, options = list(lengthMenu = c(50, 100)))
  })
  
  # Just giving some instructions for the data tab
  output$Instructions_plot_1 <- renderUI({ 
    Instruction1 <- paste0("1. Select which dataset you'd like to view data for (annual averages or daily data). <br/> 2. Select below which parameters to plot (you need to select at least one). <br/> 3. Choose the period for which you want to visualize the data on the left sidebar using the slider option. Currently, data are displayed for the ", input$range[1],"-",input$range[2]," period. <br/> 4. Lastly, select the stations you want to see the data for using the dropdown menu below.<br/><br/>")
    HTML(paste(Instruction1))
  })
  
  # This may not be necessary anymore once we incorporate the daily dataset.
  output$info_plot_3 <- renderUI({ 
    info3 <- paste0("Values for each parameter are averages for the date range selected with the slider. You can select which stations to view with the selection boxes at the top of this page.")
    HTML(paste(info3))
  })
  
  # Info for chemical and biological parameter table
  output$parameter_table_help <- renderUI({ 
    p_table_help <- paste0("Here, you'll find more information on the chemical and biological parameters collected through the Lake Champlain Long-Term Monitoring Project. To organize and quickly find parameters, they are separated into categories. <br/><br/>")
    HTML(paste(p_table_help))
  })
  
  # Raw data table text - annual and daily averages data
  output$mytable1help <- renderUI({ 
    table1help <- paste0("<h3>", ifelse(input$data_toggle == 1, "Annual", "Daily"), " averages data</h3> <br/> For more information on the parameters in this table, visit the parameters tab on the left side. There, you'll see the units they were measured in, a description of the overall importance of the parameter, as well as the date of availability for the data. <br/><br/>")
    HTML(paste(table1help))
  })
  
  # Text for map with monitoring site locations (not density map)
  output$mymap1help <- renderUI({ 
    map1title <- paste0("<h3> Lake Champlain monitoring sites </h3>")
    map1help <- paste0("In the map below, you'll see the lake and tributary stations where data were collected. X's indicate the location of a tributary station while the circles represent the location of a lake station. If you click on the icons, you'll be able to see the name of the station, the station ID, the latitude and longitude of the station, as well as the station depth.<br/><br/>")
    HTML(paste(map1title, map1help))
  })
  
  # Map with monitoring sites
  output$mymap1 <- renderLeaflet({
    leaflet(stations_metadata_subset) %>% 
      addTiles() %>% # Add default OpenStreetMap map tiles
      # addRasterImage(raster_LC_leaflet, colors = pal, opacity = 0.8) %>% 
      addCircleMarkers(color = "black", opacity = 1, weight = 4, fillOpacity = 0, radius = 5, data = stations_metadata_subset, lat = as.numeric(stations_metadata_subset$Latitude[stations_metadata_subset$WaterbodyType == "Lake"]), 
                       lng = as.numeric(stations_metadata_subset$Longitude[stations_metadata_subset$WaterbodyType == "Lake"]), 
                       popup = paste0("<b>Station name: </b>", stations_metadata_subset$StationName[stations_metadata_subset$WaterbodyType == "Lake"], "</br><b> StationID: </b>", stations_metadata_subset$StationID[stations_metadata_subset$WaterbodyType == "Lake"], 
                                      "</br><b> Latitude: </b>", stations_metadata_subset$Latitude[stations_metadata_subset$WaterbodyType == "Lake"], "</br><b> Longitude: </b>", stations_metadata_subset$Longitude[stations_metadata_subset$WaterbodyType == "Lake"],
                                      "</br><b> Station depth: </b>", as.numeric(stations_metadata_subset$StationDepth[stations_metadata_subset$WaterbodyType == "Lake"]), " meters")) %>%
      addMarkers(data = stations_metadata_subset, icon = xIcon, lat = as.numeric(stations_metadata_subset$Latitude[stations_metadata_subset$WaterbodyType == "Trib"]), 
                 lng = as.numeric(stations_metadata_subset$Longitude[stations_metadata_subset$WaterbodyType == "Trib"]),
                 popup = paste0("<b>Station name: </b>", stations_metadata_subset$StationName[stations_metadata_subset$WaterbodyType == "Trib"], "</br><b> StationID: </b>", stations_metadata_subset$StationID[stations_metadata_subset$WaterbodyType == "Trib"], 
                                "</br><b> Latitude: </b>", stations_metadata_subset$Latitude[stations_metadata_subset$WaterbodyType == "Trib"], "</br><b> Longitude: </b>", stations_metadata_subset$Longitude[stations_metadata_subset$WaterbodyType == "Trib"]))
  })
  
  # Text for density map 
  output$mymap2help <- renderUI({
    map2title <- paste0("<h3> Density map of the parameters by station </h3>")
    map2help <- paste0("In the dropdown menu below, you'll see the chemical and biological parameters collected. Select a parameter to visualize each station's value for that parameter relative to one another. Please note: if you receive an error that says, 'wasn't able to determine range of domain', please widen your time range. Average values for that parameter within that time range don't exist. Click on each station to view the data value for which the color represents.</br></br>")
    HTML(paste(map2title, map2help))
  })
  
  # Text for output visualization 
  output$mymap2break <- renderUI ({
    map2breaks <- paste0("<h4> You've selected to plot <b>", input$parameters_toshow4, "</b> for the years <b>", input$range_map[1], "</b> through <b>", input$range_map[2], "</b>. </h4> <i> Data for <b>", input$parameters_toshow4, "</b> are available from ", min(dt_out$year[!is.na(dt_out[,input$parameters_toshow4])]), " through ", max(dt_out$year[!is.na(dt_out[,input$parameters_toshow4])]), ". </br></br>")
    HTML(paste(map2breaks))
  })
  
  # Function for the density map 
  avg_by_station <- function(p, yr_input1, yr_input2) {
    parameter <- p
    min_yr    <- yr_input1
    max_yr    <- yr_input2
    mysubset <- total[which(as.numeric(substring(total$VisitDate, 1, 4)) >= min_yr & as.numeric(substring(total$VisitDate, 1, 4))<=max_yr),]
    
    n <- length(which(!is.na(mysubset[,parameter])))
    
    if(n>1) min_yr    <- min(as.numeric(substring(mysubset$VisitDate, 1, 4)))
    if(n>1) max_yr    <- max(as.numeric(substring(mysubset$VisitDate, 1, 4)))
    
    summ_subset <- with(mysubset, 
                        tapply(mysubset[,parameter],list("Sites"=as.factor(mysubset$StationID)), mean, na.rm=T))
    summ_subset <- as.data.frame(summ_subset)
    summ_subset$value <- as.numeric(summ_subset$summ_subset)
    # summ_subset$StationID <- as.numeric(rownames(summ_subset))
    # summ_subset$prop_subset <- as.numeric(summ_subset$summ_subset / max(summ_subset$summ_subset))
  }
  
  
  
  # Density map
  output$mymap2 <- renderLeaflet({
    clrs <- rev(brewer.pal(7, "Reds"))
    qpal <- colorBin(palette = "YlOrRd", domain = avg_by_station(p = input$parameters_toshow4, yr_input1 = input$range_map[1], yr_input2 = input$range_map[2]), bins = 7)
    leaflet(stations_metadata_subset) %>% 
      addProviderTiles("Esri.WorldImagery") %>%  # Add default OpenStreetMap map tiles
      # addRasterImage(raster_LC_leaflet, colors = pal, opacity = 0.8) %>% 
      addCircles(color = ~qpal(avg_by_station(p = input$parameters_toshow4, yr_input1 = input$range_map[1], yr_input2 = input$range_map[2])), fillOpacity = 1, radius = 2000,
                 # radius = avg_by_station(p = input$parameters_toshow4, yr_input1 = input$range_map[1], yr_input2 = input$range_map[2]) * 2500
                 data = stations_metadata_subset, lat = as.numeric(stations_metadata_subset$Latitude[stations_metadata_subset$WaterbodyType == "Lake"]), 
                 lng = as.numeric(stations_metadata_subset$Longitude[stations_metadata_subset$WaterbodyType == "Lake"]), 
                 popup = paste0("<b>Station name: </b>", stations_metadata_subset$StationName[stations_metadata_subset$WaterbodyType == "Lake"], "</br><b> StationID: </b>", stations_metadata_subset$StationID[stations_metadata_subset$WaterbodyType == "Lake"], 
                                "</br><b> Latitude: </b>", stations_metadata_subset$Latitude[stations_metadata_subset$WaterbodyType == "Lake"], "</br><b> Longitude: </b>", stations_metadata_subset$Longitude[stations_metadata_subset$WaterbodyType == "Lake"],
                                "</br><b> Station depth: </b>", as.numeric(stations_metadata_subset$StationDepth[stations_metadata_subset$WaterbodyType == "Lake"]), " meters",
                                "</br><b> Average value: </b>", round(as.numeric(avg_by_station(p = input$parameters_toshow4, yr_input1 = input$range_map[1], yr_input2 = input$range_map[2])), 2))) %>% 
      addLegend(title = c(input$parameters_toshow4), pal = qpal, values = ~avg_by_station(p = input$parameters_toshow4, yr_input1 = input$range_map[1], yr_input2 = input$range_map[2]), opacity = 1)
  })
  
  # Scatterplot
  output$myplot1 <- renderPlot({
    dt_out <- dt_all[which(as.numeric(dt_all$type)==as.numeric(input$data_toggle)),]
    if (length(input$parameters_toshow) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$parameters_toshow, 
                   function(b) ggplot(      dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow),], 
                                            aes(x=as.Date(dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow),1]),
                                                y=dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow), b])) +
                     geom_point() +
                     xlab("Year") + ylab(b) +
                     scale_x_date(limits=as.Date(c(paste0(as.numeric(paste(input$range[1])),"-01-01"),
                                                   paste0(as.numeric(paste(input$range[2])),"-01-01"))))
                   #ggcolors(~input$mysites)
      )
      ifelse(length(gl) == 1, gl, grid.arrange(grobs = gl, nrow = round(length(gl)/2)))
    }
    
  })
  
  # Scatterplot with trendline 
  output$myplot2 <- renderPlot({
    dt_out <- dt_all[which(as.numeric(dt_all$type)==as.numeric(input$data_toggle)),]
    if (length(input$parameters_toshow) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$parameters_toshow, 
                   function(b) ggplot(      dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow),], 
                                            aes(x=as.Date(dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow),1]),
                                                y=dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow),b])) +
                     geom_point() + 
                     stat_smooth(method=loess, formula=y~x) +
                     xlab("Year") + ylab(b) +
                     scale_x_date(limits=as.Date(c(paste0(as.numeric(paste(input$range[1])),"-01-01"),
                                                   paste0(as.numeric(paste(input$range[2])),"-01-01"))))
      )
      ifelse(length(gl) == 1, gl, grid.arrange(grobs = gl, nrow = round(length(gl)/2)))
    }
    
  })
  
  # Boxplots
  output$myplot3 <- renderPlot({
    dt_out <- dt_all[which(as.numeric(dt_all$type)==as.numeric(input$data_toggle)),]
    if (length(input$parameters_toshow) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$parameters_toshow, 
                   function(b) ggplot(dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow) & dt_out$year>=input$range[1] & dt_out$year<=input$range[2],], 
                                      aes(x=as.factor(dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow) & dt_out$year>=input$range[1] & dt_out$year<=input$range[2],"StationID"]),
                                          y=dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow) & dt_out$year>=input$range[1] & dt_out$year<=input$range[2], b])) +
                     geom_boxplot() +
                     labs(x = "Station ID", y = b)
      )
      ifelse(length(gl) == 1, gl, grid.arrange(grobs = gl, nrow = round(length(gl)/2)))
    }
  })
  
  
  # Data table with raw data - annual and daily averages data
  output$mytable1 <- DT::renderDataTable({
    dt_out <- dt_all[which(as.numeric(dt_all$type)==as.numeric(input$data_toggle)),]
    if (input$data_toggle==1) dt_out$VisitDate <- dt_out$year
    DT::datatable(dt_out[dt_out$year >= as.numeric(input$range[1]) & dt_out$year <= as.numeric(input$range[2]), -which(colnames(dt_out)=="year")],
                  filter = 'top', options = list(orderClasses = TRUE, scrollX = TRUE))
  })
  
  # Stats text
  output$Stats1.1  <- renderUI({ 
    Header1       <- "<h2><u>Correlation</u></h2>"
    Theory1       <- paste0("Correlation between two variables (Y1 and Y2 for example) is a statistical measure of the extent to which they fluctuate together. Correlation varies between -1 and 1. A positive correlation between Y1 and Y2 indicates that when Y1 increases, Y2 increases as well; a negative correlation between Y1 and Y2 indicates that when one variable increases, the other decreases. A value close to 0 indicates that the two variables are not strongly correlated. </br>
                            </br>This tool allows you to calculate the correlation between two parameters. Correlation doesn't mean causation, but a strong correlation can hint to important processes. </br>
                            </br>For example, the correlation between Dissolved Oxygen (DO) and Temperature (T) is strongly negative. When the water in the epilimnion is warm, the dissolved oxygen concentration is lower.</br>
                            </br>")
    
    HTML(paste(Header1, Theory1))
  })
  
  # Correlation plot image
  output$corr_plot <- renderUI({
    Img1           <- img(src='20190521_corrplot.pdf', width = as.integer(input$size_slider))
    HTML(paste(Img1))
  })
  
  # Calculates correlation and number of observations. Need them here since they are in their own individual output boxes
  output$mcor    <- renderText(if(length(input$parameters_toshow2)>1) round(cor(dt_out[dt_out$year>input$range[1] & dt_out$year<input$range[2], input$parameters_toshow2[1]], dt_out[dt_out$year>input$range[1] & dt_out$year<input$range[2], input$parameters_toshow2[2]], use = "na.or.complete"),4) else HTML("<i> Select another variable </i>"))
  output$n       <- renderText(if(length(input$parameters_toshow2)>1) nrow(dt_out[dt_out$year>=input$range[1] & dt_out$year<=input$range[2] & !is.na(dt_out[,input$parameters_toshow2[1]]) & !is.na(dt_out[,input$parameters_toshow2[2]]), ]) else "NA")
  
  # More stats tab text
  output$Stats1.2 <- renderUI({
    Header12      <- "<h3>Try it for yourself!</h3>"
    Instruct_Corr <- paste0("<b>Instructions:</b> select TWO parameters from the dropdown menu below. If you select more than that, the correlation will be calculated for the two first parameters.</br></br>")
    HTML(paste(Header12, Instruct_Corr))
  })
  
  # More stats tab text
  output$Stats1.3 <- renderUI({
    Results_Corr  <- paste0("</br>The correlation between <b>", input$parameters_toshow2[1], "</b> and <b>", input$parameters_toshow2[2], "</b> is:", verbatimTextOutput("mcor"),"calculated from </br>",verbatimTextOutput("n")," observations. </br></br>This is calculated across ALL sites, for the <b>", input$range[1],"-",input$range[2],"</b> period.
                            If NA are displayed, try another parameter or change the time period. Some variables were never measured at the same time.</br></br>")
    Corr_plot_instruct <- paste0("Below is a correlation plot of all of the parameters plotted against one another. Use the slider to expand or shrink the image.</br></br>")
    HTML(paste(Results_Corr, Corr_plot_instruct))
  })
  
  # More stats tab text
  output$Stats2.1 <- renderUI({
    Header2       <- paste0("<h2><u>Basic statistics</u></h2>")
    Theory2       <- paste0("This tool allows you to compute basic statistics on the dataset, including mean, minimal and maximal values. 
                            </br>The statistics can be done with the annual or daily averages dataset!)
                            </br>")
    Header22      <- "<h3>Try it for yourself!</h3>"
    Instruct_basic_stats <- paste0("<b>Instructions:</b> select as many parameters and stations as desired from the dropdown menus below. </br></br>")
    HTML(paste(Header2, Theory2, Header22, Instruct_basic_stats))
  })
  
  # More stats tab text
  output$Stats2.2 <- renderUI({
    mstations     <- paste(as.numeric(input$stations_toshow3),sep="", collapse=", ")
    Results_basic_stats <- paste0("</br>The stats are calculated for stations ", mstations,", selected in the plot tab, for the period <b>",input$range[1],"-",input$range[2],"</b>. </br>")
    mmean         <- NULL
    if(length(input$parameters_toshow3)>0) for (i in 1:length(input$parameters_toshow3)) mmean <- paste(mmean, input$parameters_toshow3[i], "       – mean: ",round(mean(dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow2) & dt_out$VisitDate>input$range[1] & dt_out$VisitDate<input$range[2],input$parameters_toshow3[i]], na.rm=T),2), ", calculated from n= ",nrow(dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow2) & dt_out$year>=input$range[1] & dt_out$year<=input$range[2] & !is.na(dt_out[,input$parameters_toshow3[i]]),])," observations. </br>")
    HTML(paste(Results_basic_stats, mmean))
  })           
  
}

shinyApp(ui, server)

