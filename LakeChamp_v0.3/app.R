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
# raster.LC <- raster("LakeChamp_v0.3/data/raster_LC.tif")
# raster.LC.leaflet <- projectRasterForLeaflet(raster.LC, method = "ngb")

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
        "Map", 
        tabName = "m_lake", 
        icon = icon("map") #icon("globe"),
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
      # conditionalPanel(
      #   'input$id == "graph"',
      #   checkboxGroupInput("parameters_toshow", "Phosphorus:",
      #                      colnames(out[,c(36,10,25)])),
      #   checkboxGroupInput("parameters_toshow", "Nitrogen:",
      #                      colnames(out[,c(34,32,33,31)])),
      #   checkboxGroupInput("parameters_toshow", "Carbon:",
      #                      colnames(out[,c(35,9,8)])),
      #   checkboxGroupInput("parameters_toshow", "Other minerals/nutrients/elements:",
      #                      colnames(out[,c(12,4,14,29,27,11,13)])),
      #   checkboxGroupInput("parameters_toshow", "Water quality",
      #                      colnames(out[,c(38,37,6,3,5,30,7,26,28)])),
      #   checkboxGroupInput("parameters_toshow", "Phytoplankton",
      #                      colnames(out[,c(24,23,20,19,16,15,18,17,22,21)])))#, selected = colnames(out)))
      menuItem(
        "Historic data", 
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
        box(
          title = "Lake Champlain monitoring sites",
          width = "100%",
          height = "100%",
          collapsible = TRUE,
          htmlOutput("mymap1help"),
          tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
          leafletOutput("mymap1", height = 900, width = 660)
        )
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
          #Render an output text
          htmlOutput("Instructions_plot_1"),
          # radioButtons(
          #   inputId = "data_toggle_plots", label = "Annual or daily data", choices = list("Annual data" = 1, "Daily data" = 2)
          # ),
          div(style="display: inline-block;vertical-align:top; width: 150px;", dropdownButton(
            label = "Parameters to plot", status = "default", width = 80, circle = FALSE,
            div(style='max-height: 40vh; overflow-y: auto;', checkboxGroupInput("parameters_toshow", "Check any parameters:",
                                                                                colnames(dt_out))))),
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
        # radioButtons(
        #   inputId = "data_toggle_table", label = "Annual or daily data", choices = list("Annual data" = 1, "Daily data" = 2)
        # ),
        DT::dataTableOutput("mytable1")
      ),
      tabItem(
        tabName = "d_stats",
        tabsetPanel(type = "tabs", tabPanel("Correlation", htmlOutput("Stats1")),
                    tabPanel("Basic stats", htmlOutput("Stats2"))),
        dropdownButton(
          label = "Parameters to plot", status = "default", width = 80, circle = FALSE, 
          div(style='display: inline-block; max-height: 40vh; overflow-y: auto; width: 150px;', checkboxGroupInput("parameters_toshow2", "Check any boxes:",
                                                                              colnames(dt_out)))),
        dropdownButton(
          label = "Stations to plot", status = "default", width = 80, circle = FALSE,
          div(style='display: inline-block; max-height: 40vh; overflow-y: auto; width: 150px;', checkboxGroupInput("stations_toshow2", "Check any boxes:",
                                                                              sort(unique(dt_out$StationID)), selected = sort(unique(dt_out$StationID)), inline = FALSE)))
      ),
      
      # div(style="display: inline-block;vertical-align:top; width: 150px;",
      tabItem(
        tabName = "history",
        #Render an output text
        htmlOutput("history_info")
      )
    )
  )
)

# Server ####
server <- function(input, output, session) {
  
  # output$which_dt_out<-renderUI({
  #   selectInput("which_dt_out", label = h4("Annual or daily data"),
  #               choices = c("Annual data" = 1, "Daily data" = 2),selected = 1
  #   )
  # })

  # output$selected_dt_out <- reactive({
  #   selected_dt_out <- load_data()
  #   selected_dt_out <- dt_all[dt_all$type==input$data_toggle_plots,]
  #   selected_dt_out <- selected_dt_out[,-which(colnames(selected_dt_out)=="type")]
  #   selected_dt_out
  # })

  #dt_out <- input$selected_dt_out()
  
  # plot
  output$mytext1 <- renderUI({ 
    header <- "<h2>Lake Champlain</h2>"
    myparagraph1 <- "Spanning a length of 190 km from Whitehall, NY to its outlet at the Richelieu River in Québec, Canada, Lake Champlain covers 113,000 hectares and is estimated to hold roughly 25 trillion liters. Average lake depth is 19.5 meters (64.5 feet), with the greatest lake depth of 122 meters (400 feet). A majority of the water that enters Lake Champlain runs through its basin, which covers over 21,000 square kilometers. Over half of the basin is in Vermont, about a third in New York, and less than a tenth in the Province of Québec. The water retention time varies by lake segment, ranging between two months in the South Lake to about 3 years in the Main Lake. <br/> <br/>"
    header2 <- "<h3> Lake Champlain Long-Term Water Quality and Biological Monitoring Program (LTMP) </h3>"
    myparagraph2 <- "Data have been collected through the Lake Champlain Long-Term Water Quality and Biological Monitoring Program since 1992, with one of the original purposes to provide hydrodynamic, eutrophication, and food web models for the lake, although their purpose changed in 1995 (Vermont Department of Environmental Conservation, & New York State Department of Environmental Conservation, 2017). Data were collected by the Vermont Department of Environmental Conservation and the New York State Department of Environmental Conservation, with the support of the Lake Champlain Basin Program. Fifteen lake and twenty-two tributary stations have been sampled throughout the years, although some biological and chemical measurements were only collected during more recent years along with the acquisition of newer equipment. Data are stored in a database and available on request for research, management, consulting, and learning purposes. <br/> <br/>"
    linkToSite <- "To visit the Lake Champlain Basin Program's website, click <a href = 'http://www.lcbp.org/'>here</a>. <br/> <br/>"
    linkToData <- "To retrieve the data on Lake Champlain used for this project, click <a href = 'https://dec.vermont.gov/watershed/lakes-ponds/monitor/lake-champlain'>here</a>."
    HTML(paste(header, myparagraph1, header2, myparagraph2, linkToSite, linkToData))
  })
  
  output$history_info <- renderUI({
    header_history <- "<h3> Historic data of Lake Champlain </h3> <br/>"
    header_body <- "Here, you'll find historic data collected on Lake Champlain by various researchers. To be finished soon..."
    HTML(paste(header_history, header_body))
  })
  
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
  
  
  output$mytable1help <- renderUI({ 
    table1help <- paste0("<h3>Annual averages data</h3> <br/> For more information on the parameters in this table, visit the parameters tab on the left side. There, you'll see the units they were measured in, a description of the overall importance of the parameter, as well as the date of availability for the data. <br/><br/>")
    HTML(paste(table1help))
  })
  
  output$mymap1help <- renderUI({ 
    map1help <- paste0("In the map below, you'll see the lake and tributary stations where data were collected. X's indicate the location of a tributary station while the circles represent the location of a lake station. If you click on the icons, you'll be able to see the name of the station, the station ID, the latitude and longitude of the station, as well as the station depth. Unfortunately, there are no depths for tributary stations.<br/><br/>")
    HTML(paste(map1help))
  })
  
  output$mymap1 <- renderLeaflet({
    leaflet(stations_metadata_subset) %>% 
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addRasterImage(raster.LC.leaflet, colors = pal, opacity = 0.8) %>% 
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
  # <b>Instructions:</b>
  
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
  
  # An attempt to put in a bar plot of a single parameter's average by StationID, also should be dependent upon the time range selected
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
  
  
  # table
  output$mytable1 <- DT::renderDataTable({
    dt_out <- dt_all[which(as.numeric(dt_all$type)==as.numeric(input$data_toggle)),]
    if (input$data_toggle==1) dt_out$VisitDate <- dt_out$year
    DT::datatable(dt_out[dt_out$year >= as.numeric(input$range[1]) & dt_out$year <= as.numeric(input$range[2]), -which(colnames(dt_out)=="year")],
                  filter = 'top', options = list(orderClasses = TRUE, scrollX = TRUE))
  })
  
  # Stats
  output$Stats1 <- renderUI({ 
    Header1      <- "<h2><u>Correlation</u></h2>"
    Theory1 <- paste0("Correlation between two variables (Y1 and Y2 for example) is a statistical measure of the extent to which they fluctuate together. Correlation varies between -1 and 1. A positive correlation between Y1 and Y2 indicates that when Y1 increases, Y2 increases as well; a negative correlation between Y1 and Y2 indicates that when one variable increases, the other decreases. A value close to 0 indicates that the two variables are not strongly correlated. </br>
                      </br>This tool allows you to calculate the correlation between two parameters. Correlation doesn't mean causation, but a strong correlation can hint to important processes. </br>
                      </br>For example, the correlation between Dissolved Oxygen (DO) and Temperature (T) is strongly negative. When the water in the epilimnion is warm, the dissolved oxygen concentration is lower.</br>
                      </br>")
    Img1         <- img(src='20190521_corrplot.pdf', align = "right", width=700)
    Header12     <- "<h3>Try it for yourself!</h3> <br/>"
    mcor         <- if(length(input$parameters_toshow2)>1) round(cor(dt_out[dt_out$year>input$range[1] & dt_out$year<input$range[2],input$parameters_toshow2[1]],dt_out[dt_out$year>input$range[1] & dt_out$year<input$range[2],input$parameters_toshow2[2]], use = "na.or.complete"),4) else "<i> Select another variable </i>"
    n            <- if(length(input$parameters_toshow2)>1) length(!is.na(dt_out[!is.na(dt_out[dt_out$year>input$range[1] & dt_out$year<input$range[2],input$parameters_toshow2[2]]),input$parameters_toshow2[1]])) else "NA"
    Results_Corr <- paste0("<b>Instructions:</b> select TWO parameters from the dropdown menu below. If you select more than that, the correlation will be calculated for the two first parameters.</br>
                           </br>The correlation between <b>", input$parameters_toshow2[1], "</b> and <b>", input$parameters_toshow2[2], "</b> is: ", mcor,", calculated from ",n," observations. </br></br>This is calculated across ALL sites, for the <b>",input$range[1],"-",input$range[2],"</b> period. 
                           If NA are displayed, try another parameter or change the time period. Some variables were never measured at the same time.</br>")
    
    HTML(paste(Header1, Theory1, Img1, Header12, Results_Corr))
    
  })
  
  output$Stats2 <- renderUI({
    Header2      <- paste0("<h2><u>Basic statistics</u></h2>")
    Theory2      <- paste0("This tool allows you to compute basic statistics on the dataset, including mean, minimal and maximal values. 
                           </br>The statistics are done on the annual averages dataset. Annual averages were computed to ease the visualization. A future version of the app should allow the user to chose what to visualize (raw dataset or annual averages).)
                           </br>")
    Header22     <- "<h3>Try it for yourself!</h3>"
    mmean        <- NULL
    if(length(input$parameters_toshow2)>0) for (i in 1:length(input$parameters_toshow2)) mmean <- paste(mmean, input$parameters_toshow2[i], "       – mean: ",round(mean(dt_out[dt_out$StationID %in% as.numeric(input$stations_toshow2) & dt_out$VisitDate>input$range[1] & dt_out$VisitDate<input$range[2],input$parameters_toshow2[i]], na.rm=T),2), ", calculated from n= ",length(!is.na(dt_out[,input$parameters_toshow2[i]]))," observations. </br>")
    mstations <- paste(as.numeric(input$stations_toshow2),sep="", collapse=", ")
    Results_basic_stats <- paste0("<b>Instructions:</b> select parameters from the dropdown menu below. </br>
                                  The stats are calculated for stations ",mstations,", selected in the plot tab, for the period <b>",input$range[1],"-",input$range[2],"</b>. </br>")
    
    HTML(paste(Header2, Theory2, Header22, Results_basic_stats, mmean))
    
  })           
  
}

shinyApp(ui, server)

