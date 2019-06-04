#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)
library(DT)
library(yaml)
library(shinydashboard)
library(leaflet)
# names(tags) # list of tags recognized by shiny.
library(htmlTable)

out <- total_year
out$year <- as.numeric(substring(out[,1],1,4))
str(out$VisitDate)
out <- out[order(out$year,decreasing = F),]
out <- round(out, digits = 2)

out$StationID[out$Station == "2"] <- "02"
out$StationID[out$Station == "4"] <- "04"
out$StationID[out$Station == "7"] <- "07"
out$StationID[out$Station == "9"] <- "09"
as.numeric(out$StationID)

stations_metadata <- read.delim(paste0(getpath4data(),"/LCM_bio_PeteStangel/Plankton data stations.txt"))
lake_stations_subset <- stations_metadata[stations_metadata$StationID %in% out$StationID,]
trib_stations_subset <- stations_metadata[stations_metadata$StationID %in% LCMcoord$TStation,]
stations_metadata_subset <- rbind(lake_stations_subset, trib_stations_subset)

xIcon <- makeIcon(
  iconUrl = "https://cdn4.iconfinder.com/data/icons/defaulticon/icons/png/256x256/cancel.png",
  iconWidth = 20, iconHeight = 20)

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

# Define UI for slider demo app ----
ui <- dashboardPage(
  #define color
  skin = "black",
  # App title ----
  #embedment of logo is not working:
  dashboardHeader(title = tags$a(href='https://www.uvm.edu/rsenr/rubensteinlab',
                                 icon("home")),
                  titleWidth = 200,
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
      menuItem(
        "About this project", 
        tabName = "about", 
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
        sliderInput("range", "Years selected",
                    min = min(out$year,na.rm=FALSE), max = max(out$year,na.rm=FALSE),
                    value = c(2000,2012),sep = "")
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
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "about",
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
          title = "Chemical and Biological Parameters",
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
          div(style="display: inline-block;vertical-align:top; width: 150px;", dropdownButton(
            label = "Stations to plot", status = "default", width = 80, circle = FALSE,
            checkboxGroupInput("stations_toshow", "Sites to show:",
                               sort(unique(out$StationID)), selected = sort(unique(out$StationID)), inline = FALSE))),
            div(style="display: inline-block;vertical-align:top; width: 150px;", dropdownButton(
            label = "Parameters to plot", status = "default", width = 80, circle = FALSE,
            checkboxGroupInput("parameters_toshow", "Parameters to show:",
                               colnames(out))))
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
        htmlOutput("Stats")
      )
    )
  )
)

server <- function(input, output) {
  
  # plot
  output$mytext1 <- renderUI({ 
    header <- "<h1>Lake Champlain</h1> <br/>"
    myparagraph1 <- "Spanning a length of 190 km from Whitehall, NY to its outlet at the Richelieu River in Québec, Canada, Lake Champlain covers 113,000 hectares and is estimated to hold roughly 25 trillion liters. Average lake depth is 19.5 meters (64.5 feet), with the greatest lake depth of 122 meters (400 feet). A majority of the water that enters Lake Champlain runs through its basin, which covers over 21,000 square kilometers. Over half of the basin is in Vermont, about a third in New York, and less than a tenth in the Province of Québec. The water retention time varies by lake segment, ranging between two months in the South Lake to about 3 years in the Main Lake. <br/> <br/>"
    myparagraph2 <- "Data have been collected through the Lake Champlain Long-Term Water Quality and Biological Monitoring Program since 1992, with one of the original purposes to provide hydrodynamic, eutrophication, and food web models for the lake, although their purpose changed in 1995 (Vermont Department of Environmental Conservation, & New York State Department of Environmental Conservation, 2017). Data were collected by the Vermont Department of Environmental Conservation and the New York State Department of Environmental Conservation, with the support of the Lake Champlain Basin Program. Fifteen lake and twenty-two tributary stations have been sampled throughout the years, although some biological and chemical measurements were only collected during more recent years along with the acquisition of newer equipment. Data are stored in a database and available on request for research, management, consulting, and learning purposes. <br/> <br/>"
    linkToSite <- "To visit the Lake Champlain Basin Program's website, click <a href = 'http://www.lcbp.org/'>here</a>. <br/> <br/>"
    linkToData <- "To retrieve the data on Lake Champlain used for this project, click <a href = 'https://dec.vermont.gov/watershed/lakes-ponds/monitor/lake-champlain'>here</a>."
    HTML(paste(header, myparagraph1, myparagraph2, linkToSite, linkToData))
  })
  
  output$parameters_table = DT::renderDataTable({
    DT::datatable(parameters_info, options = list(lengthMenu = c(50, 100)))
  })

  output$Instructions_plot_1 <- renderUI({ 
    Instruction1 <- paste0("Select on the left side the parameters to plot (you need to select at least one), as well as the period for which you want to visualize the data. Currently, data are displayed for the ", input$range[1],"-",input$range[2]," period.<br/>Then, select the sites you want to see the data for.<br/><br/>")
    HTML(paste(Instruction1))
  })
  
  output$info_plot_3 <- renderUI({ 
    info3 <- paste0("Values for each parameter are averages for the date range selected with the slider. You can select which stations to view with the selection boxes at the top of this page.")
    HTML(paste(info3))
  })
  
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
    if (length(input$parameters_toshow) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$parameters_toshow, 
                   function(b) ggplot(out[out$StationID %in% as.numeric(input$stations_toshow),], 
                     aes(x=out[out$StationID %in% as.numeric(input$stations_toshow),1],y=out[out$StationID %in% 
                     as.numeric(input$stations_toshow), b])) +
                     geom_point() +
                     xlab("Year") + ylab(b) +
                     xlim(c(input$range[1],input$range[2]))
                     #ggcolors(~input$mysites)
      )
      ifelse(length(gl) == 1, gl, grid.arrange(grobs = gl, nrow = round(length(gl)/2)))
    }
    
  })
  
  output$myplot2 <- renderPlot({
    if (length(input$parameters_toshow) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$parameters_toshow, 
                   function(b) ggplot(out[out$StationID %in% as.numeric(input$stations_toshow),], 
                     aes(x=out[out$StationID %in% as.numeric(input$stations_toshow),1],y=out[out$StationID %in% 
                     as.numeric(input$stations_toshow), b])) +
                     geom_point() + 
                     stat_smooth(method=loess, formula=y~x) +
                     xlab("Year") + ylab(b) +
                     xlim(c(input$range[1],input$range[2]))
      )
      ifelse(length(gl) == 1, gl, grid.arrange(grobs = gl, nrow = round(length(gl)/2)))
    }
    
  })
  
  # An attempt to put in a bar plot of a single parameter's average by StationID, also should be dependent upon the time range selected
  output$myplot3 <- renderPlot({
    if (length(input$parameters_toshow) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$parameters_toshow, 
                   function(b) ggplot(out[out$StationID %in% as.numeric(input$stations_toshow),], 
                                      aes(x=as.factor(out[out$StationID %in% as.numeric(input$stations_toshow),"StationID"]), y=out[out$StationID %in% as.numeric(input$stations_toshow), b])) +
                     geom_boxplot() +
                     labs(x = "Station ID", y = b)
      )
      ifelse(length(gl) == 1, gl, grid.arrange(grobs = gl, nrow = round(length(gl)/2)))
    }
  })

  
  # table
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(out[out$year >= as.numeric(input$range[1]) & out$year <= as.numeric(input$range[2]), ], filter = 'top', options = list(orderClasses = TRUE, scrollX = TRUE))
  })
  
  # Stats
  output$Stats <- renderUI({ 
    Header       <- "<h1>Stat tools</h1> <br/>"
    Header1      <- "<h2>Correlation</h2> <br/>"
    Header11     <- "<h3>Theory</h3> <br/>"
    Theory1 <- paste0("Correlation between two variables (Y1 and Y2 for example) is a statistical measure of the extent to which they fluctuate together. Correlation varies between -1 and 1. A positive correlation between Y1 and Y2 indicates that when Y1 increases, Y2 increases as well; a negative correlation between Y1 and Y2 indicates that when one variable increases, the other decreases. A value close to 0 indicates that the two variables are not strongly correlated. </br>
                           </br>This tool allows you to calculate the correlation between two parameters. Correlation doesn't mean causation, but a strong correlation can hint to important processes. </br>
                           </br>For example, the correlation between Dissolved Oxygen (DO) and Temperature (T) is strongly negative. When the water in the epilimnion is warm, the dissolved oxygen concentration is lower.</br>
                           </br>")
    Img1         <- img(src='20190521_corrplot.pdf', align = "right", width=700)
    Header12     <- "<h3>Try it for yourself!</h3> <br/>"
    mcor         <- if(length(input$parameters_toshow)>1) round(cor(out[out$VisitDate>input$range[1] & out$VisitDate<input$range[2],input$parameters_toshow[1]],out[out$VisitDate>input$range[1] & out$VisitDate<input$range[2],input$parameters_toshow[2]], use = "na.or.complete"),4) else "<i> Select another variable </i>"
    n            <- if(length(input$parameters_toshow)>1) length(!is.na(out[!is.na(out[out$VisitDate>input$range[1] & out$VisitDate<input$range[2],input$parameters_toshow[2]]),input$parameters_toshow[1]])) else "NA"
    Results_Corr <- paste0("<b>Instructions:</b> select TWO parameters from the left menu. If you select more than that, the correlation will be calculated for the two first parameters.</br>
                           </br>The correlation between <b>", input$parameters_toshow[1], "</b> and <b>", input$parameters_toshow[2], "</b> is: ", mcor,", calculated from ",n," observations. </br></br>This is calculated across ALL sites, for the <b>",input$range[1],"-",input$range[2],"</b> period. 
                           If NA are displayed, try another parameter or change the time period. Some variables were never measured at the same time.</br>")
    Header2      <- "<h2>Basic stats</h2> <br/>"
    Header21     <- "<h3>Theory</h3> <br/>"
    Theory2      <- paste0("</br>This tool allows you to compute basic statistics on the dataset, including mean, minimal and maximal values. 
                           </br>The statistics are done on the annual averages dataset. Annual averages were computed to ease the visualization. A future version of the app should allow the user to chose what to visualize (raw dataset or annual averages).)
                           </br>")
    Header22     <- "<h3>Try it for yourself!</h3> <br/>"
    mmean        <- NULL
    if(length(input$parameters_toshow)>0) for (i in 1:length(input$parameters_toshow)) mmean <- paste(mmean, input$parameters_toshow[i], "       – mean: ",round(mean(out[out$StationID %in% as.numeric(input$stations_toshow) & out$VisitDate>input$range[1] & out$VisitDate<input$range[2],input$parameters_toshow[i]], na.rm=T),2), ", calculated from n= ",length(!is.na(out[,input$parameters_toshow[i]]))," observations. </br>")
    mstations <- paste(as.numeric(input$stations_toshow),sep="", collapse=", ")
    Results_basic_stats <- paste0("<b>Instructions:</b> select parameters from the left menu. </br>
                                    The stats are calculated for stations ",mstations,", selected in the plot tab, for the period <b>",input$range[1],"-",input$range[2],"</b>. </br>")
    HTML(paste(Header,
               Header1,Header11,Theory1, Img1, Header12,Results_Corr,
               Header2,Header21,Theory2, Header22,Results_basic_stats, mmean))
   
  })
  
}

shinyApp(ui, server)

