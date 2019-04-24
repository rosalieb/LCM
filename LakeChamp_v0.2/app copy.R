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
library(shiny)
library(DT)
library(yaml)
library(shinydashboard)
library(leaflet)

out <- total_year
out$year <- as.numeric(substring(out[,1],1,4))
str(out$VisitDate)
out <- out[order(out$year,decreasing = F),]

colors1 <- colors(distinct = TRUE)
set.seed(1585) # to set random generator seed
colors2 <- sample(colors1, 15)
colScale <- scale_color_manual(name = "grp", values = colors2)

boatIcon <- makeIcon(
  iconUrl = "https://www.materialui.co/materialIcons/maps/directions_boat_black_192x192.png",
  iconWidth = 20, iconHeight = 20)

xIcon <- makeIcon(
  iconUrl = "https://cdn4.iconfinder.com/data/icons/defaulticon/icons/png/256x256/cancel.png",
  iconWidth = 20, iconHeight = 20)

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
        sliderInput("range", "Years selected",
                    min = min(out$year,na.rm=FALSE), max = max(out$year,na.rm=FALSE),
                    value = c(2000,2012),sep = ""),
        conditionalPanel(
          'input$id == "graph"',
          checkboxGroupInput("sites_toshow", "Columns to show:",
                             colnames(out)))#, selected = colnames(out)))
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
          collapsible = TRUE,
          tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
          leafletOutput("mymap1")
        )
      ),
      tabItem(
        tabName = "d_chart",
        title = "Instruction: Select data to plot",
        box(
          title = "Station Selection",
          collapsible = TRUE,
          width = "100%", 
          height = "70%",
          conditionalPanel(
            'input$id2 == "sites"',
            checkboxGroupInput("param_toshow", "Sites to show:",
                               sort(unique(out$StationID)), selected = sort(unique(out$StationID)))
          )
        ),
        box(
          title = "Dot Charts",
          collapsible = TRUE,
          width = "100%",
          height = "70%",
          plotOutput("myplot1")
        ),
        box(
          title = "Line Charts",
          collapsible = TRUE,
          width = "100%",
          height = "70%",
          plotOutput("myplot2")
        )
      ),
      tabItem(
        tabName = "d_table",
        DT::dataTableOutput("mytable1")
      )
    )
  )
)

server <- function(input, output) {
  
  # plot
  output$mytext1 <- renderUI({ 
    header <- "<h1>Lake Champlain</h1>"
    myparagraph1 <- "<b> Spanning </b> a length of 190 km from Whitehall, NY <br/> to its outlet at the Richelieu River in Québec, Canada, Lake Champlain covers 113,000 hectares and is estimated to hold roughly 25 trillion liters. Average lake depth is 19.5 meters (64.5 feet), with the greatest lake depth of 122 meters (400 feet). A majority of the water that enters Lake Champlain runs through its basin, which covers over 21,000 square kilometers. Over half of the basin is in Vermont, about a third in New York, and less than a tenth in the Province of Québec. The water retention time varies by lake segment, ranging between two months in the South Lake to about 3 years in the Main Lake."
    myparagraph2 <- "test"
    linkToSite <- "Click <a href = 'http://www.lcbp.org/'>here</a> to visit the Lake Champlain Basin Program website."
    HTML(paste(header, myparagraph1, myparagraph2, linkToSite, sep = '<br/>'))
  })
  
  output$mymap1 <- renderLeaflet({
    leaflet(LCMcoord) %>% 
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(data = LCMcoord, icon = boatIcon, lat = as.numeric(LCMcoord$LLatitude), 
                 lng = as.numeric(LCMcoord$LLongitude), 
                 popup = LCMcoord$LStation) %>%
      addMarkers(data = LCMcoord, icon = xIcon, lat = as.numeric(LCMcoord$TLatitude), 
                 lng = as.numeric(LCMcoord$TLongitude),
                 popup = LCMcoord$TStation)
  })
  
  output$myplot1 <- renderPlot({
    if (length(input$sites_toshow) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$sites_toshow, 
                   function(b) ggplot(out[out$StationID %in% as.numeric(input$param_toshow),], 
                     aes(x=out[out$StationID %in% as.numeric(input$param_toshow),1],y=out[out$StationID %in% 
                     as.numeric(input$param_toshow), b]), color = grp) +
                     geom_point() +
                     xlab("Year") + ylab(b) +
                     xlim(c(input$range[1],input$range[2]))
                     #ggcolors(~input$mysites)
      )
      grid.arrange(grobs = gl, nrow = 1)
    }
    
  })
  
  output$myplot2 <- renderPlot({
    if (length(input$sites_toshow) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$sites_toshow, 
                   function(b) ggplot(out[out$StationID %in% as.numeric(input$param_toshow),], 
                     aes(x=out[out$StationID %in% as.numeric(input$param_toshow),1],y=out[out$StationID %in% 
                     as.numeric(input$param_toshow), b]), color = StationID) +
                     geom_point(colScale = out$StationID) + 
                     stat_smooth(method=loess, formula=y~x) +
                     xlab("Year") + ylab(b) +
                     xlim(c(input$range[1],input$range[2]))
      )
      grid.arrange(grobs = gl, nrow = 1)
    }
    
  })
  
  # table
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(out[out$year >= as.numeric(input$range[1]) & out$year <= as.numeric(input$range[2]), ], filter = 'top', options = list(orderClasses = TRUE, scrollX = TRUE))
  })
  
}

shinyApp(ui, server)

