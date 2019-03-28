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
library(shinydashboardPlus)
library(leaflet)

out <- total_year
out$year <- as.numeric(substring(out[,1],1,4))
str(out$VisitDate)
out <- out[order(out$year,decreasing = F),]

boatIcon <- makeIcon(
  iconUrl = "https://www.materialui.co/materialIcons/maps/directions_boat_black_192x192.png",
  iconWidth = 20, iconHeight = 20)

xIcon <- makeIcon(
  iconUrl = "https://cdn4.iconfinder.com/data/icons/defaulticon/icons/png/256x256/cancel.png",
  iconWidth = 20, iconHeight = 20)

# Define UI for slider demo app ----
ui <- dashboardPagePlus(
  #define color
  skin = "black",
  # App title ----
  #embedment of logo is not working:
  header = dashboardHeaderPlus(enable_rightsidebar = TRUE,
                               title = tags$a(href='https://www.uvm.edu/rsenr/rubensteinlab',
                                              tags$img(src='logo_rubenstein_lab.png')),
                               titleWidth = 350
  ),
  
  # Sidebar layout with input and output definitions ----
  sidebar = dashboardSidebar(
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
        menuSubItem("Charts", tabName = "d_chart", icon = icon("line-chart"), selected = TRUE),
        menuSubItem("Table", tabName = "d_table", icon = icon("table")),
        sliderInput("range", "Years selected",
                    min = min(out$year,na.rm=FALSE), max = max(out$year,na.rm=FALSE),
                    value = c(2000,2012),sep = ""),
        conditionalPanel(
          'input$id == "graph"',
          checkboxGroupInput("toshow", "Columns to show:",
                             colnames(out)))#, selected = colnames(out)))
      )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "about",
        #Render an output text
        textOutput("mytext1")
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
          title = "dot charts",
          collapsible = TRUE,
          width = "100%",
          height = "70%",
          plotOutput("myplot1")
        ),
        box(
          title = "line charts",
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
  )#,
  # rightsidebar = rightSidebar(
  #   background = "dark", width = 80,
  #   rightSidebarTabContent(
  #     id = 1,
  #     icon = "desktop",
  #     title = "Tab 1",
  #     active = TRUE,
  #     conditionalPanel(
  #       'input$id2 == "sites"',
  #       checkboxGroupInput("toshow2", "Sites to show:",
  #                          unique(out$StationID), selected = unique(out$StationID)))
  #   )
  # )
)
server <- function(input, output) {
  
  # plot
  output$mytext1 <- renderText({ 
    "Here we can write a description about Lake Champlain. We can do that later using section from your report."
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
    if (length(input$toshow) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$toshow, 
                   function(b) ggplot(out, aes(x=out[,1],y=out[, b])) +
                     geom_point() +
                     xlab("Year") + ylab(b) +
                     xlim(c(input$range[1],input$range[2]))
      )
      grid.arrange(grobs = gl, nrow = 1)
    }
    
  })
  
  output$myplot2 <- renderPlot({
    if (length(input$toshow) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$toshow, 
                   function(b) ggplot(out[out$StationID %in% as.numeric(input$toshow2),], aes(x=out[out$StationID %in% as.numeric(input$toshow2),1],y=out[out$StationID %in% as.numeric(input$toshow2), b])) +
                     geom_point() + 
                     stat_smooth(method=loess, formula=y~x) +
                     xlab("Year") + ylab(b) +
                     xlim(c(input$range[1],input$range[2]))
      )
      if (length(gl)==1) grid.arrange(grobs = gl, nrow = 1) else grid.arrange(grobs = gl, nrow = round(length(gl)/2))
    }
    
  })
  
  # table
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(out[out$year >= as.numeric(input$range[1]) & out$year <= as.numeric(input$range[2]), ], filter = 'top', options = list(orderClasses = TRUE, scrollX = TRUE))
  })
  
}

shinyApp(ui, server)


