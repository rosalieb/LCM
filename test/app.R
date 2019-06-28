#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Demo", tags$li(class = "dropdown", actionButton("home", "Home"))),
  dashboardSidebar(sidebarMenu(id = "sidebar", # id important for updateTabItems
                               menuItem("Home", tabName = "home", icon = icon("house")),
                               menuItem("Tab1", tabName = "tab1", icon = icon("table")),
                               menuItem("Tab2", tabName = "tab2", icon = icon("line-chart")),
                               menuItem("Tab3", tabName = "tab3", icon = icon("line-chart")))
  ),
  
  dashboardBody(
    tabItems(
      tabItem("home", "This is the home tab"),
      tabItem("tab1", "This is Tab1"),
      tabItem("tab2", "This is Tab2"),
      tabItem("tab3", "This is Tab3")
    ))
)
server = function(input, output, session){
  observeEvent(input$home, {
    updateTabItems(session, "sidebar", "home")
  })
}
shinyApp(ui, server)