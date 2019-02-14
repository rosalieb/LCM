library(shiny)
library(ggplot2)

total_app <- total_year[,1:10]
names(total_app) <- paste("Var",1:10, sep="")
names(total_app)
ncol(total_year)


###############################################################
#devtools::install_github("rstudio/shiny")
#install.packages("devtools")
#install.packages("DT")
library(shiny)
library(devtools)
library(DT)
library(yaml)

out <- total_year
out$year <- as.numeric(substring(out[,1],1,4))
str(out$VisitDate)
out <- out[order(out$year,decreasing = F),]
myout <- out

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      # Input: Specification of range within an interval ----
      sliderInput("range", "Range:",
                  min = min(out$year,na.rm=FALSE), max = max(out$year,na.rm=FALSE),
                  value = c(2000,2001),sep = "")
    ),
    mainPanel(
      DT::dataTableOutput("mytable")
    )
  )
)

# server <- function(input, output) {
#   
#   # sorted columns are colored now because CSS are attached to them
#   # output$mytable <- DT::renderDataTable({
#   #   DT::datatable(out, options = list(orderClasses = TRUE))
#   # })
#   minRowVal <- reactive({
#     min(which(grepl(input$range[[1]], out$year)))        #Retrieve row number that matches selected range on sliderInput
#   })
#   
#   maxRowVal <- reactive({
#     max(which(grepl(input$range[[2]], out$year)))        #Retrieve row number that matches selected range on sliderInput
#   })
#   
#   # observeEvent(input$range, {
#   #   output$mytable <- DT::renderDataTable({
#   #     DT::datatable[minRowVal():maxRowVal(), ]
#   #   })
#   # })
#   
#   output$mytable <- DT::renderDataTable({
#     DT::datatable(out, filter = 'top', options = list(orderClasses = TRUE))
#   })
#   
# }

server <- function(input, output) {
  
  # data <- makeReactiveBinding("out")
  # 
  # # sorted columns are colored now because CSS are attached to them
  # # output$mytable <- DT::renderDataTable({
  # #   DT::datatable(out, options = list(orderClasses = TRUE))
  # # })
  # minRowVal <- reactive({
  #   min(which(grepl(as.numeric(input$range[1]), as.numeric(myout$year))), na.rm=T)        #Retrieve row number that matches selected range on sliderInput
  # })
  # 
  # maxRowVal <- reactive({
  #   max(which(grepl(as.numeric(input$range[2]), as.numeric(myout$year))), na.rm=T)        #Retrieve row number that matches selected range on sliderInput
  # })
  # 
  # 
  # output$mytable <- DT::renderDataTable({
  #   DT::datatable(data()[minRowVal():maxRowVal(), ], filter = 'top', options = list(orderClasses = TRUE))
  # })

  # output$mytable <- DT::renderDataTable({
  #   DT::datatable(data()[data()$year > input$range[1] && data()$year < input$range[2], ], filter = 'top', options = list(orderClasses = TRUE))
  # })
  
  # output$mytable <- DT::renderDataTable({
  #   DT::datatable(data()[data()$year >= as.numeric(input$range[1]) & data()$year <= as.numeric(input$range[2]), ], filter = 'top', options = list(orderClasses = TRUE))
  # })
  
  output$mytable <- DT::renderDataTable({
    DT::datatable(out[out$year >= as.numeric(input$range[1]) & out$year <= as.numeric(input$range[2]), ], filter = 'top', options = list(orderClasses = TRUE))
  })
}

shinyApp(ui, server)

any(duplicated(colnames(out) )==TRUE)
