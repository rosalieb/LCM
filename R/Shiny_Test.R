library(shiny)
library(ggplot2)

total_app <- total_year[,1:10]
names(total_app) <- paste("Var",1:10, sep="")
names(total_app)
ncol(total_year)

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("LCM"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Specification of range within an interval ----
      sliderInput("range", "Range:",
                  min = 1992, max = 2017,
                  value = c(1992,2017))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      tableOutput("year_slider")
      
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Year Range"),
      Value = as.character(c(paste(input$range, collapse = " through "))),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$year_slider <- renderTable({
    sliderValues()
  })
  
  # Creating the data table ----
  # Not sure where to call the table...
    total_app2 = total_app[sample(nrow(total_app), 67), ]
    output$mytable <- DT::renderDataTable({
      DT::datatable(total_app2[drop = FALSE])
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
