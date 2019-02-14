library(shiny)
library(ggplot2)
library(gridExtra)
library(shiny)
library(DT)
library(yaml)

out <- total_year
out$year <- as.numeric(substring(out[,1],1,4))
str(out$VisitDate)
out <- out[order(out$year,decreasing = F),]

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Selection"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      # Input: Specification of range within an interval ----
      sliderInput("range", "Years selected",
                  min = min(out$year,na.rm=FALSE), max = max(out$year,na.rm=FALSE),
                  value = c(2000,2012),sep = ""),
      conditionalPanel(
                 'input$id == "graph"',
                 checkboxGroupInput("toshow", "Columns to show:",
                 colnames(out)))#, selected = colnames(out)))

    ),
    mainPanel(
      tabsetPanel(
        id = 'what do you want to see?',
        tabPanel("graph", plotOutput("myplot1")),
        tabPanel("table", DT::dataTableOutput("mytable1"))
      )
    )
  )
  
)

server <- function(input, output) {
  
  # plot
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
  
  # table
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(out[out$year >= as.numeric(input$range[1]) & out$year <= as.numeric(input$range[2]), ], filter = 'top', options = list(orderClasses = TRUE))
  })
  
}

shinyApp(ui, server)

