library(shiny)     # Calling in the libraries I want to use for the program
library(ggplot2)
library(gridExtra)
library(shiny)
library(DT)
library(yaml)

out <- total_year     # Storing the data in total_year (annual averages for each variable) in a variable 'out'
out$year <- as.numeric(substring(out[,1],1,4))     # Storing the string of the years for each station and each variable in out$year
str(out$VisitDate)     # String of all of the years
out <- out[order(out$year,decreasing = F),]     # Unsure of what's going on in this line...

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Selection"), # title at the top of the page
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      # Input: Specification of range within an interval ----
      sliderInput("range", "Years selected",     # type of slider and title of the slider 
                  min = min(out$year,na.rm=FALSE), max = max(out$year,na.rm=FALSE),    # min and max for the slider...na.rm=FALSE means we're not removing missing values
                  value = c(2000,2012),sep = ""),  # 2000 and 2012 are the values we'll start the slider on, and the sep function gets rid of the comma
      conditionalPanel(
        'input$id == "graph"',     # not sure what input$id does, but "graph" is the title 
        checkboxGroupInput("toshow", "Columns to show:",     # name of the checkbox is 'toshow', also setting a title above the names
                           colnames(out)))#, selected = colnames(out)))       using the column names in the variable 'out'
      
    ),
    mainPanel(
      img(src='logo_rubenstein_lab.png', align = "right"), # it looks like this line was an attempt to insert the picture
      tabsetPanel(
        id = 'what do you want to see?',     # not sure what this does
        tabPanel("graph", plotOutput("myplot1")),     # setting what's in each of the tabs labeled 'graph' and 'table'
        tabPanel("table", DT::dataTableOutput("mytable1"))
      )
    )
  )
  
)

server <- function(input, output) {
  
  # plot
  output$myplot1 <- renderPlot({
    if (length(input$toshow) == 0) { # if the number of items chosen = 0, then data frame = blank
      ggplot(data.frame())
    } else {
      gl <- lapply(input$toshow, 
                   function(b) ggplot(out, aes(x=out[,1],y=out[, b])) + # variables being used for the plot
                     geom_point() + # creates the scatter plot
                     xlab("Year") + ylab(b) +     # x = year, y = input variable
                     xlim(c(input$range[1],input$range[2]))     # x range is equal to the min and max value for the slider input
      )
      grid.arrange(grobs = gl, nrow = 1)     # placing each of the plots on one page
    }
    
  })
  
  # table
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(out[out$year >= as.numeric(input$range[1]) & out$year <= as.numeric(input$range[2]), ], filter = 'top', options = list(orderClasses = TRUE))
  })     # this line says to show the data within the range of the slider input, not sure what 'filter' and 'options' do, however
  
}

shinyApp(ui, server)

