#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Some notes for Alex:
#
# Recap from 16:34 
# 1. Add elements as arguments to fluidPage().
# 2. Create reactive inputs with an *Input() function.
# 3. Display reactive results with an *Output() function.
# 4. Use the serve function to assemble inputs into outputs. 
# 
# Rules for using the server function to assemble inputs into outputs...
# 1. Save the output that you build to output$ -> output$hist for example
# 2. Build the output with a render*() function -> example is already in the code
# 3. Access input values with input$ -> input$num for example
# Create reactivity by using in INPUTS to build RENDERED OUTPUTS
#
# Reactive values notes:
# 1. Reactive values act as the data streams that flow through your app. 
# 2. The input list is a list of reactive values. The values shoe the current state of the inputs. 
# 3. You can only call a reactive value from a function that is designed to work with one
# 4. Reactive values notify. The objects created by reactive functions respond.
#
# Reactive functions:
# 1. Use a code chunk to build (and rebuild) an object...What code will the function use?
# 2. The object will respond to changes in a set of reactive values...Which reactive values
#    will the object respond to?
# You can build many commands into the braces in the render function.
library(shiny)
library(DT) #opening this package to include datatables
#examplefile

total_app <- total_year[,1:5]
names(total_app) <- paste("Var",1:5, sep="")
names(total_app)
ncol(total_year)
head(diamonds)
str(names(diamonds))
str(names(total_app))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = c(20,30)),
         conditionalPanel(
           'input.dataset === "total_app"',
           checkboxGroupInput("toshow", "Columns to show:",
                              names(total_app), selected = names(total_app))
         )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
           id = 'dataset',
           tabPanel("geysers",plotOutput("distPlot")),
           tabPanel("LCM", DT::dataTableOutput("mytable1"))
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      
      # draw the histogram with the specified number of bins
      par(mfrow=c(2,1))
      
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
      hist(x, breaks = bins, col = 'red', border = 'white')
      
      
      
   })
   
   
   #Add datatable for LCM
   output$mytable1 <- DT::renderDataTable({
     DT::datatable(total_app[, input$toshow, drop = FALSE],options = list(orderClasses = TRUE))
   })

   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

