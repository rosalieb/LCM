library(shiny)
library(ggplot2)  # for the diamonds dataset

total_app <- total_year[,1:5]
names(total_app) <- paste("Var",1:5, sep="")
names(total_app)
ncol(total_year)
head(diamonds)
str(names(diamonds))
str(names(total_app))


ui <- fluidPage(
  title = "LCM Data by Station",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "total_app"',
<<<<<<< HEAD
        checkboxGroupInput("toshow", "Columns in LCM to show:",
        choiceNames =
          list(toshow(names(total_app))),
        choiceValues =
          list(names(total_app))
=======
        checkboxGroupInput("toshow", "Columns to show:",
                           names(total_app), selected = names(total_app))
>>>>>>> 788d24d1e54b77ed0306f3c52a44112eccaa93c8
      ),
      conditionalPanel(
        'input.dataset === "mtcars"',
        helpText("Display 5 records by default.")
      ),
      conditionalPanel(
        'input.dataset === "iris"',
        helpText("Display 5 records by default.")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("LCM", DT::dataTableOutput("mytable1")),
        tabPanel("mtcars", DT::dataTableOutput("mytable2")),
        tabPanel("iris", DT::dataTableOutput("mytable3"))
      )
    )
  )
)

server <- function(input, output) {
  
  # choose columns to display
  total_app2 = total_app#[sample(nrow(total_app), 100), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(total_app2[, input$toshow, drop = FALSE],options = list(orderClasses = TRUE))
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(mtcars, options = list(orderClasses = TRUE))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
}

shinyApp(ui, server)
