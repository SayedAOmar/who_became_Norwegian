#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
## Not run: 
## Only run examples in interactive R sessions
library(shiny)
library(data.table)
library(shinyWidgets)
library(xts)
library(reshape2)
library(lubridate)
library(dygraphs)
library(shinydashboard)

t_norway <<- read.csv('t_norway.csv')


ui <- dashboardPage(
  dashboardHeader(
    title = "Who became Norwegian",
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 350,
    fluidRow(
      box(
        width = 12,
        style = "color:red",
        uiOutput("select_country")
      )
    ),
    fluidRow(
      box(
        width=12,
        style = "color:red",
        uiOutput("select_range")

      ),
      box(
          width= 12,
          style = "color:red",
          actionButton("submit", "Submit",style="color:white",class="btn-danger btn-large")
      )
    ),
    tags$img(src='https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Flag_of_Norway.svg/2000px-Flag_of_Norway.svg.png', class="logo_bottom"),
    tags$style(type='text/css', ".logo_bottom { position:fixed;bottom:0px;left:0px;width:350px;height:20% }")
  ),
  dashboardBody(
    
    fluidRow(
      style = "margin-left: 20px;margin-right:20px;font-style: italic;font-family: Sans-serif;",
      tags$h4(" Do you wonder about how many people from a specific country have been naturalized and get the norwegian citizenship? choose country/ countries from the side bar and range of years, then click submit, you will see a time series graph showing the number of people nataulrized in the selected range. ")
    ),
    fluidRow(
      style = "background-color:white;margin-left: 20px;margin-right:20px;",
      height= 450,
      dygraphOutput("time_series_graph", height = 450)
    ),
    fluidRow(
      style = "margin-left: 20px;margin-right:20px;font-style:italic;",
      tags$h4("Data has been imported from Norwegian statistics bureau. "),
      tags$h4("Presented by: ")
    ),
    fluidRow(
      style = "margin-left: 5px;margin-right:5px;",
      box(
        width = 6,
        tags$a(href="https://www.linkedin.com/in/sherifahmed7/",target="_blank", "Sherif A. Rabie",style="font-size:25px;"),
        tags$h5("sherif_ahmed@foc.cu.edu.eg")
      ),
      box(
        width = 6,
        tags$a(href="https://www.linkedin.com/in/sayedomar/",target="_blank", "Sayed A. Omar",style="font-size:25px;"),
        tags$h5("devsayed1@gmail.com")
      )
    )
  )
  ,skin="red"
)



server <- function(input, output, session) {
  output$select_country <- renderUI({
    selectInput("Country", "Select Country",colnames(t_norway), selected = NULL, multiple = TRUE,selectize = TRUE, width = NULL, size = NULL)
  })
  output$select_range <-renderUI({
    sliderInput("range",label = "Select range of years:",min = min(t_norway$year), max = max(t_norway$year), value = c(min(t_norway$year), max(t_norway$year)),step=1)
  })
  observeEvent(input$submit, {
    range             <- input$range
    min_range         <- range[[1]]
    max_range         <- range[[2]]
    countries         <- input$Country
    new_ds <<- t_norway[t_norway$year>min_range & t_norway$year < max_range,c("year",countries)]
    new_ds[] <- lapply(new_ds, function(x) {
       if(is.character(x)) as.numeric(as.character(x)) else x
     })
     sapply(new_ds, class)
     new_ds$year = as.Date(as.character(new_ds$year), format="%Y")
     s=   as.xts(new_ds[, !(colnames(new_ds) == "year")],order.by=new_ds$year)
     output$time_series_graph <- renderDygraph({

      dygraph(s)%>%
      dyRangeSelector()

   })
  })
}
shinyApp(ui = ui, server = server)


