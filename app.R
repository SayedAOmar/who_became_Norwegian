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
    tags$img(src='https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Flag_of_Norway.svg/2000px-Flag_of_Norway.svg.png',height='254', class="logo_bottom"),
    tags$style(type='text/css', ".logo_bottom { position:fixed;bottom:0px;left:0px;width:350px; }")
  ),
  dashboardBody(
    fluidRow(
      tags$h3("You have to choose the countries to plot it ")
    ),
    fluidRow(
      style = "background-color:white;",
      height= 450,
      dygraphOutput("time_series_graph", height = 450)
    ),
    fluidRow(
      tags$h3("This data imported from dasdksn and developed by : "),
      box(
        width = 6,
        tags$h2("Sherif Ahmed"),
        tags$h4("abdjasbdj@nfjds.com")
      ),
      box(
        width = 6,
        tags$h2("Sayed A. Omar"),
        tags$h4("devsayed1@gmail.com")
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


