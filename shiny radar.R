####
## Shiny APP for radar plots
rm(list=ls())
library(shiny)
source("/Users/sallatoikkanen/Desktop/Reidari/github_radarplot/radarplot.R")

ui <- fluidPage(
  ## app title
  titlePanel("DC by center"),
  sidebarLayout(
  ## sidebar w checkboxes
    sidebarPanel(
      h4("Select data to be plotted"),
      selectInput("dataset", "Dataset:",
                  c("General Information"="gendata",
                    "Laboratory data" ="labdata",
                    "Post operative course" = "pocdata")),
      h4("Select centers for the graph"),
      checkboxInput("A", "A", TRUE),
      checkboxInput("B", "B", TRUE),
      checkboxInput("C", "C", TRUE),
      checkboxInput("D", "D", TRUE),
      checkboxInput("E", "E", TRUE),
      h4("Scale the graph relative to the highest value?"),
      checkboxInput("scale", "Yes", F),

      numericInput("smooth", "Smoothing parameter:", 0.6, min = 0, max = 1, step = .1,
                   width = NULL),
      
  tags$style(".well {background-color:#FFFFFF;}")
  ),
  ## plot
  mainPanel(
    tabsetPanel(
    tabPanel("Graph", plotlyOutput("plot")),
    tabPanel("Data", tableOutput("table"))
    )
#    verbatimTextOutput("event")
    ))
)

server <- function(input, output) {
  output$plot <-renderPlotly({
    cent<-c(ifelse(input$A,"A",""),
             ifelse(input$B,"B",""),
             ifelse(input$C,"C",""),
            ifelse(input$D,"D",""),
           ifelse(input$E,"E",""))
    dat<-get(input$dataset)
    sc<-ifelse(input$scale, TRUE, FALSE)
    sm<-input$smooth
    plotfun(data=dat, centres =  cent, scale=sc, smooth = sm)
  })
 output$table <- renderTable({
   dcdata<-get(input$dataset)
   as.data.frame(cbind("Organ"=rownames(dcdata), 
                                                 apply(dcdata,2,round,1)))
   })

}

shinyApp(ui, server)
