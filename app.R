
library(shiny)
library(tidyverse)
library(dplyr)
library(data.table)
library(circlize)

myfile <- file.path("data", "CoOfficers_July2017.csv") 
DF <- read.csv(myfile, header=T, na.strings=c("NA",""," "), fileEncoding="windows-1252")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("1-degree Connectivity between Mining Companies (source: S&P Metals and Mining data"),
  # titlePanel(
  #   headerPanel( title=div("Mining Connections",
  #                          h4("1-degree connectivity based on officers and management (source: S&P Metals and Mining data)", align="left", style="bold")
  #   ))
  #   ),
    # Show a plot of the ChordDiagram
    mainPanel(
      selectizeInput(width = "150%",
        "selected", 
        "select mining companies", 
        choices = unique(DF$Company), 
        selected = c("Kaminak Gold Corporation", "Strongbow Exploration Incorporated", "K2 Gold Corporation"),
        multiple = TRUE,
        options=list(maxOptions = n)
      ),
      tags$style(type='text/css', 
                 ".selectize-dropdown-content {
                 max-height: 600px; 
                 }"
             ),
      plotOutput("plot1", width="150%", height="800px"),
      tableOutput('table')
    )
  )


server <- function(input, output) {

  ## reactive to user input
  selected <- reactive({
    input$selected
  })
  
  vct1 <- reactive({
    DF %>% filter(Company %in% selected())
  })
  
  vct2 <- reactive({
    vct2a <- as.vector(as.matrix(unique(vct1()[,2:12])))
    as.factor(vct2a[!is.na(vct2a)])
  })
  
  DF3a <<- reactive({
    colnames(DF) <- c("Company","CEO","CFO","COO","President","Dir. Acquisitions","Corp. Dev.","Dir. Asset Management","Investor Relations","Chairman","VP Exploration","Chief Geo")
    subset(DF, Reduce("|", lapply(DF, '%in%', vct2())))
  })

  ## plot 1
  output$plot1 <- renderPlot({
    DF3 <- DF3a()
    rownames(DF3) <- DF3$Company
    DF3<-DF3[,-1]
    DF4 <- split(as.matrix(DF3), 1:nrow(DF3))
    DF4 <- crossprod(table(stack(DF4)))
    rownames(DF4) <- rownames(DF3)
    colnames(DF4) <- rownames(DF3)
    DF4 <- as.data.frame(DF4)
    diag(DF4) <- 0
    indx <- !!colSums(DF4)
    DF4 <- DF4[indx,indx]

    chordDiagram(as.matrix(DF4), annotationTrack = "grid", preAllocateTracks = 2)
    circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=1)
      # circos.axis(h = "top", labels.cex = 0.1, sector.index = sector.name, track.index = 1)
    }, bg.border = NA)
  })

  output$table <- renderTable({DF3a()},
                              striped = T,
                              spacing = "xs")
}

# Run the application
shinyApp(ui = ui, server = server)