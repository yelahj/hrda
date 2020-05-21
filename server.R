#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
         hist(x, breaks = bins, col = 'orange', border = 'white')

         
         data <- read_csv("main/data/dataset.csv")
         
         
    })
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- read_csv("data/dataset.csv")
    }))
    
    output$plot <- renderPlot({
        plot(data$MonthlyIncome, type=input$plotType)
    })
    
    output$summary <- renderPrint({
        summary(data$Attrition)
    })

    
})
