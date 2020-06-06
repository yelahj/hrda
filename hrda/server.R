library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(DT)


shinyServer(function(input, output) {
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data #<- read_csv("data/dataset.csv") 

        if (input$att != "All") {
            data <- data[data$Attrition == input$att,]
        }
        data
    }))
})
