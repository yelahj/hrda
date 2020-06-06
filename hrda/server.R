library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(DT)


shinyServer(function(input, output) {
    # Filter data based on selections
    # Plot
    output$genderPlot <- renderPlot({
        data <- data #read_csv("data/dataset.csv") 
        
        ggplot(data = raw, aes(x = Gender)) + geom_bar(fill="#F8766D") +
            labs(title = "EmployeeCount by Gender")
        })
    output$dpPlot <- renderPlot({
        data <- data #read_csv("data/dataset.csv") 
        
        ggplot(data = raw, aes(x = Department, y = EmployeeCount)) + geom_col(colour="#F8766D") +
            labs(title = "EmployeeCount by Department")
    })
    # Datatable
    output$table <- DT::renderDataTable(DT::datatable({
        data <- data #read_csv("data/dataset.csv") 
        
        if (input$att != "All") {
            data <- data[data$Attrition == input$att,]
        }
        data
    }))
})


