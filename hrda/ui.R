library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(DT)


# Define UI for application that draws a histogram
ui <- dashboardPage(skin="blue",
    dashboardHeader(
        title = "HR 데이터 분석"
    ),
    dashboardSidebar(
       
        sidebarMenu(
            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            menuItem("근속현황", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("상관분석", icon = icon("th"), tabName = "widgets",
                     badgeColor = "green"),
            menuItem("시각화", icon = icon("bar-chart-o"),
                     menuSubItem("Sub-item 1", tabName = "subitem1"),
                     menuSubItem("Sub-item 2", tabName = "subitem2")
            ),
            #menuItem("About", tabName = "about")
            menuItem("About", 
                     menuSubItem("프로젝트", tabName = "about"),
                     menuSubItem("raw", tabName = "raw")
            )
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(tabName = "dashboard",
                    h2("Dashboard tab content")
            ),
            
            tabItem(tabName = "widgets",
                    h2("Widgets tab content")
            ),
            tabItem(tabName = "subitem1",
                    h2("subitem1 tab content")
            ),
            
            tabItem(tabName = "subitem2",
                    h2("subitem2 tab content")
            ),
            tabItem(tabName = "about",
                    h2("개발 계획서"),
                    includeMarkdown("doc/plan.rmd")
            ),
            tabItem(tabName = "raw",
                    h2("원본 데이터"),
                    column(4,
                           selectInput("att",
                                       "퇴사여부:",
                                       c("All",
                                         unique(as.character(data$Attrition))))
                    ),
                    DT::dataTableOutput("table")
            )
        )
    )
)