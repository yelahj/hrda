library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(DT)
library(extrafont)
library(GGally)
library(plotly)

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
                     menuSubItem("모델링", tabName = "visitem1"),
                     menuSubItem("예측", tabName = "visitem2")
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
                    fluidRow(
                        
                        valueBoxOutput("employee", width = 3),                       
                        
                        valueBoxOutput("retirees", width = 3),                       
                        
                        valueBoxOutput("refresh", width = 3),
                        
                        valueBoxOutput("retention", width = 3),
                        
                        
                    ),
                    fluidRow(
                        column(6, plotOutput("round.Att")),
                        column(6, plotOutput("genderPlot")),
                    ),
                    
                    fluidRow(                    
                        column(6, plotOutput("dpPlot")),
                        column(6, plotOutput("ageplot"))
                    )
            ),
            
            tabItem(tabName = "widgets",
                    tabBox(
                        title = "EDA",
                        id = "tabset1", width="300",
                        tabPanel("1. 인구통계학적 특성에 따른 현황", 
                                 h2("인구통계학적 특성에 따른 현황 (성별, 교육 등 개인적인 요인들) "),
                                 h3("1-1. 각 성별 연령대 분포도 (평균)"),
                                 p("퇴사율이 가장 높은 연령대는 ____이며, __성이 __성보다 퇴사율이 높습니다."),
                                 fluidRow(
                                     column(10, plotOutput("gg1_1"))
                                 ),
                                 h3("1-2. 학력별 퇴사율"),
                                 p("퇴사율이 가장 높은 학력은 ____입니다."),
                                 fluidRow(
                                     column(10, plotOutput("gg1_2"))
                                 )),
                        tabPanel("2. 소속 집단에 따른 현황", 
                                 h2("소속 집단에 따른 현황 (사내 환경적 요인)"),
                                 h3("2-1. 부서별 임금별 퇴사율"),
                                 p("퇴사자의 부서별 평균 임금은 ", "____","입니다. "),
                                 fluidRow(
                                     column(10, plotOutput("gg2_1"))
                                 ),
                                 h3("2-2. 추가근무 현황별 퇴사율"),
                                 p("퇴사자들의 추가근무가 근속자의 추가근무보다 __습니다."),
                                 fluidRow(
                                     column(10, plotOutput("gg2_2"))
                                 )),
                        tabPanel("3. 만족도에 따른 현황",
                                 h2("만족도에 따른 현황 (주관적요소)"),
                                 h3("3-1. 역할(Role) 별 만족도 (업무, 환경)"),
                                 fluidRow(
                                     
                                 ),
                                 h3("3-2. 만족도에 따른 퇴사율"),
                                 fluidRow(
                                     
                                 )),
                        tabPanel("4. 라이프 스타일에 따른 현황", 
                                 h2("라이프 스타일에 따른 현황"),
                                 h3("4-1. 부서별 Work Life Balance"),
                                 fluidRow(
                                 ))
                    )
            ),
            tabItem(tabName = "visitem1",
                    h2("Random Forest"),
                    includeMarkdown("doc/RandomForest.rmd")
            ),
            
            tabItem(tabName = "visitem2",
                    h2("visitem2 tab content"),
                    sidebarPanel(width = 4,
                                 textInput("text", label = h3("사번 입력"), ),
                                 checkboxGroupInput(inputId = "EmpNo",
                                                     label = '요인:', choices = c("MonthlyIncome" = "MonthlyIncome", 
                                                                                "OverTimeHours" = "OverTimeHours",
                                                                                "TotalWorkingYears"="TotalWorkingYears",
                                                                                "BirthYear"="BirthYear","Age"="Age"
                                                     ),
                                                     selected = c("MonthlyIncome"="MonthlyIncome"),inline=TRUE),
                                 submitButton("조회")
                                 ),
                    mainPanel(
                        column(8, plotlyOutput("plot1", width = 800, height=700),
                               p("..."))
                    )
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