

# Define UI for application that draws a histogram
ui <- dashboardPage(skin="blue",
                    dashboardHeader(
                        title = "HR 데이터 분석"
                    ),
                    dashboardSidebar(
                        
                        sidebarMenu(
                            # Setting id makes input$tabs give the tabName of currently-selected tab
                            id = "tabs",
                            menuItem("법인현황", tabName = "dashboard", icon = icon("dashboard")),
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
                        tags$style(HTML("

                    .box.box-solid.box-primary>.box-header {
                    color:#000;
                    background:#fefefe
                    }

                    ")),
                        tabItems(
                            tabItem(tabName = "dashboard",
                                    
                                    fluidRow(
                                        
                                        valueBoxOutput("employee", width = 3),                       
                                        
                                        valueBoxOutput("retirees", width = 3),                       
                                        
                                        valueBoxOutput("refresh", width = 3),
                                        
                                        valueBoxOutput("retention", width = 3),
                                        
                                        
                                    ),
                                    tabBox(
                                        title = "1",
                                        id = "2", width = "800",
                                        tabPanel(
                                            fluidRow(
                                                # selectInput
                                                selectInput("Attrition",label = "퇴사여부", choices = c("All","Yes","No"), selected = "All"),
                                                
                                                box( 
                                                    title = "근속상태" 
                                                    ,status = "primary" 
                                                    ,width = 3
                                                    ,solidHeader = TRUE  
                                                    ,collapsible = TRUE  
                                                    ,plotOutput("wdplot")),
                                                
                                                
                                                box( 
                                                    title = "성별" 
                                                    ,status = "primary" 
                                                    ,width = 3
                                                    ,solidHeader = TRUE  
                                                    ,collapsible = TRUE  
                                                    ,echarts4rOutput("genderPlot")),
                                                
                                                box( 
                                                    title = "전공분야" 
                                                    ,status = "primary" 
                                                    ,width = 3
                                                    ,solidHeader = TRUE  
                                                    ,collapsible = TRUE  
                                                    ,echarts4rOutput("edplot")),
                                                
                                                
                                                box( 
                                                    title = "업무구분" 
                                                    ,status = "primary" 
                                                    ,width = 3
                                                    ,solidHeader = TRUE  
                                                    ,collapsible = TRUE  
                                                    ,plotlyOutput("jrfig")),
                                                
                                                
                                                box( 
                                                    title = "부서" 
                                                    ,status = "primary" 
                                                    ,width = 3
                                                    ,solidHeader = TRUE  
                                                    ,collapsible = TRUE  
                                                    ,plotOutput("dpPlot")),
                                                
                                                box( 
                                                    title = "연령" 
                                                    ,status = "primary" 
                                                    ,width = 3
                                                    ,solidHeader = TRUE  
                                                    ,collapsible = TRUE  
                                                    ,plotOutput("ageplot")),
                                                
                                                
                                                box( 
                                                    title = "연령분포도" 
                                                    ,status = "primary" 
                                                    ,width = 3
                                                    ,solidHeader = TRUE  
                                                    ,collapsible = TRUE
                                                    ,plotlyOutput("figplot")),
                                                
                                                
                                                box( 
                                                    title = "결혼상태" 
                                                    ,status = "primary" 
                                                    ,width = 3
                                                    ,solidHeader = TRUE  
                                                    ,collapsible = TRUE
                                                    ,plotOutput("marplot")),
                                                
                                            )
                                        )
                                    )
                            ),
                            tabItem(tabName = "widgets",
                                    tabBox(
                                        title = "Explatory Data Analysis",
                                        id = "tabset1", width="300",
                                        tabPanel("변수 살펴보기", 
                                                 h3("각 요인별 영향력"),
                                                 p("Heatmap을 통해 살펴본 Correlation"),
                                                 fluidRow(
                                                     column(10, plotOutput("gg1_3"))
                                                 ),
                                                 ),
                                        tabPanel("Random Forest & XGBoost", 
                                                 h3("1. Random Forest를 통해 살펴본 중요도 변수"),
                                                 p("상위 요인"),
                                                 fluidRow(
                                                   column(10, plotOutput("rfTop"))
                                                 ),
                                                 h3("2. XGBoost를 통해 살펴본 중요도 변수"),
                                                 p("상위 요인"),
                                                 fluidRow(
                                                  column(10, plotOutput("xgbTop"))
                                                 )),
                                        tabPanel("ROC비교 & Feature Selection",
                                                 h3("ROC"),
                                                 fluidRow(
                                                   column(5, plotOutput("roc_rf")),
                                                   column(5, plotOutput("roc_xgb")),
                                                 ),
                                                 h3("모델링 채택변수"),
                                                 fluidRow(
                                                   column(10, plotOutput("ggTop5"))
                                                 ))
                                    )
                            ),
                            tabItem(tabName = "visitem1",
                                    h2("Random Forest"),
                                    includeMarkdown("doc/RandomForest.rmd")
                            ),
                            tabItem(tabName = "visitem2",
                                    h2("개별 데이터 확인"),
                                    box(width = 2,
                                        # numericInput("empno", "사번을 입력하세요.", 0,
                                        #              min = 1, max = 10000),
                                        selectInput(
                                          "empno",
                                          label = h3("사번을 선택하세요."),
                                          choices = unique(as.character(x.scaled$result.EmployeeNumber))
                                        ),
                                        # actionButton("submit", "조회"),
                                        
                                        hr(),
                                        
                                    ),
                                    
                                    box(width = 10,
                                            column(width = 10,
                                              valueBoxOutput("attritionrate", width = 12),
                                              column(12, plotlyOutput("radar")),
                                              column(12, tableOutput("sc_tb"))
                                              #column(10, box(tableOutput("sc")))
                                            )
                                        
                                    )),
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
                                                         #unique(as.character(data$Attrition)
                                                         c("Yes" = "Yes",
                                                           "No" = "No")))
                                    ),
                                    DT::dataTableOutput("table")
                            )
                        )
                    )
)
