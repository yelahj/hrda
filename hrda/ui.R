

# Define UI for application that draws a histogram
ui <- dashboardPage(skin="blue",
                    dashboardHeader(
                        title = "HR 데이터 분석"
                    ),
                    dashboardSidebar(
                        
                        sidebarMenu(
                            id = "tabs",
                            menuItem("법인현황", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("상관분석", icon = icon("th"), tabName = "widgets",
                                     badgeColor = "green"),
                            menuItem("시각화", icon = icon("bar-chart-o"),
                                     menuSubItem("근속률 예측", tabName = "visitem2")
                            ),
                            #menuItem("About", tabName = "about")
                            menuItem("About", 
                                     menuSubItem("프로젝트", tabName = "about"),
                                     menuSubItem("원본데이터", tabName = "raw")
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
                                        id = "2", width = "800",
                                        tabPanel(
                                            fluidRow(
                                                # selectInput
                                                selectInput("Attrition",label = "퇴사여부", choices = c("All","Yes","No"), selected = "All"),
                                                
                                                box( 
                                                    title = "재직상태" 
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
                                                    title = "전공" 
                                                    ,status = "primary" 
                                                    ,width = 3
                                                    ,solidHeader = TRUE  
                                                    ,collapsible = TRUE  
                                                    ,echarts4rOutput("edplot")),
                                                
                                                
                                                box( 
                                                    title = "직무" 
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
                                                    title = "부서별 성별/연령 분포도" 
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
                                        id = "tabset1", width="300", height="1000",
                                        tabPanel("변수 살펴보기", 
                                                 h3("각 요인별 영향력"),
                                                 p("Heatmap을 통해 살펴본 Correlation"),
                                                 fluidRow(
                                                     column(10, plotOutput("gg1_3"))
                                                 ),
                                                 ),
                                        tabPanel("모델비교", 
                                                 h3("* Random Forest & XGBoost & XGBoost(Class Imbalance) 통해 살펴본 중요도 변수"),
                                                 h4("각각의 모델 별로 상위요인이 상이함을 확인 가능"),
                                                 fluidRow(
                                                   
                                                   column(5, plotOutput("rfTop")),
                                                   column(5, plotOutput("xgbTop")),
                                                   column(5, plotOutput("xgbNewTop")),
                                                   
                                                 ), height=1200),
                                        tabPanel("ROC비교 & Feature Selection",
                                                 h3("ROC"),
                                                 p("RandomForest vs XGBoost"),
                                                 fluidRow(

                                                   column(5, plotOutput("roc_rf")),
                                                   column(5, plotOutput("roc_xgb")),
                                                   column(5, plotOutput("roc_imb"))
                                                 )),
                                        tabPanel("모델링 채택변수",
                                                 h3("XGBoost 상위변수 TOP 10"),
                                                 fluidRow(
                                                   column(10, plotOutput("ggTop5", width=950, height=800)) 
                                                 )
                                                 )
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
                                          choices = unique(as.numeric(x.scaled$NewResult.EmployeeNumber))
                                        ),
                                        # actionButton("submit", "조회"),
                                        
                                        hr(),
                                        
                                    ),
                                    
                                    box(width = 10,
                                            column(width = 10,
                                              valueBoxOutput("attritionrate", width = 12),
                                              column(12, plotlyOutput("radar")),
                                              column(12, tableOutput("sc_tb1")),
                                              column(8, tableOutput("sc_tb2")),
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
