library(shiny)
library(shinythemes)
library(DT)

shinyUI(fluidPage(

    # Application title
    titlePanel("기업HR근속률과 영향을 미치는 요인"),
    # titlePanel(title=div(img(height = 65, width = 150, src="kpmglogo.jpg"))),
    navbarPage("처음",
                theme = shinythemes::shinytheme("cerulean"),  # <--- Specify theme here
                tabPanel("첫 번째 메뉴", 
                        # Create a new row for the table.
                        DT::dataTableOutput("table")
                        ),
                tabPanel("두 번째 메뉴", 

                        sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                                
                                # Input: Select the random distribution type ----
                                radioButtons("Attrition", "Attrition type:",
                                             c("Normal" = "norm",
                                               "Uniform" = "unif",
                                               "Log-normal" = "lnorm",
                                               "Exponential" = "exp")),
                                
                                # br() element to introduce extra vertical spacing ----
                                br(),
                                
                                # Input: Slider for the number of observations to generate ----
                                sliderInput("n",
                                            "Number of observations:",
                                            value = 500,
                                            min = 1,
                                            max = 1000)
                                
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                                
                                # Output: Tabset w/ plot, summary, and table ----
                                tabsetPanel(type = "tabs",
                                            tabPanel("Plot", plotOutput("plot")),
                                            tabPanel("Summary", verbatimTextOutput("summary")),
                                            tabPanel("Table", tableOutput("table"))
                                )
                                
                            )
                        )
                        
                        ),
               navbarMenu("더보기",
                          tabPanel("더보기1", 
                                   sidebarLayout(
                                       sidebarPanel(
                                           radioButtons("plotType", "Plot type",
                                                        c("Scatter"="p", "Line"="l")
                                           )
                                       ),
                                       mainPanel(
                                           plotOutput("plot")
                                       )
                                   )
                                   ),
                          tabPanel("더보기2", "더보기2 컨텐츠")
               )
    )
))
