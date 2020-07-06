

shinyServer(function(input, output) {
  # Filter data based on selections
  # Plot

  # valueBox
  output$employee <- renderValueBox({
    valueBox(length(which(raw$Attrition == "No")),"재직자수", icon = icon("user"),color = "light-blue")
  })
  output$retirees <- renderValueBox({
    valueBox(length(which(raw$Attrition == "Yes")),"퇴직자수", icon = icon("user-alt-slash"),color = "light-blue")
  })
  output$refresh <- renderValueBox({
    
    Leaves <- data %>% 
      group_by(MonthlyLeaves) %>% 
      summarise(total = n())
    
    Leaves <- Leaves %>% mutate(percent = total / sum(total)*100)
    Leavesp <- 100 - (Leaves %>% filter(MonthlyLeaves == 0) %>% select(percent))
    
    round(Leavesp,2)
    paste(round(Leavesp,2), "%")
    
    valueBox(paste(round(Leavesp,2), "%"),"휴가사용률", icon = icon("plane-departure"),color = "light-blue")
  })
  output$retention <- renderValueBox({
    valueBox("12%","인원증감률", icon = icon("user-friends"),color = "light-blue")
  })
  
  # Plot
  output$wdplot <- renderPlot({
    
    wada <- raw %>%
      select(Attrition) %>%
      filter(!is.na(Attrition)) %>%
      table() %>% sort(decreasing = TRUE)

    wada %>%
      divide_by(9.5) %>%
      waffle(rows = 9.5, color = c("#FBDE72", "#038DB2"),
             legend_pos = "bottom")   })
  
  
  output$genderPlot <- renderEcharts4r({
    if (input$Attrition != "All") {
      genraw <- raw[raw$Attrition == input$Attrition,]
    }
    
    gender2 <- table(genraw$Gender)
    gdpct <- round(gender2/sum(gender2)*100,2)
    
    gender3 <- data.frame(gdpct)
    
    gdata <- data.frame(gender3,  path = c("path://m402.001 424.743v47.257c0 4.418-3.582 8-8 8s-8-3.582-8-8v-47.257c0-36.794-29.775-66.572-66.573-66.571-17.411 0-33.208-8.87-42.258-23.728-2.299-3.773-1.103-8.695 2.67-10.994 3.772-2.3 8.695-1.103 10.994 2.67 6.123 10.051 16.812 16.051 28.594 16.051 45.637 0 82.573 36.93 82.573 82.572zm-133.47-61.948-28.53 22.087-48.508-37.555c10.471-8.411 17.242-20.645 18.339-34.54 9.573 2.818 19.697 4.337 30.17 4.337 59 0 107-48 107-107 0-.622.014-56.012-.026-56.632-.272-4.328-3.881-7.493-7.974-7.493-1.37 0-11.227 1.95-20.876-3.855-15.048-9.053-15.125-28.897-15.124-29.096.039-6.5-7.311-10.336-12.622-6.579-32.952 23.327-71.536 39.145-113.433 45.01-4.375.613-7.426 4.656-6.813 9.032.56 4 3.986 6.892 7.913 6.892 1.26 0 27.2-3.175 56.687-13.885 18.998-6.9 37.201-15.904 54.268-26.833 5.897 22.126 22.434 34.435 42.001 35.546v47.893c0 50.178-40.822 91-91 91s-91-40.822-91-91v-56.124c0-4.418-3.582-8-8-8s-8 3.582-8 8v56.125c0 42.542 24.958 79.362 61 96.595v2.11c0 18.441-14.944 33.341-33.427 33.341-10.121 0-20.093 1.831-29.526 5.437l-5.165-4.111c-40.531-32.259-48.122-89.207-20.481-130.698 11.78-17.683 18.599-38.833 18.599-61.462v-43.333c0-50.731 41.271-92.004 92-92.004h48c50.729 0 92 41.271 92 92v43.333c0 22.011 6.401 43.221 18.511 61.336 22.148 33.131 22.104 76.614-.781 109.822-2.507 3.638-1.591 8.62 2.047 11.127 3.638 2.506 8.619 1.59 11.126-2.048 26.641-38.655 26.672-89.254.909-127.793-10.345-15.475-15.813-33.609-15.813-52.444v-43.333c0-59.551-48.449-108-108-108h-48c-59.551 0-108 48.451-108 108.004v43.329c0 18.78-5.503 36.967-15.916 52.597-32.255 48.422-23.114 114.278 23.327 151.671-22.814 14.966-37.411 40.382-37.411 69.142v47.257c0 4.418 3.582 8 8 8s8-3.582 8-8v-47.257c0-36.795 29.775-66.572 66.573-66.571 5.302 0 10.54-.841 15.563-2.498l58.966 45.651c2.884 2.232 6.912 2.232 9.795 0l33.427-25.879c3.494-2.705 4.133-7.729 1.428-11.223-2.704-3.492-7.728-4.133-11.223-1.428z",
                                           "path://m402.032 424.806v47.257c0 4.418-3.582 8-8 8s-8-3.582-8-8v-47.257c0-36.795-29.775-66.572-66.573-66.571-17.411 0-33.208-8.87-42.259-23.728-2.298-3.773-1.103-8.696 2.671-10.994 3.773-2.299 8.695-1.103 10.994 2.671 6.122 10.051 16.811 16.051 28.594 16.051 45.637-.002 82.573 36.93 82.573 82.571zm-139.606-80.193c.941 4.317-1.796 8.579-6.113 9.52-21.054 4.587-42.467-.005-59.516-11.642-16.878 18.087-39.176 15.744-36.191 15.744-36.795-.001-66.573 29.773-66.573 66.571v47.257c0 4.418-3.582 8-8 8s-8-3.582-8-8v-47.257c0-45.636 36.929-82.571 82.571-82.571 18.462 0 33.429-14.875 33.429-33.342v-2.107c-34.919-16.697-59.429-51.784-60.923-92.643-14.37-3.455-25.077-16.317-25.077-31.62v-41.473c-.437-20.3 2.577-71.143 39.648-106.877 45.775-44.126 119.183-41.323 173.161-15.338 5.261 2.535 6.06 9.643 1.691 13.324 27.345 6.67 50.925 23.48 66.074 47.538.782 1.239 2.214 3.184 1.84 6.287-.232 1.931-.807 3.565-2.295 5.075-9.75 9.888-15.119 22.991-15.119 36.896v54.57c0 4.418-3.582 8-8 8s-8-3.582-8-8v-54.57c0-16.037 5.479-31.259 15.542-43.487-15.338-21.936-39.268-36.044-66.332-38.942l-14.061-1.506c-8.222-.88-9.835-12.207-2.194-15.352l6.395-2.633c-83.286-29.035-172.351 3.226-172.351 114.928v41.56c0 6.348 3.656 11.865 9 14.636v-51.863c0-30.878 25.122-56 56-56h102c30.878 0 56 25.12 56 55.997v65.503c0 69.574-67.988 122.42-137.17 102.053-.45 5.708-1.871 11.216-4.186 16.336 13.458 9.242 30.453 12.97 47.23 9.314 4.317-.94 8.579 1.797 9.52 6.114zm-22.394-43.425c50.178 0 91-40.822 91-91v-64.895c0-22.054-17.944-39.997-40-39.997h-102c-22.056 0-40 17.944-40 40v64.892c0 50.178 40.822 91 91 91zm81 137.875h-24c-4.418 0-8 3.582-8 8s3.582 8 8 8h24c4.418 0 8-3.582 8-8s-3.582-8-8-8z"))
    
    
    gdata %>% 
      e_charts(Var1) %>% 
      e_x_axis(splitLine=list(show = FALSE), 
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel= list(show=FALSE)) %>%
      e_y_axis(max=100, 
               splitLine=list(show = FALSE),
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel=list(show=FALSE)) %>%
      e_color(color = c('#69cce6','#A9A9A9')) %>%
      e_pictorial(Freq, symbol = path, z=10, name= '비율', 
                  symbolBoundingData= 100, symbolClip= TRUE) %>% 
      e_pictorial(Freq, symbol = path, name= '전체', 
                  symbolBoundingData= 100) %>% 
      e_labels(position = "bottom", offset= c(0, 10), 
               textStyle =list(fontSize= 20, fontFamily= 'Arial', 
                               fontWeight ='bold', 
                               color= '#69cce6'),
               formatter="{@[1]}%") %>%
      e_legend(show = TRUE) %>%
      e_theme("westeros")
  })
  
  output$edplot <- renderEcharts4r({
    
    if (input$Attrition != "All") {
      raw <- raw[raw$Attrition == input$Attrition,]
    }
    
    Edu <- raw %>% 
      group_by(EducationField) %>% 
      summarise(EmployeeCount = sum(EmployeeCount)) %>% 
      ungroup() %>% 
      arrange(desc(EmployeeCount))
    
    
    Edu2 <- data.frame(
      x= Edu$EducationField,
      y= Edu$EmployeeCount
    )
    
    Edu2 %>% 
      e_charts(x) %>% 
      e_pictorial(y, symbol = ea_icons("user"), 
                  symbolRepeat = TRUE, z = -1,
                  symbolSize = c(20, 20)) %>% 
      e_theme("westeros") %>%
      e_flip_coords() %>%
      e_legend(show = FALSE) %>%
      e_x_axis(splitLine=list(show = FALSE)) %>%
      e_y_axis(splitLine=list(show = FALSE)) %>%
      e_labels(fontSize = 14, fontWeight ='bold', position = "right", offset=c(10, 0))
    
  })
  
  output$jrfig <- renderPlotly({
    if (input$Attrition != "All") {
      raw <- raw[raw$Attrition == input$Attrition,]
    }
    
    colors = brewer.pal(8, "PuBu")
    
    jrda <- raw
    jrda <- jrda %>% group_by(JobRole)
    jrda <- jrda %>% summarize(count = n())
    jrda <- jrda %>% plot_ly(labels = ~JobRole, values = ~count, marker=list(colors = colors))
    jrda <- jrda %>% add_pie(hole = 0.6)
    jrda <- jrda %>% layout(legend = list(orientation = 'h', xanchor="center", x = 0.5),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  
  output$dpPlot <- renderPlot({
    if (input$Attrition != "All") {
      raw <- raw[raw$Attrition == input$Attrition,]
    }
    ggplot(data = raw, aes(x = Department, y = EmployeeCount)) + geom_col(colour="#3690C0") +
      coord_cartesian(xlim = c(),  ylim = c(0,1000)) + theme_minimal()
  })
  
  output$ageplot <- renderPlot({
    if (input$Attrition != "All") {
      raw <- raw[raw$Attrition == input$Attrition,]
    }
    ggplot(data = raw, aes(x = Age)) + geom_bar(fill="#FBDE72") +
      coord_cartesian(xlim = c(),  ylim = c(0,80)) + theme_minimal()
  })    
  
  output$figplot <- renderPlotly({
    
    if (input$Attrition != "All") {
      raw <- raw[raw$Attrition == input$Attrition,]
    }
    
    
    dp <- raw %>%
      group_by(Department, Age, Gender) %>%
      summarise(EmployeeCount = sum(EmployeeCount)) %>%
      ungroup() %>%
      arrange(desc(EmployeeCount))
    
    fig2 <- plot_ly(dp, x = ~Department, y = ~Age, text = ~Gender, type = 'scatter', mode = 'markers',
                    marker = list(size = ~EmployeeCount, opacity = 0.5))
    
    fig2 <- fig2 %>% layout(xaxis = list(showgrid = FALSE),
                            yaxis = list(showgrid = FALSE))
    
  })
  
  output$marplot <- renderPlot({
    
    if (input$Attrition != "All") {
      raw <- raw[raw$Attrition == input$Attrition,]
    }
    
    Marital <- raw %>% 
      group_by(MaritalStatus) %>% 
      summarize(count = n()) %>% 
      arrange(desc(count))
    
    
    Marital %>%
      filter(!is.na(count)) %>%
      arrange(count) %>%
      tail(6) %>%
      mutate(MaritalStatus=factor(MaritalStatus, MaritalStatus)) %>%
      ggplot( aes(x=MaritalStatus, y=count) ) +
      geom_bar(fill="#87cefa", stat="identity") +
      geom_text(hjust = 1, size = 3, aes( y = 0, label = paste(MaritalStatus," "))) +
      theme_ipsum() +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        axis.text = element_blank()
      ) +
      xlab("") +
      ylab("") +
      coord_polar(theta = "y") +
      ylim(0,900) 
    
    
  })
  
  # output$gg1_1 <- renderPlot({
  #   da_yes <-
  #     data %>%
  #     filter(Attrition ==  "Yes")
  # 
  #   gg1_1_1 <- da_yes %>%
  #     ggplot(aes(x=Age)) +
  #     geom_density(alpha = 1) +
  #     geom_histogram(fill="cornsilk", colour="grey", size=.2) +
  #     labs(title = "연령대별 퇴사자") +
  #     #geom_vline(aes(xintercept = mean(Age))) +
  #     theme_light() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트
  # 
  #   gg1_1_2 <- da_yes %>%
  #     filter(Gender == "Male") %>%
  #     ggplot(aes(x=Age)) +
  #     geom_density(fill = "lightblue", alpha = 0.7) +
  #     geom_vline(aes(xintercept = mean(Age))) +
  #     labs(title = "연령대별 퇴사자(남성)") +
  #     theme_minimal() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트
  # 
  #   gg1_1_3 <- da_yes %>%
  #     filter(Gender == "Female") %>%
  #     ggplot(aes(x=Age)) +
  #     geom_density(fill = "tomato", alpha = 0.7) +
  #     geom_vline(aes(xintercept = mean(Age))) +
  #     labs(title = "연령대별 퇴사자(여성)") +
  #     theme_minimal() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트
  # 
  #   ggarrange(gg1_1_1,ggarrange(gg1_1_2, gg1_1_3),nrow = 2)
  # 
  # })
  # output$gg1_2 <- renderPlot({
  #   data %>%
  #     ggplot(aes(x=Education)) +
  #     geom_bar(aes(fill=Attrition), position = position_dodge(), alpha = 0.8) +
  #     labs(title = "학력별", x="학력", y="수") +
  #     scale_fill_manual(values=c("grey", "tomato")) +
  #     theme_minimal() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트
  # 
  # })
  # 
  output$gg2_1 <- renderPlot({

    data %>% select(Department, MonthlyIncome, Attrition) %>% group_by(Attrition, Department) %>%
      summarize(avg.inc=mean(MonthlyIncome)) %>%
      ggplot(aes(x=reorder(Department, avg.inc), y=avg.inc, fill=Attrition)) + geom_bar(stat="identity", position="dodge", alpha = 0.8) + facet_wrap(~Attrition) +
      theme_minimal() + theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.8)) +
      scale_fill_manual(values=c("grey", "tomato")) +
      labs(y="평균임금", x="부서", title="퇴사/재직자 별 부서별 임금비교") +
      geom_text(aes(x=Department, label= round(avg.inc,2)),
                hjust=0.5, vjust=1, size=4) +
      theme_minimal() +
      theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트
  })

  output$gg2_2 <- renderPlot({
    ggplot(data,
           aes(x = OverTime, group = Attrition)) +
      geom_bar(aes(y = ..prop.., fill = factor(..x..)),
               stat="count",
               alpha = 0.8) +
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),
                stat= "count",
                vjust = -.5) +
      labs(y = "Percentage", fill= "OverTime") +
      facet_grid(~Attrition) +
      scale_fill_manual(values = c("grey","tomato")) +
      theme_minimal() +
      theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
      theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트

  })
  
  output$ggTop5 <- renderPlot({
    
    source('Visualization.R',local=TRUE, encoding="utf-8")
    
  })    
  
  
  output$rfTop <- renderPlot({
    
    source('Visualization.R',local=TRUE, encoding="utf-8")
    
  }) 
  
  output$xgbTop <- renderPlot({
    
    source('Visualization.R',local=TRUE, encoding="utf-8")
    
  }) 
  
  
  selectedData1 <- reactive({
    raw %>%
      filter(raw$EmployeeNumber == gsub("[[:space:]]*$","",gsub("- .*",'',input$empno)))
  })
  
  output$radar <- renderPlotly({
    
    input$submit
    input$empno
    
    #if (input$empno != "") {}
    
    sc <- x.scaled %>% 
      filter(result.EmployeeNumber == input$empno )

    sc <-
      sc %>% select(-"result.EmployeeNumber", -"EmployeeNumber")      
    
    sc.pivot <- gather(sc, var, value) 
    
    fig <- plot_ly(
      type = 'scatterpolar',
      r = sc.pivot$value,
      theta = sc.pivot$var,
      fill = 'toself'
    ) 
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,5)
          )
        ),
        showlegend = T
      )
    
    fig
    
  })  
    
  
  output$attritionrate <- renderValueBox({
    input$empno
    
    prob_att <- 
      result %>%
      filter(EmployeeNumber == input$empno ) %>%
      select(.pred_0) * 100
    
    
    if (is.null(prob_att)) {
      prob_att = 0
    } 
    
    valueBox(paste0(prob_att,"%"),"근속가능성", icon = icon("user"),color = "light-blue")
  })   
  
  output$sc <- renderFormattable({
    input$submit
    input$empno
    
    sc <- x.scaled %>% 
      filter(result.EmployeeNumber == input$empno )

    sc <-
      sc %>% select(-"result.EmployeeNumber", -"EmployeeNumber")    
        
    #library(tidyr)
    sc.pivot <- gather(sc, var, value)
    formattable(sc.pivot, list())
  })
  
  output$sc_tb <- renderTable({
    input$submit
    input$empno
    
    sc_tb <- result %>% 
      select("EmployeeNumber", top_ten_variable_v) %>%
      filter(EmployeeNumber == input$empno )
    
    sc_tb <- gather(sc_tb, 변수, 점수)
    formattable(sc_tb, list())
  })
  
  # Heatmap
  output$gg1_3 <- renderPlot({
    
    str(eda_data)
    
    # # 각 변수별 상관관계
    options(repr.plot.width=30, repr.plot.height=30)

    nums <- select_if(eda_data, is.numeric)

    corr <- round(cor(nums), 1)
    
    # ggcorrplot(corr, hc.order = TRUE, type = "lower",
    #            outline.col = "white",
    #            ggtheme = ggplot2::theme_minimal(),
    #            colors = c("#FBDE72", "white", "#038DB2"))
    
    ggcorr(corr,  nbreaks = 3,  color= "grey50", hjust = 1)

  })
  
  # Datatable
  output$table <- DT::renderDataTable(DT::datatable({
    raw <- raw #read_csv("data/dataset.csv") 
    if (input$att != "All") {
      raw <- data[raw$Attrition == input$att,]
    }
    raw
  }))
})

