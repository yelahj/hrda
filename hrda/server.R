library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(DT)
library(corrplot)
library(extrafont)
library(GGally)

shinyServer(function(input, output) {
    # Filter data based on selections
    # Plot

    
    # valueBox
    output$employee <- renderValueBox({
        valueBox(length(which(data$Attrition == "No")),"재직자수", icon = icon("credit-card"),color = "blue")
    })
    output$retirees <- renderValueBox({
        valueBox(length(which(data$Attrition == "Yes")),"퇴직자수", icon = icon("credit-card"),color = "blue")
    })
    output$refresh <- renderValueBox({
        valueBox("80%","휴가사용률", icon = icon("credit-card"),color = "blue")
    })
    output$retention <- renderValueBox({
        valueBox("60%","근속률", icon = icon("credit-card"),color = "blue")
    })
    
    # Plot
    output$round.Att <- renderPlot({
        
        pie(table(raw$Attrition), col = (colors = c("rosybrown2", "#F8766D")), radius = 1.25, labels = paste(names(round.Att), "\n", pct, "%"), cex=1.5, main = "Employee Status")
    })
    
    
    output$genderPlot <- renderPlot({
        ggplot(data = data, aes(x = Gender)) + geom_bar(fill="#F8766D") +
            labs(title = "EmployeeCount by Gender")
    })
    
    output$dpPlot <- renderPlot({
        
        ggplot(data = data, aes(x = Department, y = EmployeeCount)) + geom_col(colour="#F8766D") +
            labs(title = "EmployeeCount by Department")
    })
    
    output$ageplot <- renderPlot({
        
        ggplot(data = data, aes(x = Age)) + geom_bar(fill="#F8766D") +
            labs(title = "EmployeeCount by Age")
    })    
    
    output$gg1_1 <- renderPlot({
        da_yes <- 
            data %>%
            filter(Attrition ==  "Yes")
        
        gg1_1_1 <- da_yes %>% 
            ggplot(aes(x=Age)) + 
            geom_density(alpha = 1) +
            geom_histogram(fill="cornsilk", colour="grey", size=.2) +
            labs(title = "연령대별 퇴사자") +
            #geom_vline(aes(xintercept = mean(Age))) +
            theme_light() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트
        
        gg1_1_2 <- da_yes %>%
            filter(Gender == "Male") %>%
            ggplot(aes(x=Age)) + 
            geom_density(fill = "lightblue", alpha = 0.7) + 
            geom_vline(aes(xintercept = mean(Age))) +
            labs(title = "연령대별 퇴사자(남성)") +
            theme_minimal() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트
        
        gg1_1_3 <- da_yes %>%
            filter(Gender == "Female") %>%
            ggplot(aes(x=Age)) + 
            geom_density(fill = "tomato", alpha = 0.7) +
            geom_vline(aes(xintercept = mean(Age))) +
            labs(title = "연령대별 퇴사자(여성)") +
            theme_minimal() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트
        
        ggarrange(gg1_1_1,ggarrange(gg1_1_2, gg1_1_3),nrow = 2)
        
    })    
    output$gg1_2 <- renderPlot({
        data %>% 
            ggplot(aes(x=Education)) +
            geom_bar(aes(fill=Attrition), position = position_dodge(), alpha = 0.8) +
            labs(title = "학력별", x="학력", y="수") +
            scale_fill_manual(values=c("grey", "tomato")) + 
            theme_minimal() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트
        
    })   
    
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
    # Datatable
    output$table <- DT::renderDataTable(DT::datatable({
        data <- data #read_csv("data/dataset.csv") 
        if (input$att != "All") {
            data <- data[data$Attrition == input$att,]
        }
        data
    }))
})


