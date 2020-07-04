# 
library(ggcorrplot)
library(rpart)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
# 
# 
# 0. 환경설정 ------
library(caret)
library(janitor)
library(ggridges)
library(ggthemes)
library(cowplot)
library(corrplot)
library(corrr)
library(plotly)
library(crosstalk)
library(waffle)
library(pROC)



output$ggTop5 <- renderPlot({
  
  p1 <-  ggplot(raw,aes(MonthlyIncome, fill=Attrition))+geom_density(alpha = 0.8) + theme_minimal() + scale_fill_manual(values = c("#FBB45C", "#038DB2"))
  p2 <-  ggplot(raw,aes(OverTimeHours, fill = Attrition))+geom_density(alpha = 0.8) + theme_minimal() + scale_fill_manual(values = c("#FBB45C", "#038DB2"))
  p3 <-  ggplot(raw,aes(Age, fill=Attrition))+geom_bar(alpha = 0.8) + theme_minimal() + scale_fill_manual(values = c("#FBB45C", "#038DB2"))
  p4 <-  ggplot(raw,aes(TotalWorkingYears, fill=Attrition))+geom_bar(alpha = 0.8) + theme_minimal() + scale_fill_manual(values = c("#FBB45C", "#038DB2"))
  p5 <-  ggplot(raw,aes(NumCompaniesWorked, fill = Attrition))+geom_bar(alpha = 0.8) + theme_minimal() + scale_fill_manual(values = c("#FBB45C", "#038DB2"))
  
  grid.arrange(p1, p2, p3,p4, p5, nrow = 2)
  
})  

output$rfTop <- renderPlot({
  
  rfTopPlot
  
}) 

output$xgbTop <- renderPlot({
  
  xgbTopPlot
  
}) 


output$roc_rf <- renderPlot({
  
  
  plot.roc(as.numeric(rfdata_test$Attrition),as.numeric(rftest_predict),lwd=2, type="b", print.auc=TRUE,col ="steelblue")
  
  # rocComp <- 
  # ggplot() + 
  #   geom_roc(aes(d = as.numeric(rfdata_test$Attrition), m = as.numeric(rftest_predict), color="RandomForest"), rfdata_test) + 
  #   geom_roc(aes(d = as.numeric(xgbdata_test$Attrition), m = as.numeric(xgbPredict), color="XGBoost"), xgbdata_test) + 
  #   scale_color_manual(values=c("RandomForest"="steelblue", "XGBoost"="seagreen"), 
  #                      name="ROC Curve", guide="legend") + 
  #   style_roc()
  # rocComp
  
  
}) 


output$roc_xgb <- renderPlot({
  
  
  plot.roc(as.numeric(xgbdata_test$Attrition),as.numeric(xgbPredict),lwd=2, type="b", print.auc=TRUE,col ="seagreen")
  
  
}) 


output$sc = DT::renderDataTable({
  sc <- x.scaled %>% 
    filter(data.EmployeeNumber == input$empno )
  sc
})
