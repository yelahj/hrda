# Library
library(fmsb)
library(tidyverse)
library(scales)
library(data.table)
library(plotly)


output$sc = DT::renderDataTable({
  sc <- x.scaled %>% 
    filter(data.EmployeeNumber == input$empno )
  sc
})

output$gg2_1 <- renderPlot({
  #remove(x.scaled)
  # 데이터 정규화...
  
  radar <- as.data.frame(matrix( sample( 1:10 , 10 , replace=T) , ncol=10))
  colnames <- colnames(x.scaled)
  #names(x.scaled) <- colnames
  
  


  
  # radar <- rbind(rep(20,10) , rep(10,10) , sc)
  # 
  # colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
  # colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  # 
  # # plot with default options:
  # radarchart( radar, axistype=1 ,
  #             pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
  #             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1,6,1), cglwd=0.8,
  #             vlcex=0.8
  # )
  # 
  # # Add a legend
  # legend(x=0.7, y=1, legend = rownames(radar[-c(1,2),]), bty = "n", pch=5 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
  # 
  # radar
})



# #########################################
fig <- plot_ly(
  type = 'scatterpolar',
  mode = 'lines',
  fill = 'toself')

fig <- fig %>%
  add_trace(
    r = sc
    ,theta = colnames(sc),
    fill = 'toself'
  )
fig


# fig <- plot_ly(
#   type = 'scatterpolar',
#   mode = "closest",
#   #fill = 'toself',
#   r = x.scaled %>%
#     filter(EmployeeNumber == 153) %>%
#     select('MonthlyIncome','OverTimeHours','TotalWorkingYears', 'BirthYear', 'Age'),
#   theta = c('range','임금','초과근무','근속년수', '출생년도', '나이')
# 
# )
# fig <- fig %>%
#   layout(
#     polar = list(
#       radialaxis = list(
#         visible = T,
#         range = c(0,5)
#       )
#     ),
#     showlegend = T
#   )
# 
# fig


