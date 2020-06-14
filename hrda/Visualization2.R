
library(scales)
library(data.table)
library(plotly)

# 데이터 정규화...
x.scaled <-

data %>% 
  #select('MonthlyIncome','OverTimeHours','TotalWorkingYears', 'BirthYear', 'Age') %>% 
  mutate(rescale(data$MonthlyIncome,  to = c(1, 5)) ) %>%
  mutate(rescale(data$OverTimeHours, to = c(1, 5))) %>%
  mutate(rescale(data$TotalWorkingYears, to = c(1, 5))) %>%
  mutate(rescale(data$BirthYear, to = c(1, 5))) %>%
  mutate(rescale(data$Age, to = c(1, 5)))


x.scaled <- data.frame(
    rescale(data$MonthlyIncome,  to = c(1, 5)) ,
    rescale(data$OverTimeHours, to = c(1, 5)),
    rescale(data$TotalWorkingYears, to = c(1, 5)),
    rescale(data$BirthYear, to = c(1, 5)),
    rescale(data$Age, to = c(1, 5)),
    data$EmployeeNumber
  ) 
  colnames <- c("MonthlyIncome", "OverTimeHours","TotalWorkingYears","BirthYear","Age", "EmployeeNumber")
  
  names(x.scaled) <- colnames
  
  head(x.scaled)
  
scale(x)
x <- data
x<- (x - mean(data$MonthlyIncome)) / sd(data$MonthlyIncome)  #drop target varaible to avoid its scaling
x
x.scaled <- scale(x)

fig <- plot_ly(
  type = 'scatterpolar',
  mode = 'lines',
  fill = 'toself')

fig <- fig %>%
  add_trace(
    r = x.scaled %>% 
      filter(EmployeeNumber == '10') %>%
      select('MonthlyIncome','OverTimeHours','TotalWorkingYears', 'BirthYear', 'Age','MonthlyIncome'),
    theta = c('월급','Overtime','근속년수', '출생년도', '나이','월급'),
    fill = 'toself'
  ) 
fig 


fig <- plot_ly(
  type = 'scatterpolar',
  mode = "closest",
  #fill = 'toself',
  r = x.scaled %>% 
    filter(EmployeeNumber == 153) %>%
    select('MonthlyIncome','OverTimeHours','TotalWorkingYears', 'BirthYear', 'Age'),
  theta = c('range','임금','Overtime','근속년수', '출생년도', '나이')
  
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


