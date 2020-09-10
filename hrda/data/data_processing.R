require(tidyverse)
require(dplyr)


#detach("package:plyr", unload=TRUE) 

# wp <- paste0(getwd(), "/hrda")
# setwd(wp)

# 1. IBM HR Analysis on Kaggle에서 제공하는 IBM 데이터를 활용하여 데이터를 분석합니다.
raw <- read_csv("data/dataset.csv")

# 1-1. 종속변수 선정: 근속률을 예측하기 위해 퇴사 여부를 확인할 수 있는 Attrition을 종속변수로 선정. 
#       기존 데이터의 Y/N 을 0/1 로 변경 = Attrition ( 0 = 'no', 1 = 'yes')
#        Attrition 값을 1,0으로 변경
# raw$Attrition <- as.integer(as.character(raw$Attrition)=="Yes")
# raw$Attrition <-as.factor(raw$Attrition)
# 
# # gender 값을 1,0으로 변경
# raw$Gender <- as.integer(as.character(raw$Gender)=="Female")
# raw$Gender <-as.factor(raw$Gender)

# 
# 삭제 변수

raw <-
  raw %>%
  mutate(Department = case_when(
    Department == "Research & Development" ~ "Audit",
    Department == "Sales" ~ "Tax",
    Department == "Human Resources" ~ "CS&DA"
  )) %>% 
  mutate(DistanceFromHome = case_when(
    DistanceFromHome %in%  1:10  ~ "30분미만",
    DistanceFromHome %in%  11:20 ~ "1시간미만",
    DistanceFromHome %in%  21:30 ~ "1시간이상"
  )) %>% 
  mutate(EducationField = case_when(
    EducationField == "Marketing" ~ "인문학",
    EducationField == "Life Sciences" ~ "기술공학",
    EducationField == "Medical" ~ "상경계",
    EducationField == "Technical Degree" ~ "자연과학",
    EducationField == "Human Resources" ~ "예체능",
    EducationField == "Other" ~ "기타"
  ))  %>% 
  mutate(JobRole = case_when(
    JobRole == "Sales Executive" ~ "감사직",
    JobRole == "Research Scientist" ~ "컨설팅직",
    JobRole == "Healthcare Representative" ~ "투자자문직",
    JobRole == "Laboratory Technician" ~ "기술컨설팅직",
    JobRole == "Manufacturing Director" ~ "세무직",
    JobRole == "Human Resources" ~ "사무직",
    JobRole == "Manager" ~ "R&D",
    JobRole == "Research Director" ~ "기술직",
    JobRole == "Sales Representative" ~ "영업직"
  )) 

raw <-
  raw %>% 
  #mutate(BirthYear = 2020 - Age) %>% 
  mutate(MonthlyLeaves = sample(0:3, n(), replace = TRUE))  %>%
  mutate(OverTimeHours = ifelse(OverTime == "Yes", sample(1:52, n(), replace = TRUE), 0))


drop.cols <- c("DailyRate", "HourlyRate", "MonthlyRate", "Over18", "StockOptionLevel", "StandardHours")

raw <-
  raw %>%
  select (-drop.cols )


data <- raw

data$Department <- as.numeric(as.factor(data$Department))
data$BusinessTravel <- as.numeric(as.factor(data$BusinessTravel))
data$EducationField <- as.numeric(as.factor(data$EducationField))
data$Gender <- as.numeric(as.factor(data$Gender))
data$JobRole <- as.numeric(as.factor(data$JobRole))
data$MaritalStatus <- as.numeric(as.factor(data$MaritalStatus))
#data$OverTime <- as.numeric(as.factor(data$OverTime))
data$DistanceFromHome <- as.numeric(as.factor(data$DistanceFromHome))
#data$Over18 <- as.numeric(as.factor(data$Over18))
# data$Gender <- as.numeric(data$Gender)
# data$Gender <-as.factor(data$Gender)

data <- data  %>%
  select(-"OverTime")

# eda_data <-
#   raw %>%
#   select (-drop.cols )


# 결측치를 제거한 값을 EDA에 활용
eda_data <-
  data %>%
  select (-"EmployeeCount", -"EmployeeNumber")

eda_data <- eda_data %>%
  mutate(Attrition = ifelse(Attrition == 1,"Yes","No"))

eda_data$Attrition <- 
  as.factor(eda_data$Attrition)

#str(eda_data) 
eda_data <- mutate_if(data, is.character, as.factor)



data$Attrition <- as.numeric(as.character(data$Attrition)=="Yes")
data$Attrition <-as.factor(data$Attrition)
# gender 값을 1,0으로 변경

