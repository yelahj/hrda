library(tidyverse)
library(dplyr)

# 1. IBM HR Analysis on Kaggle에서 제공하는 IBM 데이터를 활용하여 데이터를 분석합니다.
raw <- read_csv("hrda/data/dataset.csv")

view(raw)
dim(raw)
summary(raw)
str(raw)

# 1-1. 종속변수 선정: 근속률을 예측하기 위해 퇴사 여부를 확인할 수 있는 Attrition을 종속변수로 선정. 
#       기존 데이터의 Y/N 을 0/1 로 변경 = Attrition ( 0 = 'no', 1 = 'yes')
#        Attrition 값을 1,0으로 변경
raw$Attrition <- as.integer(as.character(raw$Attrition)=="Yes")
raw$Attrition <-as.factor(raw$Attrition)

# 전체 데이터셋에 Index 변수 추가 (index)
raw$ID <- seq.int(nrow(raw))

# gender 값을 1,0으로 변경
raw$Gender <- as.integer(as.character(raw$Gender)=="Female")
raw$Gender <-as.factor(raw$Gender)

# 삭제 변수
drop.cols <- c("DailyRate", "EmployeeCount", "HourlyRate", "MonthlyRate", "Over18", "StockOptionLevel")
raw = 
  raw %>%
  select (-drop.cols)

# 데이터 치환
# Departmemt Update
table(raw$Department)
c("Audit", "Tax", "CS&DA")
# Distance from Home 
table(raw$DistanceFromHome)
# 거리: 30분 미만(1~10), 1시간 미만(11~21), 1시간 이상(22~29)

# EducationField 
table(raw$EducationField)
c("인문학", "자연과학", "마케팅", "경제", "기타", "이공계")

# JobRole
table(raw$JobRole)
c("감사직","컨설팅직","투자자문직", "기술컨설팅직", "세무직", "사무직", "연구직", "기술직", "영업직")

# 추가 변수
c("BirthYear", "Functions", "OverTimeHours", "MonthlyLeaves")

raw =
  raw %>%
  mutate(Department = case_when(
    Department == "Research & Development" ~ "Audit",
    Department == "Sales" ~ "Tax",
    Department == "Human Resources" ~ "CS & DA"
  )) %>% 
  mutate(DistanceFromHome = case_when(
    DistanceFromHome %in%  1:10  ~ "30분미만",
    DistanceFromHome %in%  11:20 ~ "1시간미만",
    DistanceFromHome %in%  21:30 ~ " 1시간이상"
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
    JobRole == "Manager" ~ "연구직",
    JobRole == "Research Director" ~ "기술직",
    JobRole == "Sales Representative" ~ "영업직"
  )) %>% 
  mutate(BirthYear = 2020 - Age) %>% 
  mutate(MonthlyLeaves = sample(0:3, n(), replace = TRUE))  %>%
  mutate(OverTimeHours = ifelse(OverTime == "Yes", sample(1:52, n(), replace = TRUE), 0))



str(raw)
raw$Department <- as.numeric(as.factor(raw$Department))
raw$BusinessTravel <- as.numeric(as.factor(raw$BusinessTravel))
raw$EducationField <- as.numeric(as.factor(raw$EducationField))
raw$Gender <- as.numeric(as.factor(raw$Gender))
raw$JobRole <- as.numeric(as.factor(raw$JobRole))
raw$MaritalStatus <- as.numeric(as.factor(raw$MaritalStatus))
raw$OverTime <- as.numeric(as.factor(raw$OverTime))
# raw$Over18 <- as.numeric(as.factor(raw$Over18))
raw$DistanceFromHome <- as.numeric(as.factor(raw$DistanceFromHome))


hist(raw$DailyRate, main ="DailyRate",xlab="") 
hist((raw$Department), main = "Department",xlab="")
hist((raw$MonthlyIncome), main = "MonthlyIncome",xlab="")
hist(raw$MaritalStatus, main="MaritalStatus",xlab="")
hist(raw$OverTime, main ="OverTime",xlab="")
hist(raw$JobRole, main = "JobRole",xlab="")
hist(raw$TotalWorkingYears, main = "TotalWorkingYears",xlab="")

boxplot(raw$DailyRate, main ="DailyRate",xlab="") 
boxplot((raw$Department), main = "Department",xlab="")
boxplot((raw$MonthlyIncome), main = "MonthlyIncome",xlab="")
boxplot(raw$MaritalStatus, main="MaritalStatus",xlab="")
boxplot(raw$OverTime, main ="OverTime",xlab="")
boxplot(raw$JobRole, main = "JobRole",xlab="")
boxplot(raw$TotalWorkingYears, main = "TotalWorkingYears",xlab="")

library(raw.table)
library(scales)
x <- raw
x
str(raw)

# 정규화
raw.scaled <- scale(x)
raw.scaled <- cbind(raw[2], raw.scaled) #add target variable back


#skewed raw
library(moments)
skewness(raw.scaled$MonthlyIncome)
skewness(raw.scaled$Department)
skewness(raw.scaled$MaritalStatus)
skewness(raw.scaled$TotalWorkingYears)
skewness(raw.scaled$JobRole)



data <- raw 