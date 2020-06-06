

library(tidyverse)

library(dplyr)


remove(raw)

# 1. IBM HR Analysis on Kaggle에서 제공하는 IBM 데이터를 활용하여 데이터를 분석합니다.
raw <- read_csv("data/dataset.csv")

view(raw)
dim(raw)
summary(raw)

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

# Train / Test 데이터로 분류 [*] Attritiond을 기준으로 비율을 정해야 한다.
prop.table(table(raw$Attrition)) 
## 75%로 샘플 사이즈 변경하기 smp_size는 75% 로 실행
smp_size <- floor(0.75 * nrow(raw))
set.seed(123)

## train_ind <- sample 인덱스
train_ind <- sample(seq_len(nrow(raw)), size = smp_size)

### Train & Test data 
train <- raw[train_ind, ]
test <- raw[-train_ind, ]

test_label <- test$ID
train_label <- train$ID

# 인덱스 drop..
# test$Id <- NULL
# train$Id <- NULL

# 테스트 위해 test데이터의 Attrition 값 drop 
# test$Attrition <- NA


# 삭제 변수
removal_raw <- c("DailyRate", "EmployeeCount", "HourlyRate", "MonthlyRate", "Over18", "StockOptionLevel")
 drop.cols <- removal_raw


# 데이터 치환

# Departmemt Update
table(raw$Department)
c("Audit", "Tax", "CS&DA")

raw =
  raw %>%
  mutate(Department = case_when(
    Department == "Research & Development" ~ "Audit",
    Department == "Sales" ~ "Tax",
    Department == "Human Resources" ~ "CSDA"
  ))


# Distance from Home 
table(raw$DistanceFromHome)
# 거리: 30분 미만(1~10), 1시간 미만(11~21), 1시간 이상(22~29)
raw =
  raw %>%
  mutate(DistanceFromHome = case_when(
    DistanceFromHome %in%  1:10  ~ "30분 미만",
    DistanceFromHome %in%  11:20 ~ "1시간 미만",
    DistanceFromHome %in%  21:30 ~ " 1시간 이상"
  ))


# EducationField 
table(raw$EducationField)
c("인문학", "자연과학", "마케팅", "경제", "기타", "이공계")

# EducationField 
raw =
  raw %>% 
  mutate(EducationField = case_when(
    EducationField == "Marketing" ~ "인문학",
    EducationField == "Life Sciences" ~ "기술공학",
    EducationField == "Medical" ~ "상경계",
    EducationField == "Technical Degree" ~ "자연과학",
    EducationField == "Human Resources" ~ "예체능",
    EducationField == "Other" ~ "기타"
  ))


table(raw$JobRole)
c("감사직","컨설팅직","투자자문직", "기술컨설팅직", "세무직", "사무직", "연구직", "기술직", "영업직")

raw=
  raw %>% 
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
  ))



 # 추가 변수
 add_raw <- c("BirthYear", "Functions", "OverTimeHours", "MonthlyLeaves")
 
 
 # BirthYear
  raw =
  raw %>% 
     mutate(BirthYear = 2020 - Age
     )
  
 
  # MonthlyLeaves
  raw = 
    raw %>% 
    mutate(MonthlyLeaves = sample(0:3, n(), replace = TRUE))  


  # OverTimeHours
  raw = raw %>% 
    #filter(OverTime == "Yes") %>%
    mutate(OverTimeHours = ifelse(OverTime == "Yes", sample(1:52, n(), replace = TRUE), 0))
  

data = raw


  # 그래프-부서별 인원수

install.packages("ggplot2")
library(ggplot2)
library(Mass)

count.gender <- ggplot(data = raw, aes(x = Gender)) + geom_bar(fill="#F8766D") +
  labs(title = "EmployeeCount by Gender")

count.dp <- ggplot(data = raw, aes(x = Department, y = EmployeeCount)) + geom_col(colour="#F8766D") +
  labs(title = "EmployeeCount by Department")



    