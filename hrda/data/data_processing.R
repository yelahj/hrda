library(tidyverse)
library(plyr)
library(dplyr)

# 1. IBM HR Analysis on Kaggle에서 제공하는 IBM 데이터를 활용하여 데이터를 분석합니다.
raw <- read_csv("data/dataset.csv")

view(raw)
dim(raw)

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

# 값 조정 변수
edit_raw <- c("Department", "DistancFromHome","EducationField", "JobRole", "")

table(raw$Department)
c("Audit", "Tax", "CS", "Deal", "Other")
table(raw$DistanceFromHome)
# 1 ~ 27, 1 ~ 10 : 서울, 11 - 20 : 수도권, 21 - 27 : 지방?
c("")
table(raw$Education)
table(raw$EducationField)
c("인문학", "기술공학", "경제경영학", "자연과학", "기타")
table(raw$JobRole)
c("감사직","컨설팅직","투자자문직", "기술컨설팅직", "세무직", "사무직", "연구직")
# 추가 변수
add_raw <- c("BirthYear", "Functions", "OverTimeHours", "MonthlyLeaves")

# CPA 여부
# 정규/계약직



