
library(tidyverse)
library(tidymodels)
library(randomForest)
library(yardstick)
library(showtext)

font_add_google("Nanum Gothic", "nanumgothic")
# install.packages('randomForest')

raw = data
data = raw
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

----------------------------------------"tidymodels : 전처리 필요"
data %>%
  initial_split(prop=0.7) -> data_split

data_split
str(data_split)

data_split %>%
  training()

data_split %>%
  testing()

colnames(data)

data_split %>% training() %>%
  recipe(Attrition~
           + Age
         + BusinessTravel
         + Department              
         + DistanceFromHome
         + Education
         + EducationField
         # + EmployeeNumber          
         + EnvironmentSatisfaction
         + Gender
         + JobInvolvement
         + JobLevel                
         + JobRole
         + JobSatisfaction
         + MaritalStatus
         + MonthlyIncome           
         + NumCompaniesWorked
         + OverTime
         + PercentSalaryHike
         + PerformanceRating       
         + RelationshipSatisfaction
         + StandardHours
         + TotalWorkingYears
         + TrainingTimesLastYear   
         + WorkLifeBalance
         + YearsAtCompany
         + YearsInCurrentRole
         + YearsSinceLastPromotion 
         + YearsWithCurrManager
         #+ ID
         #+ BirthYear
         #+ MonthlyLeaves           
         + OverTimeHours  )

# 결측치 없음..?
sum(is.na(data))

view(data_split)

data_split %>% training() %>%
  recipe(Attrition~
           + Age
         + BusinessTravel
         + Department              
         + DistanceFromHome
         + Education
         + EducationField
         # + EmployeeNumber          
         + EnvironmentSatisfaction
         + Gender
         + JobInvolvement
         + JobLevel                
         + JobRole
         + JobSatisfaction
         + MaritalStatus
         + MonthlyIncome           
         + NumCompaniesWorked
         + OverTime
         + PercentSalaryHike
         + PerformanceRating       
         + RelationshipSatisfaction
         + StandardHours
         + TotalWorkingYears
         + TrainingTimesLastYear   
         + WorkLifeBalance
         + YearsAtCompany
         + YearsInCurrentRole
         + YearsSinceLastPromotion 
         + YearsWithCurrManager
         #+ ID
         #+ BirthYear
         #+ MonthlyLeaves           
         + OverTimeHours  
           ) %>%
  #step_corr(all_predictors()) %>% # 상관관계가 지나치게 큰 변수를 제거
  step_center(all_predictors(), -all_outcomes()) %>% # 평균을 0으로 하는 척도  
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep() -> data_recipe

select_if(data, is.numeric)

data_recipe

data_recipe %>%
  bake(data_split %>% testing()) -> data_testing

data_testing

data_recipe %>%
  juice() -> data_training

data_training



#---------------------랜덤포레스트 : 회귀 
# mode는 회귀와 분류가 있따. 

rand_forest(trees=100, mode='regression') %>%
  set_engine('randomForest') %>%
  fit(Attrition~
        + Age
      + BusinessTravel
      + Department              
      + DistanceFromHome
      + Education
      + EducationField
      # + EmployeeNumber          
      + EnvironmentSatisfaction
      + Gender
      + JobInvolvement
      + JobLevel                
      + JobRole
      + JobSatisfaction
      + MaritalStatus
      + MonthlyIncome           
      + NumCompaniesWorked
      + OverTime
      + PercentSalaryHike
      + PerformanceRating       
      + RelationshipSatisfaction
      + StandardHours
      + TotalWorkingYears
      + TrainingTimesLastYear   
      + WorkLifeBalance
      + YearsAtCompany
      + YearsInCurrentRole
      + YearsSinceLastPromotion 
      + YearsWithCurrManager
      #+ ID
      #+ BirthYear
      #+ MonthlyLeaves   
      + OverTimeHours   
    , data=data_training) -> data_rg


result = 
data_rg %>% 
  predict(data_testing) %>%
  bind_cols(data_testing)


# bind해서 test한 데이터와 train한 데이터 비교
data_rg %>%
  predict(data_testing) %>%
  bind_cols(data_testing)



# 성능측정

data_rg %>%
  predict(data_testing) %>%
  bind_cols(data_testing) %>%
  metrics(truth=Attrition, estimate=.pred_res)

remove(df)
library(doBy)
library(fmsb)
library(MASS)

df_radarchart <- function(df) {
  df <- data.frame(df)
  dfmax <- apply(df, 2, max)
  dfmin <- apply(df, 2, min)
  as.data.frame(rbind(dfmax,dfmin,df))
}

mean_by_Type <- summaryBy

mean_by_Type_scale <- 

radarchart(df = mean_by_type_sc)


