
library(cond)
library(ggcorrplot)

# GENDER 변경.. 
rfdata_train$Gender <- 
  as.numeric(rfdata_train$Gender)

rfdata_test$Gender <- 
  as.numeric(rfdata_test$Gender)


#### RANDOMFOREST FIT
rand_forest(trees=100, mode='classification') %>%
  set_engine('randomForest') %>%
  fit(Attrition~
        + Age
      + DistanceFromHome
      + Education
      + EducationField
      + EnvironmentSatisfaction
      + JobInvolvement
      + JobRole
      + JobSatisfaction
      + MaritalStatus
      + MonthlyIncome          
      + NumCompaniesWorked
      + PercentSalaryHike
      + RelationshipSatisfaction
      + TotalWorkingYears
      + TrainingTimesLastYear  
      + WorkLifeBalance
      + YearsAtCompany
      + YearsInCurrentRole
      + YearsSinceLastPromotion
      + YearsWithCurrManager
      + MonthlyLeaves          
      + OverTimeHours
      , data=rfdata_train) -> data_rg

result =
  data_rg %>%
  predict(rfdata_test, type="prob") %>%
  bind_cols(rfdata_test)


# bind해서 test한 데이터와 train한 데이터 비교
data_rg %>%
  predict(rfdata_test) %>%
  bind_cols(rfdata_test)


# 성능측정
data_rg %>%
  predict(rfdata_test) %>%
  bind_cols(rfdata_test) %>%
  metrics(truth=Attrition, estimate=.pred_class)

# 시험데이터 예측
x.scaled <- result %>% 
  select(top_ten_variable_v)


# 상위 변수로 정규화
resc <- function(x) rescale(x,to = c(1, 5))

x.scaled <- data.frame(lapply(x.scaled, resc))

x.scaled <- data.frame(
  lapply(x.scaled, resc),  
  result$EmployeeNumber
) 

# rfdata_test %>% 
#   filter(EmployeeNumber == 11)
# 
# rftest_predict <- predict(data_rf, rfdata_test)
# 
# 
# sum(rftest_predict )