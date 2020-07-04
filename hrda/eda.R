# ref : https://statkclee.github.io/model/model-class-imbalance.html

library(dplyr)
library(caret)
library(pROC)
library(randomForest)

library(ggplot2)

eda_data <- 
  data 

eda_data <-
  eda_data %>%
  select(-"EmployeeCount")

apply(is.na(eda_data), 2, sum)
glimpse(eda_data)


str(eda_data)

# RandomForest
idx <- createDataPartition(eda_data$Attrition, 
                           p = 0.7, 
                           list = FALSE, 
                           times = 1)

rfdata_train <- eda_data[ idx,]
rfdata_test  <- eda_data[-idx,]


## 2.2. 모형적합 ------
fit_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

data_rf <- train(Attrition ~ ., 
                 data = rfdata_train, 
                 method = "rf", 
                 preProcess = c("scale", "center"),
                 trControl = fit_ctrl,
                 verbose = FALSE)

## 2.3. 모형성능평가 ------
rftest_predict <- predict(data_rf, rfdata_test)
confusionMatrix(rftest_predict, rfdata_test$Attrition)



# 3. 모형 설명 -----
## 3.1. 중요변수 추출 -----
data_rf_imp <- varImp(data_rf, scale = TRUE)

(top_ten_variable_v <- data_rf_imp$importance %>%
    as.data.frame() %>%
    rownames_to_column(var="variable") %>% 
    top_n(10, Overall) %>% 
    pull(variable))


rfTopPlot <-
  data_rf_imp$importance %>%
  as.data.frame() %>%
  rownames_to_column(var="variable") %>%
  ggplot(aes(x = reorder(variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#1F77B4", alpha = 0.8) +
  coord_flip() +
  labs(y="중요도", x="요소") +
  theme_minimal(base_family="NanumGothic")



rfROC<-
  plot.roc (as.numeric(rfdata_test$Attrition), as.numeric(rftest_predict),lwd=2, type="b", print.auc=TRUE,col ="steelblue")



#XGBdata


# factor 전처리 : XGB 분석을 위해 종속변수 값을 chr-factor로 변경 (1:0 시 에러)  
xgb_data <- eda_data %>%
  mutate(Attrition = ifelse(Attrition == 1,"Yes","No"))

xgb_data$Attrition <- 
  as.factor(xgb_data$Attrition)

set.seed(123)
#xgbData <- eda_data 

indexes <- sample(1:nrow(xgb_data), size=0.7*nrow(xgb_data))

xgbdata_train <- xgb_data[indexes,]
xgbdata_test <- xgb_data[-indexes,]

formula = Attrition~.
#levels(eda_data$Attrition) <- make.names(levels(factor(eda_data$Attrition)))
fitControl <- trainControl(method="cv", number = 3, classProbs = TRUE)

xgbGrid <- expand.grid(nrounds = 50,
                       max_depth = 12,
                       eta = .03,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9
)

xgbModel <- train(formula, data = xgbdata_train,
                   method = "xgbTree"
                   ,trControl = fitControl
                   , verbose=0
                   , maximize=FALSE
                   ,tuneGrid = xgbGrid
)


importance <- varImp(xgbModel)
varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                            Importance = round(importance[[1]]$Overall,2))


rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
# ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
#                            y = Importance)) +
#   geom_bar(stat='identity',colour="white", fill = "lightgreen") +
#   geom_text(aes(x = Variables, y = 1, label = Rank),
#             hjust=0, vjust=.5, size = 4, colour = 'black',
#             fontface = 'bold') +
#   labs(x = 'Variables', title = 'Relative Variable Importance') +
#   coord_flip() + 
#   theme_minimal()

xgbTopPlot <-
  rankImportance %>%
  as.data.frame() %>%
  #rownames_to_column(var="Variable") %>%
  ggplot(aes(x = reorder(Variables, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "seagreen", alpha = 0.8) +
  coord_flip() +
  labs(y="중요도", x="요소") +
  theme_minimal(base_family="NanumGothic")

xgbPredict <- predict(xgbModel,xgbdata_test)
confusionMatrix(xgbPredict, xgbdata_test$Attrition)

xgbROC<-
  plot.roc (as.numeric(xgbdata_test$Attrition), as.numeric(xgbPredict),lwd=2, type="b", print.auc=TRUE,col ="seagreen")


# eda_data$Attrition <- as.integer(as.character(eda_data$Attrition)=="Yes")
# eda_data$Attrition <-as.factor(eda_data$Attrition)



# RandomForest로 모델링...


# plot(rftest_predict)
# plot(xgbPredict)