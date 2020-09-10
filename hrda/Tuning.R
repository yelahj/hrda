library(xgboost)
library(Matrix)

data_sparse <- sparse.model.matrix(~.-1, data = as.data.frame(xgb_data))

dtrain <- xgb.DMatrix(data = data_sparse[1:nrow(xgbdata_train), ], label = xgbdata_train$Attrition) 
dtest <- xgb.DMatrix(data = data_sparse[(nrow(xgbdata_train)+1):nrow(xgb_data), ])

gc()

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6), 
                                colsample_bytree = c(0.5, 0.6),
                                max_depth = c(3, 4),
                                min_child = seq(1), 
                                eta = c(0.1)
)

ntrees <- 500

system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = ntrees, nfold = 5, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))}))


output <- as.data.frame(t(rmseErrorsHyperparameters))
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")
names(output) <- varnames
head(output)





----------------------------------------------------
  
  
library(tidyverse)
library(tidymodels)

# Specify the model and the parameters to tune (parnsip)
model <-
  boost_tree(tree_depth = tune(), mtry = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

# Specify the resampling method (rsample)
splits <- vfold_cv(xgb_data, v = 2)

# Specify the metrics to optimize (yardstick)
metrics <- metric_set(roc_auc)

# Specify the parameters grid (or you can use dials to automate your grid search)
grid <- expand_grid(tree_depth = c(4, 6, 8, 10),
                    mtry = c(2, 10, 50)) # You can add others

# Run each model (tune)
tuned <- tune_grid(formula = Attrition ~ .,
                   model = xgbModel,
                   resamples = splits,
                   grid = grid,
                   metrics = metrics,
                   control = control_grid(verbose = TRUE))

# Check results
show_best(tuned)
autoplot(tuned)
select_best(tuned)

# Update model
tuned_model <- 
  model %>% 
  finalize_model(select_best(tuned)) %>% 
  fit(Y ~ ., data = X_train)

# Make prediction 
predict(tuned_model, X_train)
predict(tuned_model, X_test)
