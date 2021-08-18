##### Predictive Model train

#Loading libraries
library(xgboost)
library(tidyverse)
library(dplyr)
library(MLmetrics)



#Load in game data

load('./data/modeling_data.Rdata')



#Split into train and test

#Removing games with NA fields
modeling_data <- na.omit(game_data_final2)


#Splitting into train and test
dt <- sort(sample(nrow(modeling_data), nrow(modeling_data)*.8))


train <-  modeling_data[dt, ]
test <- modeling_data[-dt, ]



#Removing unecessary features

train <- train %>% 
  select(-home_rink_side_start, -type, -date_time_GMT, -venue_time_zone_tz, -outcome_type, -win, -outcome,
         -game_prior, -season, -game_id, -goals_against)



test_label <- test[, 'goals_for']

test <- test %>% 
  select(-home_rink_side_start, -type, -date_time_GMT, -venue_time_zone_tz, -outcome_type, -win, -outcome,
         -game_prior, -season, -game_id, -goals_for, -goals_against)



View(train)


#Creating Validation set
dv <- sort(sample(nrow(train), nrow(train)*.8))

train = train[dv, ]
val = train[-dv, ]

train_label <- train[, "goals_for"]
val_label <- val[, "goals_for"]

train <- train %>% 
  select(-goals_for)

val <- val %>% 
  select(-goals_for)


#Training Model


#Set up hyperparameter search
learning_rate = c(.01,.05,.1)
subsample = c(.4, .6, .8)
nrounds = c(500, 750, 1000)
depth = c(4, 6, 8 ,10)

xgb_grid <- expand.grid(learning_rate, subsample, nrounds, depth) %>% 
  rename(eta = Var1, subsample = Var2, nrounds = Var3, depth = Var4)



#Convert data to matrix format
boost_data <- xgb.DMatrix(data = as.matrix(train), label=train_label[[1]])
val_data <- xgb.DMatrix(data = as.matrix(val), label = val_label[[1]])


#Create table to store metric results
results <- xgb_grid
results$MAE <- 0
results$MSE <- 0


#Loop through hyper parameters
for (i in 1:dim(xgb_grid)[1]) {
  print(paste0('Training Model: ', i))
  xgb <- xgb.train(data =boost_data, nrounds = xgb_grid$nrounds[i], subsample = xgb_grid$subsample[i], 
                   depth = xgb_grid$depth[i], eta = xgb_grid$eta[i], label = train_label[[1]])
  print(paste0('Model ', i, ' done'))
  preds <- predict(xgb, val_data)
  results$MAE[i] <- MAE(preds, val_label[[1]])
  results$RMSE[i] <- RMSE(preds, val_label[[1]])
}


#Choosing Model with lowest RMSE
results <- results %>% 
  arrange(RMSE)

best_params <- results[1, 1:4]

#Train model on full training set
train <-  modeling_data[dt, ]

train_label <- train[, 'goals_for']
train <- train %>% 
  select(-home_rink_side_start, -type, -date_time_GMT, -venue_time_zone_tz, -outcome_type, -win, -outcome,
         -game_prior, -season, -game_id, -goals_against, -goals_for)

boost_data <- xgb.DMatrix(data = as.matrix(train), label=train_label[[1]])

xgb <- xgb.train(data =boost_data, nrounds = best_params$nrounds, subsample = best_params$subsample, 
                 depth = best_params$depth, eta = best_params$eta, label = train_label[[1]])


#Get Test Results

test_data <- xgb.DMatrix(data = as.matrix(test), label = test_label[[1]])

preds <- predict(xgb, test_data)
MAE(preds, val_label[[1]])
RMSE(preds, val_label[[1]])

game_results <- data.frame(game_id = modeling_data$game_id[-dt], 
                          test_label, pred_goals = round(preds), goals_against = modeling_data$goals_against[-dt])

game_results$true_win <- 0
game_results$true_win[(game_results$goals_for > game_results$goals_against)] <- 1
game_results$pred_win <- 0
game_results$pred_win[(game_results$pred_goals > game_results$goals_against)] <- 1
game_results$diff <- abs(game_results$true_win - game_results$pred_win)

#Prediction Accuracty
1 - (sum(game_results$diff)/length(game_results$diff))


val_data <- xgb.DMatrix(data = as.matrix(val), label = val_label[[1]])




xgb_grid$eta[1]

make_xgb <- function(train, test, yvar, xvars, params, family) {
  traintest <- getOneWayVars(train, test, yvar)
  ow_train <-  traintest[[1]]
  ow_test <- traintest[[2]]
  boost_data <- xgb.DMatrix(data = ow_train, label=train[, yvar][[1]])
  xgb <- xgb.train(data =boost_data, nrounds = params$nrounds, subsample = subsample, 
                   depth = depth, eta = params$eta, label = train[, yvar][[1]], objective = family)
}
