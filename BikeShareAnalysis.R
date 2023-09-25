## Libraries I am going to need
library(tidyverse)
library(tidymodels)
library(poissonreg)
library(vroom)
library(glmnet)

## Read in the data
bikeTrain <- vroom("train.csv")
bikeTest <- vroom("test.csv")

## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)

## Cleaning & Feature Engineering
bike_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>% #make dummy variables7
  step_normalize(all_numeric_predictors()) # Make mean 0, sd=1

prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, new_data = bikeTrain) #Make sure recipe work on train
bake(prepped_recipe, new_data = bikeTest) #Make sure recipe works on test

## Define the model
lin_model <- linear_reg() %>%
  set_engine("lm")

## Set up the whole workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data=bikeTrain)

## Look at the fitted LM model this way
extract_fit_engine(bike_workflow) %>%
  summary()

## Get Predictions for test set AND format for Kaggle
test_preds <- predict(bike_workflow, new_data = bikeTest) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write prediction file to CSV
vroom_write(x=test_preds, file="./TestPreds.csv", delim=",") (edited) 

## Trying Poisson Model
pois_mod <- poisson_reg() %>% #Type of model
  set_engine("glm") # GLM = generalized linear model

bike_pois_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = bikeTrain) # Fit the workflow

bike_predictions_pois <- predict(bike_pois_workflow,
                                 new_data=bikeTest) # Use fit to predict

## Get Predictions for test set AND format for Kaggle
test_preds_pois <- predict(bike_pois_workflow, new_data = bikeTest) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write prediction file to CSV for pois
vroom_write(x=test_preds_pois, file="poisTest.csv", delim=",")

## Transform to log(count) - I can only do this on train set because
## test set does not have count.  Hence, I am doing this outside of recipe
## because I only apply this to the train set
logTrainSet <- bikeTrain %>%
  mutate(count=log(count))

## Define the model for log-lin
lin_model <- linear_reg() %>%
  set_engine("lm")

## Set up the whole workflow for log-lin
log_lin_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data=logTrainSet) #Make sure to use the log(count) dataset

## Get Predictions for test set AND format for Kaggle for log-lin
log_lin_preds <- predict(log_lin_workflow, new_data = bikeTest) %>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write log-lin predictions to CSV
vroom_write(x=log_lin_preds, file="./LogLinearPreds.csv", delim=",")

## Penalized log-lin regression model
preg_model <- linear_reg(penalty=1, mixture=0) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data=logTrainSet)

penalized_predictions <- predict(preg_wf, new_data=bikeTest)%>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write penalized log predictions to CSV
vroom_write(x=penalized_predictions, file="./PenLogLinearPreds.csv", delim=",")

