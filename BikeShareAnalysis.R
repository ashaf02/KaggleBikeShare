
##Installing packages
install.packages("tidymodels")
install.packages("tidyverse")

##Libraries
library(tidyverse)
library(tidymodels)
library(vroom)
library(recipes)
library(dplyr)
library(tibble)

##Read in the Data
bike <- vroom("train.csv")
test <- vroom("test.csv")

test

test <- test %>%
  mutate(
    datetime = ymd_hms(datetime),  # Convert to datetime format
    datetime = format(datetime, format = "%m/%d/%Y %H:%M")  # Format as '1/20/2011 0:00'
  )

##Cleaning
bike_cleaned <- bike %>%
  mutate(weather = ifelse(weather == 4, 3, weather))%>%
  select(-casual, -registered)

bike_cleaned

##Feature Engineering
my_formula <- count ~ datetime + season + holiday + workingday + weather + temp + atemp + humidity + windspeed

my_recipe <- recipe(my_formula, data = bike_cleaned) %>%
  # Convert 'season', 'holiday', 'workingday', and 'weather' to factors
  step_mutate(
    season = factor(season, levels = c(1, 2, 3, 4), labels = c("spring", "summer", "fall", "winter")),
    holiday = factor(holiday, levels = c(0, 1), labels = c("no", "yes")),
    workingday = factor(workingday, levels = c(0, 1), labels = c("no", "yes")),
    weather = factor(weather, levels = c(1, 2, 3), labels = c("Clear", "Mist", "Light Rain"))
  )

prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet14
bake(prepped_recipe, new_data=test)

##Linear Regression 
my_mod <- linear_reg() %>% 
  set_engine("lm")

bike_workflow <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(my_mod) %>% 
  fit(data = bike_cleaned)

#Create predictions
bike_predictions <- predict(bike_workflow, 
                            new_data=test)

# Change 'NA' predictions to 0
bike_predictions <- mutate(bike_predictions, .pred = ifelse(is.na(.pred), 0, .pred))

# Round negative predictions to 0
bike_predictions <- mutate(bike_predictions, 
  .pred = ifelse(.pred < 0, 0, .pred))

#Format properly
submission_df <- tibble(
  datetime = test$datetime,   # Assuming 'datetime' is a column in your 'test' data frame
  count = bike_predictions$.pred
)

# Write the submission CSV file using vroom::vroom_write with column names
vroom::vroom_write(submission_df, "mytest.csv")
