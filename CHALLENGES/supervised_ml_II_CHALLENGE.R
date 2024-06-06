# Standard
library(tidyverse)

# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)
library(workflows)

# Modeling Error Metrics
library(yardstick)

# 1.0 Build a model (GLM) and load Data ----

model_glm <- linear_reg() %>% 
  set_engine("glm")

bike_features_tbl <- readRDS("~/GitHub/ss24-bdml-Adrian-0402/source_data/raw_data/bike_features_tbl.rds")

bike_features_tbl <- bike_features_tbl %>% 
  
  select(model:url, `Rear Derailleur`, `Shift Lever`) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything(), -url)

# Split the data first

set.seed(seed = 1112)
split_obj <- rsample::initial_split(bike_features_tbl, prop   = 0.80, 
                                    strata = "category_2")

# Check if testing contains all category_2 values
split_obj %>% training() %>% distinct(category_2)
split_obj %>% testing() %>% distinct(category_2)

# Assign training and test data
train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

# We have to remove spaces and dashes from the column names
train_tbl <- train_tbl %>% set_names(str_replace_all(names(train_tbl), " |-", "_"))
test_tbl  <- test_tbl  %>% set_names(str_replace_all(names(test_tbl),  " |-", "_"))

# 2.0 Create features with the recipes package ----

recipe_obj <- recipe(price ~ ., data = train_tbl) %>%
  step_rm(c(model:weight),category_1,c(category_3:gender)) %>%
  update_role(id, new_role = "ID") %>% 
  step_dummy(all_nominal(), - all_outcomes(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  # Use median to guess missing values
  step_impute_median(all_predictors())

# train_transformed_tbl <- bake(recipe_obj, new_data = train_tbl)
# test_transformed_tbl  <- bake(recipe_obj, new_data = test_tbl)

# 3.0 Bundle model and recipe with the workflow package

# Trying to use prep() or bake() and then bundle it into a workflow
# gives us the error:

# Error in `add_recipe()`:
# !Can't add a trained recipe to a workflow.

# One cannot use prep() and bake() if it is to be bundled into a workflow
# therefore we omit those steps from above and use it "unprepped"

bikes_wflow <-
  workflow() %>% 
  add_model(model_glm) %>% 
  add_recipe(recipe_obj)

# 4.0 Evaluate model with the yardstick package

# Fit the data using workflow
bikes_fit <- 
  bikes_wflow %>% 
  fit(data = train_tbl)

calc_metrics <- function(model, new_data = test_tbl) {
  
  model %>%
    predict(new_data = new_data) %>%
    
    bind_cols(new_data %>% select(price)) %>%
    
    yardstick::metrics(truth = price, estimate = .pred_res)
  
}

# Evaluate
bikes_fit %>% calc_metrics(new_data = test_tbl)

# 5.0 Some Remarks

# Using all_nominal() creates a lot more columns for every specific type
# as opposed to the advanced string manipulation done for rear derailleurs and 
# shift levers in the business case (which grouped similar names). Therefore, unknown values can appear
# during predictions, because they only appeared in the training data, and not in the testing data.
# Thus, step_dummy() could not create variables for the unknown testing data set.

# Although step_impute_median() makes predicting with missing values possible,
# the result is hardly accurate. This can be seen by large errors for certain data
# points.

bikes_fit %>% predict(new_data = test_tbl) %>% 
  bind_cols(test_tbl %>% select(price)) %>% view()
