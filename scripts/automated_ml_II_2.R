# H2O modeling
library(h2o)

employee_attrition_tbl <- read_csv("~/GitHub/ss24-bdml-Adrian-0402/source_data/automated/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl    <- read_excel("~/GitHub/ss24-bdml-Adrian-0402/source_data/automated/data_definitions.xlsx", sheet = 1, col_names = FALSE)
employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)
set.seed(seed = 1113)
split_obj                       <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)
train_readable_tbl              <- training(split_obj)
test_readable_tbl               <- testing(split_obj)

process_hr_data_readable <- function(data, definitions_tbl) {
  
  definitions_list <- definitions_tbl %>%
    fill(...1, .direction = "down") %>%
    filter(!is.na(...2)) %>%
    separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
    rename(column_name = ...1) %>%
    mutate(key = as.numeric(key)) %>%
    mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
    split(.$column_name) %>%
    map(~ select(., -column_name)) %>%
    map(~ mutate(., value = as_factor(value))) 
  
  for (i in seq_along(definitions_list)) {
    list_name <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
  }
  
  data_merged_tbl <- list(HR_Data = data) %>%
    append(definitions_list, after = 1) %>%
    reduce(left_join) %>%
    select(-one_of(names(definitions_list))) %>%
    set_names(str_replace_all(names(.), pattern = "_value", 
                              replacement = "")) %>%
    select(sort(names(.))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                      "Travel_Rarely", 
                                                      "Travel_Frequently"),
      MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                     "Married", 
                                                     "Divorced")
    )
  
  return(data_merged_tbl)
  
}

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# Modeling
h2o.init()

# Split data into a training and a validation data frame
# Setting the seed is just for reproducability
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

# Set the target and predictors
y <- "Attrition"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 60,
  nfolds            = 5 
)

automl_models_h2o@leaderboard

# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
  
  model_name <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}

automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(6) %>% 
  h2o.getModel()

# DeepLearning_grid_3_AutoML_2_20240613_211800_model_1

h2o.getModel("DeepLearning_grid_3_AutoML_2_20240613_211800_model_1") %>% 
  h2o.saveModel(path = "~/GitHub/ss24-bdml-Adrian-0402/source_data/automated/")

deep_learning_h2o <- h2o.loadModel("~/GitHub/ss24-bdml-Adrian-0402/source_data/automated/DeepLearning_grid_3_AutoML_2_20240613_211800_model_1")


predictions <- h2o.predict(deep_learning_h2o, newdata = as.h2o(test_tbl))

predictions_tbl <- predictions %>% as_tibble()
