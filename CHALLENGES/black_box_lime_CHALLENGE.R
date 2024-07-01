# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)
library(rsample)
library(glue)

# Processing Pipeline
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

# Load Data
employee_attrition_tbl <- read_csv("~/GitHub/ss24-bdml-Adrian-0402/source_data/automated/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl    <- read_excel("~/GitHub/ss24-bdml-Adrian-0402/source_data/automated/data_definitions.xlsx", sheet = 1, col_names = FALSE)

employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# Split into test and train
set.seed(seed = 1113)
split_obj <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)

# Assign training and test data
train_readable_tbl <- training(split_obj)
test_readable_tbl  <- testing(split_obj)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel("source_data/automated/models/StackedEnsemble_BestOfFamily_5_AutoML_1_20240616_131553")

# 3. LIME ----

# 3.1 Making Predictions ----

predictions_tbl <- automl_leader %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )

# 3.2 Single Explanation ----

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
    model           = automl_leader,
    bin_continuous  = TRUE,
    n_bins          = 4,
    quantile_bins   = TRUE
  )

explanation <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(
    
    # Pass our explainer object
    explainer = explainer,
    # Because it is a binary classification model: 1
    n_labels   = 1,
    # number of features to be returned
    n_features = 8,
    # number of localized linear models
    n_permutations = 5000,
    # Let's start with 1
    kernel_width   = 0.5
  )

explanation %>% 
  as.tibble()

case_1 <- explanation %>%
  filter(case == 1)

# Recreate plot_features()
g <- plot_features(explanation = case_1, ncol = 1)

# Create text for label
case_1 <- case_1 %>% 
  mutate(subtitle_text = glue("Case: {case}\nLabel: {label}\nProbability: {round(label_prob, 2)}\nExplanation Fit: {round(model_r2, 2)}")) 

# Recreated plot
case_1 %>% 
  arrange(abs(feature_weight)) %>%
  mutate(feature_desc = as_factor(feature_desc)) %>%
  mutate(sign = ifelse(feature_weight >= 0, "Supports", "Contradicts")) %>%
  
  ggplot(aes(feature_weight, feature_desc)) +
  geom_col(aes(fill = sign)) +
  scale_fill_manual(values = c("#c40d00", "#000875")) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  
  labs(
    x = "Weight",
    y = "Feature",
    subtitle = case_1$subtitle_text
  )

# Original plot
g

# 3.3 Multiple Explanations ----

# Recreated plot
explanation %>% 
  mutate(case = as_factor(case)) %>% 
  mutate(feature_desc = factor(feature_desc, levels = feature_desc[order(feature, feature_value)] %>% unique() %>% rev())) %>% 
  
  ggplot(aes(case, feature_desc, fill = feature_weight)) +
  geom_tile() +
  facet_wrap(~label) +
  
  scale_x_discrete("Case", expand = c(0, 0)) +
  scale_y_discrete("Feature", expand = c(0, 0)) +
  scale_fill_gradient2(low = "#d43547", mid = "#ffffff", high = "#525cd1") +
  
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = 'grey20', size = 1),
        panel.grid = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  
  labs(
    x = "Case",
    y = "Feature",
    fill = "Feature\nweight"
  )

# Original plot
plot_explanations(explanation)