# Libraries 
library(tidyverse)
library(readxl)
library(skimr)
library(GGally)

# Load Data data definitions

path_data_definitions <- "~/GitHub/ss24-bdml-Adrian-0402/source_data/automated/data_definitions.xlsx"
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

employee_attrition_tbl <- read_csv("~/GitHub/ss24-bdml-Adrian-0402/source_data/automated/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Step 1: Data Summarization -----

skim(employee_attrition_tbl)

# Character Data Type
employee_attrition_tbl %>%
  select_if(is.character) %>%
  glimpse()

# Get "levels"
employee_attrition_tbl %>%
  select_if(is.character) %>%
  map(unique)

# Proportions    
employee_attrition_tbl %>%
  select_if(is.character) %>%
  map(~ table(.) %>% prop.table())

# Numeric Data
employee_attrition_tbl %>%
  select_if(is.numeric) %>%
  map(~ unique(.) %>% length())

employee_attrition_tbl %>%
  select_if(is.numeric) %>%
  map_df(~ unique(.) %>% length()) %>%
  # Select all columns
  pivot_longer(everything()) %>%
  arrange(value) %>%
  filter(value <= 10)

# Step 2: Data Visualization ----

employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  ggpairs()

employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  ggpairs(aes(color = Attrition), lower = "blank", legend = 1,
          diag  = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

# Create data tibble, to potentially debug the plot_ggpairs function (because it has a data argument)
data <- employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome)

plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  
  color_expr <- enquo(color)
  
  if (rlang::quo_is_null(color_expr)) {
    
    g <- data %>%
      ggpairs(lower = "blank") 
    
  } else {
    
    color_name <- quo_name(color_expr)
    
    g <- data %>%
      ggpairs(mapping = aes_string(color = color_name), 
              lower = "blank", legend = 1,
              diag = list(continuous = wrap("densityDiag", 
                                            alpha = density_alpha))) +
      theme(legend.position = "bottom")
  }
  
  return(g)
  
}

employee_attrition_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)
