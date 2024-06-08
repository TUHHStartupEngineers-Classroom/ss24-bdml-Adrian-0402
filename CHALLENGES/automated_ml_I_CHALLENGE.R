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

# skim(employee_attrition_tbl)
# We had a look at this before

# Step 2: Data Visualization ----

# Helper function
count_to_pct <- function(data, ..., col = n) {
  
  # capture the dots
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  ret <- data %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
    ungroup()
  
  return(ret)
  
}

# Plot function
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

# Step 3: Investigate Features and answer Questions ----

# Question 1:
employee_attrition_tbl %>% 
  select(Attrition,MonthlyIncome) %>% 
  plot_ggpairs(Attrition)

# We can see a strong peak for "Yes" at lower monthly incomes.
# Therefore, those that are leaving have a lower monthly income (c).

# Question 2:

employee_attrition_tbl %>% 
  select(Attrition,PercentSalaryHike) %>% 
  plot_ggpairs(Attrition)

# The Boxplot shows very similar median values of the salary hike.
# The density plot shows us a mostly overlapping behavior with minor
# differences in the distribution. Thus it is not entirely clear
# to deduce anything based on just the Percent Salary Hike (d).

# Question 3:

employee_attrition_tbl %>% 
  select(Attrition,StockOptionLevel) %>% 
  plot_ggpairs(Attrition)

# Here we can confidently say that the density for higher stock options
# (1 and 2) is higher for those that are staying (Attrition: "No").
# Therefore, those that are staying have a higher stock option level (b).

# Question 4:

employee_attrition_tbl %>% 
  select(Attrition,EnvironmentSatisfaction) %>% 
  plot_ggpairs(Attrition)

# One can see that for higher satisfaction levels, significantly more
# people are staying. This in turn means, that a higher proportion of those
# that are leaving have a lower satisfaction level, especially for 1 (="low").
# It is also visible on the boxplot, because for "Yes" attrition, the lower 25% quantile
# extends to 1.
# So in summary: A higher proportion of those leaving have a low environment satisfaction level (a).

# Question 5:

employee_attrition_tbl %>% 
  select(Attrition,WorkLifeBalance) %>% 
  plot_ggpairs(Attrition)

# Looking at the densities again, we can see that the 2's and 3's of people that are
# staying are significantly higher (b).

# Question 6:

employee_attrition_tbl %>%
  select(Attrition,JobInvolvement) %>%
  plot_ggpairs(Attrition)

# The density plot shows, that the 3's and 4's are much more present for people that
# are staying (especially visible for 3's). In turn this means, that hose that are leaving
# have a lower density of 3's and 4's (a).

# Question 7:

employee_attrition_tbl %>%
  select(Attrition,OverTime) %>%
  plot_ggpairs(Attrition)

# Here we can see that for people who are leaving, the relative proportion compared
# to those not leaving is higher when looking at those working overtime. The absolute numbers
# for attrition is almost the same for either working overtime or not. But for people who are
# staying its visibly lower. We look at it from another perspective by computing concrete numbers:

employee_attrition_tbl %>%
  count(OverTime, Attrition) %>%
  count_to_pct(OverTime)

# 30% of people who are working overtime are leaving. Only 10% of people
# who are not working overtime are leaving.
# Or if you change the perspective one more time:

employee_attrition_tbl %>%
  count(Attrition, OverTime) %>%
  count_to_pct(Attrition)

# 54% of people who are leaving are working overtime. Only 23% who are staying are
# working overtime.

# Overall you can say generally, that the proportion of those leaving that are working overtime
# are high compared to those that are not leaving (a). (Unless you would say that working overtime
# shall result in more attrition)

# Question 8:

employee_attrition_tbl %>%
  select(Attrition,TrainingTimesLastYear) %>%
  plot_ggpairs(Attrition)

# In the plot we can see, that for most higher numbers of training times
# (2, 3, 5 and 6), the density of those staying is significantly higher.
# In the boxplot we can also see, that the median for leaving is 2, while it is 3 
# for those who stay. Therefore we can say that people that leave tend 
# to have less annual trainings (b).

# Question 9:

employee_attrition_tbl %>%
  select(Attrition,YearsAtCompany) %>%
  plot_ggpairs(Attrition)

# The density plot shows a higher concentration of people who are leaving that have
# less years at the company (around 0 to 3). After that, people tend to stay more.
# The boxplot again confirms this, as the quantiles from the people leaving contain lower values.
# Therefore, people that leave tend to have less working years at the company (b).

# Question 10:

employee_attrition_tbl %>%
  select(Attrition,YearsSinceLastPromotion) %>%
  plot_ggpairs(Attrition)

# Once again the density plot shows, that a slight but a visible amount of people
# that are leaving have a higher density for fewer years since their last promotion.
# However, we have seen before that also more people tend to leave that have
# fewer working years in general, so they were probably never promoted since they just started.
# There is some sort of correlation, so based on just this plot we cannot make a
# clear statement (c).

  

