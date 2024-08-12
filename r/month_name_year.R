# Convert POSIXCT date variable to month - year variable e.g Jun 2023

month_year <- function(data, var, var_name){
  data %>% 
    mutate(
      month_year = paste(month.abb[month({{var}})], 
                             year({{var}}), sep = " "),
      {{var_name}} := forcats::fct_reorder(month_year, {{var}})
    )
}

# test <- month_year(data, visit_date, month_test)