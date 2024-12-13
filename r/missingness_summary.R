# Create flextable showing a missingness summary of the number of missing vs
# non-missing rows for each variable.

# The arguments are the dataframe, the variables of interest and a dataframe
# with two columns - a column of variable names named "var" and a column of
# variable labels named "label".

missing_summary <- function(df, vars, label_df){
  # Get count for number of rows with any missing value
  total <- df %>% filter(if_any(everything(), is.na)) %>% nrow()
  
  # Initiate empty data frame for missing value summary 
  test_na_total <- data.frame()
  
  # Iteratively add missing counts for each variable into the data frame
  for(var in vars){
    missing_rows <- df %>% ungroup() %>% select({{var}}) %>%
      filter(is.na(df[[var]])) %>%
      nrow()
    test_na_total <- rbind(test_na_total, cbind(var, missing_rows))
  }
  
  # Replace variable names with variable labels
  test_na_total <- label_df %>% right_join(test_na_total, by = "var") %>% 
    select(-c(var)) 
  
  # Add a row with the total missing row count to the missing summary data frame
  test_na_total <- test_na_total %>% rbind(c("Total", total)) %>%
    mutate(
      # Add a column with counts of rows with non-missing data.
      non_missing_rows =  nrow(df) - as.numeric(missing_rows)
    )
  
  # Set init value for paginate based on number of variables
  if(length(vars) < 20){
    init <- TRUE
  } else{
    init <- FALSE
  }
  
  test_na_total %>% flextable() %>% 
    set_header_labels(label = "",
                      missing_rows = "Missing",
                      non_missing_rows = "Non-missing") %>%
    bold(bold = TRUE, i = c(nrow(test_na_total)) , part = "body") %>%
    bold(bold = TRUE, j = 1 , part = "body") %>%
    bold(bold = TRUE, part = "header") %>%
    width(j = c(1,2,3), width = c(4.5, 1,1.4)) %>%
    add_footer_row(
      values = c("The values in the 'Missing' column represent the number of rows with missing values for each variable, or in the case of the total, the number of rows with any missing value. The values in the 'Non-missing' column represent the number of rows with non-missing values for each variable, or in the case of the total, the number of rows no missing values."),
      colwidths = c(3), top = TRUE) %>%
    paginate(init = init, hdr_ftr = TRUE)
}