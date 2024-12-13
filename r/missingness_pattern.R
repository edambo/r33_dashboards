# Create flextable showing missingness patterns for a data frame.

# The arguments are the dataframe, the variables of interest and a dataframe
# with two columns - a column of variable names named "var" and a column of
# variable labels named "label".

missing_pattern <- function(df, vars, label_df){
  # Initialize empty list for the total number of missing values for each variable
  var_na_total <- c()
  for(i in vars){
    var_na <- df %>% ungroup() %>% select({{i}}) %>%
      filter(is.na(.data[[i]])) %>%
      nrow()
    var_na_total <- append(var_na_total, var_na)
  }
  # Add blank value for the "count" column
  var_na_total <- append(var_na_total, "")
  
  # Create an object containing the number of columns in the output table.
  col_count <- length(vars) + 1
  
  # Set header rotation
  if(col_count < 5){
    rotation <- "lrtb" # horizontal
  } else{
    rotation <- "btlr" # vertical
  }
  
  # Set column width based on number of columns
  if(col_count < 21){
    col_width <- 6.45/col_count # Protrait orientation
  } else{
    col_width <- 8.95/col_count # Landscape orientation
  }
  
  # For each variable, create a new column that has the value 0 when the original
  # value is 'NA' and the value 1 when the original value is not 'NA'.
  sum_df <- df %>% ungroup() %>%
    select(all_of(vars)) %>%
    mutate(
      across(
        .cols = everything(),
        .fns = ~case_when(
          is.na(.x) ~ 0,
          TRUE      ~ 1
        )
      )
    ) %>% 
    group_by(across(all_of(vars))) %>%
    reframe(count = n()) %>% arrange() %>%
    
    # Add row of the counts of missing values for each variable.
    rbind(var_na_total)
  
  # Set header labels values
  col_names <- names(sum_df)
  label_df <- label_df %>% filter(var %in% col_names)
  values <- setNames(label_df[["label"]], label_df[["var"]]) %>% as.list()
  
  # Create flextable
  sum_df %>% flextable() %>% add_footer_row(
    values = c("Each row corresponds to a missing data pattern (1 = observed, 0 = missing).
               The right-most column represents the number of rows with each pattern and the bottom row represents the total counts of missing values for each variable."),
    colwidths = c(col_count), top = TRUE) %>% 
    set_header_labels(values = values) %>%
    align(align = "left", part = "all") %>%
    bold(bold = TRUE, j = c(col_count) , part = "body") %>% 
    bold(bold = TRUE, i = c(nrow(sum_df)) , part = "body") %>%
    bold(bold = TRUE, j = 1:(col_count) , part = "header") %>%
    color(j = 1:col_count, color = "#a6a6a6", part = "header") %>%
    height(height = 1.2, part = "header") %>%
    hrule(i = 1, rule = "exact", part = "header") %>%
    rotate(j = 1:col_count, align = "bottom", 
           rotation = rotation, part = "header") %>% 
    set_caption("Missing Data Patterns") %>%
    width(width = col_width) %>%
    paginate(init = TRUE, hdr_ftr = TRUE)
}