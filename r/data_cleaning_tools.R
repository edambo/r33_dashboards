# This file contains functions used in the data management data cleaning files. 
# These functions provide summaries of data variable values that help in the 
# data-cleaning process.


# Function that produces a combined list of unique values in multiple dataframe 
# columns given the dataframe and a character vector of the column names as 
# arguments
get_values <- function(df, vars){
  all_values <- c()
  for (var in vars){
    tab_list <- as.list(table(df[[var]]))
    values <- sort(attributes(tab_list)[["names"]]) 
    all_values <- unique(append(all_values, values))
  }
  
  all_values
}

# A function to get a list of dataframe variables with the same unique
# values as a provided character vector. The arguments are a dataframe and a 
# character vector containing the desired values.
get_vars_with_values <- function(df, values){
  vars <- names(df)
  vars_list <- c()
  for (var in vars){
    tab_list <- as.list(table(df[[var]]))
    var_values <- sort(attributes(tab_list)[["names"]])
    if(setequal(var_values, sort(values)) == TRUE){
      vars_list <- append(vars_list, var)
    }
  }
  vars_list
}

# A function to create a dataframe of column names, their data class, range of
# values (numeric variables) and number of levels (factor variables)
col_characteristics <- function(df){
  cols <- names(df)
  desc_list <- c()
  for(col in cols){
    type <- class(df[[col]])
    if(length(type) == 1){
      if(type == "numeric"){
        range_of <-  paste(min(df[[col]], na.rm = TRUE), "-", 
                           max(df[[col]], na.rm = TRUE))
        descr <- paste(type, "with", "range:", range_of)
      }else if(type == "factor"){
        n_levels <- nlevels(df[[col]])
        descr <- paste(type, "with",  n_levels, "levels")
      }else if(type == "character"){
        descr <- paste(type)
      }else{
        descr <- paste(type)
      }
    }else{
      descr <- paste(type, collapse = ", ")
    }
    desc_list <- append(desc_list, descr)
  }
  data.frame(col_names = cols, descriptions = desc_list)
}

# Function that drops the original variable if it is the same as the created 
#variable with a "cat" suffix.
drop_dup_orig <- function(df, cols, cols_suffix){
  # Create a nested list that has a column name and its "cat" column as the
  # components of each list level.
  if (length(cols) > 1){
    cols_cat_list <- sort(c(cols, cols_suffix)) %>% cbind() %>% 
      split(., cut(seq_along(.), length(cols), labels = FALSE))
  }else if(length(cols) == 1){
    cols_cat_list <- list(sort(c(cols, cols_suffix)))
  }
  
  # Create list of names of original variables that are the same as their "cat"
  # columns and need to be dropped.
  drop_original <- c()
  for (i in cols_cat_list){
    col_name <- i[[1]]
    col_suffix_name <- i[[2]]
    
    # Convert NA to 999 to prevent error due to NA
    df_na_char <- df %>% 
      mutate(
        across(
          .cols = all_of(c(cols, cols_suffix)),
          .fns = ~ case_when(
            is.na(.x) == TRUE ~ 999,
            TRUE      ~ .x
          )
        )
      )
    # Drop the variables with names in the cols character vector if they have 
    # the same values as the variables with names in the cols_suffix vector
    if(
      all(df_na_char[[col_name]] == df_na_char[[col_suffix_name]])
    ){
      drop_original <- append(drop_original, col_name)
    } 
  }
  # Drop created list of variables from the data set.
  df %>% select(-c(all_of(drop_original)))
}

# # Test
# cols_test <- c("one", "two", "three")
# cols_suffix_test <- c("one_cat", "two_cat", "three_cat")
# one <- c(rep(1,5), rep(0,5))
# two <- c(rep(2,5), rep(0,5))
# three <- c(rep(3,5), rep(0,5))
# one_cat <- c(rep(1,5), rep(0,5))
# two_cat <- c(rep(2,4), rep(0,6))
# three_cat <- c(rep(3,5), rep(0,5))
# test_df <- data.frame(one = one, one_cat = one_cat, two = two, two_cat = two_cat, 
#                       three = three, three_cat = three_cat)
# 
# drop_test <- drop_dup_orig(test_df, cols_test, cols_suffix_test)