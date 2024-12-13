# The purpose of the function in this file is to compare the variables included 
# in the last run of the codebook file to the variables in the last run of the 
# data cleaning file so it's easier to determine which variables need to be 
# included and which need to be removed from the codebook.

# Inputs
# df: This is the dataframe object of the cleaned data.
# desc_rds_file_name: This is the name of the RDS file in the variable 
# descriptions folder for the specific data set that was created at the previous
# run of the codebook creation file.

vars_to_update <- function(df, desc_rds_file_name){
  # Load variable description file
  
  desc_file_path <- here::here("codebooks", "variable_descriptions", 
                               {{desc_rds_file_name}})
  var_desc <- readr::read_rds(desc_file_path) 
  
  # Create list of variable names in previous version of the code book
  var_names_cb <- var_desc[['name']] %>% sort()
  
  # Create list of variable names in current version of the the cleaned data set
  var_names_df <- df %>% names() %>% sort()
  
  # Get list of values that differ between the two lists
  old <- setdiff(var_names_cb, var_names_df) %>% as.data.frame() # in old, not in new
  new <- setdiff(var_names_df, var_names_cb) %>% as.data.frame() # in new, not in old
  
  if(nrow(old) == nrow(new) & nrow(old) == 0){
    message <- "The codebook creation file is up-to-date. No changes to variables necessary."
    return(message)
  }else if(nrow(old) != 0 & nrow(new) == 0){
    message <- c("There are variables in the codebook creation file that are not",
                 "\nin the most recently updated version of the cleaned data file.")
    out <- list(message, old)
    return(out)
  }else if(nrow(new) != 0 & nrow(old) == 0){
    message <- c("There are variables in the in the most recently updated version",
                 "\nof the cleaned data file that are not in the codebook creation file.")
    out <- list(message, new)
    return(out)
  }else if(nrow(new) != 0 & nrow(old) != 0){
    message <- c("There are variables in the codebook creation file that are not",
                 "\nin the most recently updated version of the cleaned data file.",
                 "There are also variables in the in the most recently updated version",
                 "\nof the cleaned data file that are not in the codebook creation file."
                 )
    out <- list(message = message, `in codebook not in dataset` = old, 
                `in dataset not in codebook` = new)
    return(out)
  }
}