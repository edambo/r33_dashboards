# This function generates a data frame of column names and descriptions based on the attributes created in the codebook generation files
# using the cb_add_col_ attributes function.
# It requires the codebook and dplyr packages to work.



var_descriptions <- function(cb_df) {
  # Extract df column names into a list
  col_names <- cb_df %>% names()

  # Create a function that produces a nested list containing a column name and its description
  col_desc <- function(col_name, cb_df) {
    # extract the list of attributes for a variable
    indv_attri <- attributes(cb_df[[col_name]])
    # extract the description attribute from the list
    desc_list <- indv_attri[names(indv_attri) == "description"]
    # combine the variable name and description into a list, name first
    desc_list <- append(desc_list, list(name = col_name), after = 0)
    return(desc_list)
  }

  # Apply col_desc function over list of column names to produce a nested list containing the column names and descriptions
  df_desc_df <- lapply(col_names, col_desc, cb_df = cb_df) %>%
    # bind nested lists into a matrix with two rows named "names" and "descriptions".
    do.call(cbind, .) %>%
    # transpose matrix to move the row names to the column names.
    t() %>%
    # convert matrix to dataframe
    as.data.frame() %>%
    # convert any list columns to character columns
    mutate(
      across(
        .cols = everything(),
        .fns = ~ unlist(.x),
        .names = "{col}"
      )
    )
}
