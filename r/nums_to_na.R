# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function To Help Convert Selected Numeric Values To NAs
# Brad Cannell
# 2024-04-09
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# After making modifications, test for problems using:
# testthat::test_file(here::here("tests", "testthat", "test-nums_to_na.R"))

# Style Notes: 
# - Leave 5 blank lines between functions to make the code easier to read.





#' Function To Help Convert Selected Numeric Values To NA
#' 
#' @description
#' Numeric categorical variables often code responses like "Don't know" and 
#' "Refused" as 7 and 9 or 77 and 99. This function will help us convert those
#' response to NA while preserving column attributes (e.g., labels). 
#'
#' @param .col The name of the column containing the numerical values to be 
#'   converted to NA.
#' @param .na_values A vector of numeric values to convert to NA. 
#'
#' @return A numeric vector.
num_to_na <- function(.col, .na_values) {
  # Store the attributes
  x_attr <- attributes(.col)
  # Convert numeric values to NA's
  .col <- dplyr::if_else(.col %in% .na_values, NA_real_, .col)
  # Add attributes back to the vector
  attributes(.col) <- x_attr
  # Return .col
  .col
}





#' Function To Help Convert Selected Numeric Values To NA Across Multiple Columns
#' 
#' @description
#' This function is similar to num_to_na(). However, instead of passing it a 
#' single numeric column, you pass it a data frame and a vector of column names 
#' with numeric values to convert to NA.
#'
#' @param .data A data frame.
#' @param .cols The name of the columns containing the numerical values to be 
#'   converted to NA.
#' @param .na_values A vector of numeric values to convert to NA. 
#' @param .suffix A suffix attached to the original column name. This function
#'   assumes that the new column name (i.e., the column with "Don't know" and
#'   "Refused" converted to NA) will have the form 
#'   "{original column name}_{.suffix}".
#'
#' @return A data frame.
nums_to_nas <- function(.data, .cols, .na_values, .suffix) {
  .data |> 
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(.cols),
        ~ num_to_na(.x, .na_values),
        .names = "{col}_{.suffix}"
      )
    )
}





#' Relocate New Column With NA Values Immediately After The Original Column
#'
#' @param .data A data frame.
#' @param .cols A character vector of categorical numeric column names.
#' @param .suffix A suffix attached to the original column name. This function
#'   assumes that the new column name (i.e., the column with "Don't know" and
#'   "Refused" converted to NA) will have the form 
#'   "{original column name}_{.suffix}".
#'
#' @return A data frame.
#' 
relocate_na_cols <- function(.data, .cols, .suffix) {
  # Loop over each column in .cols. Relocate the new version immediately after
  # the original version.
  for (col in .cols) {
    col_suffix <- paste(col, .suffix, sep = "_")
    .data <- .data |>
      dplyr::relocate(all_of(col_suffix), .after = all_of(col))
  }
  # Return data frame
  .data
}
