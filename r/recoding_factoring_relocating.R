# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper functions for recoding character columns to numeric columns, then 
# converting the numeric columns into factors, then relocating the factor 
# version of the column directly after the numeric version.
# Brad Cannell
# 2024-04-05
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# After making modifications, test for problems using:
# testthat::test_file(here::here("tests", "testthat", "test-recoding_factoring_relocating.R"))

# Style Notes: 
# - Leave 5 blank lines between functions to make the code easier to read.





#' Recode A Character Column To Numeric Column
#' 
#' @description
#' Takes a categorical character column and a named vector of numeric category 
#' codes for each category as inputs and returns a numeric version of the 
#' column. Later, the numeric column will be converted to a factor.
#'
#' @param .col The name of the column containing the categorical character values.
#' @param .recode A named vector of numeric category codes.
#'
#' @return A categorical numeric vector.
#' 
char_to_num <- function(.col, .recode) {
  # Check to make sure that recode contains the same values as the column
  col_vals <- unique(.col)
  char_vals <- names(.recode)
  in_col_not_recode <- dplyr::setdiff(col_vals, char_vals)
  in_col_not_recode <- in_col_not_recode[!is.na(in_col_not_recode)] # Don't include NA
  if (length(in_col_not_recode) > 0) {
    stop("Values in .col, but not .recode: ", in_col_not_recode)
  }
  
  # Recode character values to numeric values
  for (i in seq_along(char_vals)) {
    .col[.col == char_vals[i]] <- .recode[i]
  }
  .col <- as.numeric(.col)
  # Return vector
  .col
}





#' Recode Multiple Character Columns To Numeric Columns
#' 
#' @description
#' Takes a character vector of categorical character column names and a named 
#' vector of numeric category codes for each category as inputs and returns a 
#' numeric version of each column. Later, the numeric columns will be converted 
#' to factors.
#'
#' @param .data A data frame.
#' @param .cols A character vector of categorical character column names.
#' @param .recode A named vector of numeric category codes for each category.
#'
#' @return
#' 
chars_to_nums <- function(.data, .cols, .recode) {
  .data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(.cols),
        ~ char_to_num(.x, .recode)
      )
    )
}





#' Coerce Multiple Numeric Columns To Factors
#' 
#' @description
#' Takes a character vector of categorical numeric column names and a named 
#' vector of numeric category codes for each category as inputs and returns a 
#' factor version of each column. A "_f" is appended to the end of the factor
#' column name.
#'
#' @param .data A data frame.
#' @param .cols A character vector of categorical numeric column names.
#' @param .recode A named vector of numeric category codes for each category.
#'
#' @return A data frame
#' 
factors <- function(.data, .cols, .recode) {
  .data |>
    dplyr::mutate(
      dplyr::across(
        .cols  = dplyr::all_of(.cols),
        .fns   = ~ factor(.x, .recode, names(.recode)),
        .names = "{col}_f"
      )
    )
}





#' Relocate Factor Version Of Columns Immediately After Numeric Version
#'
#' @param .data A data frame.
#' @param .cols A character vector of categorical numeric column names.
#'
#' @return A data frame.
#' 
relocate_factors <- function(.data, .cols) {
  # Loop over each column in .cols. Relocate the _f version immediately after
  # the numeric version.
  for (col in .cols) {
    col_f <- paste0(col, "_f")
    .data <- .data |>
      dplyr::relocate(all_of(col_f), .after = all_of(col))
  }
  # Return data frame
  .data
}
