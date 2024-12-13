#' Calculate N, Mean, and 95% Confidence Interval of a Numeric Column by Group
#'
#' @param .data A grouped data frame
#' @param .col A numeric column
#' @param .digits Number of digits to round to
#'
#' @return A data frame
#' @export
#'
#' @examples
#' mtcars |>
#'   filter(!is.na(cyl)) |>
#'   group_by(cyl) |>
#'   n_mean_ci_grouped(mpg, 1)
#'
n_mean_ci_grouped <- function(.data, .col, .digits) {
  result <- .data |> 
    meantables::mean_table({{.col}}) |> 
    meantables::mean_format("mean (lcl - ucl)", digits = .digits) |> 
    dplyr::select(var = response_var, group_cat, n, formatted_stats) |> 
    # dplyr::mutate(var = paste0(var, ", mean (95% CI)")) |> 
    # Display by group_cat
    tidyr::pivot_wider(
      names_from = "group_cat",
      values_from = c("n", "formatted_stats")
    ) 
  
  
  # Reorder columns so that n_<group> and formatted_stats_<group> always follow
  # each other
  n_group_levels     <- ncol(result) - 1
  first_half         <- (n_group_levels / 2) |> seq_len()
  second_half        <- (n_group_levels / 2) + first_half
  first_second_mixed <- c(rbind(first_half, second_half))
  first_second_mixed <- first_second_mixed + 1 # account for the first column, `var`
  result             <- dplyr::select(result, 1, all_of(first_second_mixed))
  
  # Return result
  result
}

# For testing
# mtcars |>
#   filter(!is.na(cyl)) |>
#   group_by(cyl) |>
#   n_mean_ci_grouped(mpg, 1)

# l2c_survey |> 
#   filter(!is.na(group_f)) |> 
#   group_by(group_f) |> 
#   n_mean_ci_grouped(ml_age, 1)