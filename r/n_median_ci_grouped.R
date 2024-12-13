#' Calculate N, Median, and 95% Confidence Interval of a Numeric Column by Group
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
#'   n_median_ci_grouped(mpg, 1)
#'
n_median_ci_grouped <- function(.data, .col, .digits) {
  result <- .data |>  
    dplyr::summarise(
      var    = !! rlang::enquo(.col) |> rlang::as_name(),
      n      = sum(!is.na({{.col}})),
      n_miss = sum(is.na({{.col}})),
      median = stats::median({{.col}}, na.rm = TRUE),
      lcl    = sort({{.col}})[stats::qbinom(.025, length({{.col}}), 0.5)],
      ucl    = sort({{.col}})[stats::qbinom(.975, length({{.col}}), 0.5)]
    ) |> 
    meantables::mean_format("median (lcl - ucl)", digits = .digits) |> 
    dplyr::select(var, group_cat = 1, n, formatted_stats) |>
    # dplyr::mutate(var = paste0(var, ", median (95% CI)")) |> 
    # Display by group
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
#   n_median_ci_grouped(mpg, 1)