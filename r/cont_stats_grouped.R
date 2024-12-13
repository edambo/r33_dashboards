#' Calculate N, Mean and 95% Confidence Interval, and Median and 95% Confidence Interval of a Numeric Column
#'
#' @param .data A data frame
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
#'   cont_stats_grouped(mpg, 1)
#'   
cont_stats_grouped <- function(.data, .col, .digits) {
  n_mean_ci_grouped <- .data |> 
    n_mean_ci_grouped({{.col}}, .digits)
  
  n_median_ci_grouped <- .data |> 
    n_median_ci_grouped({{.col}}, .digits)
  
  result <- dplyr::bind_rows(
    n_mean_ci_grouped,
    n_median_ci_grouped
  )
  
  # Return result
  result
}

# For testing
# mtcars |>
#   filter(!is.na(cyl)) |> 
#   group_by(cyl) |> 
#   cont_stats_grouped(mpg, 1)