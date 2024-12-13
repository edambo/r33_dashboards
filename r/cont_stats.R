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
#'   cont_stats(mpg, 1)
#'   
cont_stats <- function(.data, .col, .digits) {
  n_mean_ci <- .data |> 
    n_mean_ci({{.col}}, .digits)
  
  n_median_ci <- .data |> 
    n_median_ci({{.col}}, .digits)
  
  results <- dplyr::bind_rows(
    n_mean_ci,
    n_median_ci
  )
  
  # Return results
  results
}

# For testing
# mtcars |>
#   cont_stats(mpg, 1)