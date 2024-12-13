#' Calculate N, Mean, and 95% Confidence Interval of a Numeric Column
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
#'   n_mean_ci(mpg, 1)
#'   
n_mean_ci <- function(.data, .col, .digits) {
  .data |> 
    meantables::mean_table({{.col}}) |> 
    meantables::mean_format("mean (lcl - ucl)", digits = .digits) |> 
    dplyr::select(var = response_var, n, formatted_stats)
    # dplyr::mutate(var = paste0(var, ", mean (95% CI)"))
}

# For testing
# mtcars |>
#   n_mean_ci(mpg, 1)