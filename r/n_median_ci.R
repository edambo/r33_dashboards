#' Calculate N, Median, and 95% Confidence Interval of a Numeric Column
#'
#' @param .data A data frame
#' @param .col A numeric column
#' @param .digits Number of digits to round to
#'
#' @return A data frame
#' @export
#' @importFrom rlang !!
#'
#' @examples
#' mtcars |>
#'   n_median_ci(mpg, 1)
#'   
n_median_ci <- function(.data, .col, .digits) {
  .data |>  
    dplyr::summarise(
      var    = !! rlang::enquo(.col) |> rlang::as_name(),
      n      = sum(!is.na({{.col}})),
      n_miss = sum(is.na({{.col}})),
      median = stats::median({{.col}}, na.rm = TRUE),
      lcl    = sort({{.col}})[stats::qbinom(.025, length({{.col}}), 0.5)],
      ucl    = sort({{.col}})[stats::qbinom(.975, length({{.col}}), 0.5)]
    ) |> 
    meantables::mean_format("median (lcl - ucl)", digits = .digits) |> 
    dplyr::select(var, n, formatted_stats) 
    # dplyr::mutate(var = paste0(var, ", median (95% CI)"))
}

# For testing
# mtcars |>
#   n_median_ci(mpg, 1)