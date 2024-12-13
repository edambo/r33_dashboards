#' Calculate N, Overall Percent, and 95% Confidence Interval of a Categorical Column
#'
#' @param .data A data frame
#' @param .col A categorical column
#' @param .digits Number of digits to round to
#'
#' @return A data frame
#' @export
#'
#' @examples
#' mtcars |>
#'   n_percent_ci(cyl, 1)
#'   
n_percent_ci <- function(.data, .col, .digits) {
  .data |> 
    dplyr::filter(!is.na({{.col}})) |> 
    freqtables::freq_table({{.col}}) |> 
    freqtables::freq_format(recipe = "percent (lcl - ucl)", digits = .digits) |> 
    dplyr::select(var, cat, n, formatted_stats)
}

# For testing
# mtcars |>
#   n_percent_ci(cyl, 1)