# Because of the way freq_table() works, it's better to pass the grouping
# variable directly to freq_table() inside this function than it is to 
# pass the function a grouped data frame.
# I really need to make some edits to freq_table()
n_percent_ci_grouped <- function(.data, .col, .group_by, .digits) {
  result <- .data |> 
    dplyr::filter(!is.na({{.col}})) |> 
    freqtables::freq_table({{.group_by}}, {{.col}}) |> 
    freqtables::freq_format(recipe = "percent_row (lcl_row - ucl_row)", digits = .digits) |> 
    dplyr::select(var = col_var, group_cat = row_cat, cat = col_cat, n, formatted_stats) |> 
    # Display by group_cat
    tidyr::pivot_wider(
      names_from = "group_cat",
      values_from = c("n", "formatted_stats")
    )
  
  # Reorder columns so that n_<group> and formatted_stats_<group> always follow
  # each other
  n_group_levels     <- ncol(result) - 2 # account for `var` and `cat`
  first_half         <- (n_group_levels / 2) |> seq_len()
  second_half        <- (n_group_levels / 2) + first_half
  first_second_mixed <- c(rbind(first_half, second_half))
  first_second_mixed <- first_second_mixed + 2 # account for `var` and `cat`
  result             <- dplyr::select(result, 1, 2, all_of(first_second_mixed))
  
  # Return results
  result
}

# For testing
# mtcars |>
#   filter(!is.na(am)) |> 
#   n_percent_ci_grouped(cyl, am, 1)