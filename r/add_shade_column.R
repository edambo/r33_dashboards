#' Add a Column That We Can Use to Shade Every Other Variable Name
#' 
#' @description This function is adds a new column to a data frame called 
#'   `shade`, which is always in the first position. The value of `shade`
#'   will alternate between TRUE and FALSE according to the value of the 
#'   `var` column. It will be used to add a background to the DataTable.
#'
#' @param .data 
#' @param .shade_group 
#'
#' @return
#' @export
#'
#' @examples
add_shade_column <- function(.data, .shade_group = var) {
  .data |> 
    mutate(
      var = factor({{.shade_group}}, levels = unique({{.shade_group}}))
    ) |>
    group_by(var) |>
    mutate(
      shade = cur_group_id() %% 2 == 0
    ) |> 
    # Always make shade the first column so that it is easy to hide by position later
    select(shade, everything()) |> 
    ungroup()
}

# For testing
# mtcars |>
#   add_shade_column(.shade_group = cyl)