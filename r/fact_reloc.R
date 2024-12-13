#' Create a Factor Column and Relocate it
#' 
#' @description This function will create a new factor version of a column and 
#'   then position the new factor column directly behind the non-factor version 
#'   of the column in the data frame. The new factor column will automatically 
#'   use the `_f` naming convention. Internally, it uses a combination of 
#'   `factor()` and `dplyr::relocate()`. That's where the factor name comes from.
#'
#' @param .data A data frame
#' @param .col The column to base the factor column on
#' @param .levels an optional vector of the unique values (as character strings) 
#'   that x might have taken. The default is the unique set of values taken by 
#'   as.character(x), sorted into increasing order of x. Note that this set can 
#'   be specified as smaller than sort(unique(x)).
#' @param .labels either an optional character vector of labels for the levels 
#'   (in the same order as levels after removing those in exclude), or a 
#'   character string of length 1. Duplicated values in labels can be used to 
#'   map different values of x to the same factor level.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' mtcars |> 
#'   fact_reloc(cyl, c(4, 6, 8), c("4 cyl", "6 cyl", "8 cyl"))
#'   
fact_reloc <- function(.data, .col, .levels, .labels) {
  # Create a `_f` version of the column name. The glue syntax used in
  # `mutate()` doesn't work in `relocate()`.
  nm <- rlang::as_name(rlang::enquo(.col))
  nm_f <- paste(nm, "f", sep = "_")
  
  .data |> 
    dplyr::mutate(
      "{{.col}}_f" := factor({{.col}}, .levels, .labels)
    ) |> 
    dplyr::relocate(!! nm_f, .after = {{.col}})
}

# For data checking
# mtcars |> 
#   fact_reloc(cyl, c(4, 6, 8), c("4 cyl", "6 cyl", "8 cyl"))