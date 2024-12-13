#' Get a summary table of unique values in target columns, with counts.
#'
#' @description This function generates counts of unique values in a column,
#'   or unique combinations of values (when more than one column is passed).
#'   This is most useful if there are values common between a set of columns,
#'   such as boolean/logical TRUE/FALSE, a discrete set of integers, or a
#'   factor. The first column will be the list of all possible values found in
#'   the input data frame column(s), with counts for each column processed.
#'   
<<<<<<< HEAD
=======
#'   Dependencies: dplyr
#'   
#'   Built: R (4.2.2); dplyr (1.1.1)
>>>>>>> origin/main
#'
#' @param .df A data frame or vectorized column from a data frame ($ or [['']])
#' @param ... Specified column names. May be strings or symbols. 
#'   Ignored if .df is a vectorized column. If no argument is specified, 
#'   it will process all columns of the data frame. 
#'
#' @return A data frame
#' @export
#'
#' @examples
#' mtcars |>
#'   dplyr::select(cyl, gear) |>
#'   get_unique_value_summary()
#'   


get_unique_value_summary <- function(.df, ... ) {
  
  # Parse & Check Inputs
  # =========================================================================
  # Ensure .df is a data frame or vector
  
  if (!is.data.frame(.df) & !is.vector(.df)) {
    message <- paste(
      "The value entered into .df is not a data frame or vector.", 
      "Please check input and retry."
    )
    stop(message)
  }
  
  # If .df is a vector ....
  # ------------------------------------------------------------------------- 
  # Extract column name. This will ignore anything passed into ( ... )
  
  if (is.vector(.df)) {
    if (deparse(substitute(.df)) %like% '$')
      t_cols <- substring(
        deparse(substitute(.df)),
        which(strsplit(deparse(substitute(.df)),'')[[1]]=='$')[1]+1)
    if (deparse(substitute(.df)) %like% '\\[')
      t_cols <- substring(
        deparse(substitute(.df)), 
        # Start position of cut
        which(strsplit(deparse(substitute(.df)),'')[[1]]=='[')[2]+2,
        # End position of cut
        which(strsplit(deparse(substitute(.df)),'')[[1]]==']')[1]-2
      )
    
    # Return a warning that other arguments are being ignored
    
    if (length(list( ... )) > 0){
      message <- "First argument is a vector. Ignoring any other arguments."
      warning(message)
    }
  }
  
  # If .df is not a vector ....
  # -------------------------------------------------------------------------
  
  if (!is.vector(.df)){
    
    # If ( ... ) was empty, extract all column names to count the entire table
    if (length(list(...)) == 0) {
      t_cols <- colnames(.df)
      # Print warning, as this may be very large
      message <- paste(
        "No column names were specified. Processing all", 
        length(t_cols),
        "columns in the entered data frame. This may create a large table",
        "that is difficult to read."
      )
      warning(message)
    }
    
    # Otherwise, convert columns passed into ( ... ) into a list of 
    # string column names.
    if (length(list(...)) > 0) {
      t_cols <- paste(
        as.character(eval(substitute(alist( ... )))), 
        collapse = NULL
      )
    }
    
    # Check if any columns are missing - omit the missing columns
    missing_t_cols <- setdiff(t_cols, colnames(.df))
    t_cols <- setdiff(t_cols, missing_t_cols)
    
    # Stop processing if there are no columns remaining
    if (length(t_cols) == 0){
      message <- paste(
        "No entered variables were present in the entered data frame.",
        "Please check inputs and try again."
      )
      stop(message)
    }
    
    # Print a warning if any columns were missing.
    if (length(missing_t_cols) > 0){
      message <- paste(
        "The following,", 
        length(missing_t_cols), 
        "columns are not present in .df.", 
        "They will be omitted from processing: \n",
        paste(missing_t_cols, collapse = ', ')
      )
      warning(message)
    }
    
    rm(missing_t_cols)
  }
  
  # Processing
  # =========================================================================
  
  # Vector Processing - Convert into Data Frame with Named Column
  # -------------------------------------------------------------------------
  if (is.vector(.df)){
    .df <- data.frame(data_column = {{.df}})
    colnames(.df) <- t_cols
    
  }
  # Initialize output data frame
  # ------------------------------------------------------------------------
  # Extract a list of unique values in the target columns
  
  val <- unique(as.factor(as.vector(as.matrix(.df[t_cols]))))
  
  # Initialize output tibble with all possible values
  
  unique_summary <- data.frame("col_value"=val)
  
  # Column-wise processing
  # -------------------------------------------------------------------------
  # Get counts of unique values in each column
  
  for (i in t_cols){
    
    # utilizes table to get summary count of each column
    
    table <- as.data.frame(table(.df[i]))
    
    # sets column names to "value" and "freq"
    
    colnames(table) <- c("col_value","freq")
    
    # adds count of missing values in each column
    
    table<- add_row(table, col_value = NA, freq = sum(is.na(.df[i])))
    
    # read names of columns to "value" and the name of the target column(s)
    
    colnames(table) <- c("col_value",i)
    
    # joins table's summary counts to complete the count values
    
    unique_summary <- left_join(unique_summary,table, by="col_value")
  }
  
  # Replace any missing values in counts with 0 (does not remove it from value)
  
  unique_summary <- unique_summary |>
    dplyr::mutate_at(vars(-c(col_value)), ~replace(., is.na(.), 0))
  
  # Sort data frame by 'value'
  unique_summary <- unique_summary |> 
    dplyr::arrange(col_value)
  
  # returns completed data frame
  
  unique_summary 
}