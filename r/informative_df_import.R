#' Informative data frame import wrapper function
#'
#' @description Wrapper function that may be used to import data (CSV, RDS, 
#' Excel) into the .GlobalEnv when given the desired name (desired_name) and 
#' path (input_path). Has an option to facilitate overwriting any data at the
#' desired name that already exists (overwrite), and pass additional 
#' arguments for the readr::read_csv() and readxl::read_excel() functions (...)
#'   
#'   Dependencies: tools (base R), readxl
#'   
#'   Built: R (4.2.2); readxl (1.4.2)
#'
#' @param desired_name Desired name of data in the .GlobalEnv
#' @param input_path Path of desired file. Currently supports RDS, CSV (.csv),
#'                    and Excel files (.xlsx, .xls). Extension required in
#'                    file name in order to read.
#' @param overwrite Boolean/Logical, default = FALSE; When TRUE, indicates
#'                   that it is desireable to overwrite any existing data
#'                   in the .GlobalEnv that shares a name with desired_name, 
#'                   if it exists. If FALSE and this duplication occurs, 
#'                   raises a STOP message
#' @param ... Additional arguments for the wrapped readr::read_csv() and 
#'            readxl::read_excel() functions. These arguments should be
#'            passed just as if they were being directly passed to the target
#'            function, not wrapped in a List() or any other vector.
#'
#' @return An informative message displayed in RMD, QMD notebooks and a data
#'         frame in the .GlobalEnv with the desired name, from the desired
#'         path; otherwise, an informative error message.
#' @export
#'

informative_df_import <- function(desired_name, input_path, 
                                  overwrite = FALSE, ... ){
  # Set constants for function, for ease in editing
  .excel_extensions <- c('xlsx','xls')
  .csv_extensions <- c('csv')
  
  # 1. Parse & Check Inputs
  # =========================================================================
  # 1.a. Check that input_path is a valid path to a valid that exists.
  # -------------------------------------------------------------------------
  
  if (!file.exists(input_path)){
    .message <- paste(
      "No file exists at the provided path. Please check input and retry."
    )
    stop(.message)
  }
  
  # 1.b. Check that overwrite is boolean/logical
  # -------------------------------------------------------------------------
  
  if (!is.logical(overwrite)){
    .message <- paste(
      "The value entered for overwrite must be a boolean/logical value.",
      "Please check input and retry."
    )
    stop(.message)
  }
  
  # 1.c. Check that data_name is a potentially valid R object name.
  # -------------------------------------------------------------------------
  
  # Raise warning if data_name is not a string
  if (!is.character(desired_name) | length(desired_name) != 1){
    .message <- paste(
      "The value entered for data_name must be a single string.",
      "Please check input and retry."
    )
    stop(.message)
  }
  
  desired_name <- gsub(' ', '_', desired_name) |>
    tolower()
  
  # Additional verification checks:
  .message = c()
  
  # 1. Variable name too long:
  if (nchar(desired_name) >20) {
    .message <- c(
      .message, 
      paste0(
        "The value entered for data_name must be <= 20 characters in length.",
        " It is currently: ", 
        format(nchar(desired_name), big.mark = ','), 
        " characters long."
      )
    )
  }
  # 2. Variable name starts with anything other than a letter:
  if (!grepl(substr(desired_name,1,1), 'abcdefghijklmnopqrstuvwxyz')){
    .message <- c(
      .message,
      paste0(
        "The value entered for data_name must start with a letter. ",
        "it currently starts with: ", substr(desired_name,1,1), "."
      )
    )
  }
  # 3. Variable name ends with anything other than a letter or number:
  if (!grepl(
    substr(desired_name, nchar(desired_name), nchar(desired_name)),
    'abcdefghijklmnopqrstuvwxyz0123456789'
  )){
    .message <- c(
      .message,
      paste0(
        "The value entered for data_name must end with a letter or number. ",
        "it currently ends with: ", 
        substr(desired_name, nchar(desired_name), nchar(desired_name)), "."
      )
    )
  }
  # 4. Variable name contains any character other than a-z0-9 or underscore:
  if (!grepl('^[a-z0-9_]+$', desired_name)){
    .message <- c(
      .message,
      paste0(
        "The value entered for data_name must only include the letters a-z",
        " of the English alphabet, digits 0-9, and underscore characters."
      )
    )
  }
  # 5. If overwrite is not TRUE (default), ensure an object with the desired
  #    name does not already exist in the GlobalEnv
  if(!overwrite & exists(desired_name, envir = .GlobalEnv)){
    .message <- c(
      .message,
      paste(
        "An object named", desired_name, "already exists in the GlobalEnv ",
        "and overwrite is set to FALSE (default).",
        "Please check input and retry.")
    )
  }
  
  # Return stop message if any of the prior checks fail:
  if (length(.message) >0){
    .message <- paste(
      c(.message, "Please check input and retry."), 
      collapse= "\n"
    )
    stop(.message)
  }
  
  # 2. Import Data
  # =========================================================================
  # 2.a. Extract file extension, to determine how to read the file (tools)
  # -------------------------------------------------------------------------
  
  .file_extension <- tools::file_ext(input_path) |>
    tolower()
  
  # 2.b. Read files...
  # RDS (readr) 
  # -------------------------------------------------------------------------
  if (.file_extension == 'rds'){
    .data = readr::read_rds(input_path)
  }
  
  # Excel (readxl) - with and without ( ... ) wrapping
  # -------------------------------------------------------------------------  
  # if ( ... ) is not empty, use as additional arguments
  if (.file_extension %in% .excel_extensions & length(list(...)) > 0) {
    .data = readxl::read_excel(path = input_path, ...)
  }
  # Otherwise, use default
  if (.file_extension %in% .excel_extensions & !length(list(...)) > 0) {
    .data = readxl::read_excel(path = input_path)
  }
  
  # CSV (reader) - with and without ( ... ) wrapping
  # -------------------------------------------------------------------------  
  # if ( ... ) is not empty, use as additional arguments
  if (.file_extension %in% .csv_extensions & length(list(...)) > 0) {
    .data = readr::read_csv(file = input_path, ...)
  }
  # Otherwise, use default
  if (.file_extension %in% .csv_extensions & !length(list(...)) > 0) {
    .data = readr::read_csv(file = input_path)
  }
  
  # Assign to Global Environment
  
  assign(desired_name, .data, envir = .GlobalEnv)
  
  # 2.c Craft Export Message
  # -------------------------------------------------------------------------
  # Create Print Name (upper case, underscores replaced with spaces)
  .print_name <- gsub('_', ' ', desired_name) |>
    toupper()
  
  cat(
    "\n",
    paste0(Sys.Date(), ":"),
    .print_name, "data imported with", 
    format(nrow(.data), big.mark = ','), "rows and", 
    format(ncol(.data), big.mark = ','), "columns.\n",
    "Data last modified on OneDrive:",
    format(file.info(input_path)$mtime, "%Y-%m-%d %H:%M:%S"), "\n"
  )
  
  
  # Remove path from memory, no longer needed
  rm(list = deparse(substitute(input_path)), envir = .GlobalEnv)
}