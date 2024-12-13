# Function that generates a data file info message. 

broad_check_message <- function(data_object, file_path_object, 
                                object_type = c("dataframe", "list")){
  # Allow only one option to be selected
  object_type <- match.arg(object_type)
  
  # Create data name
  data_name <- as.character(match.call()$data_object)
  data_name <- sub("_", " ", data_name) |> toupper()
  
  if(object_type == "dataframe"){
    cat(
      "\n",
      paste0(Sys.Date(), ":"),
      data_name, "data imported with", nrow(data_object), "rows and", 
      ncol(data_object), "columns.\n",
      "Data last modified on OneDrive",
      format(file.info(file_path_object)$mtime, "%Y-%m-%d %H:%M:%S"), "\n"
    )
  }

  if(object_type == "list"){
    cat(
      "\n",
      paste0(Sys.Date(), ":"),
      data_name, "data imported with", length(data_object), "entries.\n",
      "Data last modified on OneDrive",
      format(file.info(file_path_object)$mtime, "%Y-%m-%d %H:%M:%S"), "\n"
    )
  }  
  
  # Remove path from memory, no longer needed
  rm(list = deparse(substitute(file_path_object)), envir = .GlobalEnv)
}