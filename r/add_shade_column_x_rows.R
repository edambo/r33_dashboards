# For a data frame ordered as desired, add a column that can be used to Shade every
# Other x number of rows. e.g every other 3 rows or even every other 100 rows.

add_shade_column_x_rows <- function(df, shade_size){
  # Get number of rows of df
  df_rows <- nrow(df)
  
  # Create list of row numbers to shade
  shade_rows <- c()
  
  for(r in 0:df_rows){
    
    if(shade_size == 1){
      r_1 <- 1 + 2*r
      shade_rows <- append(shade_rows, c(r_1))       
    }
    else if(shade_size > 1){
      for(s in 1:shade_size){
        out <- shade_size + s + (2*shade_size*r)
        assign(x = paste0("r_", s), value = out)
      }
      shade_values <- paste0("r_", 1:shade_size)
      shade_rows <- append(
        shade_rows, 
        unlist(lapply(shade_values, function(x){eval(parse(text = x))}))
      )
    }
    
  }
  
  # Create a binary column to indicate which rows to shade
  df %>% mutate(
    shade_col = case_when(
      row_number() %in% shade_rows == TRUE ~ 1,
      TRUE ~ 0
    )
  ) %>% relocate(shade_col)
}