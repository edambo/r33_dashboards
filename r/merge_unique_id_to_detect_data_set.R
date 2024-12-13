# The purpose of the function in this file is to merge the unique person ID to each of the DETECT FU Interview data sets.

unique_par_path <- here::here("data", "unique_id_creation", "participant_unique_id.rds")
unique_par <- readRDS(unique_par_path)%>% select(medstar_id, unique_id)

merge_id <- function(detect_df){
  df_unique_id <- detect_df %>% 
    left_join(unique_par, by = "medstar_id") %>% 
    relocate(unique_id, .before = "medstar_id")
}