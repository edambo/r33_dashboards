# =============================================================================
# Import the data that will be used to create the tables.
# Import `combined_participant_data.rds`. This dataset is created in 
# `link2care_public/data_survey_21_merge.Rmd`. Additionally, this code assumes 
# that this file is being run from the SharePoint `General` folder.
# 
# Use this code to import the data at the top of every .qmd file. That way, 
# the information in Administrative Information table on the home page is 
# correct for all tables.
#
# It looks like the Quarto team is looking for a way to pass data between qmd
# files inside of Quarto projects, but they don't have it ready yet. Maybe
# just keep an eye on it.
# Using a .Rprofile file?
# SO Post: https://stackoverflow.com/questions/72544775/passing-data-from-one-qmd-file-to-another-in-a-quarto-book-template?rq=1
# GH Issue: https://github.com/quarto-dev/quarto-cli/discussions/1045
# GH Issue: https://github.com/quarto-dev/quarto-cli/discussions/431
# =============================================================================

library(haven)

# Import the data that will be used for all of the descriptive tables.
# Some of these objects are also used in the Administrative Information table
# on the homepage.
df_nm      <- "combined_participant_data.rds"
folder     <- "../Participant Data/R Data/"
path       <- paste0(folder, df_nm)
l2c_survey <- readr::read_rds(path)


# Data management
# =============================================================================
# Keep visit 1 and 2 only
# This is a table of baseline statistics. We will only need visit 1, and in a 
# handful of cases, visit 2. This isn't strictly necessary, but it reduces the
# file size and memory overhead.
# At some point, we may want to reduce the file size even further by doing 
# by creating a data set with 1 row per participant with the baseline value 
# for each variable. 
l2c_survey_baseline <- dplyr::filter(l2c_survey, visit == 1 | visit == 2)

# Keep randomized participants only
# 2023-08-17: We want the sample to include participants who were randomized 
# only. Do not include participants who attended visit 1, but were not 
# randomized.
l2c_survey_baseline_randomized <- dplyr::filter(l2c_survey_baseline, !is.na(group))


# Calculate and save information about the size of the final data frame that 
# will be used to create the descriptive tables.
# =============================================================================
df_size    <- l2c_survey_baseline_randomized |> utils::object.size() |> format(units = "auto")
df_dim     <- dim(l2c_survey_baseline_randomized) |> format(big.mark = ",")


# Calculate and save other values that will be used across multiple Quarto files
# =============================================================================

# Total number of participants
n_participants <- length(unique(l2c_survey_baseline_randomized$id))

# Total number of participants per l2c group
source("R/fact_reloc.R") # Remove after moving the code below to merge.
l2c_survey_baseline_randomized <- l2c_survey_baseline_randomized |> 
  fact_reloc(group, 1:3, c("UCM", "UCM+SP", "L2C")) # Move to the merge code!

n_per_group <- l2c_survey_baseline_randomized |>
  dplyr::filter(visit == 1) |> 
  dplyr::count(group_f) |> 
  dplyr::pull(n) |> 
  rlang::set_names(levels(l2c_survey_baseline_randomized$group_f))