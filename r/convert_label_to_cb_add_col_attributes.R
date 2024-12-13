# =============================================================================
# Convert the GRA's `label()` function to something I can use for my `cb_add_col_attributes()` function
# Specifically,
# - Grab the value being passed to the `col` argument of `label()` so that we can pass it to the `.x` argument of the `cb_add_col_attributes()` function.
# - Grad the value being passed to the `label` argument of `label()` so that we can pass it to the `description` argument of the `cb_add_col_attributes()` function.
# - Here is the regex that should work: https://regex101.com/r/vwP3Xe/3
# =============================================================================

library(dplyr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)

# Convert one line of code
string <- 'label("id", "Subject ID")'
pattern <- 'label\\(\\"(\\w+)\\", (\\".+\\")\\)'
replacement <- 'cb_add_col_attributes(\\\n  \\1, \\\n  description = \\2\\\n) %>%\\\n'
new_code <- str_replace(string, pattern, replacement)
cat(new_code)

# Now do this with multiple stings
strings <- c('label("id", "Subject ID")', 'label("date_visit", "Date of visit")')
pattern <- 'label\\(\\"(\\w+)\\", (\\".+\\")\\)'
replacement <- 'cb_add_col_attributes(\\\n  \\1, \\\n  description = \\2\\\n) %>%\\\n'
new_codes <- str_replace_all(strings, pattern, replacement)
cat(new_codes)

# How do I grab all of the lines of `label()` code from `data_survey_23_codebook.Rmd` and store them as a vector?
# First, grab all the lines of code from `data_survey_23_codebook.Rmd`.
data_survey_23_codebook_lines <- readLines("data_survey_23_codebook.Rmd")
# Then, grab only the lines that start with "label("
# For testing
# test_line <- data_survey_23_codebook_lines[72]
pattern_start_w_label <- "^label\\("
# Create an index of lines from `data_survey_23_codebook.Rmd` that begin with `label(`
data_survey_23_codebook_lines_w_label_index <- str_detect(data_survey_23_codebook_lines, pattern_start_w_label)
# Use the index to keep the lines
data_survey_23_codebook_lines_w_label <- data_survey_23_codebook_lines[data_survey_23_codebook_lines_w_label_index]

# Now that I have all of the lines of code that begin with `label()`, let's convert them to use `cb_add_col_attributes()` instead.
strings <- data_survey_23_codebook_lines_w_label
pattern <- 'label\\(\\"(\\w+)\\", (\\".+\\")\\)'
replacement <- 'cb_add_col_attributes(\\\n  \\1, \\\n  description = \\2\\\n) %>%\\\n'
new_code_lines <- str_replace_all(strings, pattern, replacement)
cat(new_code_lines)

# Some of the lines weren't inserted into the new `cb_add_col_attributes()` as expected.
# Test label("ml_status","Master log status")
# After putting it in the regex, there isn't a space between the comma after "ml_status" and before "Master log status".
# That throws off the regex.
# Add \\s*, which means "zero or more of any whitespace character should fix this issue. Let's try it below.
string <- 'label("ml_status","Master log status")'
pattern <- 'label\\(\\"(\\w+)\\",\\s*(\\".+\\")\\)'
replacement <- 'cb_add_col_attributes(\\\n  \\1, \\\n  description = \\2\\\n) %>%\\\n'
new_code <- str_replace(string, pattern, replacement)
cat(new_code)

# That worked. Let's try running the new regular expression over all of the lines again.
strings <- data_survey_23_codebook_lines_w_label
pattern <- 'label\\(\\"(\\w+)\\",\\s*(\\".+\\")\\)'
replacement <- 'cb_add_col_attributes(\\\n  \\1, \\\n  description = \\2\\\n) %>%\\\n'
new_code_lines <- str_replace_all(strings, pattern, replacement)
cat(new_code_lines)

# Write out to a new file for easy copy and paste
file_path <- 'codebooks/new_code_lines_for_data_survey_23_codebook.R'
file.create(file_path)
writeLines(new_code_lines, file_path)

# This works, but it's kind of hard to make sure I'm getting all the variables I want in the output file for each section of survey data (e.g., master log, etc.)
# So, make one output file instead. 
file.remove(file_path)
rm(list = ls())

# Grab all lines of code.
data_survey_23_codebook_lines <- readLines("data_survey_23_codebook.Rmd")

# Keep the `label()` lines only
pattern_start_w_label <- "^label\\("
data_survey_23_codebook_lines_w_label_index <- str_detect(data_survey_23_codebook_lines, pattern_start_w_label)
data_survey_23_codebook_lines_w_label <- data_survey_23_codebook_lines[data_survey_23_codebook_lines_w_label_index]

# Keep the master log label lines only
pattern_start_w_label <- 'label\\(\\"ml_'
lines_index <- str_detect(data_survey_23_codebook_lines_w_label, pattern_start_w_label)
lines <- data_survey_23_codebook_lines_w_label[lines_index]

# Let's convert the lines to use `cb_add_col_attributes()` instead of `label()`.
pattern <- 'label\\(\\"(\\w+)\\",\\s*(\\".+\\")\\)'
replacement <- 'cb_add_col_attributes(\\\n  \\1, \\\n  description = \\2\\\n) %>%\\\n'
new_code_lines <- str_replace_all(lines, pattern, replacement)
cat(new_code_lines)

# Write out to a new file for easy copy and paste
file_path <- 'codebooks/new_code_lines_for_data_survey_23_codebook_ml.R'
file.create(file_path)
writeLines(new_code_lines, file_path)

# Now, repeat this pattern for each section of the labels code we want to update
file.remove(file_path)

convert_label_to_cb_add_col_attributes <- function(section_prefix) {
  # Keep the label lines only for the current section only
  pattern_start_w_label <- paste0('label\\(\\"', section_prefix)
  lines_index <- str_detect(data_survey_23_codebook_lines_w_label, pattern_start_w_label)
  lines <- data_survey_23_codebook_lines_w_label[lines_index]
  
  # Let's convert the lines to use `cb_add_col_attributes()` instead of `label()`.
  pattern <- 'label\\(\\"(\\w+)\\",\\s*(\\".+\\")\\)'
  replacement <- 'cb_add_col_attributes(\\\n  \\1, \\\n  description = \\2\\\n) %>%\\\n'
  new_code_lines <- str_replace_all(lines, pattern, replacement)
  
  # For data checking purposes, let's write out the number of lines that were grabbed above
  n_lines <- length(lines)
  n_lines_message <- paste0(
    "# There are ", n_lines, " lines with the section prefix ", section_prefix,
    " converted to cb_add_col_attributes() code in this file. \n"
  )
  write_out <- c(n_lines_message, new_code_lines)
  
  # Write out to a new file for easy copy and paste
  file_path <- paste0('codebooks/new_code_lines_for_data_survey_23_codebook_', section_prefix, ".R")
  file.create(file_path)
  writeLines(write_out, file_path)
}

# For testing
# convert_label_to_cb_add_col_attributes("ml_")

# Now apply the function to multiple sections
sections <- c(
  "ml_", # Master log
  "sq_|cell_|media_|access_|facebook_", # Screening questions
  "mms_" # Mini-Mental Exam
)
purrr::walk(sections, convert_label_to_cb_add_col_attributes)