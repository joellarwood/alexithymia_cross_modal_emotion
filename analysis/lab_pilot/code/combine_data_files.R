# Processing Pilot Data from lab

# Packages required for function 

library(here)
library(tidyverse)
library(janitor)

# Source processing function 

here::here("analysis",
           "pilot_process_function.R") %>% 
  source()

# Loop to create processed data 

input_path <- here::here(
  "analysis",
  "lab_pilot",
  "data",
  "raw")

output_path <- here::here(
  "analysis",
  "lab_pilot",
  "data",
  "processed",
  "individual_files")

raw_files <- input_path %>% 
  list.files()

# process each file 

for (i in 1:length(raw_files)){
  file_tmp <- raw_files[i]
  path_in_tmp <- paste0(input_path, "/", file_tmp)
  path_out_tmp <- paste0(output_path, "/", file_tmp)
  data_in_tmp <- read_csv(path_in_tmp)
  data_out_tmp <- clean_pilot(data_in_tmp)
  write_csv(data_out_tmp, path = path_out_tmp)
}

# create one dataframe 

combined_data <- list.files(path=output_path, full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

output_combined <- here::here(
  "analysis",
  "lab_pilot",
  "data",
  "processed")

write_csv(combined_data, 
          path = paste0(output_combined, "/combined_lab_pilot_data.csv")
)

