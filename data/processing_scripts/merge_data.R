# Merge MB1T data for analysis including preprocessing
## M. S. Schreiner, M. Lippold, M. Zettersten

# Loading required packages
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(janitor)
library(readxl)
library(readr)
library(langcog)
library(here)
source(here("data","processing_scripts","preprocessing_helper.R"))

## general parameters
path_to_participant_data <- here("data","raw","participants")
path_to_trial_data <- here("data","raw","trial")
write_path <- here("data","processed")
cols_to_keep <- c("subid","subid_unique","age_days","participant_gender","method","preterm","test","trial_type","stimulus",
"trial_num","looking_time","trial_error","session_error","session_error_type","lang1","lab") #columns to retain for main analysis

##read in metadata files
lab_metadata <- read_csv(here("data","raw","metadata","lab_metadata.csv"))

#read and combine all participant files
participant_files <-dir(path_to_participant_data,pattern = "*")
df_participants <- map_df(participant_files, function(fname) {
  pd <- read_participant_file(path = path_to_participant_data,
                              fname = fname)
  pd
}) %>%
  bind_rows()

#read and combine all trial files
trial_files <-dir(path_to_trial_data,pattern = "*")
df_trial <- map_df(trial_files, function(fname) {
  td <- read_trial_file(path = path_to_trial_data,
                              fname = fname)
  td
}) %>%
  bind_rows() %>%
  mutate(trial_type = case_when( #clean up trial_type coding
    is.na(trial_type) ~ training_type,
    trial_type == "Training" ~ "training",
    TRUE ~ trial_type
  )) %>%
  mutate(test = case_when( #clean up test coding
    is.na(test) ~ session,
    TRUE ~ test
  )) %>%
  select(-training_type,-session)

#join participant and trial files
df_all <- df_trial %>%
  left_join(df_participants,by=c("subid_unique","subid","test","lab")) %>%
  left_join(lab_metadata) %>%
  mutate(
    preterm = case_when(
      correct_preterm == "y" & preterm %in% c("Y","N") ~ "term",
      correct_preterm == "y" ~ "term",
      is.na(correct_preterm) ~ preterm
    )
  ) %>%
  organize_columns(columns_to_use = cols_to_keep)  #rename and select key columns

# write data for analysis
write.csv(df_all,here(write_path,"df_all.csv"), row.names=FALSE)
