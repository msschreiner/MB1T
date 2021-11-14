################################ nc_na ################################
## substitute NA for NC
# we asked poeople to mark non-recorded variables with "NC", but this was a mistake.

nc_na <- function(x) {
  if (is.character(x[1])) {
    x[x == "NC" | x == "nc"] <- NA
  }
  return(x) 
}

################################ as.num ################################
#custom as.numeric function that avoids warning messages about NA coercion
#source: https://stackoverflow.com/questions/14984989/how-to-avoid-warning-when-introducing-nas-by-coercion
as.num = function(x, na.strings = c("NA",NA)) {
  na = x %in% na.strings
  x[na] = "0"
  x = as.numeric(x)
  x[na] = NA_real_
  x
}

################################ read_multiformat_file ################################
# reads files in various formats

read_multiformat_file <- function(path, fname, file_type) {
  full_path <- here::here(path,fname)
  
  if (str_detect(fname, ".xlsx")) {
    d <- read_xlsx(full_path)
  } else if (str_detect(fname, ".xls")) {
    d <- read_xls(full_path)
  } else if (str_detect(fname, ".csv")) {
    # https://stackoverflow.com/questions/33417242/how-to-check-if-csv-file-has-a-comma-or-a-semicolon-as-separator
    L <- readLines(full_path, n = 1)
    numfields <- count.fields(textConnection(L), sep = ";")
    if (numfields == 1) {
      d <- read_csv(full_path, col_types = cols(lab =col_character()))
    } else {
      if (file_type == "trial") {
        d <- read_csv2(full_path, col_types = cols(lab =col_character(),trial_num = col_character(),total_trial_time = col_character()))
      } else {
        d <- read_csv2(full_path, col_types = cols(lab =col_character()))
      }
    }
  } else if (str_detect(fname, ".txt")) {
    d <- read_tsv(full_path, col_types = cols(lab =col_character()))
  } 
  
  return(d)
}


################################ read_participant_file ################################
read_participant_file <- function(path,fname) {
  print(paste0("reading ", fname))
  pd <- read_multiformat_file(path = path, 
                              fname = fname,
                              file_type = "participant") %>%
    janitor::clean_names() %>% #unify column naming
    mutate_all(as.character) %>% #make all columns character vectors to unify column types for binding
    mutate_all(nc_na) %>% #remove any NC values
    filter(!is.na(subid)) %>% #remove residual NA rows
    mutate(lab = case_when( #unify lab naming for Brookes BabyLab
      lab== "babylab-brookes" ~ "brookes-babylab",
      TRUE ~ lab
    )) %>%
    mutate(age_days = as.numeric(age_days)) %>%
    mutate(days_preterm = as.num(days_preterm)) %>%
    group_by(subid) %>%
    mutate(test=ifelse(age_days==min(age_days),1,2)) %>% #add information about test session (based on age at test)
    ungroup() %>%
    mutate(test = as.numeric(test)) %>%
    mutate(subid_unique = paste0(lab, ":", subid)) #add unique subject identifier (in case different labs used the same subject id format)
    
  pd
}

################################ read_trial_file ################################
read_trial_file <- function(path,fname) {
  print(paste0("reading ", fname))
  td <- read_multiformat_file(path = path, 
                              fname = fname,
                              file_type = "trial") %>%
    janitor::clean_names() %>% #unify column naming
    filter(!is.na(subid)) %>% #remove residual NA rows
    mutate_all(nc_na) %>% #remove any NC values
    mutate(subid=str_replace(subid,"Brookes-","MB_")) %>% #rename subid in Brookes dataset to match naming pattern in participants file
    mutate(
      looking_time = as.num(as.character(looking_time)),
      total_trial_time = as.num(as.character(total_trial_time)),
      trial_num=round(as.numeric(trial_num),digits = 0) #ensure trial numbers are integers
    )  %>%
    mutate(subid_unique = paste0(lab, ":", subid)) #add unique subject identifier (in case different labs used the same subject id format)
  
  td
}

################################ organize_columns ################################
## organize and rename columns to analyze
organize_columns <- function(dataset, columns_to_use) {
  dataset <- dataset %>% 
    select(columns_to_use) %>%
    #rename some columns for convenience
    rename(Age=age_days,
           LT=looking_time,
           Gender=participant_gender,
           Subject=subid,
           Subject_Unique=subid_unique,
           Condition=trial_type,
           Lab=lab,
           Method=method,
           Language=lang1,
           Session=test)
  dataset
}

