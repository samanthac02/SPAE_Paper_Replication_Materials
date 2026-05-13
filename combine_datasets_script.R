library(haven)
library(dplyr)
library(gt)
library(tidyr)
library(scales)

get_first_dataframe <- function(file_path) {
  load(file_path) 
  
  object_names <- ls()
  is_dataframe <- sapply(object_names, function(obj) is.data.frame(get(obj)))
  
  if (any(is_dataframe)) {
    first_dataframe_name <- object_names[which(is_dataframe)[1]]
    first_dataframe <- get(first_dataframe_name)
    return(first_dataframe)
  } else {
    cat("No data frames found in the .RData file.\n")
    return(NULL)
  }
}

rename_columns <- function(file_path, old_labels, new_labels) {
  df <- get_first_dataframe(file_path)
  
  # Convert haven_labelled columns to character or numeric
  df <- df %>%
    mutate(across(where(is.labelled), as_factor)) %>%
    mutate(across(where(is.factor), as.character))
  
  # Rename columns first
  for (i in seq_along(old_labels)) {
    if (old_labels[i] %in% colnames(df)) {
      colnames(df)[colnames(df) == old_labels[i]] <- new_labels[i]
    } else {
      df[[new_labels[i]]] <- NA
    }
  }
  
  if ("state" %in% colnames(df) && is.numeric(df$`state`)) {
    state_mapping <- c(
      "1" = "ALABAMA", "2" = "ALASKA", "4" = "ARIZONA", "5" = "ARKANSAS", "6" = "CALIFORNIA",
      "8" = "COLORADO", "9" = "CONNECTICUT", "10" = "DELAWARE", "11" = "DISTRICT OF COLUMBIA",
      "12" = "FLORIDA", "13" = "GEORGIA", "15" = "HAWAII", "16" = "IDAHO", "17" = "ILLINOIS",
      "18" = "INDIANA", "19" = "IOWA", "20" = "KANSAS", "21" = "KENTUCKY", "22" = "LOUISIANA",
      "23" = "MAINE", "24" = "MARYLAND", "25" = "MASSACHUSETTS", "26" = "MICHIGAN", 
      "27" = "MINNESOTA", "28" = "MISSISSIPPI", "29" = "MISSOURI", "30" = "MONTANA",
      "31" = "NEBRASKA", "32" = "NEVADA", "33" = "NEW HAMPSHIRE", "34" = "NEW JERSEY",
      "35" = "NEW MEXICO", "36" = "NEW YORK", "37" = "NORTH CAROLINA", "38" = "NORTH DAKOTA",
      "39" = "OHIO", "40" = "OKLAHOMA", "41" = "OREGON", "42" = "PENNSYLVANIA",
      "44" = "RHODE ISLAND", "45" = "SOUTH CAROLINA", "46" = "SOUTH DAKOTA", "47" = "TENNESSEE",
      "48" = "TEXAS", "49" = "UTAH", "50" = "VERMONT", "51" = "VIRGINIA", "53" = "WASHINGTON",
      "54" = "WEST VIRGINIA", "55" = "WISCONSIN", "56" = "WYOMING"
    )
    
    df <- df %>%
      mutate(`state` = state_mapping[as.character(`state`)])
  }
  
  df <- df %>%
    mutate(across(where(is.character), toupper))
  
  return(df)
}

dir <- "/Users/samantha/Desktop/SPAE/"

column_mapping <- list(
  # File name : caseid, voter fraud variables, confidence variables, demographics
  "spae_2008.RData" = c("caseid", "weight", "q36", "q37", "q38", "X", "X", "X", "q34", 
                        "birthyr", "gender", "race", "educ", "pid3", "inputzip", 
                        "countyname", "inputstate", "time_years", "ideo5", "q1"),                   #2008
  "MITU0017_OUTPUT.RData" = c("caseid", "weight", "q29a", "q29b", "q29c", "q29d", "q29e", "q29f","q25",
                              "birthyr", "gender", "race", "educ", "pid3", "lookupzip", 
                              "countyname", "regstate", "time_1", "ideo5", "q1"),                   #2012
  "MITU0019_OUTPUT_50state_1.RData" = c("caseid", "weight", "Q37A", "Q37B", "Q37C", "Q37D", "Q37E", "Q37F","Q33",
                                        "birthyr", "gender", "race", "educ", "pid3", "lookupzip", 
                                        "countyname", "inputstate", "time_1", "ideo5", "Q1"),       #2014
  "MITU0022_OUTPUT.RData" = c("caseid", "weight", "Q37A", "Q37B", "Q37C", "Q37D", "Q37E", "Q37F", "Q33",
                              "birthyr", "gender", "race", "educ", "pid3", "inputzip", 
                              "countyname", "inputstate", "time_1", "ideo5", "Q1"),                 #2016
  "MITU0031_OUTPUT2.RData" = c("caseid", "weight", "Q54A", "Q54B", "Q54C", "Q54D", "Q54E", "Q54F","Q44",
                               "birthyr", "gender", "race", "educ", "pid3", "inputzip", 
                               "countyname", "inputstate", "citylength_1", "ideo5", "Q1"),          #2020
  "MITU0042_OUTPUT_0120.RData" = c("caseid", "weight", "Q53A", "Q53B", "Q53C", "Q53D", "Q53E", "Q53F", "Q49",
                              "birthyr", "gender", "race", "educ", "pid3", "zipinput",
                              "countyname", "inputstate", "citylength_1", "ideo5", "q1"),           #2022
  "MITU0051_OUTPUT.RData" = c("caseid", "weight_final", "Q46A", "Q46B", "Q46C", "Q46D", "Q46E", "Q46F", "Q42",
                              "birthyr", "gender", "race", "educ", "pid3", "inputzip",
                              "countyname", "inputstate", "X", "ideo5", "Q1")
)

year_mapping <- list(
  "spae_2008.RData" = 2008,
  "MITU0017_OUTPUT.RData" = 2012,
  "MITU0019_OUTPUT_50state_1.RData" = 2014,
  "MITU0022_OUTPUT.RData" = 2016,
  "MITU0031_OUTPUT2.RData" = 2020,
  "MITU0042_OUTPUT_0120.RData" = 2022,
  "MITU0051_OUTPUT.RData" = 2024
)

keys <- names(column_mapping)

fraud_columns <- c(
  "Voting more than once",
  "Ballot tampering", 
  "Impersonation",
  "Non-citizen voting",
  "Mail ballot fraud",
  "Officials changing results"
)

for (key in keys) {
  file_path <- paste0(dir, key)
  
  # Rename columns
  old_labels <- column_mapping[[key]]
  new_labels <- c("caseid", "weight", "Voting more than once",
                  "Ballot tampering", 
                  "Impersonation",
                  "Non-citizen voting",
                  "Mail ballot fraud",
                  "Officials changing results",
                  "Confidence", "birth year", 
                  "gender", "race", "education", "party3", "zip code", 
                  "county", "state", "time lived in residence", 
                  "ideology", "voted")
  # inside your for (key in keys) loop, after rename_columns()
  new_df <- rename_columns(file_path, old_labels, new_labels)
  
  new_df <- new_df %>%
    mutate(race = as.character(race)) %>%
    mutate(race = case_when(
      race == "9" ~ NA_character_,
      TRUE ~ race
    ))
  
  new_df <- new_df %>%
    mutate(`party3` = case_when(
      `party3` %in% c("DEMOCRAT", "1") ~ "DEMOCRAT",
      `party3` %in% c("REPUBLICAN", "2") ~ "REPUBLICAN",
      `party3` %in% c("INDEPENDENT", "3") ~ "INDEPENDENT",
      TRUE ~ NA_character_
    ))
  
  temp_counts <- new_df %>%
    mutate(`Voting more than once` = as.character(`Voting more than once`)) %>%
    count(`Voting more than once`, name = "n") %>%
    mutate(year = year_mapping[[key]])
  
  if (!exists("raw_response_counts")) {
    raw_response_counts <- temp_counts
  } else {
    raw_response_counts <- bind_rows(raw_response_counts, temp_counts)
  }
  
  new_df <- new_df %>%
    mutate(across(all_of(fraud_columns), ~ case_when(
      . %in% c("IT IS VERY COMMON", "IT OCCURS OCCASIONALLY", "IT OCCURS FREQUENTLY") ~ 1,
      . %in% c("IT OCCURS INFREQUENTLY", "IT ALMOST NEVER OCCURS") ~ 0,
      . %in% c(1, 2) ~ 1,
      . %in% c(3, 4) ~ 0,
      . == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )))
  

  new_df <- new_df %>%
    mutate(across(everything(), ~ case_when(
      . %in% c("PHONE - REFUSED",
               -1, -2, -3, -7, -8, -9) ~ NA,
      TRUE ~ .
    )))
  
  
  # --- 2022-specific transformations ---
  if (year_mapping[[key]] == 2022) {
    new_df <- new_df %>%
      # Gender
      mutate(`gender` = case_when(
        `gender` %in% c(1, "1", "MALE")   ~ "MALE",
        `gender` %in% c(2, "2", "FEMALE") ~ "FEMALE",
        TRUE ~ NA_character_
      )) %>%
      
      mutate(`Confidence` = case_when(
        `Confidence` %in% c(1, "1") ~ "VERY CONFIDENT",
        `Confidence` %in% c(2, "2") ~ "SOMEWHAT CONFIDENT",
        `Confidence` %in% c(3, "3") ~ "NOT TOO CONFIDENT",
        `Confidence` %in% c(4, "4") ~ "NOT AT ALL CONFIDENT",
        TRUE ~ NA_character_
      )) %>%
      
      # Race
      mutate(`race` = case_when(
        `race` %in% c(1, "1") ~ "WHITE",
        `race` %in% c(2, "2") ~ "BLACK",
        `race` %in% c(3, "3") ~ "HISPANIC",
        `race` %in% c(4, "4") ~ "ASIAN",
        `race` %in% c(5, "5") ~ "NATIVE AMERICAN",
        `race` %in% c(6, "6") ~ "OTHER",
        TRUE ~ NA_character_
      )) %>%
      
      # Education
      mutate(`education` = case_when(
        `education` %in% c(1, "1") ~ "LESS THAN HS",
        `education` %in% c(2, "2") ~ "HS GRAD",
        `education` %in% c(3, "3") ~ "SOME COLLEGE",
        `education` %in% c(4, "4") ~ "2-YEAR DEGREE",
        `education` %in% c(5, "5") ~ "4-YEAR DEGREE",
        `education` %in% c(6, "6") ~ "POSTGRAD",
        TRUE ~ NA_character_
      )) %>%
      
      # Party
      mutate(`party3` = case_when(
        `party3` %in% c(1, "1", "DEMOCRAT")   ~ "DEMOCRAT",
        `party3` %in% c(2, "2", "REPUBLICAN") ~ "REPUBLICAN",
        `party3` %in% c(3, "3", "INDEPENDENT")~ "INDEPENDENT",
        TRUE ~ NA_character_
      )) %>%
      
      # Ideology
      mutate(`ideology` = case_when(
        `ideology` %in% c(1, "1") ~ "VERY LIBERAL",
        `ideology` %in% c(2, "2") ~ "LIBERAL",
        `ideology` %in% c(3, "3") ~ "MODERATE",
        `ideology` %in% c(4, "4") ~ "CONSERVATIVE",
        `ideology` %in% c(5, "5") ~ "VERY CONSERVATIVE",
        TRUE ~ NA_character_
      ))
  }
  
  # Add "year" column
  new_df$year <- year_mapping[[key]]
  
  # Keep only the columns that are common across all datasets
  columns_to_keep <- intersect(c(new_labels, "year"), colnames(new_df))
  new_df <- new_df[, columns_to_keep, drop = FALSE]
  
  # Move year to 2nd column
  new_df <- new_df[, c(1, ncol(new_df), 2:(ncol(new_df) - 1))]
  
  new_file_path = paste0(dir, "NEW_", key)
  save(new_df, file = new_file_path)
}

datasets <- list()
for (file in names(column_mapping)) {
  file_path <- paste0(dir, "NEW_", file)
  loaded_data <- get(load(file_path))
  datasets[[file]] <- loaded_data
}

combined_data <- do.call(rbind, datasets)
combined_data <- combined_data %>%
  filter(!if_all(all_of(fraud_columns), is.na))

save(combined_data, file = paste0(dir, "COMBINED_DATA.RData"))