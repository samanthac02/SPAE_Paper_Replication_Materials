library(haven)
library(dplyr)
library(tidyr)

source("config.R")

get_first_dataframe <- function(file_path) {
  e <- new.env()
  load(file_path, envir = e)

  object_names <- ls(e)
  is_df <- sapply(object_names, function(obj) is.data.frame(get(obj, envir = e)))

  if (any(is_df)) {
    return(get(object_names[which(is_df)[1]], envir = e))
  } else {
    cat("No data frames found in the .RData file.\n")
    return(NULL)
  }
}

rename_columns <- function(file_path, old_labels, new_labels) {
  df <- get_first_dataframe(file_path)

  df <- df %>%
    mutate(across(where(is.labelled), as_factor)) %>%
    mutate(across(where(is.factor), as.character))

  for (i in seq_along(old_labels)) {
    if (old_labels[i] %in% colnames(df)) {
      colnames(df)[colnames(df) == old_labels[i]] <- new_labels[i]
    } else {
      df[[new_labels[i]]] <- NA
    }
  }

  if ("state" %in% colnames(df) && is.numeric(df$state)) {
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
      mutate(state = state_mapping[as.character(state)])
  }

  df <- df %>%
    mutate(across(where(is.character), toupper))

  return(df)
}

dir <- paste0(data_dir, "/")

column_mapping <- list(
  "spae_2008.RData" = c("caseid", "weight",
    "q36", "q37", "q38", "X", "X", "X",
    "X", "X", "X", "X",
    "q34", "birthyr", "gender", "race", "educ", "pid3",
    "inputzip", "countyname", "inputstate", "time_years", "ideo5", "q1"),
  "MITU0017_OUTPUT.RData" = c("caseid", "weight",
    "q29a", "q29b", "q29c", "q29d", "q29e", "q29f",
    "X", "X", "X", "X",
    "q25", "birthyr", "gender", "race", "educ", "pid3",
    "lookupzip", "countyname", "regstate", "time_1", "ideo5", "q1"),
  "MITU0019_OUTPUT_50state_1.RData" = c("caseid", "weight",
    "Q37A", "Q37B", "Q37C", "Q37D", "Q37E", "Q37F",
    "X", "X", "X", "X",
    "Q33", "birthyr", "gender", "race", "educ", "pid3",
    "lookupzip", "countyname", "inputstate", "time_1", "ideo5", "Q1"),
  "MITU0022_OUTPUT.RData" = c("caseid", "weight",
    "Q37A", "Q37B", "Q37C", "Q37D", "Q37E", "Q37F",
    "X", "X", "X", "X",
    "Q33", "birthyr", "gender", "race", "educ", "pid3",
    "inputzip", "countyname", "inputstate", "time_1", "ideo5", "Q1"),
  "MITU0031_OUTPUT2.RData" = c("caseid", "weight",
    "Q54A", "Q54B", "Q54C", "Q54D", "Q54E", "Q54F",
    "X", "X", "X", "X",
    "Q44", "birthyr", "gender", "race", "educ", "pid3",
    "inputzip", "countyname", "inputstate", "citylength_1", "ideo5", "Q1"),
  "MITU0042_OUTPUT_0120.RData" = c("caseid", "weight",
    "Q53A", "Q53B", "Q53C", "Q53D", "Q53E", "Q53F",
    "Q53G", "Q53H", "Q53I", "Q53J",
    "Q49", "birthyr", "gender", "race", "educ", "pid3",
    "zipinput", "countyname", "inputstate", "citylength_1", "ideo5", "q1"),
  "MITU0051_OUTPUT.RData" = c("caseid", "weight_final",
    "Q46A", "Q46B", "Q46C", "Q46D", "Q46E", "Q46F",
    "Q46G", "Q46H", "Q46I", "Q46J",
    "Q42", "birthyr", "gender", "race", "educ", "pid3",
    "inputzip", "countyname", "inputstate", "X", "ideo5", "Q1")
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

fraud_columns <- c(
  "voting_more_than_once",
  "ballot_tampering",
  "impersonation",
  "non_citizen_voting",
  "mail_ballot_fraud",
  "officials_changing_results",
  "vote_counting_software",
  "paid_to_vote",
  "fraudulent_registration",
  "drop_box_fraud"
)

new_labels <- c("caseid", "weight",
  "voting_more_than_once", "ballot_tampering", "impersonation",
  "non_citizen_voting", "mail_ballot_fraud", "officials_changing_results",
  "vote_counting_software", "paid_to_vote", "fraudulent_registration", "drop_box_fraud",
  "confidence", "birth_year",
  "gender", "race", "education", "party", "zip_code",
  "county", "state", "time_lived_in_residence", "ideology", "voted")

datasets <- list()

for (key in names(column_mapping)) {
  file_path <- paste0(dir, key)
  old_labels <- column_mapping[[key]]

  new_df <- rename_columns(file_path, old_labels, new_labels)

  new_df <- new_df %>%
    mutate(race = as.character(race)) %>%
    mutate(race = case_when(
      race == "9" ~ NA_character_,
      TRUE ~ race
    ))

  new_df <- new_df %>%
    mutate(party = case_when(
      party %in% c("DEMOCRAT", "1") ~ "DEMOCRAT",
      party %in% c("REPUBLICAN", "2") ~ "REPUBLICAN",
      party %in% c("INDEPENDENT", "3") ~ "INDEPENDENT",
      TRUE ~ NA_character_
    ))

  new_df <- new_df %>%
    mutate(across(all_of(fraud_columns), ~ case_when(
      . %in% c("IT IS VERY COMMON", "IT OCCURS OCCASIONALLY", "IT OCCURS FREQUENTLY") ~ 1,
      . %in% c("IT OCCURS INFREQUENTLY", "IT ALMOST NEVER OCCURS") ~ 0,
      . %in% c(1, 2, 3) ~ 1,
      . %in% c(4) ~ 0,
      . == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )))

  response_cols <- c(fraud_columns, "confidence", "gender", "race", "education",
                     "party", "ideology", "voted", "time_lived_in_residence")
  existing_response_cols <- intersect(response_cols, colnames(new_df))

  new_df <- new_df %>%
    mutate(across(all_of(existing_response_cols), ~ case_when(
      . %in% c("PHONE - REFUSED", -1, -2, -3, -7, -8, -9) ~ NA,
      TRUE ~ .
    )))

  if (year_mapping[[key]] == 2022) {
    new_df <- new_df %>%
      mutate(gender = case_when(
        gender %in% c(1, "1", "MALE") ~ "MALE",
        gender %in% c(2, "2", "FEMALE") ~ "FEMALE",
        TRUE ~ NA_character_
      )) %>%
      mutate(confidence = case_when(
        confidence %in% c(1, "1") ~ "VERY CONFIDENT",
        confidence %in% c(2, "2") ~ "SOMEWHAT CONFIDENT",
        confidence %in% c(3, "3") ~ "NOT TOO CONFIDENT",
        confidence %in% c(4, "4") ~ "NOT AT ALL CONFIDENT",
        TRUE ~ NA_character_
      )) %>%
      mutate(race = case_when(
        race %in% c(1, "1") ~ "WHITE",
        race %in% c(2, "2") ~ "BLACK",
        race %in% c(3, "3") ~ "HISPANIC",
        race %in% c(4, "4") ~ "ASIAN",
        race %in% c(5, "5") ~ "NATIVE AMERICAN",
        race %in% c(6, "6") ~ "OTHER",
        TRUE ~ NA_character_
      )) %>%
      mutate(education = case_when(
        education %in% c(1, "1") ~ "LESS THAN HS",
        education %in% c(2, "2") ~ "HS GRAD",
        education %in% c(3, "3") ~ "SOME COLLEGE",
        education %in% c(4, "4") ~ "2-YEAR DEGREE",
        education %in% c(5, "5") ~ "4-YEAR DEGREE",
        education %in% c(6, "6") ~ "POSTGRAD",
        TRUE ~ NA_character_
      )) %>%
      mutate(party = case_when(
        party %in% c(1, "1", "DEMOCRAT") ~ "DEMOCRAT",
        party %in% c(2, "2", "REPUBLICAN") ~ "REPUBLICAN",
        party %in% c(3, "3", "INDEPENDENT") ~ "INDEPENDENT",
        TRUE ~ NA_character_
      )) %>%
      mutate(ideology = case_when(
        ideology %in% c(1, "1") ~ "VERY LIBERAL",
        ideology %in% c(2, "2") ~ "LIBERAL",
        ideology %in% c(3, "3") ~ "MODERATE",
        ideology %in% c(4, "4") ~ "CONSERVATIVE",
        ideology %in% c(5, "5") ~ "VERY CONSERVATIVE",
        TRUE ~ NA_character_
      ))
  }

  new_df$year <- year_mapping[[key]]

  columns_to_keep <- intersect(c(new_labels, "year"), colnames(new_df))
  new_df <- new_df[, columns_to_keep, drop = FALSE]
  new_df <- new_df[, c(1, ncol(new_df), 2:(ncol(new_df) - 1))]

  datasets[[key]] <- new_df
}

combined_data <- do.call(rbind, datasets)
combined_data <- combined_data %>%
  filter(!if_all(all_of(fraud_columns), is.na))

save(combined_data, file = paste0(dir, "COMBINED_DATA_ALT.RData"))
