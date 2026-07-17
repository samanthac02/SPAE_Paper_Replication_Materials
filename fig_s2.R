# Fits a 2PL IRT model on respondents' beliefs across the six voter-fraud
# questions to extract a single continuous "fraud belief" score, then regresses
# that score on demographic and political predictors (gender, race, education,
# party, birth year) using survey-weighted linear regression for a given SPAE
# year, and prints a gt table of coefficient estimates, standard errors, and
# p-values.

library(dplyr)
library(survey)
library(broom)
library(tidyr)
library(gt)
library(haven)
library(purrr)
library(mirt)

source("config.R")

load(paste0(data_dir, "/COMBINED_DATA.RData"))

full_cols <- c("voting_more_than_once", "ballot_tampering", "impersonation",
               "non_citizen_voting", "mail_ballot_fraud", "officials_changing_results",
               "education", "race", "party")
cols_2008 <- c("voting_more_than_once", "ballot_tampering", "impersonation",
               "education", "race", "party")

fraud_columns <- c(
  "voting_more_than_once",
  "ballot_tampering",
  "impersonation",
  "non_citizen_voting",
  "mail_ballot_fraud",
  "officials_changing_results"
)

combined_data <- combined_data %>%
  group_by(year) %>%
  group_modify(function(df_group, key) {
    if (key$year == 2008) {
      drop_na(df_group, any_of(cols_2008))
    } else {
      drop_na(df_group, any_of(full_cols))
    }
  }) %>%
  ungroup()

combined_data <- combined_data %>%
  mutate(across(where(is.labelled), as_factor)) %>%
  mutate(across(where(is.factor), as.character))

combined_data <- combined_data %>%
  mutate(
    female = if_else(gender == "FEMALE", 1, 0),

    white   = if_else(race == "WHITE", 1, 0),
    black   = if_else(race == "BLACK", 1, 0),
    hisp    = if_else(race == "HISPANIC", 1, 0),
    asian   = if_else(race == "ASIAN", 1, 0),
    other_r = if_else(race %in% c("OTHER",
                                  "NATIVE AMERICAN",
                                  "MIXED",
                                  "TWO OR MORE RACES",
                                  "MIDDLE EASTERN"),
                      1, 0),

    hsgrad   = if_else(education %in% c("HIGH SCHOOL GRADUATE", "HS GRAD"), 1, 0),
    somecol  = if_else(education %in% c("SOME COLLEGE", "2-YEAR", "2-YEAR DEGREE"), 1, 0),
    fouryear = if_else(education %in% c("4-YEAR", "4-YEAR DEGREE"), 1, 0),
    postgrad = if_else(education %in% c("POST-GRAD", "POSTGRAD"), 1, 0),
    nohs     = if_else(education %in% c("NO HS", "LESS THAN HS"), 1, 0),

    dem = if_else(party == "DEMOCRAT", 1, 0),
    rep = if_else(party == "REPUBLICAN", 1, 0),
    ind = if_else(party == "INDEPENDENT", 1, 0)
  )

run_irt_regression <- function(target_year) {

  data_year <- combined_data %>%
    filter(year == !!target_year)

  valid_items <- intersect(fraud_columns, names(data_year))

  if (length(valid_items) < 2) {
    stop("Need at least 2 valid fraud columns to run IRT.")
  }

  irt_data <- data_year %>%
    select(all_of(valid_items)) %>%
    mutate(across(everything(), ~as.numeric(as.character(.))))

  cat(paste("Fitting IRT model for year", target_year, "with", length(valid_items), "items...\n"))
  irt_model <- mirt(irt_data, 1, itemtype = '2PL', verbose = FALSE)

  scores <- fscores(irt_model, method = "EAP")
  data_year$fraud_score <- scores[, "F1"]

  des_year <- svydesign(
    ids = ~1,
    weights = ~weight,
    data = data_year
  )

  model_formula <- as.formula(
    "fraud_score ~ female + black + hisp + asian + other_r +
     somecol + fouryear + postgrad + nohs +
     rep + dem + birth_year"
  )

  reg_results <- svyglm(model_formula, design = des_year, family = gaussian()) %>%
    tidy() %>%
    mutate(
      model = "Combined Fraud IRT Score",
      Estimate = round(estimate, 3),
      `Std. Error` = round(std.error, 3),
      p_value = round(p.value, 3)
    )

  label_dict <- c(
    "(Intercept)" = "Intercept",
    "female"      = "Female",
    "black"       = "Black",
    "hisp"        = "Hispanic",
    "asian"       = "Asian",
    "other_r"     = "Other race",
    "somecol"     = "Some college",
    "fouryear"    = "4-year",
    "postgrad"    = "Post-grad",
    "nohs"        = "No HS",
    "rep"         = "Republican",
    "dem"         = "Democrat",
    "birth_year"  = "Birth year"
  )

  formatted_table <- reg_results %>%
    filter(term %in% names(label_dict)) %>%
    mutate(term_factor = factor(term, levels = names(label_dict))) %>%
    arrange(term_factor) %>%
    mutate(label = label_dict[as.character(term)]) %>%
    select(label, Estimate, `Std. Error`, p_value) %>%
    gt() %>%
    tab_header(
      title = paste("Linear Regression Estimates on Fraud Belief in ", target_year)
    ) %>%
    cols_label(
      label = "Predictor",
      p_value = "P-Value"
    ) %>%
    fmt_number(
      columns = c(Estimate, `Std. Error`, p_value),
      decimals = 3
    )

  return(formatted_table)
}

final_table <- run_irt_regression(2024)
print(final_table)
