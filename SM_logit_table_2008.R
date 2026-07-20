# Runs survey-weighted logistic regressions of belief in three voter-fraud types 
# (voting more than once, ballot tampering, impersonation) on demographic and 
# political predictors (interview mode, gender, race, education, party, birth 
# year) for the 2008 SPAE survey, then prints a gt table with coefficient 
# estimates and standard errors side-by-side for each fraud type.

library(dplyr)
library(survey)
library(broom)
library(tidyr)
library(gt)
library(haven)
library(purrr)

source("config.R")

load(paste0(data_dir, "/spae_2008.RData"))

spae_2008 <- data %>%
  mutate(across(where(is.labelled), as_factor)) %>%
  mutate(across(where(is.factor), as.character))

spae_2008 <- spae_2008 %>%
  select(caseid, weight, mode, q36, q37, q38,
         birthyr, gender, race, educ, pid3, inputzip)

# recode survey items
spae_2008 <- spae_2008 %>%
  mutate(
    q36 = case_when(
      q36 %in% c("it is very common", "it occurs occasionally") ~ 1,
      q36 %in% c("it almost never occurs", "it occurs infrequently") ~ 0,
      TRUE ~ NA_real_
    ),
    q37 = case_when(
      q37 %in% c("it is very common", "it occurs occasionally") ~ 1,
      q37 %in% c("it almost never occurs", "it occurs infrequently") ~ 0,
      TRUE ~ NA_real_
    ),
    q38 = case_when(
      q38 %in% c("it is very common", "it occurs occasionally") ~ 1,
      q38 %in% c("it almost never occurs", "it occurs infrequently") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  drop_na(q36, q37, q38)

# collapse categories and drop unwanted cases
spae_2008 <- spae_2008 %>%
  mutate(
    race = case_when(
      race == "white" ~ "white",
      race == "black" ~ "black",
      race == "hispanic" ~ "hispanic",
      race == "asian" ~ "asian",
      race %in% c("mixed", "native american", "other", "middle eastern") ~ "other",
      TRUE ~ NA_character_
    ),
    educ = case_when(
      educ %in% c("some college", "2-year") ~ "some college",
      educ == "4-year" ~ "4-year",
      educ == "high school graduate" ~ "high school graduate",
      educ == "post-grad" ~ "post-grad",
      educ == "no hs" ~ "no hs",
      TRUE ~ NA_character_
    ),
    birthyr = as.numeric(birthyr)
  ) %>%
  filter(
    gender != "phone - refused",
    !is.na(gender),
    !is.na(race),
    !is.na(educ),
    pid3 != "phone - refused",
    pid3 != "not sure",
    pid3 != "other",
    !is.na(pid3)
  )

# mode dummy
spae_2008 <- spae_2008 %>%
  mutate(
    phone = if_else(mode == "phone", 1, 0)
  )

# dummy variables
spae_2008 <- spae_2008 %>%
  mutate(
    female = if_else(gender == "female", 1, 0),
    male   = if_else(gender == "male", 1, 0),
    
    white = if_else(race == "white", 1, 0),
    black = if_else(race == "black", 1, 0),
    hisp  = if_else(race == "hispanic", 1, 0),
    asian = if_else(race == "asian", 1, 0),
    other_r = if_else(race == "other", 1, 0),
    
    hsgrad   = if_else(educ == "high school graduate", 1, 0),
    somecol  = if_else(educ == "some college", 1, 0),
    fouryear = if_else(educ == "4-year", 1, 0),
    postgrad = if_else(educ == "post-grad", 1, 0),
    nohs     = if_else(educ == "no hs", 1, 0),
    
    rep = if_else(pid3 == "republican", 1, 0),
    dem = if_else(pid3 == "democrat", 1, 0),
    ind = if_else(pid3 == "independent", 1, 0)
  )

# survey design
des_2008 <- svydesign(
  ids = ~1,
  weights = ~weight,
  data = spae_2008
)

# function to run logit models
run_model <- function(outcome) {
  formula <- as.formula(
    paste0(
      outcome,
      " ~ phone + female + black + hisp + asian + other_r +
        somecol + hsgrad + fouryear + postgrad + nohs +
        rep + dem + birthyr"
    )
  )
  
  svyglm(formula, design = des_2008, family = quasibinomial()) %>%
    tidy() %>%
    select(term, estimate, std.error) %>%
    mutate(model = outcome)
}

# run q36, q37, q38 models
models_list <- list(
  run_model("q36"),
  run_model("q37"),
  run_model("q38")
)

all_results <- bind_rows(models_list)

# label names
label_dict <- c(
  "(Intercept)" = "Intercept",
  "phone" = "Phone Interview",
  "female" = "Female",
  "black" = "Black",
  "hisp" = "Hispanic",
  "asian" = "Asian",
  "other_r" = "Other race",
  "somecol" = "Some college",
  "hsgrad" = "HS graduate",
  "fouryear" = "4-year",
  "postgrad" = "Post-grad",
  "nohs" = "No HS",
  "rep" = "Republican",
  "dem" = "Democrat",
  "birthyr" = "Birth year"
)

custom_order <- c(
  "(Intercept)",
  "phone",
  "female",
  "black",
  "hisp",
  "asian",
  "other_r",
  "somecol",
  "hsgrad",
  "fouryear",
  "postgrad",
  "nohs",
  "rep",
  "dem",
  "birthyr"
)

wide_table <- all_results %>%
  mutate(term = factor(term, levels = custom_order)) %>%
  arrange(term) %>%
  mutate(label = label_dict[term]) %>%
  select(label, model, estimate, std.error) %>%
  pivot_wider(
    names_from = model,
    values_from = c(estimate, std.error),
    names_sep = "_"
  ) %>%
  rename(
    Estimate_Q36 = estimate_q36,
    SE_Q36       = std.error_q36,
    Estimate_Q37 = estimate_q37,
    SE_Q37       = std.error_q37,
    Estimate_Q38 = estimate_q38,
    SE_Q38       = std.error_q38
  )

table <- wide_table %>%
  mutate(across(starts_with("Estimate_"), ~ round(., 3)),
         across(starts_with("SE_"), ~ round(., 3))) %>%
  gt() %>%
  
  tab_header(
    title = "Logistic Regression Results for 2008",
    subtitle = "Beliefs About Voter Fraud and Predictors"
  ) %>%
  
  cols_label(
    label        = "Predictor",
    Estimate_Q36 = "Estimate",
    SE_Q36       = "Std. Err.",
    Estimate_Q37 = "Estimate",
    SE_Q37       = "Std. Err.",
    Estimate_Q38 = "Estimate",
    SE_Q38       = "Std. Err."
  ) %>%
  
  tab_spanner(
    label = "Voting more than once",
    columns = c(Estimate_Q36, SE_Q36)
  ) %>%
  tab_spanner(
    label = "Ballot tampering",
    columns = c(Estimate_Q37, SE_Q37)
  ) %>%
  tab_spanner(
    label = "Impersonation",
    columns = c(Estimate_Q38, SE_Q38)
  )

print(table)
gtsave(table, file.path(figures_dir, "SM_logit_table_2008.png"))