# Runs survey-weighted logistic regressions of belief in each voter-fraud type 
# on demographic and political predictors (gender, race, education, party, 
# birth year) for a given SPAE survey year, then prints two gt tables splitting 
# the fraud outcomes across columns with coefficient estimates and standard 
# errors for each predictor.

library(dplyr)
library(survey)
library(broom)
library(tidyr)
library(gt)
library(haven)
library(purrr)

load("/Users/samantha/Desktop/SPAE/COMBINED_DATA.RData")

full_cols <- c("Voting more than once", "Ballot tampering", "Impersonation", "Non-citizen voting", "Mail ballot fraud", "Officials changing results", "education", "race", "party3")
cols_2008 <- c("Voting more than once", "Ballot tampering", "Impersonation", "education", "race", "party3")

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
    # Male is reference
    
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
    
    dem = if_else(party3 == "DEMOCRAT", 1, 0),
    rep = if_else(party3 == "REPUBLICAN", 1, 0),
    ind = if_else(party3 == "INDEPENDENT", 1, 0)
  )

fraud_columns <- c(
  "Voting more than once",
  "Ballot tampering",
  "Impersonation",
  "Non-citizen voting",
  "Mail ballot fraud",
  "Officials changing results"
)

run_year_logit <- function(target_year, outcomes = fraud_columns) {
  
  data_year <- combined_data %>% 
    filter(year == !!target_year)
  
  # Check available columns
  valid_outcomes <- intersect(outcomes, names(data_year))
  
  if (length(valid_outcomes) == 0) {
    stop("No valid outcome columns found for this year.")
  }
  
  des_year <- svydesign(
    ids = ~1,
    weights = ~weight,
    data = data_year
  )
  
  run_model <- function(outcome) {
    f <- as.formula(
      paste0(
        "`", outcome, "`",
        " ~ female + black + hisp + asian + other_r + 
        somecol + fouryear + postgrad + nohs + 
        rep + dem + `birth year`"
      )
    )
    
    tryCatch({
      svyglm(f, design = des_year, family = quasibinomial()) %>%
        tidy() %>%
        select(term, estimate, std.error) %>%
        mutate(model = outcome)
    }, error = function(e) {
      warning(paste("Model failed for:", outcome, "-", e$message))
      return(NULL)
    })
  }
  
  all_results <- valid_outcomes %>%
    map_dfr(run_model)
  
  if (nrow(all_results) == 0) return("No results generated.")
  
  # label names
  label_dict <- c(
    "(Intercept)"  = "Intercept",
    "female"       = "Female",
    "black"        = "Black",
    "hisp"         = "Hispanic",
    "asian"        = "Asian",
    "other_r"      = "Other race",
    "somecol"      = "Some college",
    "fouryear"     = "4-year",
    "postgrad"     = "Post-grad",
    "nohs"         = "No HS",
    "rep"          = "Republican",
    "dem"          = "Democrat",
    "`birth year`" = "Birth year",
    "birth year"   = "Birth year"
  )
  
  custom_order <- names(label_dict)
  
  long_data <- all_results %>%
    filter(term %in% names(label_dict)) %>%
    mutate(term = factor(term, levels = custom_order)) %>%
    arrange(term) %>%
    mutate(label = label_dict[as.character(term)]) %>% 
    mutate(
      Estimate = round(estimate, 3),
      `Std. Error` = round(std.error, 3)
    ) %>%
    select(label, model, Estimate, `Std. Error`)
  
  build_gt_half <- function(outcomes_subset, title_text) {
    wide_half <- long_data %>%
      filter(model %in% outcomes_subset) %>%
      pivot_wider(
        names_from = model,
        values_from = c(Estimate, `Std. Error`),
        names_glue = "{model}__{.value}"
      )
    
    ordered_cols <- "label"
    for (m in outcomes_subset) {
      ordered_cols <- c(ordered_cols,
                        paste0(m, "__Estimate"),
                        paste0(m, "__Std. Error"))
    }
    wide_half <- wide_half %>% select(any_of(ordered_cols))
    
    wide_half %>%
      gt() %>%
      tab_spanner_delim(delim = "__") %>%
      cols_label(label = "Predictor") %>%
      tab_header(title = title_text) %>%
      tab_style(
        style = cell_text(weight = "bold", color = "black"),
        locations = cells_column_spanners()
      )
  }
  
  half_size <- ceiling(length(valid_outcomes) / 2)
  group1 <- valid_outcomes[1:half_size]
  group2 <- valid_outcomes[(half_size + 1):length(valid_outcomes)]
  
  gt_top <- build_gt_half(
    group1,
    paste("Logistic Regression Results for", target_year)
  )
  gt_bottom <- build_gt_half(
    group2,
    paste("")
  )
  
  print(gt_top)
  print(gt_bottom)
}

tables <- run_year_logit(2012)