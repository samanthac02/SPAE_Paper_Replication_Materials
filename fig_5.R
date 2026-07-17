# Estimates the marginal effect of party affiliation (Republican vs Independent,
# Democrat vs Independent) on belief that each of six types of voter fraud is
# common, using survey-weighted logistic regression across all SPAE survey years,
# controlling for swing-state residence, then plots the year-by-year effects
# with 95% confidence intervals and linear trend lines, faceted by fraud type.

library(ggplot2)
library(dplyr)
library(survey)
library(marginaleffects)
library(haven)

source("config.R")

load(paste0(data_dir, "/COMBINED_DATA.RData"))

run_fraud_logit <- function(data, swing_state_codes, fraud_cols) {
  keep <- c("state", "party", "weight", fraud_cols)

  selected_data <- data %>%
    select(all_of(keep)) %>%
    mutate(
      rep_party = as.numeric(party == "REPUBLICAN"),
      dem_party = as.numeric(party == "DEMOCRAT"),
      swing_state = as.numeric(state %in% swing_state_codes)
    )

  independent_vars <- "rep_party + dem_party + swing_state"

  logit_results <- lapply(fraud_cols, function(dep_var) {
    cat("Running model for:", dep_var, "\n")

    model_data <- selected_data %>%
      filter(!is.na(.data[[dep_var]]))

    if (nrow(model_data) == 0) {
      cat("Skipping", dep_var, "- no non-missing data\n")
      return(NULL)
    }

    distribution <- table(model_data[[dep_var]])
    cat("Distribution:", paste(names(distribution), "=", distribution, collapse = ", "), "\n")

    tryCatch({
      formula <- as.formula(paste0("`", dep_var, "` ~ ", independent_vars))
      svy_design <- svydesign(ids = ~1, weights = ~weight, data = model_data)

      model <- svyglm(formula, design = svy_design, family = quasibinomial())

      meffects <- slopes(model, variables = c("rep_party", "dem_party"), type = "response", .average = TRUE)
      meffects_df <- as.data.frame(meffects)

      meffects_df$dependent_variable <- dep_var
      meffects_df
    }, error = function(e) {
      cat("svyglm failed for", dep_var, "- trying glm fallback\n")

      model <- glm(
        formula,
        data = model_data,
        family = binomial(link = "logit"),
        weights = model_data$weight
      )

      meffects <- slopes(model)
      meffects_df <- as.data.frame(meffects)
      meffects_df$dependent_variable <- dep_var
      meffects_df
    })
  })

  result <- bind_rows(logit_results)

  if (nrow(result) == 0 || !"term" %in% names(result)) {
    return(tibble())
  }

  result %>%
    filter(term %in% c("rep_party", "dem_party"))
}

plot_rep_and_dem_effects <- function(all_mfx_summaries, survey_years, fraud_labels_all_years) {
  combined_df <- bind_rows(all_mfx_summaries, .id = "survey_year") %>%
    filter(term %in% c("rep_party", "dem_party")) %>%
    mutate(
      party = case_when(
        term == "rep_party" ~ "Republican vs Independent",
        term == "dem_party" ~ "Democrat vs Independent"
      ),
      survey_year = factor(survey_year, levels = survey_years),
      fraud_label = fraud_labels_all_years[as.character(dependent_variable)]
    ) %>%
    filter(!is.na(estimate), !is.na(std.error), !is.na(fraud_label)) %>%
    distinct(survey_year, fraud_label, term, .keep_all = TRUE) %>%
    mutate(fraud_label = factor(fraud_label, levels = c(
      "Ballot tampering",
      "Impersonation",
      "Voting more than once",
      "Mail ballot fraud",
      "Non-citizen voting",
      "Officials changing results"
    )))

  if (nrow(combined_df) == 0) {
    message("No data to plot.")
    return(NULL)
  }

  ggplot(combined_df, aes(x = survey_year, y = estimate, color = party, group = party)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(position = position_dodge(width = 0.6), size = 2.5) +
    geom_errorbar(
      aes(
        ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error
      ),
      width = 0.2,
      position = position_dodge(width = 0.6)
    ) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      linetype = "dotted",
      linewidth = 0.9
    ) +
    scale_color_manual(
      values = c(
        "Democrat vs Independent" = "#1f77b4",
        "Republican vs Independent" = "#d62728"
      )
    ) +
    facet_wrap(~ fraud_label, scales = "fixed") +
    labs(
      x = "Survey Year",
      y = "Marginal Effect (vs Independent)",
      title = "Marginal Effects of Political Party on Belief in Voter Fraud",
      color = "Party"
    ) +
    theme_minimal() +
    theme(
      panel.spacing = unit(2, "lines"),
      legend.position = "bottom"
    )
}

# === Define fraud columns and labels ===
fraud_cols <- c(
  "voting_more_than_once",
  "ballot_tampering",
  "impersonation",
  "non_citizen_voting",
  "mail_ballot_fraud",
  "officials_changing_results"
)

fraud_labels_all_years <- c(
  "voting_more_than_once"      = "Voting more than once",
  "ballot_tampering"           = "Ballot tampering",
  "impersonation"              = "Impersonation",
  "non_citizen_voting"         = "Non-citizen voting",
  "mail_ballot_fraud"          = "Mail ballot fraud",
  "officials_changing_results" = "Officials changing results"
)

# === Swing state codes by year (aligned with fig_6.R) ===
swing_by_year <- list(
  "2008" = c("FLORIDA", "MINNESOTA", "MISSOURI", "INDIANA", "NORTH CAROLINA"),
  "2012" = c("FLORIDA", "NORTH CAROLINA", "OHIO"),
  "2014" = c("FLORIDA", "NORTH CAROLINA", "OHIO"),
  "2016" = c("FLORIDA", "IOWA", "MICHIGAN", "MINNESOTA", "NEVADA", "NEW HAMPSHIRE", "PENNSYLVANIA", "WISCONSIN"),
  "2020" = c("ARIZONA", "GEORGIA", "MICHIGAN", "NEVADA", "NORTH CAROLINA", "PENNSYLVANIA", "WISCONSIN"),
  "2022" = c("ARIZONA", "GEORGIA", "MICHIGAN", "NEVADA", "NORTH CAROLINA", "PENNSYLVANIA", "WISCONSIN"),
  "2024" = c("GEORGIA", "MICHIGAN", "NEW HAMPSHIRE", "PENNSYLVANIA", "WISCONSIN")
)

# === Run analysis for all years ===
survey_years <- sort(unique(combined_data$year))
cat("Found years:", paste(survey_years, collapse = ", "), "\n\n")

all_mfx_summaries <- lapply(survey_years, function(y) {
  cat("\n", rep("=", 50), "\n")
  cat("Processing year:", y, "\n")
  cat(rep("=", 50), "\n")

  yearly_data <- combined_data %>% filter(year == y)
  cat("Sample size for year", y, ":", nrow(yearly_data), "\n")

  swing_states <- swing_by_year[[as.character(y)]]
  if (is.null(swing_states)) {
    cat("No swing states defined for year", y, "\n")
    swing_states <- c()
  }

  result <- run_fraud_logit(yearly_data, swing_states, fraud_cols)
  cat("Returned", nrow(result), "marginal effects\n")

  return(result)
})
names(all_mfx_summaries) <- as.character(survey_years)

plot_result <- plot_rep_and_dem_effects(all_mfx_summaries, as.character(survey_years), fraud_labels_all_years)
print(plot_result)
ggsave(file.path(figures_dir, "fig_5.png"), plot_result, width = 12, height = 8, dpi = 300)
