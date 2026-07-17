# Estimates the marginal effect of living in a swing state (defined per election 
# year) on belief that each of six voter-fraud types is common, using 
# survey-weighted logistic regression across all SPAE survey years, then plots 
# the year-by-year effects with 95% confidence intervals, faceted by fraud type.

library(ggplot2)
library(dplyr)
library(survey)
library(marginaleffects)
library(haven)

source("config.R")

load(paste0(data_dir, "/COMBINED_DATA.RData"))
 
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

desired_order <- c(
  "Ballot tampering",
  "Impersonation",
  "Voting more than once",
  "Mail ballot fraud",
  "Non-citizen voting",
  "Officials changing results"
)

run_swing_state_logit <- function(data, swing_state_codes, fraud_cols) {
  keep <- c("state", "party", "weight", fraud_cols)
  
  selected_data <- data %>%
    select(all_of(keep)) %>%
    mutate(
      swing_states = as.numeric(state %in% swing_state_codes)
    )
  
  independent_vars <- "swing_states"
  
  logit_results <- lapply(fraud_cols, function(dep_var) {
    cat("Running swing state model for:", dep_var, "\n")
    
    model_data <- selected_data %>%
      filter(!is.na(.data[[dep_var]]))
    
    if (nrow(model_data) == 0) {
      cat("Skipping", dep_var, "- no non-missing data\n")
      return(NULL)
    }
    
    swing_distribution <- table(model_data$swing_states)
    
    if (length(swing_distribution) < 2) {
      cat("Skipping", dep_var, "- no swing state variation\n")
      return(NULL)
    }
    
    if (min(swing_distribution) < 5) {
      cat("WARNING: Very small swing state sample (", min(swing_distribution), ") - estimates may be unstable\n")
    }
    
    tryCatch({
      formula <- as.formula(paste0("`", dep_var, "` ~ ", independent_vars))
      svy_design <- svydesign(ids = ~1, weights = ~weight, data = model_data)
      
      model <- svyglm(formula, design = svy_design, family = quasibinomial())
      
      meffects <- slopes(model, variables = "swing_states", type = "response", .average = TRUE)
      meffects_df <- as.data.frame(meffects)
      
      meffects_df$dependent_variable <- dep_var

      return(meffects_df)
      
    }, error = function(e) {
      cat("svyglm failed for", dep_var, "- trying glm fallback\n")
      
      tryCatch({
        model <- glm(
          formula,
          data = model_data,
          family = binomial(link = "logit"),
          weights = model_data$weight
        )
        
        meffects <- slopes(model, variables = "swing_states", type = "response")
        meffects_df <- as.data.frame(meffects)
        meffects_df$dependent_variable <- dep_var
        
        result <- meffects_df %>%
          group_by(term, dependent_variable) %>%
          summarise(
            estimate = mean(estimate),
            std.error = sqrt(mean(std.error^2, na.rm = TRUE)),
            .groups = "drop"
          )

        return(result)

      }, error = function(e2) {
        cat("Both models failed for", dep_var, "- skipping\n")
        return(NULL)
      })
    })
  })
  
  result <- bind_rows(logit_results)
  
  if (nrow(result) == 0 || !"term" %in% names(result)) {
    return(tibble())
  }
  
  result %>%
    filter(term == "swing_states")
}

plot_swingstate_effects <- function(all_mfx_summaries, survey_years, fraud_labels_all_years, panel_order) {
  combined_df <- bind_rows(all_mfx_summaries, .id = "survey_year") %>%
    filter(term == "swing_states") %>%
    mutate(
      group = "Swing vs Non-Swing",
      survey_year = factor(survey_year, levels = survey_years),
      fraud_label = fraud_labels_all_years[as.character(dependent_variable)]
    ) %>%
    filter(!is.na(estimate), !is.na(std.error), !is.na(fraud_label)) %>%
    distinct(survey_year, fraud_label, term, .keep_all = TRUE) %>%
    mutate(fraud_label = factor(fraud_label, levels = panel_order))
  
  if (nrow(combined_df) == 0) {
    message("No data to plot.")
    return(NULL)
  }
  
  ggplot(combined_df, aes(x = survey_year, y = estimate, color = group, group = group)) +
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
    scale_color_manual(
      values = c("Swing vs Non-Swing" = "darkturquoise")
    ) +
    facet_wrap(~ fraud_label, scales = "fixed", ncol = 3) +
    coord_cartesian(ylim = c(-0.05, 0.10)) +
    labs(
      x = "Survey Year",
      y = "Marginal Effect (Swing State vs Non-Swing)",
      title = "Marginal Effect of Swing State Residence on Belief in Voter Fraud",
      color = "State Type"
    ) +
    theme_minimal() +
    theme(
      panel.spacing = unit(2, "lines"),
      legend.position = "bottom",
      axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5), 
      axis.text.y = element_text(size = 9),
      strip.text = element_text(size = 11, face = "bold", margin = margin(b = 10))
    )
}

swing_by_year <- list(
  "2008" = c("FLORIDA", "MINNESOTA", "MISSOURI", "INDIANA", "NORTH CAROLINA"),
  "2012" = c("FLORIDA", "NORTH CAROLINA", "OHIO"),
  "2014" = c("FLORIDA", "NORTH CAROLINA", "OHIO"),
  "2016" = c("FLORIDA", "IOWA", "MICHIGAN", "MINNESOTA", "NEVADA", "NEW HAMPSHIRE", "PENNSYLVANIA", "WISCONSIN"),
  "2020" = c("ARIZONA", "GEORGIA", "MICHIGAN", "NEVADA", "NORTH CAROLINA", "PENNSYLVANIA", "WISCONSIN"),
  "2022" = c("ARIZONA", "GEORGIA", "MICHIGAN", "NEVADA", "NORTH CAROLINA", "PENNSYLVANIA", "WISCONSIN"),
  "2024" = c("GEORGIA", "MICHIGAN", "NEW HAMPSHIRE", "PENNSYLVANIA", "WISCONSIN")
)

survey_years <- sort(unique(combined_data$year))
cat("SWING STATE ANALYSIS\n")
cat("===================\n")
cat("Found years:", paste(survey_years, collapse = ", "), "\n\n")

all_swing_summaries <- lapply(survey_years, function(y) {
  cat("\n", rep("=", 60), "\n")
  cat("Processing swing state analysis for year:", y, "\n")
  cat(rep("=", 60), "\n")
  
  yearly_data <- combined_data %>% filter(year == y)
  cat("Sample size for year", y, ":", nrow(yearly_data), "\n")
  
  swing_states <- swing_by_year[[as.character(y)]]
  if (is.null(swing_states)) {
    cat("No swing states defined for year", y, "\n")
    return(tibble())
  }
  
  result <- run_swing_state_logit(yearly_data, swing_states, fraud_cols)
  cat("Returned", nrow(result), "swing state marginal effects\n")
  
  return(result)
})
names(all_swing_summaries) <- as.character(survey_years)

cat("\n\nCreating swing state plot...\n")

swing_plot <- plot_swingstate_effects(all_swing_summaries, as.character(survey_years), fraud_labels_all_years, desired_order)

if (!is.null(swing_plot)) {
  print(swing_plot)
  ggsave(file.path(figures_dir, "fig_6.png"), swing_plot, width = 12, height = 8, dpi = 300)
  cat("Swing state plot created successfully!\n")
} else {
  cat("Swing state plot creation failed - no data to plot.\n")
}