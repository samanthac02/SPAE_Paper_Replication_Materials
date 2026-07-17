# Plots the percentage of Democrats vs. Republicans who believe each form of
# election fraud is common. Also plots the share who are "not confident" their
# vote was counted, tracked across SPAE survey years 2008–2024. Each fraud type
# gets its own facet, with 95% CI ribbons around the trend lines.

library(ggplot2)
library(dplyr)
library(tidyr)
library(survey)
library(haven)

source("config.R")

load(paste0(data_dir, "/COMBINED_DATA.RData"))

combined_data$year <- as.numeric(as.character(combined_data$year))

fraud_columns <- c(
  "voting_more_than_once",
  "ballot_tampering",
  "impersonation",
  "non_citizen_voting",
  "mail_ballot_fraud",
  "officials_changing_results"
)

confidence_col <- "confidence"

fraud_labels <- c(
  "voting_more_than_once"      = "Voting More Than Once",
  "ballot_tampering"           = "Ballot Tampering",
  "impersonation"              = "Impersonation",
  "non_citizen_voting"         = "Non-Citizen Voting",
  "mail_ballot_fraud"          = "Mail Ballot Fraud",
  "officials_changing_results" = "Officials Changing Results",
  "not_confident_vote_counted" = "Not Confident Vote Counted"
)

desired_order <- c(
  "Mail Ballot Fraud",
  "Non-Citizen Voting",
  "Officials Changing Results",
  "Ballot Tampering",
  "Impersonation",
  "Voting More Than Once",
  "Not Confident Vote Counted"
)

cols_not_in_2008 <- c("non_citizen_voting", "mail_ballot_fraud", "officials_changing_results")

dk_values <- c("DON'T KNOW", "I DON'T KNOW", "I DON’T KNOW")

combined_data <- combined_data %>%
  mutate(
    .conf_raw = toupper(trimws(as.character(.data[[confidence_col]]))),
    not_confident_vote_counted = case_when(
      .conf_raw %in% c("NOT TOO CONFIDENT", "NOT AT ALL CONFIDENT") ~ 1,
      .conf_raw %in% c("VERY CONFIDENT", "SOMEWHAT CONFIDENT")     ~ 0,
      .conf_raw %in% dk_values                                     ~ NA_real_,
      TRUE                                                         ~ NA_real_
    )
  ) %>%
  select(-.conf_raw)

all_outcome_columns <- c(fraud_columns, "not_confident_vote_counted")

analysis_data <- combined_data %>%
  filter(
    year %in% c(2008, 2012, 2014, 2016, 2020, 2022, 2024),
    party %in% c("DEMOCRAT", "REPUBLICAN")
  ) %>%
  mutate(
    party_label = case_when(
      party == "DEMOCRAT" ~ "Democrat",
      party == "REPUBLICAN" ~ "Republican"
    )
  )

results_list <- list()

for (yr in sort(unique(analysis_data$year))) {
  yr_data <- analysis_data %>% filter(year == yr)
  svy <- svydesign(ids = ~1, weights = ~weight, data = yr_data)

  yr_outcomes <- if (yr == 2008) setdiff(all_outcome_columns, cols_not_in_2008) else all_outcome_columns
  for (col in yr_outcomes) {
    tryCatch({
      by_party <- svyby(
        as.formula(paste0("~", col)),
        ~party_label,
        svy,
        svymean,
        na.rm = TRUE
      )

      results_list[[length(results_list) + 1]] <- data.frame(
        year = yr,
        fraud_type = col,
        party_label = by_party$party_label,
        proportion = by_party[[col]],
        std_error = as.numeric(SE(by_party)),
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)
  }
}

plot_data <- bind_rows(results_list) %>%
  mutate(
    ci_lower = pmax(0, proportion - 1.96 * std_error),
    ci_upper = pmin(1, proportion + 1.96 * std_error),
    percentage = proportion * 100,
    ci_lower_pct = ci_lower * 100,
    ci_upper_pct = ci_upper * 100,
    fraud_type = factor(fraud_labels[fraud_type], levels = desired_order)
  )

p <- ggplot(plot_data, aes(x = factor(year), y = percentage, color = party_label, fill = party_label)) +
  geom_ribbon(
    aes(ymin = ci_lower_pct, ymax = ci_upper_pct, group = party_label),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(aes(group = party_label), linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("Democrat" = "#1f77b4", "Republican" = "#d62728")
  ) +
  scale_fill_manual(
    values = c("Democrat" = "#1f77b4", "Republican" = "#d62728")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, NA),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.1))
  ) +
  facet_wrap(~ fraud_type, scales = "fixed", ncol = 3) +
  labs(x = "Year", y = "", color = "", fill = "") +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.5, "lines"),
    strip.text = element_text(size = 11, face = "bold", margin = margin(b = 10)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.margin = margin(t = 15),
    axis.text.x = element_text(size = 9, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )

print(p)
ggsave(file.path(figures_dir, "fig_3.png"), p, width = 12, height = 8, dpi = 300)
