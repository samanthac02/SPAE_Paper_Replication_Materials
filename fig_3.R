# Plots the percentage of Democrats vs. Republicans who believe each form of
# election fraud is common. Also plots the share who are "not confident" their
# vote was counted, tracked across SPAE survey years 2008–2024. Each fraud type
# gets its own facet, with 95% CI ribbons around the trend lines.

library(ggplot2)
library(dplyr)
library(tidyr)
library(survey)
library(haven)

load("/Users/samantha/Desktop/SPAE/COMBINED_DATA.RData")

combined_data$year <- as.numeric(as.character(combined_data$year))

fraud_columns <- c(
  "Voting more than once",
  "Ballot tampering",
  "Impersonation",
  "Non-citizen voting",
  "Mail ballot fraud",
  "Officials changing results"
)

confidence_col <- "Confidence"

desired_order <- c(
  "Mail ballot fraud",
  "Non-citizen voting",
  "Officials changing results",
  "Ballot tampering",
  "Impersonation",
  "Voting more than once",
  "Not confident vote counted"
)

dk_values <- c("DON'T KNOW", "I DON'T KNOW", "I DON\u2019T KNOW")  # \u2019 = curly '

combined_data <- combined_data %>%
  mutate(
    .conf_raw = toupper(trimws(as.character(.data[[confidence_col]]))),
    `Not confident vote counted` = case_when(
      .conf_raw %in% c("NOT TOO CONFIDENT", "NOT AT ALL CONFIDENT") ~ 1,
      .conf_raw %in% c("VERY CONFIDENT", "SOMEWHAT CONFIDENT")     ~ 0,
      .conf_raw %in% dk_values                                     ~ NA_real_,
      TRUE                                                         ~ NA_real_
    )
  ) %>%
  select(-.conf_raw)

all_outcome_columns <- c(fraud_columns, "Not confident vote counted")



proportions_table <- combined_data %>%
  filter(
    year %in% c(2008, 2012, 2014, 2016, 2020, 2022, 2024),
    party3 %in% c("DEMOCRAT", "REPUBLICAN")
  ) %>%
  mutate(
    party = case_when(
      party3 == "DEMOCRAT" ~ "Democrat",
      party3 == "REPUBLICAN" ~ "Republican"
    )
  ) %>%
  pivot_longer(
    cols = all_of(all_outcome_columns),
    names_to = "fraud_type",
    values_to = "believes_common"
  ) %>%
  filter(!is.na(believes_common)) %>%
  group_by(year, fraud_type, party) %>%
  summarise(
    n_total = n(),
    n_common = sum(believes_common == 1),
    proportion_common = round(n_common / n_total, 3),
    percentage_common = round((n_common / n_total) * 100, 1),
    .groups = "drop"
  )

# --- STEP 4: PREPARE PLOT DATA ---
plot_data <- proportions_table %>%
  mutate(
    std_error = sqrt((proportion_common * (1 - proportion_common)) / n_total),
    ci_lower = pmax(0, proportion_common - 1.96 * std_error),
    ci_upper = pmin(1, proportion_common + 1.96 * std_error),
    percentage = proportion_common * 100,
    ci_lower_pct = ci_lower * 100,
    ci_upper_pct = ci_upper * 100
  ) %>%
  mutate(fraud_type = factor(fraud_type, levels = desired_order))

# --- STEP 5: CREATE PLOT ---
p <- ggplot(plot_data, aes(x = factor(year), y = percentage, color = party, fill = party)) +
  geom_ribbon(
    aes(ymin = ci_lower_pct, ymax = ci_upper_pct, group = party),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(aes(group = party), linewidth = 1.2) +
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