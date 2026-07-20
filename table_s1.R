# Builds a gt table showing the count and proportion of missing (NA) responses
# for each of six voter-fraud belief questions across SPAE survey years, with
# rows for each fraud type and columns for each year.

library(dplyr)
library(tidyr)
library(gt)
library(scales)

source("config.R")

dir <- paste0(data_dir, "/")

load(paste0(dir, "COMBINED_DATA.RData"))

fraud_columns <- c(
  "voting_more_than_once",
  "ballot_tampering",
  "impersonation",
  "non_citizen_voting",
  "mail_ballot_fraud",
  "officials_changing_results"
)

fraud_labels <- c(
  "voting_more_than_once"      = "Voting More Than Once",
  "ballot_tampering"           = "Ballot Tampering",
  "impersonation"              = "Impersonation",
  "non_citizen_voting"         = "Non-Citizen Voting",
  "mail_ballot_fraud"          = "Mail Ballot Fraud",
  "officials_changing_results" = "Officials Changing Results"
)

na_by_year <- combined_data %>%
  pivot_longer(cols = all_of(fraud_columns), names_to = "Question", values_to = "response") %>%
  group_by(year, Question) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(response)),
    na_prop = mean(is.na(response)),
    .groups = "drop"
  )

desired_order <- c(
  "Ballot Tampering",
  "Impersonation",
  "Voting More Than Once",
  "Mail Ballot Fraud",
  "Non-Citizen Voting",
  "Officials Changing Results"
)

na_combined <- na_by_year %>%
  mutate(Question = fraud_labels[Question]) %>%
  mutate(Question = factor(Question, levels = desired_order)) %>%
  arrange(Question) %>%
  mutate(
    html_count = paste0(
      "<span style='display: inline-block; width: 50px; text-align: right;'>",
      scales::comma(na_count, accuracy = 1),
      "</span>"
    ),
    html_prop = paste0(
      "<span style='display: inline-block; width: 65px; text-align: right; margin-left: 10px;'>",
      "(", scales::percent(na_prop, accuracy = 0.1), ")",
      "</span>"
    ),
    combined_val = paste0(html_count, html_prop)
  ) %>%
  select(Question, year, combined_val) %>%
  pivot_wider(
    names_from = year,
    values_from = combined_val
  )

table_s1 <- gt(na_combined) %>%
    tab_header(
      title = "Excluded Data for Election Fraud Belief Questions"
    ) %>%
    tab_spanner(
      label = "Count (Proportion%)",
      columns = -Question
    ) %>%
    cols_label(
      Question = "Fraud Type"
    ) %>%
    fmt_markdown(
      columns = -Question
    ) %>%
    tab_options(
      table.font.size = px(13),
      heading.title.font.size = px(15),
      column_labels.font.weight = "bold",
      data_row.padding = px(5),
      table.align = "left"
    ) %>%
    cols_align(
      align = "center",
      columns = -Question
    )

print(table_s1)
gtsave(table_s1, file.path(figures_dir, "table_s1.png"))
