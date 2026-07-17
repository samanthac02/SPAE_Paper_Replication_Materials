# Builds a horizontal stacked bar chart showing the share of SPAE respondents
# who believe each type of voter fraud is common (1) vs. not common (0),
# with bars sorted by belief in fraud and percentage labels inside each segment.

library(ggplot2)
library(dplyr)
library(tidyr)

source("config.R")

load(paste0(data_dir, "/COMBINED_DATA.RData"))

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

fraud_labels <- c(
  "voting_more_than_once"      = "Multiple Voting",
  "ballot_tampering"           = "Ballot Tampering",
  "impersonation"              = "Voter Impersonation",
  "non_citizen_voting"         = "Non-Citizen Voting",
  "mail_ballot_fraud"          = "Mail Ballot Fraud",
  "officials_changing_results" = "Official Tampering",
  "vote_counting_software"     = "Voting Software",
  "paid_to_vote"               = "Paid to Vote",
  "fraudulent_registration"    = "Fraudulent Registration",
  "drop_box_fraud"             = "Multiple Ballots (Drop Boxes)"
)

plot_data <- combined_data %>%
  select(all_of(fraud_columns)) %>%
  pivot_longer(everything(), names_to = "fraud_type", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(belief = if_else(value == 1, "Belief in Fraud", "No Belief in Fraud")) %>%
  count(fraud_type, belief) %>%
  group_by(fraud_type) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  mutate(fraud_type = fraud_labels[fraud_type])

fraud_order <- plot_data %>%
  filter(belief == "Belief in Fraud") %>%
  arrange(desc(proportion)) %>%
  pull(fraud_type)

plot_data <- plot_data %>%
  mutate(fraud_type = factor(fraud_type, levels = rev(fraud_order)))

p <- ggplot(plot_data, aes(x = proportion, y = fraud_type, fill = belief)) +
    geom_col(position = "stack", width = 0.7) +
    geom_text(
      aes(label = paste0(round(proportion * 100), "%")),
      position = position_stack(vjust = 0.5),
      color = "black",
      size = 4
    ) +
    scale_fill_manual(values = c(
      "No Belief in Fraud" = "ivory2",
      "Belief in Fraud" = "darkseagreen"
    )) +
    scale_x_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.1))) +
    labs(
      title = "Belief in Voter Fraud by Type",
      y = NULL,
      x = "Proportion",
      fill = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(hjust = 0),
      plot.title = element_text(hjust = 0.5)
    )

print(p)
ggsave(file.path(figures_dir, "fig_2.png"), p, width = 10, height = 6, dpi = 300)
