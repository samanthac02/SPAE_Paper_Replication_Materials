# Builds faceted horizontal bar charts showing the survey-weighted share of
# pooled SPAE respondents (2012–2024) who believe each of six types of voter
# fraud is common vs. not common, broken down by political party (Democrat,
# Republican, Independent), with one panel per fraud type and percentage labels
# inside each segment.

library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)

source("config.R")

load_obj <- load(paste0(data_dir, "/COMBINED_DATA.RData"))
combined_data <- get(load_obj)

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

# Recode fraud responses
combined_data <- combined_data %>%
  mutate(across(all_of(fraud_columns), ~ case_when(
    . %in% c(1, "IT IS VERY COMMON", "IT OCCURS OCCASIONALLY") ~ "Common",
    . %in% c(0, "IT OCCURS INFREQUENTLY", "IT ALMOST NEVER OCCURS") ~ "Not Common",
    TRUE ~ NA_character_
  )))

# Pool 2012–2024 and recode demographics
data_pooled <- combined_data %>%
  filter(year >= 2012, year <= 2024) %>%
  mutate(
    party_label = recode(haven::as_factor(party),
                   "DEMOCRAT" = "Democrat",
                   "REPUBLICAN" = "Republican",
                   "INDEPENDENT" = "Independent",
                   .default = "Other/Missing"),

    gender = recode(haven::as_factor(gender),
                    "MALE" = "Male",
                    "FEMALE" = "Female",
                    .default = "Other/Missing"),

    race = recode(haven::as_factor(race),
                  "WHITE" = "White",
                  "BLACK" = "Black",
                  "HISPANIC" = "Hispanic",
                  "ASIAN" = "Asian",
                  .default = "Other/Missing"),

    education = case_when(
      haven::as_factor(education) %in% c("HIGH SCHOOL GRADUATE", "NO HS") ~ "High School or Less",
      haven::as_factor(education) %in% c("SOME COLLEGE", "2-YEAR") ~ "Some College",
      haven::as_factor(education) == "4-YEAR" ~ "4-year degree",
      haven::as_factor(education) == "POST-GRAD" ~ "Post-Graduate",
      TRUE ~ "Other/Missing"
    ),

    age = year - as.numeric(as.character(birth_year)),
    age_group = case_when(
      is.na(age) ~ "Missing",
      age < 35 ~ "18-34",
      age < 50 ~ "35-49",
      age < 65 ~ "50-64",
      TRUE ~ "65+"
    )
  )

fraud_long_pooled <- data_pooled %>%
  select(all_of(fraud_columns), party_label, gender, race, education, age_group, weight) %>%
  pivot_longer(cols = all_of(fraud_columns), names_to = "fraud_type", values_to = "belief") %>%
  filter(!is.na(belief), !is.na(weight)) %>%
  mutate(fraud_type = fraud_labels[fraud_type])

p <- fraud_long_pooled %>%
  filter(party_label %in% c("Democrat", "Republican", "Independent")) %>%
  group_by(fraud_type, party_label, belief) %>%
  summarise(weighted_n = sum(weight), .groups = "drop") %>%
  group_by(fraud_type, party_label) %>%
  mutate(percentage = weighted_n / sum(weighted_n)) %>%
  ungroup() %>%
  ggplot(aes(y = party_label, x = percentage, fill = belief)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(round(percentage * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5
  ) +
  facet_wrap(~ fraud_type, ncol = 2) +
  scale_fill_manual(values = c("Common" = "#F8766D", "Not Common" = "#00BFC4")) +
  labs(
    title = "Fraud Belief by Party (Pooled 2012–2024, Weighted)",
    y = "Political Party",
    x = "Proportion",
    fill = "Belief"
  ) +
  theme_minimal()

print(p)
ggsave(file.path(figures_dir, "fig_s4.png"), p, width = 10, height = 8, dpi = 300)
