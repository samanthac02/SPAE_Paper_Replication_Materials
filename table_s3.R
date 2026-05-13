# Builds a gt table cross-tabulating respondents' confidence in election 
# administration against the number of fraud types (0–6) they believe are common, 
# with rows for each confidence level (Not at all → Very confident) and columns 
# showing counts for each possible fraud-belief total.

library(dplyr)
library(tidyr)
library(haven)
library(gt)

load("/Users/samantha/Desktop/SPAE/COMBINED_DATA.RData")

combined_data <- combined_data %>%
  mutate(
    fraud_count = rowSums(
      across(c(`Voting more than once`, `Ballot tampering`, `Impersonation`,
               `Non-citizen voting`, `Mail ballot fraud`, `Officials changing results`),
             ~ as.numeric(.) == 1),
      na.rm = TRUE
    ),
    fraud_any_na = rowSums(
      across(c(`Voting more than once`, `Ballot tampering`, `Impersonation`,
               `Non-citizen voting`, `Mail ballot fraud`, `Officials changing results`),
             ~ is.na(.))
    ),
    confidence_lbl = as.character(haven::as_factor(Confidence))
  )

table_data <- combined_data %>%
  filter(fraud_any_na == 0,
         !is.na(confidence_lbl),
         confidence_lbl %in% c("NOT AT ALL CONFIDENT", "NOT TOO CONFIDENT",
                               "SOMEWHAT CONFIDENT", "VERY CONFIDENT")) %>%
  mutate(
    Confidence = recode(confidence_lbl,
                        "NOT AT ALL CONFIDENT" = "Not at all confident",
                        "NOT TOO CONFIDENT"    = "Not too confident",
                        "SOMEWHAT CONFIDENT"   = "Somewhat confident",
                        "VERY CONFIDENT"       = "Very confident"),
    Confidence = factor(Confidence,
                        levels = c("Not at all confident", "Not too confident",
                                   "Somewhat confident", "Very confident"))
  ) %>%
  count(Confidence, fraud_count) %>%
  pivot_wider(names_from = fraud_count, values_from = n, values_fill = 0) %>%
  arrange(Confidence)

for (k in as.character(0:6)) {
  if (!k %in% names(table_data)) table_data[[k]] <- 0
}
table_data <- table_data[, c("Confidence", as.character(0:6))]

gt_table <- table_data %>%
  gt(rowname_col = "Confidence") %>%
  tab_header(title = "Counts of Confidence Level by Fraud Types Believed") %>%
  tab_spanner(label = "Number of Fraud Types Believed (0–6)",
              columns = as.character(0:6)) %>%
  cols_align(align = "center", columns = as.character(0:6)) %>%
  tab_options(table.font.size = 14,
              heading.title.font.size = 18)

print(gt_table)