# Builds a horizontal stacked bar chart showing the share of SPAE respondents 
# who believe each of twelve types of voter fraud occurs frequently vs. rarely, 
# with bars sorted by belief in fraud and percentage labels inside each segment.

library(ggplot2)
library(dplyr)
library(tidyr)

make_combined_support_belief_bar <- function(data, columns) {
  yes_labels <- c(
    "IT OCCURS FREQUENTLY",
    "IT IS VERY COMMON"
  )
  
  no_labels <- c(
    "IT ALMOST NEVER OCCURS",
    "IT OCCURS OCCASIONALLY", 
    "IT OCCURS INFREQUENTLY"
  )
  
  results <- data.frame()
  
  for (col in columns) {
    values <- as.character(data[[col]])
    values[values == "PHONE - REFUSED" | is.na(values)] <- "NOT ANSWERED"
    
    belief <- case_when(
      values %in% yes_labels ~ "Belief in Fraud",
      values %in% no_labels ~ "No Belief in Fraud",
      TRUE ~ NA_character_
    )
    
    df_col <- data.frame(
      Question = col,
      Belief = belief
    ) %>%
      filter(!is.na(Belief)) %>%
      count(Question, Belief) %>%
      group_by(Question) %>%
      mutate(Proportion = n / sum(n))
    
    results <- rbind(results, df_col)
  }
  
  question_labels <- c(
    "VF: multiple voting" = "Multiple Voting",
    "VF: ballot tampering" = "Ballot Tampering",
    "VF: impersonation" = "Voter Impersonation",
    "VF: non-citizen voting" = "Non-Citizen Voting",
    "VF: mail ballot fraud" = "Mail Ballot Fraud",
    "VF: official tampering" = "Official Tampering",
    "VF: voting software fraud" = "Voting Software",
    "VF: paid to vote" = "Paid to Vote",
    "VF: fraudulent registration" = "Fraudulent Registration",
    "VF: multiple ballots" = "Multiple Ballots",
    "VF: nationwide hacking" = "Hacking (Nationwide)",
    "VF: local hacking" = "Hacking (Local)"
  )
  
  results$Question <- recode(results$Question, !!!question_labels)
  
  # Compute ordering based on belief in fraud
  fraud_order <- results %>%
    filter(Belief == "Belief in Fraud") %>%
    arrange(desc(Proportion)) %>%
    pull(Question)
  
  results$Question <- factor(results$Question, levels = rev(fraud_order))
  
  # Fixed: Removed print() wrapper and returned the plot instead
  ggplot(results, aes(x = Proportion, y = Question, fill = Belief)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    geom_text(
      aes(label = paste0(round(Proportion * 100), "%")),
      position = position_stack(vjust = 0.5),
      hjust = 0.5,  # Changed from hjust = 0 for better centering
      color = "black",
      size = 4
    ) +
    scale_fill_manual(values = c(
      "No Belief in Fraud" = "ivory2",
      "Belief in Fraud" = "darkseagreen"
    )) +
    labs(
      title = "Belief in Voter Fraud by Type",
      y = NULL,
      x = "Proportion"
    ) +
    scale_x_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.1))) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(hjust = 0),
      plot.title = element_text(hjust = 0.5)
    )
}

# Load data
load("/Users/samantha/Desktop/SPAE/FRAUD_DATA.RData")

# Define fraud columns
fraud_columns <- c(
  "VF: multiple voting", 
  "VF: ballot tampering", 
  "VF: impersonation",
  "VF: non-citizen voting",
  "VF: mail ballot fraud",
  "VF: official tampering",
  "VF: voting software fraud",
  "VF: paid to vote",
  "VF: fraudulent registration",
  "VF: multiple ballots",
  "VF: nationwide hacking",
  "VF: local hacking"
)

# Fixed: Corrected recode syntax
combined_data[fraud_columns] <- lapply(combined_data[fraud_columns], function(column) {
  recode(as.numeric(as.character(column)),
         `1` = "IT IS VERY COMMON",        # Fixed: Added backticks around numbers
         `2` = "IT OCCURS FREQUENTLY", 
         `3` = "IT OCCURS OCCASIONALLY", 
         `4` = "IT OCCURS INFREQUENTLY",
         `5` = "IT ALMOST NEVER OCCURS",
         `9` = "NOT ANSWERED",
         .default = "NOT ANSWERED"
  )
})

print(make_combined_support_belief_bar(combined_data, fraud_columns))