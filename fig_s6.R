# Builds faceted heatmaps showing the proportion of SPAE respondents (2012 
# onward) believing 0–6 voter-fraud types at each confidence-in-elections level, 
# with one panel per party (Democrat, Independent, Republican) and cells colored 
# and labeled by within-column percentage.

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(gt)
library(stringr)

load("/Users/samantha/Desktop/SPAE/COMBINED_DATA.RData")

confidence_var <- "Confidence"
party_var <- "party3" 

fraud_cols <- c("Voting more than once", "Ballot tampering", "Impersonation", 
                "Non-citizen voting", "Mail ballot fraud", "Officials changing results")

analysis_data <- combined_data %>%
  filter(year >= 2012) %>%
  select(year, all_of(party_var), all_of(fraud_cols), all_of(confidence_var)) %>%
  
  filter(!grepl("DON'T KNOW", .data[[confidence_var]], ignore.case = TRUE)) %>%
  filter(!grepl("I DON’T KNOW", .data[[confidence_var]], ignore.case = TRUE)) %>%
  drop_na(all_of(party_var)) %>%
  drop_na()

analysis_data <- analysis_data %>%
  mutate(fraud_count = rowSums(select(., all_of(fraud_cols))))

analysis_data <- analysis_data %>%
  mutate(
    !!party_var := str_to_title(.data[[party_var]])
  )

analysis_data[[party_var]] <- factor(
  analysis_data[[party_var]],
  levels = c("Democrat", "Independent", "Republican")
)

analysis_data[[confidence_var]] <- factor(
  analysis_data[[confidence_var]], 
  levels = c("NOT AT ALL CONFIDENT", 
             "NOT TOO CONFIDENT", 
             "SOMEWHAT CONFIDENT", 
             "VERY CONFIDENT"),
  labels = c("Not at all confident", 
             "Not too confident", 
             "Somewhat confident", 
             "Very confident")
)

plot_data <- analysis_data %>%
  count(.data[[party_var]], .data[[confidence_var]], fraud_count) %>%
  group_by(.data[[party_var]], .data[[confidence_var]]) %>%
  mutate(prop = n / sum(n)) %>% 
  ungroup()

plot <- ggplot(plot_data, aes(x = .data[[confidence_var]], y = factor(fraud_count), fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  scale_fill_gradientn(
    colors = c("#ffffd9", "#7fcdbb", "#1d91c0"),
    labels = scales::percent
  ) +
  
  geom_text(aes(
    label = scales::percent(prop, accuracy = 1),
    color = prop > 0.5
  ), 
  size = 3, 
  fontface = "bold",
  show.legend = FALSE
  ) +
  
  scale_color_manual(values = c("black", "white")) +
  
  facet_wrap(vars(.data[[party_var]])) +
  
  labs(
    title = "Proportions for Number of Fraud Types Believed by Confidence Level and Party",
    x = "Confidence Level",
    y = "Number of Fraud Types Believed",
    fill = "Proportion"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

print(plot)