# Runs PCA on respondents' beliefs across the six fraud-frequency questions
# (2012 onward) and plots the variable loadings on PC1 vs. PC2 as arrows from
# the origin, showing which fraud beliefs cluster together along the top two
# principal components.

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

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

binary_data <- combined_data %>%
  filter(year >= 2012) %>%
  select(any_of(fraud_cols)) %>%
  mutate(across(everything(), ~as.numeric(.))) %>% 
  drop_na()

if (nrow(binary_data) == 0) stop("No data found. Check column names.")

pca_result <- prcomp(binary_data, center = TRUE, scale. = TRUE)

loadings_df <- as.data.frame(pca_result$rotation)
loadings_df$variable <- rownames(loadings_df)
scores_df <- as.data.frame(pca_result$x)

p <- ggplot(loadings_df, aes(x = PC1, y = PC2, label = variable)) +
  geom_segment(aes(xend = PC1, yend = PC2), x = 0, y = 0,
               arrow = arrow(length = unit(0.2, "cm")), color = "darkred") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 4, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  xlim(-1, 1) + ylim(-1, 1) +
  coord_fixed() +
  labs(
    title = "PCA Decomposition of Fraud Beliefs",
    x = "PC1",
    y = "PC2",
  ) +
  theme_minimal()

print(p)
ggsave(file.path(figures_dir, "fig_s3.png"), p, width = 8, height = 8, dpi = 300)