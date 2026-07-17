# Computes and visualizes the pairwise correlation matrix of all numeric
# survey variables (excluding caseid, year, and weight) from the SPAE data,
# with renamed fraud-question columns and an upper-triangle corrplot using a
# yellow-green-blue color gradient.

source("config.R")

load(paste0(data_dir, "/COMBINED_DATA.RData"))
df <- combined_data

colnames(df) <- gsub("voting_more_than_once", "Multiple Voting", colnames(df))
colnames(df) <- gsub("ballot_tampering", "Ballot Tampering", colnames(df))
colnames(df) <- gsub("impersonation", "Impersonation", colnames(df))
colnames(df) <- gsub("non_citizen_voting", "Non-citizen Voting", colnames(df))
colnames(df) <- gsub("mail_ballot_fraud", "Mail Ballot Fraud", colnames(df))
colnames(df) <- gsub("officials_changing_results", "Official Tampering", colnames(df))

num_vars <- df[sapply(df, is.numeric)]
num_vars <- subset(num_vars, select = -caseid)
num_vars <- subset(num_vars, select = -year)
num_vars <- subset(num_vars, select = -weight)

cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")
print(round(cor_matrix, 2))

library(corrplot)
corr_colors <- colorRampPalette(c("lightyellow1", "palegreen", "cyan3", "steelblue3"))(200)
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.8,
         col = corr_colors,
         cl.lim = c(-1, 1))
