# Computes and visualizes the pairwise correlation matrix of all numeric 
# survey variables (excluding caseid, year, and weight) from the SPAE data, 
# with renamed fraud-question columns and an upper-triangle corrplot using a 
# yellow-green-blue color gradient.

load("/Users/samantha/Desktop/SPAE/COMBINED_DATA.RData")
df <- combined_data

colnames(df) <- gsub("VF: multiple voting", "Multiple Voting", colnames(df))
colnames(df) <- gsub("VF: ballot tampering", "Ballot Tampering", colnames(df))
colnames(df) <- gsub("VF: impersonation", "Impersonation", colnames(df))
colnames(df) <- gsub("VF: non-citizen voting", "Non-citizen Voting", colnames(df))
colnames(df) <- gsub("VF: mail ballot fraud", "Mail Ballot Fraud", colnames(df))
colnames(df) <- gsub("VF: official tampering", "Official Tampering", colnames(df))

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