setwd(dirname(sys.frame(1)$ofile))

source("config.R")

cat("Running combine_datasets_script.R\n")
source("combine_datasets_script.R")

scripts <- c(
  "fig_1.R",
  "fig_2.R",
  "fig_3.R",
  "fig_4.R",
  "fig_5.R",
  "fig_6.R",
  "fig_s1.R",
  "fig_s2.R",
  "fig_s3.R",
  "fig_s4.R",
  "fig_s5.R",
  "fig_s6.R",
  "table_s1.R",
  "table_s2.R",
  "table_s3.R",
  "SM_logit_table_2008.R",
  "SM_logit_tables_2012_to_2024.R"
)

for (s in scripts) {
  cat(paste0("\nRunning ", s, " \n"))
  source(s)
}

cat(paste0(
  "\n========================================\n",
  "All scripts completed successfully.\n",
  "Figures saved to: ", figures_dir, "\n",
  "========================================\n"
))
