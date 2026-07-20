# SPAE Paper Replication Materials

Replication code for analyzing voter fraud beliefs using data from the Survey of Performance of American Elections (SPAE), spanning survey years 2008–2024.

## Prerequisites

Install the following R packages:

```r
install.packages(c(
  "here", "dplyr", "tidyr", "ggplot2", "gt", "survey",
  "haven", "broom", "purrr", "scales", "stringr",
  "corrplot", "mirt", "marginaleffects", "webshot2"
))
```

## How to Run

Scripts must be run from the project root directory (where this README lives).

### Quick Start

To generate everything at once, run `orchestrator.R`. It sources the configuration, builds the combined dataset, and runs all figure, table, and model scripts in sequence:

```r
source("orchestrator.R")
```

When complete, it prints the directory path where all figures are saved.

### Running Scripts Individually

If you prefer to run scripts one at a time, follow the steps below.

### Step 1: Configuration

Source `config.R` first. It is automatically sourced by every script, but it sets:

- `data_dir` — the project root, resolved portably via `here::here()`
- `figures_dir` — a `figures/` subdirectory (created automatically) where all plots are saved
- `set.seed(12345)` — for reproducibility of stochastic models (IRT)

### Step 2: Combine datasets

Run **one** of the following to harmonize the raw SPAE survey files into a single combined dataset:

| Script | Output | Fraud belief threshold |
|---|---|---|
| `combine_datasets_script.R` | `COMBINED_DATA.RData` | "Very common" and "Frequently" coded as 1; "Occasionally" and "Infrequently" coded as 0 |
| `combine_datasets_script_alt.R` | `COMBINED_DATA_ALT.RData` | "Very common", "Frequently", and "Occasionally" coded as 1; only "Infrequently" coded as 0 |

These scripts load each year's raw `.RData` file (e.g., `MITU0017_OUTPUT.RData`), rename and harmonize columns across years, recode fraud belief responses to binary 0/1, and row-bind into a single dataframe.

### Step 3: Run figure, table, or model scripts

Once the combined dataset exists, run any of the scripts below in any order. Each sources `config.R` and loads `COMBINED_DATA.RData` automatically.

## Script Descriptions

### Main Figures

| Script | Output | Description |
|---|---|---|
| `fig_1.R` | gt table (console) | Shows which fraud questions appeared in each SPAE survey year (2008–2024), with checkmarks marking availability. |
| `fig_2.R` | `figures/fig_2.png` | Horizontal stacked bar chart of the proportion of respondents who believe each type of voter fraud is common, across all 10 fraud questions. |
| `fig_3.R` | `figures/fig_3.png` | Line plot tracking Democrat vs. Republican belief in each fraud type over time (2008–2024), with survey-weighted proportions and 95% confidence interval ribbons. Includes a panel for confidence in vote counting. |
| `fig_4.R` | `figures/fig_4.png` | Stacked bar chart of fraud belief by party (Democrat, Republican, Independent) for 2024 only, faceted by fraud type with percentage labels. |
| `fig_5.R` | `figures/fig_5.png` | Marginal effects of party affiliation (Republican vs. Independent, Democrat vs. Independent) on belief in each fraud type, estimated via survey-weighted logistic regression controlling for swing state residence, plotted across survey years with trend lines. |
| `fig_6.R` | `figures/fig_6.png` | Marginal effect of living in a swing state (defined per election year) on belief in each fraud type, estimated via survey-weighted logistic regression, plotted across survey years with 95% CIs. |

### Supplementary Figures

| Script | Output | Description |
|---|---|---|
| `fig_s1.R` | Correlation matrix plot (console) | Pairwise correlation matrix of all numeric survey variables, visualized as an upper-triangle corrplot. |
| `fig_s2.R` | gt table (console) | Fits a 2PL IRT model on the six fraud belief questions to extract a single fraud belief score, then regresses it on demographics via survey-weighted linear regression. Prints a table of coefficients, standard errors, and p-values. |
| `fig_s3.R` | `figures/fig_s3.png` | PCA biplot of variable loadings on PC1 vs. PC2 for the six fraud belief questions (2012 onward), showing which beliefs cluster together. |
| `fig_s4.R` | `figures/fig_s4.png` | Weighted stacked bar chart of fraud belief by party, pooled across 2012–2024, faceted by fraud type. |
| `fig_s5.R` | `figures/fig_s5.png` | Heatmap of the proportion of respondents believing 0–6 fraud types at each confidence-in-elections level (2012 onward). |
| `fig_s6.R` | `figures/fig_s6.png` | Same as fig_s5 but faceted by party (Democrat, Independent, Republican). |

### Supplementary Tables

| Script | Output | Description |
|---|---|---|
| `table_s1.R` | gt table (console) | Count and proportion of missing (NA) responses for each fraud question by survey year. |
| `table_s2.R` | gt table (console) | Sample demographics (gender, age, education, party, ideology) with unweighted and survey-weighted percentages. |
| `table_s3.R` | gt table (console) | Cross-tabulation of confidence in election administration against the number of fraud types (0–6) believed to be common. |

### Logistic Regression Tables

| Script | Output | Description |
|---|---|---|
| `SM_logit_table_2008.R` | gt table (console) | Survey-weighted logistic regression of belief in three fraud types (voting more than once, ballot tampering, impersonation) on demographic predictors for 2008. Loads the raw 2008 data directly. |
| `SM_logit_tables_2012_to_2024.R` | gt table (console) | Survey-weighted logistic regression of belief in six fraud types on demographic predictors for a specified year (2012–2024). Uses `COMBINED_DATA.RData`. |

## Data Files

The raw SPAE data files are named by their MITU survey IDs (e.g., `MITU0017_OUTPUT.RData` for 2012). The combine datasets script maps these to survey years internally. The `spae_2008.RData` file contains the 2008 survey data.

## Notes

- All ggplot figures are both displayed on screen and saved as 300 DPI PNGs to the `figures/` directory.
- gt tables are printed to the console and can be exported manually using `gtsave()`.
- `SM_logit_table_2008.R` is self-contained and loads the 2008 raw data directly rather than the combined dataset, because the 2008 survey includes a phone/internet interview mode variable not present in other years.
