# Builds a gt table summarizing SPAE sample demographics (gender, age group,
# education, party, ideology) with both unweighted and survey-weighted
# percentages, grouped by demographic category with one row per level.

library(dplyr)
library(tidyr)
library(gt)

source("config.R")

dir <- paste0(data_dir, "/")
load(paste0(dir, "COMBINED_DATA.RData"))

demo <- combined_data %>%
  mutate(
    birth_year = suppressWarnings(as.numeric(birth_year)),
    age = year - birth_year,
    age_group = case_when(
      age >= 18 & age <= 29 ~ "18–29",
      age >= 30 & age <= 44 ~ "30–44",
      age >= 45 & age <= 64 ~ "45–64",
      age >= 65            ~ "65+",
      TRUE ~ NA_character_
    ),
    gender_clean = case_when(
      toupper(as.character(gender)) %in% c("MALE", "1")   ~ "Male",
      toupper(as.character(gender)) %in% c("FEMALE", "2") ~ "Female",
      TRUE ~ NA_character_
    ),
    educ_clean = case_when(
      toupper(as.character(education)) %in% c("LESS THAN HS", "NO HS", "1")   ~ "No High School",
      toupper(as.character(education)) %in% c("HS GRAD", "HIGH SCHOOL GRADUATE", "2") ~ "High School",
      toupper(as.character(education)) %in% c("SOME COLLEGE", "3")            ~ "Some College",
      toupper(as.character(education)) %in% c("2-YEAR DEGREE", "4")           ~ "Some College",
      toupper(as.character(education)) %in% c("4-YEAR DEGREE", "4-YEAR", "5") ~ "4-year",
      toupper(as.character(education)) %in% c("POSTGRAD", "POST-GRAD", "6")   ~ "Post-grad",
      TRUE ~ NA_character_
    ),
    party_clean = case_when(
      toupper(as.character(party)) == "DEMOCRAT"    ~ "Democrat",
      toupper(as.character(party)) == "INDEPENDENT" ~ "Independent",
      toupper(as.character(party)) == "REPUBLICAN"  ~ "Republican",
      TRUE ~ NA_character_
    ),
    ideo_clean = case_when(
      toupper(as.character(ideology)) %in% c("VERY LIBERAL", "1")      ~ "Very Liberal",
      toupper(as.character(ideology)) %in% c("LIBERAL", "2")           ~ "Liberal",
      toupper(as.character(ideology)) %in% c("MODERATE", "3")          ~ "Moderate",
      toupper(as.character(ideology)) %in% c("CONSERVATIVE", "4")      ~ "Conservative",
      toupper(as.character(ideology)) %in% c("VERY CONSERVATIVE", "5") ~ "Very Conservative",
      TRUE ~ NA_character_
    ),
    weight = suppressWarnings(as.numeric(weight))
  )

# Gets unweighted + weighted % for a single variable
get_pct <- function(df, var, level_order) {
  df %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(level = .data[[var]]) %>%
    summarise(
      n_unw = n(),
      n_w   = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `Unweighted %` = 100 * n_unw / sum(n_unw),
      `Weighted %`   = 100 * n_w   / sum(n_w)
    ) %>%
    select(Feature = level, `Unweighted %`, `Weighted %`) %>%
    mutate(Feature = factor(Feature, levels = level_order)) %>%
    arrange(Feature) %>%
    mutate(Feature = as.character(Feature))
}

gender_tbl <- get_pct(demo, "gender_clean",
                      c("Male", "Female")) %>%
  mutate(section = "Gender")

age_tbl <- get_pct(demo, "age_group",
                   c("18–29", "30–44", "45–64", "65+")) %>%
  mutate(section = "Age Group")

educ_tbl <- get_pct(demo, "educ_clean",
                    c("No High School", "High School", "Some College", "4-year", "Post-grad")) %>%
  mutate(section = "Education")

party_tbl <- get_pct(demo, "party_clean",
                     c("Democrat", "Independent", "Republican")) %>%
  mutate(section = "Party")

ideo_tbl <- get_pct(demo, "ideo_clean",
                    c("Very Liberal", "Liberal", "Moderate", "Conservative", "Very Conservative")) %>%
  mutate(section = "Ideology")

demo_tbl <- bind_rows(gender_tbl, age_tbl, educ_tbl, party_tbl, ideo_tbl) %>%
  select(section, Feature, `Unweighted %`, `Weighted %`)

demo_gt <- demo_tbl %>%
  gt(groupname_col = "section") %>%
  tab_header(title = "Sample Demographics") %>%
  fmt_number(columns = c(`Unweighted %`, `Weighted %`), decimals = 1) %>%
  cols_align(align = "center", columns = c(`Unweighted %`, `Weighted %`)) %>%
  cols_align(align = "center", columns = Feature) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    column_labels.font.weight = "bold",
    row_group.background.color = "#f5f5f5",
    data_row.padding = px(6),
    table.align = "center"
  )

print(demo_gt)
