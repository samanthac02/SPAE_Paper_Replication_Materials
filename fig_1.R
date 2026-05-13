# Builds a gt table showing which election-fraud questions appeared in each SPAE 
# survey year (2008–2024), with rows for each fraud type and checkmarks marking 
# the years it was asked.

library(dplyr)
library(gt)

fraud_questions <- data.frame(
  question = c(
    "<strong>Multiple Voting</strong> - People voting more than once in an election",
    "<strong>Ballot Tampering</strong> - People stealing or tampering with ballots that have been voted",
    "<strong>Voter Impersonation</strong> - People pretending to be someone else when going to vote",
    "<strong>Non-Citizen Voting</strong> - People voting who are not U.S. citizens",
    "<strong>Mail Ballot Fraud</strong> - People voting an absentee ballot intended for another person",
    "<strong>Official Tampering</strong> - Officials changing the reported vote count in a way that is not a true reflection of the ballots that were actually counted",
    "<strong>Voting Software</strong> - Nationwide computer hacking in the administration of elections in the most recent election year",
    "<strong>Voting Software</strong> - Local computer hacking in the administration of elections in the most recent election year",
    "<strong>Voting Software</strong> - Vote counting software manipulated in a way to not count ballots as intended",
    "<strong>Paid to Vote</strong> - Paying voters to cast a ballot for a particular candidate",
    "<strong>Fraudulent Registration</strong> - Voting under fraudulent voter registrations that use a fake name and fake address",
    "<strong>Multiple Voting</strong> - People submitting too many ballots in drop boxes on behalf of others"
  ),
  years_asked = I(list(
    c(2008, 2012, 2014, 2016, 2020, 2022, 2024),
    c(2008, 2012, 2014, 2016, 2020, 2022, 2024),
    c(2008, 2012, 2014, 2016, 2020, 2022, 2024),
    c(2012, 2014, 2016, 2020, 2022, 2024),
    c(2012, 2014, 2016, 2020, 2022, 2024),
    c(2012, 2014, 2016, 2020, 2022, 2024),
    c(2016, 2020),
    c(2016, 2020),
    c(2022, 2024),
    c(2022, 2024),
    c(2022, 2024),
    c(2022, 2024)
  ))
)

# Transform data to have years as columns
all_years <- c(2008, 2012, 2014, 2016, 2020, 2022, 2024)

# Create a matrix with checkmarks
checkbox_data <- fraud_questions %>%
  rowwise() %>%
  mutate(
    `2008` = ifelse(2008 %in% years_asked, "✓", ""),
    `2012` = ifelse(2012 %in% years_asked, "✓", ""),
    `2014` = ifelse(2014 %in% years_asked, "✓", ""),
    `2016` = ifelse(2016 %in% years_asked, "✓", ""),
    `2020` = ifelse(2020 %in% years_asked, "✓", ""),
    `2022` = ifelse(2022 %in% years_asked, "✓", ""),
    `2024` = ifelse(2024 %in% years_asked, "✓", "")
  ) %>%
  select(-years_asked) %>%
  ungroup()

print(checkbox_data %>%
  gt() %>%
  tab_header(
    title = "Election Fraud Frequency Questions",
    subtitle = "Years each question was asked in SPAE surveys (✓ = asked)"
  ) %>%
  cols_label(
    question = "Question"
  ) %>%
  cols_width(
    question ~ px(400),
    everything() ~ px(60)
  ) %>%
  opt_row_striping() %>%
  tab_style(
    style = cell_text(
      align = "center",
      size = px(16),
      weight = "bold"
    ),
    locations = cells_body(columns = -question)
  ) %>%
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(columns = question)
  ) %>%
  tab_style(
    style = cell_text(
      align = "center",
      weight = "bold"
    ),
    locations = cells_column_labels(columns = -question)
  ) %>%
  fmt_markdown(columns = question))