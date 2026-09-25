library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

rm(list = ls())

combined_data <- read_excel("data/processed/combined_all_cleaned_2.xlsx")
#view(combined_data)

#fill NAs in treatment_group column
combined_data_filtered <- combined_data %>%
  select(-enrichment_stage, -depositional_flux_kg_m2_yr) %>%
  mutate(treatment_group = if_else(is.na(treatment_group), time_point, treatment_group))


# Detect presence/absence columns
presence_absence_cols <- names(combined_data_filtered) %>%
  keep(~ {
    vals <- unique(combined_data_filtered[[.]])
    vals <- na.omit(vals)                  # Remove real missing NA
    vals <- vals[vals != "NA"]              # Remove text "NA"
    all(vals %in% c(0, 1))
  })

# Exclude specific columns
exclude_cols <- c('enrichment', 'flesh_weight_6wks', 'from_histology')
presence_absence_cols <- setdiff(presence_absence_cols, exclude_cols)

# Calculate percentage prevalence for presence/absence columns
numeric_cols <- combined_data_filtered %>%
  select(where(is.numeric)) %>%
  names() %>%
  setdiff(presence_absence_cols)  # Exclude the presence/absence columns

# Calculate group medians, MAD, IQR for numeric columns and percentage prevalence for presence/absence columns
combined_summary <- combined_data_filtered %>%
  filter(!treatment_group %in% c("Field Baseline", 'Post-Acclimation', "Final")) |> 
  group_by(treatment_group, species) %>%
  summarise(
    # For numeric columns, calculate median, MAD, and IQR
    across(
      all_of(numeric_cols),
      list(
        median = ~median(.x, na.rm = TRUE),
        mad = ~mad(.x, na.rm = TRUE),
        iqr = ~IQR(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    
    # For presence/absence columns, calculate percentage prevalence
    across(
      all_of(presence_absence_cols),
      ~ mean(. == 1, na.rm = TRUE) * 100,  # Percentage prevalence
      .names = "{.col}_prevalence"
    ),
    
    .groups = "drop"
  )

combined_summary

folder_path<-"data/processed"
file_name <- "combined_median_prevalence_2.csv"
file_path <- file.path(folder_path, file_name)
write_excel_csv(combined_summary, file_path)


# baseline summaries-------------
# Continuous variables
numeric_summary_baseline <- combined_data_filtered |>
  filter(treatment_group %in% c("Field Baseline", "Post-Acclimation")) |> 
  group_by(species, treatment_group) |>
  summarise(
    across(
      all_of(numeric_cols),
      list(
        median = \(x) median(x, na.rm = TRUE),
        mad    = \(x) mad(x, na.rm = TRUE),
        iqr    = \(x) IQR(x, na.rm = TRUE),
        min    = \(x) min(x, na.rm = TRUE),
        max    = \(x) max(x, na.rm = TRUE),
        n      = \(x) sum(!is.na(x))
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) |>
  clean_names() |>
  pivot_longer(
    cols = -c(species, treatment_group),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(median|mad|iqr|min|max|n)$"
  ) |>
  filter(n > 0) |>
  drop_na(median, mad, iqr, min, max) |>
  mutate(
    across(c(median, mad, iqr, min, max), ~ round(.x, 2))
  ) |>
  arrange(species, treatment_group, variable)

numeric_summary_baseline
write_csv(numeric_summary_baseline, 'tables/numeric_summary_baseline.csv')


sig_vars <- read_csv('tables/GAM_thresholds_combined.csv', show_col_types = F) |>
  distinct(species, variable) 

sig_numeric_summary_baseline <- 
  numeric_summary_baseline |> 
  semi_join(sig_vars, by = c("species", "variable")) |>
  rename(
    Median = median,
    MAD = mad,
    IQR = iqr,
    Min. = min,
    Max. = max
  )

sig_numeric_summary_baseline

  
write_csv(sig_numeric_summary_baseline, 'tables/significant_numeric_summary_baseline.csv')


# Presence/absence variables
prevalence_summary_baseline <- combined_data_filtered |>
  filter(treatment_group %in% c("Field Baseline", 'Post-Acclimation')) |> 
  group_by(species, treatment_group) |>
  summarise(
    across(
      all_of(presence_absence_cols),
      list(
        n_examined = \(x) sum(!is.na(x)),
        n_present  = \(x) sum(x == 1, na.rm = TRUE),
        prevalence = \(x) mean(x == 1, na.rm = TRUE) * 100
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) |>
  clean_names() |>
  pivot_longer(
    cols = -c(species, treatment_group),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(n_examined|n_present|prevalence)$"
  ) |>
  filter(n_examined > 0) |>
  drop_na(prevalence) |>
  arrange(species, variable)

prevalence_summary_baseline

write_csv(prevalence_summary_baseline, 'data/processed/prevalence_summary_baseline.csv')

