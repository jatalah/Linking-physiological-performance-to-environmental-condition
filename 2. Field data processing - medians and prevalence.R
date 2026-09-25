library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

rm(list = ls())

combined_data<- read_excel("data/processed/combined_data_field_cleaned_2.xlsx")
# view(combined_data)

#fill NAs in treatment_group column
# combined_data_filtered<- combined_data%>%
# select(-enrichment_stage, -depositional_flux_kg_m2_yr)%>%
# mutate(
#   treatment_group = if_else(
#     is.na(treatment_group),
#      time_point,
#      treatment_group
#    )
#  )

#remove baseline groups from dataset
combined_data_filtered <- combined_data 

# view(combined_data_filtered)

#remove any columns with no data
combined_data_filtered <- combined_data_filtered %>%
  select(where( ~ !all(is.na(.))))

# Detect presence/absence columns
presence_absence_cols <- names(combined_data_filtered) %>%
  keep(~ {
    vals <- unique(combined_data_filtered[[.]])
    vals <- na.omit(vals)                  # Remove real missing NA
    vals <- vals[vals != "NA"]              # Remove text "NA"
    all(vals %in% c(0, 1))
  })

# Define the list of columns you want to remove
remove_these_cols <- c(
  "c14_1_myristoleic_acid_Gonad",         
  "c14_1_myristoleic_acid_DG",            
  "c15_1_cis_10_pentadecanoic_acid_DG",  
  "c21_0_heneicosanoic_acid_Gonad",       
  "c21_0_heneicosanoic_acid_DG",          
  "c22_1n9_cetoleic_erucic_acid_DG",     
  "c22_2_dicosadienoic_acid_Non-gonadal",
  "c24_1_nervonic_acid_Gonad",            
  "c24_1_nervonic_acid_DG"
)

# Remove these from your presence_absence_cols
presence_absence_cols <- setdiff(presence_absence_cols, remove_these_cols)

# Exclude specific columns
#exclude_cols <- c('enrichment', 'flesh_weight_6wks', 'from_histology')
#presence_absence_cols <- setdiff(presence_absence_cols, exclude_cols)

# Calculate percentage prevalence for presence/absence columns
#prevalence_summary <- combined_data_filtered %>%
# select(all_of(presence_absence_cols)) %>%
# summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100))

# Include 'treatment_group' and 'species' back in the presence_absence_cols
#presence_absence_cols <- c('treatment_group', 'species', presence_absence_cols)

# Detect numeric columns (excluding any presence/absence columns)
numeric_cols <- combined_data_filtered %>%
  select(where(is.numeric)) %>%
  names() %>%
  setdiff(presence_absence_cols)  # Exclude the presence/absence columns

# Calculate group medians, MAD, IQR for numeric columns and percentage prevalence for presence/absence columns
combined_summary <- combined_data_filtered %>%
  filter(!time %in% c("Field Baseline")) |> 
  group_by(site.x, species) %>%
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


# view the combined summary
# view(combined_summary)


folder_path<-"data/processed"
file_name <- "combined_median_prevalence_field_2.xlsx"
file_path <- file.path(folder_path, file_name)
write_xlsx(combined_summary, file_path)


combined_summary_long <- combined_summary %>%
  clean_names() %>%
  pivot_longer(
    cols = matches("_(median|mad|iqr)$"),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(median|mad|iqr)$"
  ) %>%
  drop_na(median, mad, iqr)


# baseline data summaries-----------
# Continuous variables
numeric_summary_baseline <- combined_data_filtered |>
  filter(time == "Field Baseline") |>
  group_by(species) |>
  summarise(
    across(
      all_of(numeric_cols),
      list(
        n      = \(x) sum(!is.na(x)),
        median = \(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE),
        mad    = \(x) if (all(is.na(x))) NA_real_ else mad(x, na.rm = TRUE),
        iqr    = \(x) if (all(is.na(x))) NA_real_ else IQR(x, na.rm = TRUE),
        min    = \(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
        max    = \(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) |>
  clean_names() |>
  pivot_longer(
    cols = -species,
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(n|median|mad|iqr|min|max)$"
  ) |>
  filter(n > 0) |>
  mutate(
    across(c(median, mad, iqr, min, max), ~ round(.x, 2)),
    n = as.integer(n)
  ) |>
  select(species, variable, median, mad, iqr, min, max, n) |>
  arrange(species, variable)

write_csv(numeric_summary_baseline, 'tables/numeric_summary_baseline.csv')


# select significant variables--------
sig_vars <- read_csv('tables/GAM_thresholds_combined.csv') |>
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

sig_numeric_summary_baseline 
  



# Presence/absence variables
prevalence_summary_baseline <- combined_data_filtered |>
  filter(time == "Field Baseline") |>
  group_by(species) |>
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
    cols = -species,
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(n_examined|n_present|prevalence)$"
  ) |>
  filter(n_examined > 0) |>
  drop_na(prevalence) |>
  arrange(species, variable) |> 
  mutate(
    prevalence = round(prevalence, 2),
    across(c(n_examined, n_present), as.integer)
  )


write_csv(prevalence_summary_baseline, 'data/processed/prevalence_summary_baseline.csv')

