dir.create("data/raw", recursive = TRUE)
dir.create("data/processed", recursive = TRUE)
dir.create("scripts")
dir.create("results")
dir.create("tables")
dir.create("figures")

library(tidyverse)
library(readxl)
library(janitor)


setwd("C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Lab data/data/raw")
#Measurement, and treatment data########################################################################################################
# List of your Excel files
files <- c(
  "Laboratory trial info_brachiopods.xlsx",                                    
  "Laboratory trial info_horse mussels.xlsx",                                 
  "Laboratory trial info_scallops.xlsx"      
)


# Combine into one tidy tibble
combined_data <- files %>%
  set_names() %>%  # So we can keep track of which file each row comes from
  map_dfr(read_excel, .id = "source_file")  # .id = file name goes into 'source_file'

combined_data <- combined_data %>%
  clean_names() %>%
  mutate(species = case_when(
    str_detect(source_file, "brachiopods") ~ "brachiopod",
    str_detect(source_file, "horse mussels") ~ "horse_mussel",
    str_detect(source_file, "scallops") ~ "scallop",
    TRUE ~ "unknown"
  ))

view(combined_data)

cleaned_data <- combined_data %>%
  filter(`time_point` != "Time Point")

view(cleaned_data)




#Histology data#########################################################################################################
# Path to the histology file
histology_file <- "Laboratory trial_histology.xlsx"

# Get sheet names
sheet_names <- excel_sheets(histology_file)

# Read and combine all sheets, tagging them by species
histology_raw <- sheet_names %>%
  set_names() %>%
  map_dfr(~ read_excel(histology_file, sheet = .x, col_types = "text"), .id = "species")

# Get the column index of "repro stage histo"
repro_index <- which(names(histology_raw) == "repro stage histo")

# Step 3: Convert all columns to the right of that to numeric
histology_cleaned <- histology_raw %>%
  clean_names() %>%
  mutate(across((repro_index + 1):ncol(.), as.numeric))

combined_with_H<- full_join(cleaned_data, histology_cleaned, by = "animal_id")

combined_with_H <- full_join(
  cleaned_data %>% mutate(from_cleaned = TRUE),
  histology_cleaned %>% mutate(from_histology = TRUE),
  by = "animal_id"
)

view(combined_with_H)

#Fatty acid data#########################################################################################################
fatty_acids <- read_excel("Laboratory trial_raw fatty acids.xlsx")

fatty_acids <- fatty_acids %>%
  select(-IA, -IT, -FLQ) %>%
  mutate(
    Tissue = case_when(
      Species == "Brachiopod" & Tissue == "Whole" ~ "Gonad",
      Species == "Brachiopod" & Tissue == "DG" ~ "Non-gonadal",
      TRUE ~ Tissue
    )
  )


fatty_wide <- fatty_acids %>%
  clean_names() %>%
  pivot_wider(
    id_cols = c(animal_id, time_point, tank_number),
    names_from = tissue,
    values_from = where(is.numeric),
    names_glue = "{.value}_{tissue}"
  )

view(fatty_wide)

#add fatty acid data to table
combined_with_HF <- full_join(combined_with_H, fatty_wide, by = "animal_id")

view(combined_with_HF)

#Respiration data#########################################################################################################

# Set up file and sheet names
respiration_file <- "Laboratory trial_respiration.xlsx"
sheet_names <- excel_sheets(respiration_file)

# Define columns to keep (will be matched safely using any_of)
columns_to_keep <- c(
  "treatment", "chamber", "animal id",  "vo2", "vo2_per_g",
  "temp", "flesh_weight", "flesh_weight_of_bucket"
)

respiration_summary <- sheet_names %>%
  set_names() %>%
  map_dfr(~ {
    df <- read_excel(respiration_file, sheet = .x) %>%
      clean_names() %>%
      mutate(
        species = str_extract(.x, "Scallops|Brachiopods|Horse mussels"),
        weeks = str_extract(.x, "\\d+ weeks")
      ) %>%
      group_by(species, weeks, chamber, treatment) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
      select(any_of(c("species", "weeks",'chamber', "treatment", "animal id", columns_to_keep)))
  })


view(respiration_summary)

#add animal ID to respiration data and then link into main data set later




#Oxidative stress data########################################################################################################

# Set up file and sheet names
OS_file <- "Laboratory trial_oxidative stress.xlsx"
sheet_names <- excel_sheets(OS_file)
sheets_to_use <- c("TAC final", "MDA final R1", "MDA final R2")

# Read and clean each sheet
oxidative_data <- sheets_to_use %>%
  set_names() %>%
  map(~ read_excel(OS_file, sheet = .x) %>%
        clean_names() %>%
        select(-any_of(c("species", "treatment"))) %>%# remove overlapping columns
        group_by(animal_id) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop"))



# Now join each one to combined_all by animal_id
combined_with_HFO <- combined_with_HF

for (sheet_name in names(oxidative_data)) {
  combined_with_HFO <- combined_with_HFO %>%
    full_join(oxidative_data[[sheet_name]], by = "animal_id")
}

view(combined_with_HFO)

#unmatched_all <- map(oxidative_data, ~ anti_join(.x, combined_with_HF, by = "animal_id"))


#adding information on ES and depositional flux##############################################################################################

#adding new column to define treatment groups
combined_with_HFO <- combined_with_HFO %>%
  mutate(
    treatment_group = case_when(
      animal_id %in% c("PN7", "PN9", "PN10", "PN4.1", "BR1", "BR2", "BR3", "BR4", "BR5", "BR6", "BR7", "BR8", "BR9", "BR10") ~ "Field Baseline",
      animal_id == "PN107b" ~ "high",
      time_point.x %in% c("Post-Acclimation", "Field Baseline") ~ time_point.x,
      str_starts(as.character(tank_number.x), "1") ~ "control",
      str_starts(as.character(tank_number.x), "2") ~ "low",
      str_starts(as.character(tank_number.x), "3") ~ "moderate",
      str_starts(as.character(tank_number.x), "4") ~ "high",
      TRUE ~ NA_character_
    )
  )%>%
  select(1:2, treatment_group, everything())

view(combined_with_HFO)

missing_treatment_group <- combined_with_HFO %>%
  filter(is.na(treatment_group))

#defining ES and depositional flux for treatment groups
combined_with_HFOD <- combined_with_HFO %>%
  mutate(
    enrichment_stage = case_when(
      treatment_group == "control" ~ 1.88,
      treatment_group == "low" ~ 3.39,
      treatment_group == "moderate" ~ 5.07,
      treatment_group == "high" ~ 6.19,
      TRUE ~ NA_real_
    ),
    depositional_flux_kg_m2_yr = case_when(
      treatment_group == "control" ~ 0,
      treatment_group == "low" ~ 2.81,
      treatment_group == "moderate" ~ 18.48,
      treatment_group == "high" ~ 40.85,
      TRUE ~ NA_real_
    )
  )%>%
  select(1:8, enrichment_stage, depositional_flux_kg_m2_yr, everything())

view(combined_with_HFOD)

#Respiration data#########################################################################################################

# Set up file and sheet names
respiration_file <- "Laboratory trial_respiration.xlsx"
sheet_names <- excel_sheets(respiration_file)

# Split sheet names based on species
scallop_brachiopod_sheets <- sheet_names[grepl("Scallops|Brachiopods", sheet_names)]
horsemussel_sheets <- sheet_names[grepl("Horse mussels", sheet_names)]

# Define columns to keep (will be matched safely using any_of)
columns_to_keep <- c(
  "treatment", "chamber", "animal id",  "vo2", "vo2_per_g",
  "temp", "flesh_weight", "flesh_weight_of_bucket"
)


process_respiration_summary <- function(sheet, join_by_animal = TRUE) {
  df <- read_excel(respiration_file, sheet = sheet) %>%
    clean_names() %>%
    mutate(
      species = str_extract(sheet, "Scallops|Brachiopods|Horse mussels"),
      weeks = str_extract(sheet, "\\d+ weeks")
    )
  
  if (join_by_animal) {
    df %>%
      group_by(species, weeks, chamber, treatment, animal_id) %>%
      summarise(across(any_of(columns_to_keep), mean, na.rm = TRUE), .groups = "drop")
    
  } else {
    df %>%
      group_by(species, weeks, chamber, treatment) %>%
      summarise(across(any_of(columns_to_keep), mean, na.rm = TRUE), .groups = "drop")%>%      
      rename(tank_number.x = chamber) %>%
      mutate(tank_number.x = tolower(tank_number.x))
  }
}

# Read and process sheets
non_hm_sheets <- c("Scallops 6 weeks", "Scallops 12 weeks", 
                   "Brachiopods 6 weeks", "Brachiopods 12 weeks")

hm_sheets <- c("Horse mussels 6 weeks", "Horse mussels 12 weeks")


# Combine and clean respiration data BEFORE joining
respiration_all <- bind_rows(
  map_dfr(non_hm_sheets, ~ process_respiration_summary(.x, TRUE)),
  map_dfr(horsemussel_sheets, ~ process_respiration_summary(.x, FALSE))
)

# After you've combined respiration_all:
respiration_all_wide <- respiration_all %>%
  mutate(weeks = str_replace(weeks, " weeks", "wks")) %>%  # Make it more concise: "6wks", "12wks"
  pivot_wider(
    names_from = weeks,
    values_from = c(vo2, vo2_per_g, temp, flesh_weight, flesh_weight_of_bucket),
    names_glue = "{.value}_{weeks}"
  )

respiration_animal_wide <- respiration_all_wide %>% filter(!is.na(animal_id))#splits data 
respiration_chamber_wide <- respiration_all_wide %>% filter(is.na(animal_id) & !is.na(tank_number.x))

respiration_animal_wide <- respiration_animal_wide %>%
  select(-tank_number.x)

combined_all <- combined_with_HFOD %>%
  full_join(respiration_animal_wide, by = "animal_id") %>%
  full_join(respiration_chamber_wide, by = "tank_number.x")

view(combined_all)

# Identify all duplicated columns with .x or .y suffix
dup_cols <- names(combined_all)[grepl("\\.x$|\\.y$", names(combined_all))]

# Extract the base names of duplicated columns
dup_bases <- unique(gsub("\\.[xy]$", "", dup_cols))

# For each base name, create a unified column using coalesce (preferring .x over .y)
for (base in dup_bases) {
  col_x <- paste0(base, ".x")
  col_y <- paste0(base, ".y")
  
  combined_all[[base]] <- dplyr::coalesce(
    if (col_x %in% names(combined_all)) combined_all[[col_x]] else NULL,
    if (col_y %in% names(combined_all)) combined_all[[col_y]] else NULL
  )
}

# Remove all the .x and .y columns
combined_all <- combined_all %>%
  select(-all_of(dup_cols))

# Optional: view cleaned column names
# names(combined_all)

# View the final result
View(combined_all)


combined_all <- combined_all %>%
  select(enrichment_stage, depositional_flux_kg_m2_yr, treatment_group,species, chamber,tank_number, time_point, animal_id, treatment, date, analysis_type,animal_id,
         cassette_number, sex_stage, death, sampling_event, sex, notes,length_mm, width_mm, depth_mm,wet_weight_g, shell_weight_g, soft_tissue_weight_g,
         condition_score, everything())


view(combined_all)

combined_all <- combined_all %>%
  select(-("cassette_number"),
         -("treatment"),
         -("date"),
         -("sex_stage"),
         -("sampling_event"),
         -("source_file"),
         -("from_cleaned"))

# Update 'time_point' column: change "T1" to "Field Baseline"
combined_all <- combined_all %>%
  mutate(time_point = ifelse(time_point == "T1", "Field Baseline", time_point))

# Define the mapping of animal_id to time_point
animal_timepoints <- c(
  "PN4.1" = "Field Baseline",
  "PN107b" = "high",
  "PN37" = "low",
  "PN83" = "high",
  "PN26" = "control",
  "PN47" = "low",
  "PN76" = "moderate"
)

# Vector of animal_ids to update
scallop_ids <- names(animal_timepoints)

# Apply updates
combined_all <- combined_all %>%
  mutate(
    time_point = ifelse(animal_id %in% scallop_ids,
                        animal_timepoints[animal_id],
                        time_point),
    species = ifelse(animal_id %in% scallop_ids, "scallop", species)
  )


combined_all <- combined_all %>%
  mutate(
    species = case_when(
      species == "Brachiopods" ~ "brachiopod",
      species == "Scallops" ~ "scallop",
      species == "scallops" ~ "scallop",
      species == "Brachiopod" ~ "brachiopod",
      TRUE ~ species
    )
  )

view(combined_all)

library(writexl)

write_xlsx(combined_all, "combined_all_cleaned.xlsx")

#############################################
#transcriptomics data

# Step 1: Read transcriptomics data with no header

transcript_raw <- read_excel("transcriptomics - selected genes from lab results.xlsx", col_names = TRUE)
transcript_raw<-transcript_raw %>%
  clean_names()
view(transcript_raw)
# Step 2: Ensure animal_id columns are of the same type
combined_all$animal_id <- as.character(combined_all$animal_id)
transcript_raw$animal_id <- as.character(transcript_raw$animal_id)

# Step 3: Join the data using animal_id
combined_all_with_transcriptomics <- left_join(combined_all, transcript_raw, by = "animal_id")

view(combined_all_with_transcriptomics)

library(writexl)

write_xlsx(combined_all_with_transcriptomics, "combined_all_cleaned_2.xlsx")

