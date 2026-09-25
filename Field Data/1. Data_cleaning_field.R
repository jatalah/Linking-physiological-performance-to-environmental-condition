library(tidyverse)
library(readxl)
library(janitor)
setwd("C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Field data/data/raw")
#Measurement, and treatment data########################################################################################################
# Set the file path
dissected_file <- "Field trial info_deployments.xlsx"
retrieval_file <- "Field trial_retrieval.xlsx"

# Read deployment data
horse_dissected <- read_excel(dissected_file, sheet = "HorseMussels_Dissected")
brachiopod_dissected <- read_excel(dissected_file, sheet = "Brachiopods_Dissected")
combined_data <- bind_rows(horse_dissected, brachiopod_dissected)

# Load the new retrieval data
horse_retrieval <- read_excel(retrieval_file, sheet = "Horse mussel")
brachiopod_retrieval <- read_excel(retrieval_file, sheet = "Brachiopod")

# Combine the new retrieval data
retrieval_data <- bind_rows(horse_retrieval, brachiopod_retrieval)

# Now join all the data together (this will align by column names and fill in NAs)
combined_data2 <- bind_rows(combined_data, retrieval_data)%>%
  clean_names() %>%
  mutate(animal_id = if_else(animal_id == "A24", "H24", animal_id))

# View or export
#View(combined_data2)

#Add condition info####################################################
# Path to the condition file
condition_data <- read_excel("Field trial info_condition_retrieval.xlsx") %>%
  clean_names() %>%
  mutate(animal_id = if_else(animal_id == "A24", "H24", animal_id))

# Select only the relevant columns for the join
condition_subset <- condition_data %>%
  select(animal_id, tissue_wet_weight, condition)

# join the data
combined_data3 <- combined_data2 %>%
  full_join(condition_subset, by = "animal_id")
#view(combined_data3)


#Add fatty acid info######################################################
fatty_acids <- read_excel("Field trial_fatty acids.xlsx")%>%
  clean_names()

fatty_acids <- fatty_acids %>%
  select(-ia, -it, -flq) %>%
  mutate(
    tissue = case_when(
      species == "Brachiopod" & tissue == "Whole" ~ "Gonad",
      species == "Brachiopod" & tissue == "DG" ~ "Non-gonadal",
      TRUE ~ tissue
    )
  )



fatty_wide <- fatty_acids %>%
  clean_names() %>%
  pivot_wider(
    id_cols = c(animal_id, site, species),
    names_from = tissue,
    values_from = where(is.numeric),
    names_glue = "{.value}_{tissue}"
  )

#view(fatty_wide)

#add fatty acid data to table
combined_data4 <- full_join(combined_data3, fatty_wide, by = "animal_id")

#view(combined_data4)

#Add histology info######################################################
# Load and clean column names for 'Horse mussels'
hm_histology <- read_excel("Field trial_histology.xlsx", sheet = "Horse mussels") %>%
  clean_names()

# Select columns from 'sex' to 'f-_he_level'
hm_histology_subset <- hm_histology %>%
  select(animal_id, sex:f_he_level)

# Load and clean column names for 'Brachiopods'
brachiopod_histology <- read_excel("Field trial_histology.xlsx", sheet = "Brachiopods") %>%
  clean_names()

# Select columns from 'sex' to 'dg_bactrial_cyst'
brachiopod_histology_subset <- brachiopod_histology %>%
  select(animal_id, sex:dg_bactrial_cyst)

# Combine histology data from both species
histology_combined <- bind_rows(hm_histology_subset, brachiopod_histology_subset)

# Join with your combined data
combined_data_final <- combined_data4 %>%
  full_join(histology_combined, by = "animal_id")

# View or export
#View(combined_data_final)


#Further grooming#############################################################

# Identify all duplicated columns with .x or .y suffix
dup_cols <- names(combined_data_final)[grepl("\\.x$|\\.y$", names(combined_data_final))]

# Extract the base names of duplicated columns
dup_bases <- unique(gsub("\\.[xy]$", "", dup_cols))

# For each base name, create a unified column using coalesce (preferring .x over .y)
for (base in dup_bases) {
  col_x <- paste0(base, ".x")
  col_y <- paste0(base, ".y")
  
  combined_data_final[[base]] <- dplyr::coalesce(
    if (col_x %in% names(combined_data_final)) combined_data_final[[col_x]] else NULL,
    if (col_y %in% names(combined_data_final)) combined_data_final[[col_y]] else NULL
  )
}

# Remove all the .x and .y columns
combined_data_final <- combined_data_final %>%
  select(-all_of(dup_cols))

# Optional: view cleaned column names
 #names(combined_data_final)

# View the final result
#View(combined_data_final)


#delete unwanted columns
combined_data_final <- combined_data_final %>%
  select(-("cassette_id"),
         -("fate"),
         -("notes"),
         -("mol"),
         -("fa"),
         -("os"),
         -("notes_2"),
         -("mol_os_fa"),
         -("weight_in_field_g"),
         -("tissue_wet_weight"),
         -("cassette_number"))

#merge shell weight columns into one
combined_data_final <- combined_data_final %>%
  mutate(shell_weight_g = coalesce(shell_weight_g, shell_weight_g_2)) %>%
  select(-shell_weight_g_2)  # removes the second column


#select order of columns
combined_data_final <- combined_data_final %>%
  select(site, species, animal_id, time, sex, everything())

View(combined_data_final)


#############################################
#transcriptomics data

# Step 1: Read transcriptomics data with no header

transcript_raw <- read_excel("transcriptomics - selected genes from field results.xlsx", col_names = TRUE)
transcript_raw<-transcript_raw %>%
  clean_names()
view(transcript_raw)
# Step 2: Ensure animal_id columns are of the same type
combined_data_final$animal_id <- as.character(combined_data_final$animal_id)
transcript_raw$animal_id <- as.character(transcript_raw$animal_id)

# Step 3: Join the data using animal_id
combined_all_with_transcriptomics <- left_join(combined_data_final, transcript_raw, by = "animal_id")

view(combined_all_with_transcriptomics)

library(writexl)

write_xlsx(combined_all_with_transcriptomics, path = "C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Field data/data/processed/Combined_data_field_cleaned_2.xlsx")

#############################################
