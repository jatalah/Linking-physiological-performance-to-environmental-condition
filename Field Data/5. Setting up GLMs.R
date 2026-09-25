library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(ggpubr)
library(broom)
library(writexl)
library(ordinal)
library(MASS)         # for polr
library(broom.mixed)  # for mixed model support, optional
library(conflicted)

conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
)

rm(list = ls()) 

combined_data <- read_excel("data/processed/Combined_data_field_cleaned_2.xlsx")%>%
  clean_names()

#view(combined_data)

#fill NAs in treatment_group column
combined_data_filtered <-
  combined_data %>%
  mutate(site_x = if_else(is.na(site_x), time, site_x))

#remove baseline groups from dataset
combined_data_filtered <- 
  combined_data_filtered %>%
  filter(!site_x %in% c("Field Baseline"))

#filtering dataset to include preferred variable subset only
vars_to_keep <- c("site_x", "species","condition",
                  "c18_1n9c_oleic_acid_dg",
                  "fat_percent_dg",
                  "fat_percent_gonad",
                  "sum_of_n_6_pufa_dg",
                  "sum_of_sfa_dg",
                  "mantle_epithilial_elo",
                  "fatty_acid_synthase_fas",
                  "sodium_dependent_multivitamin_transporter_slc5a6",
                  "sum_of_pufa_gonad",
                  "gut_vacuolisation",
                  "cytochrome_p450_cyp_family_1_subfamily_a1",
                  "growth_arrest_and_dna_damage_inducible_protein_gadd45",
                  "gut_f_he",
                  "cathepsin_d",
                  "glutathione_reductase",
                  "hspa5_bi_p",
                  "sum_of_pufa_dg",
                  "sum_of_sfa_gonad",
                  "gut_bc",
                  "copper_transporter_slc31a1",
                  "coup_transcription_factor_1",
                  "c20_4n6_arachidonic_acid_aa_gonad",
                  "c22_5n3_docosapentaenoic_acid_dpa_gonad",
                  "dg_and_gi_elo",
                  "dg_atrophy",
                  "tubulin_alpha_tuba",
                  "c18_0_stearic_acid_gonad",
                  "sodium_hydrogen_exchanger_slc9a10",
                  "gill_elo_t1",
                  "donson_protein_downstream_neighbour_of_son",
                  "pcna_proliferating_cell_nuclear_antigen",
                  "fat_percent_non_gonadal",
                  "c18_1n9c_oleic_acid_gonad",
                  "sum_of_mufa_gonad",
                  "sum_of_n_3_pufa_non_gonadal",
                  "sum_of_pufa_non_gonadal",
                  "sum_of_n_3_pufa_gonad",
                  "sum_of_n_6_pufa_gonad" 
)

# Filter your dataset to retain only those columns
combined_data_filtered <- combined_data_filtered %>%
  select(all_of(vars_to_keep))

combined_data_filtered <- combined_data_filtered %>%
  mutate(site_x = ifelse(site_x == "300M", "300", site_x))

# Create a lookup table with unique values per site
env_lookup <- data.frame(
  site_x = c("300", "Pen", "Ctl-1", "Ctl-3"),
  'depositional_flux (macrofauna)' = c(0.33, 6.18, 0.07, 0.02),
  'es (macrofauna)' = c(2.52, 3.96, 2.12, 1.89),
  'depositional_flux (molecular)' = c(0.89, 5.49, 0.14, 0.07),
  'es (molecular)' = c(2.89, 3.92, 2.28, 2.16)
)

# Join and reposition new columns
combined_data_filtered <- combined_data_filtered %>%
  left_join(env_lookup, by = "site_x") %>%
  relocate('depositional_flux..macrofauna.', 'es..macrofauna.', 'depositional_flux..molecular.', 'es..molecular.', .after = species)

#view(combined_data_filtered)

#converting data to long format######################################################
#replace text in condition_score column with NA
#combined_data_filtered <- combined_data_filtered %>%
#  mutate(condition_score = ifelse(grepl("^[0-9.]+$", condition_score), 
#                                  as.numeric(condition_score), 
#                                  NA))

#removed column repro_stage_histo
#combined_data_filtered <- combined_data_filtered %>%
# select(-repro_stage_histo)

combined_data_filtered[combined_data_filtered == "NA"] <- NA

combined_data_long <- combined_data_filtered %>%
  # Convert all specified columns to numeric
  mutate(across(condition:sum_of_n_6_pufa_gonad, ~as.numeric(as.character(.)))) %>%
  # Pivot longer
  pivot_longer(
    cols = condition:sum_of_n_6_pufa_gonad,
    names_to = "variable",
    values_to = "value"
  )


#setting up multiple models by species and variable##########################################
#x <- 
#combined_data_long %>% 
# drop_na(value) %>% 
#group_by(species, variable) %>%
#nest() %>%
#mutate(
# scatter_plots = map(data, ~ggplot(.x, aes(depositional_flux , value)) + geom_point()))

binary_vars<-combined_data_long %>%
  group_by(variable) %>%
  summarise(n_unique = n_distinct(value)) %>%
  filter(n_unique == 3) %>%
  pull(variable)


#scatter plots for continuous variables###########
plot_data <- combined_data_long %>%
  filter(!variable %in% binary_vars) %>%
  drop_na(value) %>%
  group_by(variable, species) %>%
  mutate(n_x = n_distinct(es..macrofauna.)) %>%
  ungroup()

x<- ggplot(plot_data, aes(x = es..macrofauna., y = value, color = species)) +
  geom_point(alpha = 0.7) +
  # Use lm if too few x-points for loess
  geom_smooth(data = filter(plot_data, n_x >= 4), method = "loess", se = FALSE, linewidth = 0.8, span = 0.5) +
  geom_smooth(data = filter(plot_data, n_x < 4), method = "lm", se = FALSE, linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "ES (macrofauna)", y = "Response")

plot_data <- combined_data_long %>%
  filter(!variable %in% binary_vars) %>%
  drop_na(value) %>%
  group_by(variable, species) %>%
  mutate(n_x = n_distinct(es..molecular.)) %>%
  ungroup()

y<- ggplot(plot_data, aes(x = es..molecular., y = value, color = species)) +
  geom_point(alpha = 0.7) +
  # Use lm if too few x-points for loess
  geom_smooth(data = filter(plot_data, n_x >= 4), method = "loess", se = FALSE, linewidth = 0.8, span = 0.5) +
  geom_smooth(data = filter(plot_data, n_x < 4), method = "lm", se = FALSE, linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "ES (molecular)", y = "Response")

# Save the plots
ggsave(plot = x, width = 8, height = 5, dpi = 300, bg = 'white', "results/ES (macrofauna) continuous var scatter plots.png")
ggsave(plot = x, width = 8, height = 5, dpi = 300, bg = 'white', "results/ES (molecular) continuous var scatter plots.png")

#proportions of jitter dots for binary variables###########
z<-combined_data_long %>%
  filter(variable %in% binary_vars) %>%
  mutate(value = as.numeric(value)) %>%  # Ensure it's 0/1
  drop_na(value) %>%
  ggplot(aes(x = es..macrofauna., y = value, color = species)) +
  geom_jitter(height = 0.1, width = 0, alpha = 0.5) +  # Jitter to avoid overplotting
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, linewidth = 0.8) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "ES (macrofauna)", y = "Probability (Predicted Value 1)")

# Save the plot
ggsave(plot = z, width = 8, height = 5,bg = 'white', dpi = 300, "results/ES (macrofauna) binary var jitter dot plots.png")

a<-combined_data_long %>%
  filter(variable %in% binary_vars) %>%
  mutate(value = as.numeric(value)) %>%
  drop_na(value) %>%
  ggplot(aes(x = es..molecular., y = value, color = species)) +
  geom_jitter(height = 0.1, width = 0, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, linewidth = 0.8) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "ES (molecular)", y = "Probability (Predicted Value 1)")

# Save the plot
ggsave(plot = a, width = 8, height = 5, dpi = 300,bg = 'white', "results/ES (molecular) binary var jitter dot plots.png")


#ggarrange(plotlist = x$scatter_plots)
#x <- combined_data_long %>% 
#  drop_na(value) %>% 
#  group_by(species, variable) %>%
#  nest() %>%
#  mutate(  
#glm_model = map(data, species ~ glm(condition_score ~ es + depositional_flux, data = ., family = gaussian())))

#models###########
# Filter to continuous variables only
continuous_data <- combined_data_long %>%
  filter(!(variable %in% binary_vars)) %>%
  drop_na(value)

#Nest data by species and variable
model_data <- continuous_data %>%
  group_by(species, variable) %>%
  nest()

#Fit a linear model for each group (ES macrofauna)
model_data <- model_data %>%
  mutate(model = map(data, ~lm(value ~ es..macrofauna., data = .x)))

#Extract model summaries
model_summaries <- model_data %>%
  mutate(tidy_output = map(model, tidy)) %>%
  unnest(tidy_output)

#Filter significant effects
significant_results <- model_summaries %>%
  filter(p.value < 0.10)


#save table to excel spreadsheet
# Define the file path 
write_xlsx(model_summaries, "results/LM model summaries (ES macrofauna).xlsx")

#Fit a linear model for each group (ES molecular)
model_data <- model_data %>%
  mutate(model = map(data, ~lm(value ~ es..molecular., data = .x)))

#Extract model summaries
model_summaries <- model_data %>%
  mutate(tidy_output = map(model, tidy)) %>%
  unnest(tidy_output)

#Filter significant effects
significant_results <- model_summaries %>%
  filter(p.value < 0.10)


#save table to excel spreadsheet
# Define the file path 
write_xlsx(model_summaries, "results/LM model summaries (ES molecular).xlsx")

#automatically choosing the model based on variable type (continuous, binary, ordinal)######################

#Identify variable types for each variable
clean_data_es_macro <- combined_data_long %>%
  filter(!is.na(value) & !is.na(es..macrofauna.) & !is.na(es..macrofauna.))

library(writexl)
write_xlsx(clean_data, "data/processed/cleaned_data (ES macrofauna) in long format.xlsx")

variable_model_types <- 
  clean_data %>%
  group_by(variable) %>%
  mutate(n_unique = n_distinct(value)) %>%
  ungroup() %>%
  mutate(
    model_type = case_when(
      n_unique <= 3 ~ "binary",
      TRUE ~ "continuous"
    )
  )


# Join variable model types to the long-format data
#model_data <- clean_data %>%
#  left_join(variable_model_types %>% select(variable, model_type), by = "variable") %>%
# drop_na(value)

# Group by species and variable, nest data
model_data_nested <- variable_model_types %>%
  group_by(species, variable, model_type) %>%
  nest()

library(pscl)       # for pseudo-R²
library(tibble)
#fit model based on type
model_data_nested <- model_data_nested %>%
  mutate(
    model = pmap(
      list(data, model_type),
      function(df, type) {
        if (type == "continuous") {
          try(lm(value ~ es..macrofauna., data = df), silent = TRUE)
        } else if (type == "binary") {
          try(glm(value ~ es..macrofauna., data = df, family = binomial), silent = TRUE)
        } else {
          NULL
        }
      }
    ),
    model_summary = map(model, ~suppressWarnings(try(tidy(.x), silent = TRUE))),
    
    r2 = pmap_dbl(
      list(model, model_type),
      function(mod, type) {
        if (inherits(mod, "try-error") || is.null(mod)) {
          return(NA_real_)
        } else if (type == "continuous") {
          return(summary(mod)$r.squared)
        } else if (type == "binary") {
          r2s <- try(pscl::pR2(mod), silent = TRUE)
          if (inherits(r2s, "try-error")) return(NA_real_)
          return(r2s["McFadden"])
        } else {
          return(NA_real_)  # Drop "ordinal" and all else
        }
      }
    )
  )

model_data_nested %>%
  # Keep only models where the summary is a data frame (i.e., successful)
  filter(map_lgl(model_summary, ~inherits(.x, "data.frame"))) %>%
  unnest(model_summary)%>%
  write_xlsx("results/all_model_results_ES macrofauna.xlsx")

#testing transformations for response variables#########################################################
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
)
library(MASS)      # for boxcox
library(ggplot2)   # for plotting
library(dplyr)
library(tidyr)
library(purrr)

# Loop through each variable (this one works)

response_vars <- c(
  "condition",
  "c18_1n9c_oleic_acid_dg",
  "fat_percent_dg",
  "fat_percent_gonad",
  "sum_of_n_6_pufa_dg",
  "sum_of_sfa_dg",
  "fatty_acid_synthase_fas",
  "sodium_dependent_multivitamin_transporter_slc5a6",
  "sum_of_pufa_gonad",
  "cytochrome_p450_cyp_family_1_subfamily_a1",
  "growth_arrest_and_dna_damage_inducible_protein_gadd45",
  "cathepsin_d",
  "glutathione_reductase",
  "hspa5_bi_p",
  "sum_of_pufa_dg",
  "sum_of_sfa_gonad",
  "copper_transporter_slc31a1",
  "coup_transcription_factor_1",
  "c20_4n6_arachidonic_acid_aa_gonad",
  "c22_5n3_docosapentaenoic_acid_dpa_gonad",
  "tubulin_alpha_tuba",
  "c18_0_stearic_acid_gonad",
  "sodium_hydrogen_exchanger_slc9a10",
  "donson_protein_downstream_neighbour_of_son",
  "pcna_proliferating_cell_nuclear_antigen",
  "fat_percent_non_gonadal",
  "c18_1n9c_oleic_acid_gonad",
  "sum_of_mufa_gonad",
  "sum_of_n_3_pufa_non_gonadal",
  "sum_of_pufa_non_gonadal",
  "sum_of_n_3_pufa_gonad",
  "sum_of_n_6_pufa_gonad"
)

for (var in response_vars) {
  cat("\n--- Processing:", var, "---\n")
  
  # Filter and prepare data
  temp_data <- combined_data_long %>%
    filter(variable == var) %>%
    mutate(value = as.numeric(value)) %>%
    drop_na(value, es..macrofauna.)
  
  # Shift values if needed
  min_value <- min(temp_data$value, na.rm = TRUE)
  shift <- 0
  if (min_value <= 0) {
    shift <- abs(min_value) + 1e-6
    temp_data <- temp_data %>%
      mutate(value = value + shift)
    cat("Shifted by", shift, "\n")
  }
  
  # Fit linear model
  lm_model <- lm(value ~ es..macrofauna., data = temp_data)
  
  # Apply Box-Cox
  boxcox_result <- boxcox(lm_model, lambda = seq(-2, 2, 0.1))
  
  # Find optimal lambda
  lambda_best <- boxcox_result$x[which.max(boxcox_result$y)]
  cat("Best lambda:", lambda_best, "\n")
  
  # Transform the response variable
  temp_data <- temp_data %>%
    mutate(transformed_value = if (abs(lambda_best) < 0.1) {
      log(value)
    } else {
      (value^lambda_best - 1) / lambda_best
    })
  
  # Refit model with transformed response
  lm_transformed <- lm(transformed_value ~ es..macrofauna., data = temp_data)
  
  # Print summary
  print(summary(lm_transformed))
}
