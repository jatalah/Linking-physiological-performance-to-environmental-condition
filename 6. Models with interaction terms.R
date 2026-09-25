library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(dplyr)
library(ggpubr)
library(ggplot2)
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

clean_data <- read_excel("data/processed/combined_data_long.xlsx")%>%
  clean_names()

#Identify variable types for each variable
variable_model_types <- 
  clean_data %>%
  group_by(variable) %>%
  mutate(n_unique = n_distinct(value)) %>%
  ungroup() %>%
  mutate(
    model_type = case_when(
      n_unique <= 2 ~ "binary",
      TRUE ~ "continuous"
    )
  )

# Group by species and variable, nest data
model_data_nested <- variable_model_types %>%
  group_by(variable, model_type) %>%
  nest()

#Update model fitting to include species interactions
model_data_nested <- model_data_nested %>%
  mutate(
    model = pmap(
      list(data, model_type),
      function(df, type) {
        if (type == "continuous") {
          try(lm(value ~ species * es_macrofauna, data = df), silent = TRUE)
        } else if (type == "binary") {
          try(glm(value ~ species * es_macrofauna, data = df, family = binomial), silent = TRUE)
        } else {
          NULL
        }
      }
    ),
    model_summary = map(model, ~suppressWarnings(try(broom::tidy(.x), silent = TRUE))),
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
        }  else {
          return(NA_real_)
        }
      }
    )
  )

model_data_nested %>%
  # Keep only models where the summary is a data frame (i.e., successful)
  filter(map_lgl(model_summary, ~inherits(.x, "data.frame"))) %>%
  unnest(model_summary)%>%
  write_xlsx("results/interaction model results.xlsx")

################Tranforming response variables###########################
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
)
library(MASS)      # for boxcox
library(ggplot2)   # for plotting
library(dplyr)
library(tidyr)
library(purrr)

# Loop through each variable and transforms if needed (continuous variables only)

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
  "sum_of_n_6_pufa_gonad",
  "c18_3n3_alpha_linolenic_acid_ala_gonad",
  "c20_0_arachidic_acid_non_gonadal",
  "sum_of_mufa_gonad",
  "lc_n_3_gonad",
  "lc_n_6_non_gonadal",
  "sodium_e_calcium_exchanger_slc8a",
)

model_log <- list()
transformed_data_all <- list()

for (var in response_vars) {
  cat("\n--- Processing:", var, "---\n")
  
  # Filter and prepare data
  temp_data <- clean_data %>%
    filter(variable == var) %>%
    mutate(value = as.numeric(value)) %>%
    drop_na(value, es_macrofauna, species)
  
  if (nrow(temp_data) == 0) {
    cat("Skipping:", var, "- no data after filtering\n")
    next
  }
  
  # Shift if needed for Box-Cox
  min_value <- min(temp_data$value, na.rm = TRUE)
  shift <- 0
  if (min_value <= 0) {
    shift <- abs(min_value) + 1e-6
    temp_data <- temp_data %>%
      mutate(value = value + shift)
    cat("Shifted by", shift, "\n")
  }
  
  # Fit initial model to get Box-Cox
  if (n_distinct(temp_data$species) < 2) {
    cat("Only one species present - fitting model without interaction\n")
    lm_model <- lm(value ~ es_macrofauna, data = temp_data)
    model_type <- "es only"
  } else {
    lm_model <- lm(value ~ species * es_macrofauna, data = temp_data)
    model_type <- "interaction"
  }
  
  # Apply Box-Cox
  boxcox_result <- boxcox(lm_model, lambda = seq(-2, 2, 0.1))
  lambda_best <- boxcox_result$x[which.max(boxcox_result$y)]
  cat("Best lambda:", lambda_best, "\n")
  
  # Transform response
  temp_data <- temp_data %>%
    mutate(transformed_value = if (abs(lambda_best) < 0.1) {
      log(value)
    } else {
      (value^lambda_best - 1) / lambda_best
    },
    variable = var,
    shift = shift,
    lambda = lambda_best)
  
  # Save the transformed dataset for later export
  transformed_data_all[[var]] <- temp_data
  
  # Fit transformed model
  if (model_type == "es only") {
    lm_transformed <- lm(transformed_value ~ es_macrofauna, data = temp_data)
  } else {
    lm_transformed <- lm(transformed_value ~ species * es_macrofauna, data = temp_data)
  }
  
  
  # Combine all into one data frame
  transformed_data_df <- bind_rows(transformed_data_all)
  
  # Save tranfermed data to Excel
  write_xlsx(transformed_data_df, "results/transformed_data.xlsx")
  
  # Optional: Save model info for further inspection
  model_log[[var]] <- list(
    model_type = model_type,
    lambda = lambda_best,
    summary = summary(lm_transformed),
    r2 = summary(lm_transformed)$r.squared
  )
}

model_log[["cathepsin_d"]]$summary
model_log[["cathepsin_d"]]$model_type

# Create a results data frame
model_results_df <- bind_rows(
  lapply(names(model_log), function(var) {
    model_info <- model_log[[var]]
    tidy_df <- try(tidy(model_info$summary), silent = TRUE)
    
    if (inherits(tidy_df, "try-error")) return(NULL)
    
    tidy_df %>%
      mutate(
        variable = var,
        lambda = model_info$lambda,
        model_type = model_info$model_type,
        r_squared = model_info$r2  # <- include R2 here
      )
  }),
  .id = NULL
)

# Reorder columns
model_results_df <- model_results_df %>%
  select(variable, model_type, lambda, r_squared, everything())

# Write to Excel
write_xlsx(model_results_df, path = "results/transformed updated models.xlsx")



