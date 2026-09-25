library(tidyverse)
library(readxl)
library(dplyr)
library(mgcv)
library(broom)
library(tibble)
library(purrr)
library(writexl)
library(dplyr)
library(conflicted)
library(janitor)
library(rsample)

conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
)

rm(list = ls())

clean_data <- read_excel("results/transformed_data.xlsx")%>%
  clean_names()

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


model_compare_log <- list()
skipped_vars <- list()  # To track skipped variables and reasons

for (var in response_vars) {
  cat("\n--- Processing:", var, "---\n")
  
  # Filter and prepare data
  temp_data <- clean_data %>%
    filter(variable == var) %>%
    mutate(
      transformed_value = as.numeric(transformed_value),
      species = factor(species, levels = c("horse_mussel", setdiff(unique(species), "horse_mussel")))
    ) %>%
    drop_na(transformed_value, es_macrofauna)
  
  # Log row count
  cat("   Observations after filtering:", nrow(temp_data), "\n")
  
  if (nrow(temp_data) < 8) {
    cat("   ❌ Skipped:", var, "→ too few observations (<8)\n")
    skipped_vars[[var]] <- "Too few observations"
    next
  }
  
  if (n_distinct(temp_data$species) > 1) {
    formula <- transformed_value ~ es_macrofauna * species
  } else {
    formula <- transformed_value ~ es_macrofauna
  }
  
  # Fit LM
  lm_mod <- try(lm(formula, data = temp_data), silent = TRUE)
  
  # Fit Polynomial (quadratic)
  poly_mod <- try(lm(update(formula, . ~ poly(es_macrofauna, 2) * species), data = temp_data), silent = TRUE)
  
  # Calculate AIC (only lm and poly)
  aic_transformed_values <- list(
    lm = if (!inherits(lm_mod, "try-error")) AIC(lm_mod) else NA,
    poly = if (!inherits(poly_mod, "try-error")) AIC(poly_mod) else NA
  )
  
  # Calculate R2 (only lm and poly)
  r2_transformed_values <- list(
    lm = if (!inherits(lm_mod, "try-error")) summary(lm_mod)$adj.r.squared else NA,
    poly = if (!inherits(poly_mod, "try-error")) summary(poly_mod)$adj.r.squared else NA
  )
  
  # Determine best model (lowest AIC) ignoring GAM
  best_model <- names(which.min(unlist(aic_transformed_values)))
  
  # Save summary of the best model
  best_model_obj <- switch(best_model,
                           lm = lm_mod,
                           poly = poly_mod)
  
  # Check if best model failed
  if (inherits(best_model_obj, "try-error")) {
    cat("   ❌ Skipped:", var, "→ best model failed to fit\n")
    skipped_vars[[var]] <- "Best model failed"
    next
  }
  
  # Save model summary
  tidy_result <- try(tidy(best_model_obj), silent = TRUE)
  if (inherits(tidy_result, "try-error")) {
    cat("   ❌ Skipped:", var, "→ summary(tidy) failed\n")
    skipped_vars[[var]] <- "Tidy summary failed"
    next
  }
  
  model_compare_log[[var]] <- list(
    variable = var,
    best_model = best_model,
    aic = aic_transformed_values,
    r2 = r2_transformed_values,
    summary = tidy_result
  )
}


model_comparison_df <- bind_rows(
  lapply(model_compare_log, function(entry) {
    if (inherits(entry$summary, "try-error")) return(NULL)
    
    entry$summary %>%
      mutate(
        variable = entry$variable,
        best_model = entry$best_model,
        aic_lm = entry$aic$lm,
        aic_poly = entry$aic$poly,
        r2_lm = entry$r2$lm,
        r2_poly = entry$r2$poly
      )
  })
)

# Reorder columns
model_comparison_df <- model_comparison_df %>%
  select(variable, best_model, starts_with("aic"), starts_with("r2"), everything())

# Save to Excel
writexl::write_xlsx(model_comparison_df, "results/model_comparison_results.xlsx")

#plotting new selected models########################################
library(ggplot2)
library(dplyr)
library(mgcv)
library(purrr)

# Get variables with valid best models
valid_vars <- model_comparison_df %>%
  filter(!is.na(best_model)) %>%
  pull(variable) %>%
  unique()

# Function to make a single plot
plot_model_fit <- function(var) {
  cat("Plotting:", var, "\n")
  
  # Get best model info
  model_entry <- model_comparison_df %>%
    filter(variable == var, !is.na(best_model)) %>%
    dplyr::slice(1)
  
  if (nrow(model_entry) == 0) return(NULL)
  
  best_model_type <- model_entry$best_model
  
  # Prepare data
  temp_data <- clean_data %>%
    filter(variable == var) %>%
    mutate(
      transformed_value = as.numeric(transformed_value),
      species = factor(species, levels = c("horse_mussel", setdiff(unique(species), "horse_mussel")))
    ) %>%
    drop_na(transformed_value, es_macrofauna)
  
  if (nrow(temp_data) < 8) return(NULL)
  
  # Select formula
  formula <- if (n_distinct(temp_data$species) > 1) {
    transformed_value ~ es_macrofauna * species
  } else {
    transformed_value ~ es_macrofauna
  }
  
  # Fit model
  fitted_model <- tryCatch({
    switch(
      best_model_type,
      lm = lm(formula, data = temp_data),
      poly = lm(update(formula, . ~ poly(es_macrofauna, 2) * species), data = temp_data),
      gam = gam(formula, data = temp_data),
      stop("Unknown model type")
    )
  }, error = function(e) NULL)
  
  if (is.null(fitted_model)) return(NULL)
  
  # Create prediction grid
  grid <- temp_data %>%
    group_by(species) %>%
    summarise(es_macrofauna = seq(min(es_macrofauna), max(es_macrofauna), length.out = 100), .groups = "drop")
  
  grid$transformed_value <- predict(fitted_model, newdata = grid)
  
  # Build plot
  ggplot(temp_data, aes(x = es_macrofauna, y = transformed_value, color = species)) +
    geom_point(alpha = 0.6) +
    geom_line(data = grid, aes(y = transformed_value), linewidth = 1) +
    labs(
      title = var,
      x = "Environmental Stressor (es)",
      y = "Response"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
}

# Generate all plots into a named list
plots <- setNames(map(valid_vars, plot_model_fit), valid_vars)

# Filter out NULLs
plots <- compact(plots)

# Optionally: show one, or combine with patchwork or cowplot
# Example: print first plot
print(plots[[1]])

walk(plots, print)

# Optional: save all plots to file
# walk2(plots, names(plots), ~ggsave(filename = paste0("plots/", .y, ".png"), plot = .x, width = 6, height = 4))

library(patchwork)

# Combine the first 4 plots (for example)
wrap_plots(plots[1:4], ncol = 2)

# Create the plot
combined_plot <- wrap_plots(
  list(
    plots[[5]], plots[[6]], plots[[7]], plots[[8]],
    plots[[11]], plots[[12]], plots[[13]], plots[[14]],
    plots[[15]], plots[[19]], plots[[20]], plots[[21]],
    plots[[22]], plots[[23]], plots[[24]], plots[[25]]
  ),
  ncol = 4
)

# Save to a specific location
ggsave("results/selected_plots_lm_polynomials.png", combined_plot, width = 20, height = 16, dpi = 300)
