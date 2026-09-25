library(tidyverse)
library(readxl)
library(mgcv)
library(gratia)
library(tidyverse)
library(broom)
library(ggpubr)
library(patchwork)
rm(list = ls())

d <- read_excel('data/processed/transformed_data.xlsx') |> 
  rename(es = es_macrofauna)

source('clean_labels_function.R')

# Create GAM models and analysis objects
gams <-
  d %>%
  filter(!str_detect(variable, 'p_a') & !str_detect(variable, 'dev_score')) %>%
  group_by(species, variable) %>%
  nest() %>%
  mutate(
    # Fit GAM models
    gam_model = map(data, ~ gam(value ~ s(es, k = 3), data = .x)),
    
    # Get summary statistics
    gam_summaries = map(gam_model, ~ summary(.x)),
    
    # Extract p-value for the smooth term
    p_value = map_dbl(gam_summaries, ~ .x$s.table[4]),
    
    # Format p-values for display
    p_value_formatted = map_chr(p_value, ~ scales::pvalue(.x, accuracy = 0.001, add_p = TRUE)),
    
    # Check if model is significant at alpha = 0.05
    is_significant = p_value < 0.05,
    
    # Calculate deviance explained percentage
    dev_explained = map_dbl(gam_summaries, ~ .x$dev.expl * 100),
    
    # get edf 
    edf = map_dbl(gam_summaries, ~ .x$s.table[1, "edf"]),
    
    # Get smooth estimates for plotting
    gam_smooth_est = map2(
      .x = gam_model,
      .y = data,
      ~ smooth_estimates(.x) %>% add_confint()
    ),
    
    # Calculate derivatives to check for significant changes
    derivatives = map2(
      .x = gam_model,
      .y = data,
      ~ derivatives(.x, type = "central") %>%
        mutate(significant = if_else(.lower_ci > 0 | .upper_ci < 0, TRUE, FALSE))
    ),
    
    # Check if model has any significant regions in the smooth
    has_significant_regions = map_lgl(derivatives, ~ any(.x$significant)),
    
    # Get partial residuals
    gam_res = map2(
      .x = data,
      .y = gam_model,
      ~ .x %>% add_partial_residuals(.y) %>% select(es, `s(es)`)),
    
    # Combine smooth estimates with derivative significance
    smooth_deriv = map2(
      .x = gam_smooth_est, 
      .y = derivatives, 
      ~ bind_cols(.x, .y %>% select(significant))
    ),
    
    # Create plots for each model with significance indication
    es_plots = pmap(
      list(smooth_deriv, gam_res, variable, species, is_significant, dev_explained, p_value_formatted),
      ~ ggplot(..1 %>% filter(.smooth == "s(es)")) +
        geom_line(
          aes(
            x = es,
            y = .estimate,
            colour = significant,
            group = 1
          ),
          linewidth = 1
        ) +
        geom_ribbon(
          aes(ymin = .lower_ci, ymax = .upper_ci, x = es), 
          alpha = 0.2
        ) +
        geom_point(
          data = ..2,
          aes(x = es, y = `s(es)`),
          fill = "gray50",
          color = 'gray20',
          size = 1,
          pch = 21,
          position = position_jitter(width = 0.2),
          alpha = .7
        ) +
        theme_minimal(base_size = 8) +
        theme(
          legend.position = 'none',
          plot.title = element_text(size = 8),
          plot.subtitle = element_text(size = 7)
        ) +
        labs(
          x = "ES", 
          y = NULL,
          title = clean_labels(..3), 
          parse = TRUE,
          subtitle = paste0("Dev: ", round(..6, 1), "%, ", ..7)
        )
    )
  )


gams %>%
  ungroup() %>%
  mutate(
    p_fdr = p.adjust(p_value, method = "BH"),
    significant_fdr = p_fdr < 0.05
  ) |> 
  summarise(
    n_models = n(),
    significant_raw = sum(p_value < 0.05, na.rm = TRUE),
    significant_fdr = sum(p_fdr < 0.05, na.rm = TRUE)
  )

# Extract only significant models
significant_gams <- 
  gams %>%
  filter(is_significant == TRUE)

# Print summary of significant models
cat("Number of significant GAM models:", nrow(significant_gams), "out of", nrow(gams), "\n\n")

# combine plots and save----
desired_order_p1 <- c(
  "condition",
  "fat_percent_dg",
  "sum_of_sfa_dg",
  "sum_of_mufa_gonad",
  "sum_of_n_6_pufa_gonad",
  "sum_of_n_6_pufa_dg",
  "c18_1n9c_oleic_acid_dg",
  "c18_1n9c_oleic_acid_gonad",
  "sodium_dependent_multivitamin_transporter_slc5a6",
  "growth_arrest_and_dna_damage_inducible_protein_gadd45"
)

p1 <- wrap_plots(
  plotlist = significant_gams |>
    filter(species ==  'Horse mussel') |>
    mutate(variable = factor(variable, levels = desired_order_p1)) |>
    arrange(variable) |>
    pull(es_plots),
  nrow = 3
) + plot_annotation(title = "A.")

desired_order_p2 <- c(
  "fat_percent_gonad",
  "sum_of_mufa_gonad",
  "sum_of_pufa_non_gonadal",
  "sum_of_n_6_pufa_gonad",
  "sum_of_n_3_pufa_non_gonadal",
  "c18_1n9c_oleic_acid_gonad",
  "c22_5n3_docosapentaenoic_acid_dpa_gonad",
  "c18_3n3_alpha_linolenic_acid_ala_gonad"
)

p2 <- wrap_plots(
  plotlist = significant_gams |>
    filter(species ==  'Brachiopod') |>
    mutate(variable = factor(variable, levels = desired_order_p2)) |>
    arrange(variable) |>
    pull(es_plots),
  nrow = 2
) + plot_annotation(title = "B.")


ggsave(plot = p1,  "figures/GAMs_pdp_plots_horse_mussel.svg",
       width = 8, height = 6, dpi = 300, bg = "white")

ggsave(plot = p2,  "figures/GAMs_pdp_plots_brachiopod.svg",
       width = 8, height = 4, dpi = 300, bg = "white")



# Print significant models by species and variable
significant_summary <- 
  significant_gams %>%
  select(species, variable, p_value, p_value_formatted, dev_explained) %>%
  arrange(species, p_value, p_value_formatted)

significant_summary |>
  ungroup() |>
  group_by(species) |> 
  get_summary_stats(dev_explained)


gam_summary <- 
  gams %>%
  select(species, variable, p_value, p_value_formatted, dev_explained, edf) %>%
  arrange(species, p_value)

write_csv(gam_summary, 'tables/GAMs_summary_table.csv')

# calculate the thresholds----------
nonlinear_thres_updated <- significant_gams |> 
  filter(has_significant_regions == TRUE, edf > 1.01) |> 
  mutate(
    # Calculate 90% CI bounds at es = 1.89 and 2.12
    ci_90_bounds = map(gam_model, ~ {
      es_vals <- c(1.89, 2.12)
      preds <- predict(.x, newdata = tibble(es = es_vals), se.fit = TRUE)
      fit <- preds$fit
      se <- preds$se.fit
      lower <- fit - 1.645 * se
      upper <- fit + 1.645 * se
      tibble(lower = min(lower), upper = max(upper))
    }),

    # Predict values over fine ES grid
    es_grid = map(data, ~ tibble(es = seq(min(.x$es), max(.x$es), length.out = 500))),
    pred_grid = map2(gam_model, es_grid, ~ {
      .y |> mutate(pred = predict(.x, newdata = .y))
    }),

    # Determine slope sign (approximate linear trend)
    slope_sign = map_dbl(gam_model, ~ {
      preds <- predict(.x, newdata = tibble(es = c(1.5, 4)))
      sign(preds[2] - preds[1])
    }),

    # Original threshold from derivative-based method
    original_threshold = map(derivatives, ~ {
      .x |> 
        filter(significant == TRUE & es > 2) |> 
        slice_min(es) |> 
        pull(es)
    }),

    # Final threshold: if original < 2.12, use 90% CI target
    threshold_es = pmap_dbl(
      list(original_threshold, slope_sign, ci_90_bounds, pred_grid),
      function(orig_thres, slope, ci_tbl, preds) {
        if (is.na(orig_thres) || orig_thres < 2.12) {
          target <- if (slope > 0) ci_tbl$upper[[1]] else ci_tbl$lower[[1]]
          preds$es[which.min(abs(preds$pred - target))]
        } else {
          orig_thres
        }
      }
    )
  ) |> 
  select(species, variable, threshold_es) |> 
  mutate(model_type = "nonlinear_CI90")

#significant_gams |> 
  #filter(has_significant_regions=="TRUE" & edf > 1.01) |>
  #select(derivatives) |> 
  #unnest(derivatives) |> 
  #filter(significant == TRUE & es>2) |> 
  #slice_min(es) |> 
  #select(es) |> 
  #print(n = Inf)

# --- Non-linear thresholds (significant derivatives) ---
#non_linear_thres <- significant_gams |> 
 # filter(has_significant_regions == TRUE, edf > 1.01) |> 
 # select(species, variable, derivatives) |> 
 # unnest(derivatives) |> 
 # filter(significant == TRUE, es > 2) |> 
 # group_by(species, variable) |> 
 # slice_min(es, with_ties = FALSE) |> 
 # summarise(threshold_es = es, .groups = "drop") |> 
 # mutate(model_type = "non_linear")

#nonlinear thresholds updated (Recalculate thresholds using linear method when the original non-linear threshold was below 2.12)
#nonlinear_thres_updated <- significant_gams |> 
#  filter(has_significant_regions == TRUE, edf > 1.01) |> 
#  mutate(
#    # Get control range for ES = 1.89 and 2.12
#    control_range = map(data, ~ {
#      .x |> 
#        filter(es %in% c(1.89, 2.12)) |> 
#        summarise(min_val = min(value), max_val = max(value))
#    }),
#    
    # Predict values over fine grid
#    es_grid = map(data, ~ tibble(es = seq(min(.x$es), max(.x$es), length.out = 500))),
#    pred_grid = map2(gam_model, es_grid, ~ {
#      .y |> mutate(pred = predict(.x, newdata = .y))
#    }),
    
    # Determine slope sign (approximate linear trend)
#    slope_sign = map_dbl(gam_model, ~ {
#      preds <- predict(.x, newdata = tibble(es = c(1.5, 4)))
#      sign(preds[2] - preds[1])
#    }),
    
    # Original threshold from derivative-based method
#    original_threshold = map(derivatives, ~ {
#      .x |> 
#        filter(significant == TRUE & es > 2) |> 
#        slice_min(es) |> 
#        pull(es)
#    }),
    
    # Final threshold: if original < 2.12, use control range method
 #   threshold_es = pmap_dbl(
#      list(original_threshold, slope_sign, control_range, pred_grid),
#      function(orig_thres, slope, range_tbl, preds) {
        # If original threshold is NA or below 2.12, use control range intersection method
#        if (is.na(orig_thres) || orig_thres < 2.12) {
#         target <- if (slope > 0) range_tbl$max_val else range_tbl$min_val
#          preds$es[which.min(abs(preds$pred - target))]
#        } else {
#          orig_thres
#        }
#      }
#    )
#  ) |> 
#  select(species, variable, threshold_es) |> 
#  mutate(model_type = "nonlinear")


# --- Linear thresholds (based on 90% CI) ---
linear_thres <- significant_gams |> 
  filter(edf <= 1.01, is_significant == TRUE) |> 
  mutate(
    # Determine slope direction
    slope_sign = map_dbl(gam_model, ~ {
      preds <- predict(.x, newdata = tibble(es = c(1.5, 4)))
      sign(preds[2] - preds[1])
    }),
    
    # Get 90% CI bounds at es = 1.89 and 2.12
    ci_90_bounds = map(gam_model, ~ {
      es_vals <- c(1.89, 2.12)
      preds <- predict(.x, newdata = tibble(es = es_vals), se.fit = TRUE)
      fit <- preds$fit
      se <- preds$se.fit
      lower <- fit - 1.645 * se
      upper <- fit + 1.645 * se
      tibble(lower = min(lower), upper = max(upper))  # Range of 90% CI bounds
    }),
    
    # Predict across ES grid
    es_grid = map(data, ~ tibble(es = seq(min(.x$es), max(.x$es), length.out = 500))),
    pred_grid = map2(gam_model, es_grid, ~ {
      .y |> mutate(pred = predict(.x, newdata = .y))
    }),
    
    # Find threshold: where predicted value is closest to 90% CI bound
    threshold_es = pmap_dbl(
      list(slope_sign, ci_90_bounds, pred_grid),
      function(slope, ci_tbl, preds) {
        target <- if (slope > 0) ci_tbl$upper[[1]] else ci_tbl$lower[[1]]
        preds$es[which.min(abs(preds$pred - target))]
      }
    )
  ) |> 
  select(species, variable, threshold_es) |> 
  mutate(model_type = "linear_CI90")

# --- Combine both into one table ---
combined_thresholds <- bind_rows(nonlinear_thres_updated, linear_thres)

# Optionally write to file
write_csv(combined_thresholds, "tables/GAM_thresholds_combined (90% CI for linear mods).csv")


combined_thresholds |> 
  group_by(species) |> 
  get_summary_stats()

# --- END-------------------------------------------------------


