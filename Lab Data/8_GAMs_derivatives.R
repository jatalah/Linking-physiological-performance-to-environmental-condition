library(tidyverse)
library(readxl)
library(mgcv)
library(gratia)
library(broom)
library(patchwork)
library(ggpubr)
library(conflicted)
rm(list = ls())

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

d <- read_excel('data/processed/transformed_data.xlsx')
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
        scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +  # <<-- THIS LINE
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
    significant_fdr = p_fdr < 0.10
  ) %>%
  summarise(
    n_models = n(),
    significant_raw = sum(p_value < 0.05, na.rm = TRUE),
    significant_fdr = sum(significant_fdr, na.rm = TRUE)
  )

# select( species, variable, p_value , p_value_formatted ,  p_fdr) 


  gams %>%
    mutate(p_fdr = p.adjust(p_value, method = "BH"),
           significant_fdr10 = p_fdr < 0.10) |>
    summarise(
      n_models = n(),
      significant_raw = sum(p_value < 0.05, na.rm = TRUE),
      significant_fdr = sum(p_fdr < 0.05, na.rm = TRUE)
    )
  
  
  sum(gams$significant_fdr10, na.rm = TRUE)  
  
  
# Extract only significant models
significant_gams <- 
  gams %>%
  filter(is_significant == TRUE)

# Print summary of significant models
cat("Number of significant GAM models:", nrow(significant_gams), "out of", nrow(gams), "\n\n")

# # combine plots and save----
# spp <- d |> distinct(species) |> deframe()
# 
# combined_plots <- map(spp, ~ {
#   p_list <- significant_gams |>
#     filter(species == .x) |>
#     pull(es_plots)
#   
#   ggarrange(plotlist = p_list) |>
#     annotate_figure(top = text_grob(str_to_sentence(.x), face = "plain", size = 10))
# })

# combine plots and save----
desired_order_p1 <- c("condition_score", "vo2_per_g_6wks", "vo2_per_g_12wks", "pufa_dg", "c18_1n9c_oleic_acid_gonad", "omega_6_dg", 
                   "donson_protein_downstream_neighbour_of_son", "fatty_acid_synthase_fas", "glutathione_reductase", "hspa5_bi_p",  "pcna_proliferating_cell_nuclear_antigen", "cathepsin_d", "growth_arrest_and_dna_damage_inducible_protein_gadd45","copper_transporter_slc31a1", "sodium_dependent_multivitamin_transporter_slc5a6", "sodium_hydrogen_exchanger_slc9a10", "sodium_calcium_exchanger_slc8a")

p1 <- significant_gams |>
  filter(species == 'horse_mussel') |>
  mutate(variable = factor(variable, levels = desired_order_p1)) |>
  arrange(variable) |>
  pull(es_plots) |>
  wrap_plots(nrow = 5) +
  plot_annotation(title = "A.")

desired_order_p2 <- c("vo2_per_g_6wks", "vo2_per_g_12wks")

p2 <- significant_gams |>
  filter(species == 'scallop') |>
  mutate(variable = factor(variable, levels = desired_order_p2)) |>
  arrange(variable) |>
  pull(es_plots) |>
  wrap_plots(nrow = 1) +
  plot_annotation(title = "B.")

desired_order_p3 <- c("vo2_per_g_12wks", "c18_1n9c_oleic_acid_gonad", "omega_3_gonad", "c22_5n3_docosapentaenoic_acid_dpa_gonad")
p3 <- significant_gams |>
  filter(species == 'brachiopod') |>
  mutate(variable = factor(variable, levels = desired_order_p3)) |>
  arrange(variable) |>
  pull(es_plots) |>
  wrap_plots(nrow = 1) +
  plot_annotation(title = "C.")

ggsave(plot = p1,  "figures/GAMs_pdp_plots_horse_musselA.svg",
       width = 8, height = 8, dpi = 300, bg = "white")

ggsave(plot = p2,  "figures/GAMs_pdp_plots_scallopB.svg",
       width = 8, height = 2, dpi = 300, bg = "white")

ggsave(plot = p3,  "figures/GAMs_pdp_plots_brachiopodC.svg",
       width = 8, height = 2, dpi = 300, bg = "white")

# Function to arrange plots for each species in 4 columns
make_patch_panel <- function(species_name) {
  plots <- significant_gams |>
    filter(species == species_name) |>
    pull(es_plots)
  
  wrap_plots(species_name = plots, ncol = 4)
}

# Create wrapped panels

p1 <- make_patch_panel("brachiopod") 
p2 <- make_patch_panel("horse_mussel") 
p3 <- make_patch_panel("scallop")

# Combine panels and label with a, b, c at the species level
final_plot <- (p1 / p3 /p2) +
  plot_layout(ncol = 1, heights = c(.1,.1, 1)) 

final_plot

# Save
ggsave(
  "figures/GAMs_pdp_combined_species.png",
  final_plot,
  width = 6,
  height = 12,
  dpi = 300,
  bg = "white"
)


# combined_plots

# save plots
# # Define filenames and dimensions
# plot_info <- tibble::tibble(
#   filename = c(
#     "figures/GAMs_pdp_plots_brachiopod.png",
#     "figures/GAMs_pdp_plots_horse_mussel.png",
#     "figures/GAMs_pdp_plots_scallop.png"
#   ),
#   width = c(4, 8, 8),
#   height = c(4, 8, 4)
# )
# 
# # Save plots
# pwalk(
#   .l = list(
#     plot = combined_plots,
#     filename = plot_info$filename,
#     width = plot_info$width,
#     height = plot_info$height
#   ),
#   .f = ggsave,
#   dpi = 300,
#   bg = "white"
# )

# Print significant models by species and variable
gam_summary <- 
  gams %>%
  select(species, variable, p_value, p_value_formatted, dev_explained, edf) %>%
  arrange(species, p_value)

write_csv(gam_summary, 'tables/GAMs_summary_table.csv')

# --- Non-linear thresholds (significant derivatives) ---
non_linear_thres <- significant_gams |> 
  filter(has_significant_regions == TRUE, edf > 1.01) |> 
  select(species, variable, derivatives) |> 
  unnest(derivatives) |> 
  filter(significant == TRUE, es > 3) |> 
  group_by(species, variable) |> 
  slice_min(es, with_ties = FALSE) |> 
  summarise(threshold_es = es, .groups = "drop") |> 
  mutate(model_type = "non_linear")

# --- Linear thresholds (based on control range overlap) ---
linear_thres <- significant_gams |> 
  filter(edf <= 1.01, is_significant == TRUE) |> 
  mutate(
    # Determine slope direction
    slope_sign = map_dbl(gam_model, ~ {
      preds <- predict(.x, newdata = tibble(es = c(1.5, 4)))
      sign(preds[2] - preds[1])
    }),
    
    # Get control range at es = 1.88
    #control_range = map(data, ~ {
     # .x |> filter(es == 1.88) |> summarise(min_val = min(value), max_val = max(value))
   # }),
   
   # Get 90% confidence interval at es = 1.88
   ci_90 = map2(gam_model, data, ~ {
     newdata <- tibble(es = 1.88)
     preds <- predict(.x, newdata = newdata, se.fit = TRUE)
     lower <- preds$fit - 1.645 * preds$se.fit
     upper <- preds$fit + 1.645 * preds$se.fit
     tibble(lower = lower, upper = upper)
   }),
    
    # Predict across ES grid
    #es_grid = map(data, ~ tibble(es = seq(min(.x$es), max(.x$es), length.out = 500))),
    #pred_grid = map2(gam_model, es_grid, ~ {
      #.y |> mutate(pred = predict(.x, newdata = .y))
    #}),
   
   # Predict across ES grid with SEs
   es_grid = map(data, ~ tibble(es = seq(min(.x$es), max(.x$es), length.out = 500))),
   pred_grid = map2(gam_model, es_grid, ~ {
     preds <- predict(.x, newdata = .y, se.fit = TRUE)
     .y |> mutate(pred = preds$fit)
   }),
    
    # Find threshold: where predicted value is closest to control max/min
   # threshold_es = pmap_dbl(
    #  list(slope_sign, control_range, pred_grid),
     # function(slope, range_tbl, preds) {
      #  target <- if (slope > 0) range_tbl$max_val else range_tbl$min_val
       # preds$es[which.min(abs(preds$pred - target))]
    #  }
    #)
  #) |>
 # select(species, variable, threshold_es) |> 
  #mutate(model_type = "linear")
 
 # Find threshold: where predicted value is closest to lower or upper 90% CI bound
 threshold_es = pmap_dbl(
   list(slope_sign, ci_90, pred_grid),
   function(slope, ci_tbl, preds) {
     target <- if (slope > 0) ci_tbl$upper[[1]] else ci_tbl$lower[[1]]
     preds$es[which.min(abs(preds$pred - target))]
   }
 )
  ) |> 
  select(species, variable, threshold_es) |> 
  mutate(model_type = "linear_CI90")

# --- Combine both into one table ---
combined_thresholds <- bind_rows(non_linear_thres, linear_thres)

# Optionally write to file
write_csv(combined_thresholds, "tables/GAM_thresholds_combined (90CI for linear).csv")
