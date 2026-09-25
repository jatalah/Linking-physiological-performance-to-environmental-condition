library(tidyverse)
library(readxl)
library(mgcv)
library(gratia)
library(patchwork)
library(conflicted)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# File paths
data_path <- "data/processed/transformed_data.xlsx"
labels_path <- "clean_labels_function.R"
output_path <- "figures/selected_GAM_response_variables_by_species.svg"

d <- read_excel(data_path)
source(labels_path)

# Selected responses and their plotting order
selected_responses <- tribble(
  ~species,        ~variable,                                                  ~plot_order,
  "horse_mussel",  "vo2_per_g_6wks",                                                    1,
  "horse_mussel",  "vo2_per_g_12wks",                                                   2,
  "horse_mussel",  "hspa5_bi_p",                                                        3,
  "horse_mussel",  "c18_1n9c_oleic_acid_gonad",                                        4,
  "horse_mussel",  "omega_6_dg",                                                        5,
  "horse_mussel",  "growth_arrest_and_dna_damage_inducible_protein_gadd45",             6,
  "scallop",       "vo2_per_g_6wks",                                                    1,
  "scallop",       "vo2_per_g_12wks",                                                   2,
  "brachiopod",    "vo2_per_g_12wks",                                                   1,
  "brachiopod",    "c18_1n9c_oleic_acid_gonad",                                        2,
  "brachiopod",    "omega_3_gonad",                                                     3
)

# Fit GAMs and extract plotting information
gam_results <- d |>
  inner_join(
    selected_responses,
    by = c("species", "variable")
  ) |>
  group_by(species, variable, plot_order) |>
  nest() |>
  mutate(
    model = map(
      data,
      ~ gam(value ~ s(es, k = 3), data = .x)
    ),
    
    p_value = map_dbl(
      model,
      ~ summary(.x)$s.table[1, "p-value"]
    ),
    
    p_label = map_chr(
      p_value,
      ~ scales::pvalue(
        .x,
        accuracy = 0.001,
        add_p = TRUE
      )
    ),
    
    dev_explained = map_dbl(
      model,
      ~ summary(.x)$dev.expl * 100
    ),
    
    smooth = map(
      model,
      ~ smooth_estimates(.x) |>
        add_confint()
    ),
    
    derivative = map(
      model,
      ~ derivatives(.x, type = "central") |>
        mutate(
          significant = .lower_ci > 0 |
            .upper_ci < 0
        )
    ),
    
    residuals = map2(
      data,
      model,
      ~ .x |>
        add_partial_residuals(.y) |>
        select(es, `s(es)`)
    ),
    
    plot_data = map2(
      smooth,
      derivative,
      ~ .x |>
        arrange(es) |>
        mutate(
          significant = .y$significant[
            findInterval(
              es,
              .y$es,
              all.inside = TRUE
            )
          ]
        )
    )
  ) |>
  ungroup()

# Create one GAM plot
make_gam_plot <- function(
    plot_data,
    residuals,
    variable,
    dev_explained,
    p_label
) {
  ggplot(
    plot_data,
    aes(x = es, y = .estimate)
  ) +
    geom_ribbon(
      aes(
        ymin = .lower_ci,
        ymax = .upper_ci
      ),
      fill = "grey60",
      alpha = 0.25
    ) +
    geom_line(
      aes(
        colour = significant,
        group = 1
      ),
      linewidth = 1
    ) +
    geom_point(
      data = residuals,
      aes(x = es, y = `s(es)`),
      inherit.aes = FALSE,
      shape = 21,
      size = 1.5,
      fill = "grey50",
      colour = "grey20",
      alpha = 0.7,
      position = position_jitter(
        width = 0.10,
        height = 0,
        seed = 123
      )
    ) +
    scale_colour_manual(
      values = c(
        `FALSE` = "#E86861",
        `TRUE` = "#11AEB0"
      ),
      guide = "none"
    ) +
    scale_x_continuous(
      breaks = 2:6,
      labels = scales::number_format(
        accuracy = 0.1
      ),
      expand = expansion(
        mult = c(0.04, 0.04)
      )
    ) +
    labs(
      x = NULL,
      y = "Partial effect",
      title = clean_labels(variable)
    ) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = 9,
        margin = margin(b = 1)
      ),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 6, 5, 6),
      axis.title.y = element_text(
        size = 8,
        margin = margin(r = 2)
      )
    )
}

# Add plots to the results table
gam_results <- gam_results |>
  mutate(
    plot = pmap(
      list(
        plot_data,
        residuals,
        variable,
        dev_explained,
        p_label
      ),
      make_gam_plot
    )
  )

# Create a species-level panel
make_species_panel <- function(
    species_name,
    panel_title,
    nrow = 1,
    blank_cells = 0
) {
  plots <- gam_results |>
    filter(species == species_name) |>
    arrange(plot_order) |>
    pull(plot)
  
  if (blank_cells > 0) {
    plots <- c(
      plots,
      rep(list(plot_spacer()), blank_cells)
    )
  }
  
  species_panel <- wrap_plots(
    plots,
    ncol = 3,
    nrow = nrow
  ) +
    plot_annotation(
      title = panel_title,
      theme = theme(
        plot.title = element_text(size = 11)
      )
    )
  
  # Preserve the species title when nesting panels
  wrap_elements(full = species_panel)
}

# Species panels
horse_mussel_panel <- make_species_panel(
  species_name = "horse_mussel",
  panel_title = expression(
    "A." ~ italic("Atrina zelandica")
  ),
  nrow = 2
)

scallop_panel <- make_species_panel(
  species_name = "scallop",
  panel_title = expression(
    "B." ~ italic("Pecten novaezelandiae")
  ),
  nrow = 1,
  blank_cells = 1
)

brachiopod_panel <- make_species_panel(
  species_name = "brachiopod",
  panel_title = expression(
    "C." ~ italic("Neothyris lenticularis")
  ),
  nrow = 1
)

# Combine species panels
final_plot <- (
  horse_mussel_panel /
    scallop_panel /
    brachiopod_panel
) +
  plot_layout(
    heights = c(2, 1, 1)
  ) +
  plot_annotation(
    caption = "Enrichment Stage (ES)",
    theme = ggplot2::theme(
      plot.caption = ggplot2::element_text(
        size = 10,
        hjust = 0.5,
        margin = ggplot2::margin(t = 5)
      )
    )
  )

final_plot


# Export as SVG
dir.create(
  dirname(output_path),
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = output_path,
  plot = final_plot,
  width = 180,
  height = 180,
  units = "mm",
  bg = "white"
)

final_plot


# species_plot <- (
#   horse_mussel_panel /
#     scallop_panel /
#     brachiopod_panel
# ) +
#   plot_layout(
#     heights = c(2, 1, 1)
#   )
# 
# common_y_title <- wrap_elements(
#   full = grid::textGrob(
#     "Partial effect",
#     rot = 90,
#     gp = grid::gpar(
#       fontsize = 9,
#       fontface = "bold"
#     )
#   )
# )
# 
# # Wrap species_plot so patchwork treats it as one object
# figure_body <- common_y_title +
#   wrap_elements(full = species_plot) +
#   plot_layout(
#     ncol = 2,
#     widths = c(0.01, 1)
#   )
# 
# common_x_title <- wrap_elements(
#   full = grid::textGrob(
#     "Enrichment Stage",
#     gp = grid::gpar(
#       fontsize = 9,
#       fontface = "bold"
#     )
#   )
# )
# 
# # Wrap figure_body before adding the bottom title
# final_plot <- (
#   wrap_elements(full = figure_body) /
#     common_x_title
# ) +
#   plot_layout(
#     ncol = 1,
#     heights = c(1, 0.02)
#   )
# 
# final_plot
