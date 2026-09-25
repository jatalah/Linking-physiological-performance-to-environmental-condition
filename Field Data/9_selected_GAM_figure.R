library(tidyverse)
library(readxl)
library(mgcv)
library(gratia)
library(patchwork)
library(conflicted)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# Update these paths as needed.
data_path <- "data/processed/transformed_data.xlsx"
labels_path <- "clean_labels_function.R"
output_path <- "figures/selected_field_GAMs.svg"

d <- read_excel(data_path) |>
  rename(es = es_macrofauna)

source(labels_path)

# Selected field responses shown in the two-row figure.
selected_responses <- tribble(
  ~species,         ~variable,                                              ~plot_order,
  "Horse mussel",  "sum_of_n_6_pufa_gonad",                               1,
  "Horse mussel",  "sodium_dependent_multivitamin_transporter_slc5a6",    2,
  "Horse mussel",  "c18_1n9c_oleic_acid_dg",                              3,
  "Brachiopod",    "sum_of_pufa_non_gonadal",                              1,
  "Brachiopod",    "sum_of_n_3_pufa_non_gonadal",                         2,
  "Brachiopod",    "c18_1n9c_oleic_acid_gonad",                           3
)

# Check that all selected variables exist before fitting models.
missing_responses <- selected_responses |>
  anti_join(
    d |> distinct(species, variable),
    by = c("species", "variable")
  )

if (nrow(missing_responses) > 0) {
  stop(
    "These selected species-variable combinations are not in the data:\n",
    paste(
      paste(missing_responses$species, missing_responses$variable, sep = ": "),
      collapse = "\n"
    )
  )
}

gam_results <- d |>
  inner_join(selected_responses, by = c("species", "variable")) |>
  group_by(species, variable, plot_order) |>
  nest() |>
  mutate(
    model = map(data, ~ gam(value ~ s(es, k = 3), data = .x)),
    
    smooth = map(
      model,
      ~ smooth_estimates(.x) |>
        add_confint()
    ),
    
    derivative = map(
      model,
      ~ derivatives(.x, type = "central") |>
        mutate(significant = .lower_ci > 0 | .upper_ci < 0)
    ),
    
    partial_residuals = map2(
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
            findInterval(es, .y$es, all.inside = TRUE)
          ]
        )
    )
  ) |>
  ungroup()

make_gam_plot <- function(plot_data, partial_residuals, variable) {
  ggplot(plot_data, aes(x = es, y = .estimate)) +
    geom_ribbon(
      aes(ymin = .lower_ci, ymax = .upper_ci),
      fill = "grey60",
      alpha = 0.25
    ) +
    geom_line(
      aes(colour = significant, group = 1),
      linewidth = 1
    ) +
    geom_point(
      data = partial_residuals,
      aes(x = es, y = `s(es)`),
      inherit.aes = FALSE,
      shape = 21,
      size = 1.5,
      fill = "grey50",
      colour = "grey20",
      alpha = 0.7,
      position = position_jitter(width = 0.08, height = 0, seed = 123)
    ) +
    scale_colour_manual(
      values = c(`FALSE` = "#F8766D", `TRUE` = "#11AEB0"),
      guide = "none"
    ) +
    scale_x_continuous(
      breaks = 2:4,
      labels = scales::number_format(accuracy = 0.1),
      expand = expansion(mult = c(0.04, 0.04))
    ) +
    labs(
      x = NULL,
      y = "Partial effect",
      title = clean_labels(variable)
    ) +
    theme_minimal(base_size = 7.5) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 7.5, margin = margin(b = 1)),
      axis.text = element_text(size = 6.5),
      panel.grid.minor = element_blank(),
      plot.margin = margin(2, 3, 2, 3)
    )
}

gam_results <- gam_results |>
  mutate(
    plot = pmap(
      list(plot_data, partial_residuals, variable),
      make_gam_plot
    )
  )

get_species_plots <- function(species_name) {
  gam_results |>
    filter(species == species_name) |>
    arrange(plot_order) |>
    pull(plot)
}

# wrap_elements preserves titles when the species panels are combined.
make_species_panel <- function(species_name, panel_title) {
  wrap_elements(
    full = wrap_plots(
      get_species_plots(species_name),
      ncol = 3,
      nrow = 1
    ) +
      plot_annotation(
        title = panel_title,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 9)
        )
      )
  )
}

horse_mussel_panel <- make_species_panel(
  "Horse mussel",
  expression("A." ~ italic("Atrina zelandica"))
)

brachiopod_panel <- make_species_panel(
  "Brachiopod",
  expression("B." ~ italic("Neothyris lenticularis"))
)

final_plot <- (
  horse_mussel_panel /
    brachiopod_panel
) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    caption = "Enrichment Stage (ES)",
    theme = ggplot2::theme(
      plot.caption = ggplot2::element_text(
        size = 10,
        hjust = 0.5,
        margin = ggplot2::margin(t = 2)
      )
    )
  )

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = output_path,
  plot = final_plot,
  width = 180,
  height = 90,
  units = "mm",
  bg = "white"
)

final_plot
