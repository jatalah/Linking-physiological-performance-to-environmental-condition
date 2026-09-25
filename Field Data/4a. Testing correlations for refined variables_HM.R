# Load required packages
library(readxl)
library(dplyr)
library(ggcorrplot)
library(tidyverse)
library(janitor)

# setwd("C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Field data/data/processed")

#Read the Excel file
data <- read_excel("data/processed/combined_median_prevalence_field_2.xlsx")

variables_to_keep <- c("site_x", "species",
  "condition_median",
  "c18_1n9c_oleic_acid_dg_median",
  "fat_percent_dg_median",
  "fat_percent_gonad_median",
  "sum_of_n_6_pufa_dg_median",
  "sum_of_sfa_dg_median",
  "mantle_epithilial_elo_prevalence",
  "fatty_acid_synthase_fas_median",
  "sodium_dependent_multivitamin_transporter_slc5a6_median",
  "sum_of_pufa_gonad_median",
  "gut_vacuolisation_prevalence",
  "cytochrome_p450_cyp_family_1_subfamily_a1_median",
  "growth_arrest_and_dna_damage_inducible_protein_gadd45_median",
  "gut_f_he_prevalence",
  "cathepsin_d_median",
  "glutathione_reductase_median",
  "hspa5_bi_p_median",
  "sum_of_pufa_dg_median",
  "sum_of_sfa_gonad_median",
  "gut_bc_prevalence",
  "copper_transporter_slc31a1_median",
  "coup_transcription_factor_1_median",
  "c20_4n6_arachidonic_acid_aa_gonad_median",
  "c22_5n3_docosapentaenoic_acid_dpa_gonad_median",
  "dg_and_gi_elo_prevalence",
  "dg_atrophy_prevalence",
  "tubulin_alpha_tuba_median",
  "c18_0_stearic_acid_gonad_median",
  "sodium_hydrogen_exchanger_slc9a10_median",
  "gill_elo_t1_prevalence",
  "donson_protein_downstream_neighbour_of_son_median",
  "pcna_proliferating_cell_nuclear_antigen_median"
)

# Select only named columns and filter for species
data <- data %>%
  clean_names()

data <- data %>%
  select(all_of(variables_to_keep)) %>%
  filter(species == "Horse mussel")

#correlation plots##############################################
# Step 1: Compute correlation matrix (excluding non-numeric columns)
numeric_data <- data %>% 
  select(where(is.numeric))%>%
  drop_na()
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")

# Step 2: Convert to long format and filter high correlations (e.g., > 0.7 and < 1)
high_corr_pairs <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
  filter(var1 != var2, abs(correlation) > 0.7) %>%
  distinct()

# Step 3: Get the list of variables involved in strong correlations
high_corr_vars <- unique(c(high_corr_pairs$var1, high_corr_pairs$var2))

# Step 4: Subset numeric data to only these variables
subset_data <- numeric_data %>% select(all_of(high_corr_vars))
subset_cor_matrix <- cor(subset_data, use = "pairwise.complete.obs")

# Step 5: Plot using ggcorrplot
#subset_cor_matrix[abs(subset_cor_matrix) < 0.7] <- NA
cor_plot<-ggcorrplot(subset_cor_matrix,
                     method = "circle", 
                     type = "lower", 
                     hc.order = TRUE,
                     lab = TRUE, 
                     lab_size = 2,           # Size of correlation value text
                     title = "Variable correlations (Horse mussels)",
                     colors = c("white", "lightblue", "blue")) +
  theme(
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),  # smaller X labels
    axis.text.y = element_text(size = 6),                         # smaller Y labels
    plot.title = element_text(size = 12, face = "bold")           # title formatting
  )

# Define the file path 
file_path <- "C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Field data/results/Horse mussels/correlation_plot_HM.png"

# Save the plot
ggsave(filename = file_path, plot = cor_plot, width = 10, height = 8, dpi = 300)

#Group highly correlated variables###################################################################
library(tidyverse)
library(igraph)

# 1. Create correlation matrix
numeric_data <- data %>% select(where(is.numeric))
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# 2. Get variable pairs with high correlation (excluding self-correlations)
high_corr_edges <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2, abs(Freq) > 0.95) %>%
  distinct(pmin(Var1, Var2), pmax(Var1, Var2), .keep_all = TRUE) %>%
  select(Var1, Var2)

# 3. Create a graph from the edges
g <- graph_from_data_frame(high_corr_edges, directed = FALSE)

# 4. Find connected components (subgroups of highly correlated variables)
clusters <- components(g)

# 5. Group variables by cluster
correlation_groups <- split(names(clusters$membership), clusters$membership)

# View the groups
print(correlation_groups)


#plot highly correlated variables for visualization#########################################################################
library(igraph)
library(ggraph)
library(tidygraph)

# Convert igraph object to tidygraph
g_tidy <- as_tbl_graph(g)

# Add cluster information to each node
g_tidy <- g_tidy %>%
  mutate(cluster = as.factor(clusters$membership[name]))

cluster<-ggraph(g_tidy, layout = "fr") +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = cluster), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "Highly Correlated Variable Groups (Horse mussels) (r > 0.95)")

# Define the file path 
file_path <- "C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Field data/results/Horse mussels/cluster_plot_HM.png"

# Save the plot
ggsave(filename = file_path, plot = cluster, width = 10, height = 8, dpi = 300)

