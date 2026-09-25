# Load required packages
library(readxl)
library(ggcorrplot)
library(tidyverse)

rm(list = ls())

#Read the Excel file
data <- read_excel("data/processed/combined_median_prevalence_restored_9Sept26.xlsx")

variables_to_keep <- c("treatment_group", "species", "c18_1n9c_oleic_acid_Mantle_median",
                        "percent_fat_Gonad_median",
                        "gill_elo_t1_p_a_prevalence",
                        "trematodes_p_a_prevalence",
                        "mean_teac_median",
                        "c18_1n9c_oleic_acid_DG_median",
                        "mufa_Gonad_median",
                        "percent_fat_DG_median",
                        "pufa_DG_median",
                        "sfa_DG_median",
                        "dg_and_gi_elo_p_a_prevalence",
                        "mantle_epithilial_elo_p_a_prevalence",
                        "vo2_per_g_6wks_median",
                        "cytochrome_p450_cyp_family_1_subfamily_a1_median",
                        "donson_protein_downstream_neighbour_of_son_median",
                        "fatty_acid_synthase_fas_median",
                        "glutathione_reductase_median",
                        "hspa5_bi_p_median",
                        "sodium_calcium_exchanger_slc8a_median",
                        "pufa_Gonad_median",
                        "gut_vacuolisation_p_a_prevalence",
                        "mda_nmoles_mg_of_protein_r2_median",
                        "pcna_proliferating_cell_nuclear_antigen_median",
                        "c18_1n9c_oleic_acid_Gonad_median",
                        "vo2_per_g_12wks_median",
                        "cathepsin_d_median",
                        "copper_transporter_slc31a1_median",
                        "growth_arrest_and_dna_damage_inducible_protein_gadd45_median",
                        "sodium_dependent_multivitamin_transporter_slc5a6_median",
                        "sfa_Gonad_median",
                        "mda_nmoles_mg_of_protein_r1_median",
                        "coup_transcription_factor_1_median")

clean_variable_name <- function(var_name) {
  var_name %>%
    # Remove common suffixes
    gsub("_median$", "", .) %>%
    gsub("_p_a_prevalence$", "", .) %>%
    # Replace underscores with spaces
    gsub("_", " ", .) %>%
    # Fix known abbreviations to uppercase
    gsub("\\bdg\\b", "DG", ., ignore.case = TRUE) %>%
    gsub("\\bgi\\b", "GI", ., ignore.case = TRUE) %>%
    gsub("\\bapx\\b", "APX", ., ignore.case = TRUE) %>%
    gsub("\\bhe\\b", "HE", ., ignore.case = TRUE) %>%
    gsub("\\belo\\b", "ELO", ., ignore.case = TRUE) %>%
    gsub("\\bslc([0-9a-z]+)", "SLC\\1", ., ignore.case = TRUE) %>%
    gsub("\\bcyp\\b", "CYP", ., ignore.case = TRUE) %>%
    # Capitalize only the first letter of the entire string
    { paste0(toupper(substr(., 1, 1)), substr(., 2, nchar(.))) }
}

sapply(variables_to_keep, clean_variable_name)

# Apply your cleaning function
clean_names <- sapply(variables_to_keep, clean_variable_name)

# Create a named vector: names are original, values are cleaned
rename_lookup <- setNames(clean_names, variables_to_keep)

# Select only named columns and filter for species
data <- data %>%
  select(all_of(variables_to_keep)) %>%
  filter(species == "horse_mussel")

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


# ===>>> Apply rename_lookup to row and column names here <<<===
rownames(subset_cor_matrix) <- ifelse(
  rownames(subset_cor_matrix) %in% names(rename_lookup),
  rename_lookup[rownames(subset_cor_matrix)],
  rownames(subset_cor_matrix)
)

colnames(subset_cor_matrix) <- ifelse(
  colnames(subset_cor_matrix) %in% names(rename_lookup),
  rename_lookup[colnames(subset_cor_matrix)],
  colnames(subset_cor_matrix)
)

# Step 5: Plot using ggcorrplot
#subset_cor_matrix[abs(subset_cor_matrix) < 0.7] <- NA
cor_plot<-ggcorrplot(subset_cor_matrix,
           method = "circle", 
           type = "lower", 
           hc.order = TRUE,
           lab = TRUE, 
           lab_size = 2,           # Size of correlation value text
           title = "Variable correlations (Horse mussels)",
           colors = c("white", "pink", "red")) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, face = "bold"),  # smaller X labels
    axis.text.y = element_text(size = 8, face = "bold"),                         # smaller Y labels
    plot.title = element_text(size = 12, face = "bold")           # title formatting
  )

# Define the file path 
file_path <- "C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Lab data/results/Horse mussels/correlation_plot_HM.png"

# Save the plot
ggsave(filename = file_path, plot = cor_plot, width = 10, height = 8, dpi = 300, bg = 'white')
ggsave("C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Lab data/results/Horse mussels/correlation_plot_HM.svg", plot = cor_plot, width = 10, height = 8, bg = "white")

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
file_path <- "C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Lab data/results/Horse mussels/cluster_plot_HM_2nd iteration.png"

# Save the plot
ggsave(filename = file_path, plot = cluster, width = 10, height = 8, dpi = 300, bg ='white')


# targeted correlations 
library(lares)
corr_var(data, c18_1n9c_oleic_acid_DG_median, top = 40) + labs(title = "Atrina – oleic acid (DG)")

corr_var(numeric_data,
         c18_1n9c_oleic_acid_DG_median,
         top = 50,
         plot = F) |>
  write_csv('tables/correlations_atrina_oleics_acid_dg.csv')
