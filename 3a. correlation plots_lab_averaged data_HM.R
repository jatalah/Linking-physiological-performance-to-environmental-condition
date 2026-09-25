# Load required packages
library(readxl)
library(dplyr)
library(ggcorrplot)
library(tidyverse)
library(janitor)
library(lares)


#Read the Excel file
data <- read_excel("data/processed/combined_median_prevalence_field_2.xlsx")

variables_to_keep <- c("site.x", "species", "condition_median",
                       "bc_grade_median", "f_he_level_median",
                       "c14_0_myristic_acid_DG_median", "c14_0_myristic_acid_Gonad_median", "c14_0_myristic_acid_Non-gonadal_median",
                       "c16_0_palmitic_acid_DG_median", "c16_0_palmitic_acid_Gonad_median",
                       "c16_1_palmitoleic_acid_DG_median", "c16_1_palmitoleic_acid_Gonad_median",
                       "c18_0_stearic_acid_DG_median", "c18_0_stearic_acid_Gonad_median", "c18_0_stearic_acid_Non-gonadal_median",
                       "c18_1n9c_oleic_acid_DG_median", "c18_1n9c_oleic_acid_Gonad_median", "c18_1n9c_oleic_acid_Non-gonadal_median",
                       "c18_2n6c_linoleic_acid_DG_median", "c18_2n6c_linoleic_acid_Gonad_median", "c18_2n6c_linoleic_acid_Non-gonadal_median",
                       "c18_3n3_alpha_linolenic_acid_ala_DG_median", "c18_3n3_alpha_linolenic_acid_ala_Gonad_median", "c18_3n3_alpha_linolenic_acid_ala_Non-gonadal_median",
                       "c18_4n3_stearidonic_acid_sda_DG_median", "c18_4n3_stearidonic_acid_sda_Gonad_median", "c18_4n3_stearidonic_acid_sda_Non-gonadal_median",
                       "c20_0_arachidic_acid_DG_median", "c20_0_arachidic_acid_Gonad_median", "c20_0_arachidic_acid_Non-gonadal_median",
                       "c20_1_gadoleic_acid_DG_median", "c20_1_gadoleic_acid_Gonad_median", "c20_1_gadoleic_acid_Non-gonadal_median",
                       "c20_2_eicosadienoic_acid_DG_median", "c20_2_eicosadienoic_acid_Gonad_median", "c20_2_eicosadienoic_acid_Non-gonadal_median",
                       "c20_4n6_arachidonic_acid_aa_DG_median", "c20_4n6_arachidonic_acid_aa_Gonad_median", "c20_4n6_arachidonic_acid_aa_Non-gonadal_median",
                       "c20_5n3_eicosapentaenoic_acid_DG_median", "c20_5n3_eicosapentaenoic_acid_Gonad_median", "c20_5n3_eicosapentaenoic_acid_Non-gonadal_median",
                       "c22_5n3_docosapentaenoic_acid_dpa_DG_median", "c22_5n3_docosapentaenoic_acid_dpa_Gonad_median", "c22_5n3_docosapentaenoic_acid_dpa_Non-gonadal_median",
                       "c22_6n3_docosahexaenoic_acid_dha_DG_median", "c22_6n3_docosahexaenoic_acid_dha_Gonad_median", "c22_6n3_docosahexaenoic_acid_dha_Non-gonadal_median",
                       "sum_of_sfa_DG_median", "sum_of_sfa_Gonad_median", "sum_of_sfa_Non-gonadal_median",
                       "sum_of_mufa_DG_median", "sum_of_mufa_Gonad_median", "sum_of_mufa_Non-gonadal_median",
                       "sum_of_pufa_DG_median", "sum_of_pufa_Gonad_median", "sum_of_pufa_Non-gonadal_median",
                       "sum_of_n_3_pufa_DG_median", "sum_of_n_3_pufa_Gonad_median", "sum_of_n_3_pufa_Non-gonadal_median",
                       "sum_of_n_6_pufa_DG_median", "sum_of_n_6_pufa_Gonad_median", "sum_of_n_6_pufa_Non-gonadal_median",
                       "fat_percent_DG_median", "fat_percent_Gonad_median", "fat_percent_Non-gonadal_median",
                       "gill_congestion_prevalence", "gill_elo_t1_prevalence", "gill_vacuolation_prevalence",
                       "gut_f_he_prevalence", "gut_bc_prevalence", "gut_vacuolisation_prevalence",
                       "apicomlexan_prevalence", "mantle_epithilial_elo_prevalence", "dg_and_gi_elo_prevalence","dg_high_he_prevalence",
                       "trematodes_prevalence", "new_parasites_prevalence","metacercaria_prevalence",
                       "dg_atrophy_prevalence", "gut_apx_prevalence",
                       "dg_luminal_space_open_prevalence", "gut_epi_atrophy_prevalence", "dg_bc_prevalence",
                       "dg_thickening_sub_epithilium_prevalence", 
                       "isocitrate_dehydrogenase_idh_median", "aldehyde_dehydrogenase_median", "glutathione_reductase_median", "glutathione_peroxidase_median",
                       "acetyl_co_a_c_acetyltransferase_acat_median", "fatty_acid_synthase_fas_median",
                       "cathepsin_b_median", "cathepsin_l_median", "cathepsin_d_median",
                       "enoyl_co_a_hydratase_median", "glutathione_transferase_median",
                       "apoptosis_markers_atf4_cyclic_amp_dependent_transcription_factor_median",
                       "growth_arrest_and_dna_damage_inducible_protein_gadd45_median", "aifm1_apoptosis_inducing_factor_1_median",
                       "pcna_proliferating_cell_nuclear_antigen_median", "toll_like_receptor_1_median",
                       "tnf_ligand_superfamily_tnfsf_member_15_median",
                       "spectrin_alpha_median", "tubulin_alpha_tuba_median",
                       "cytochrome_p450_cyp_family_1_subfamily_a1_median", "cytochrome_p450_cyp_family_1_subfamily_b1_median",
                       "cytochrome_p450_cyp_family_2_subfamily_j_median", "solute_carrier_family_43_slc43a3_median",
                       "coup_transcription_factor_1_median", "nuclear_receptor_subfamily_1_group_d_member_3_median",
                       "hspa4_70k_da_a_co_chaperone_for_hsp70_median", "hspa5_bi_p_median",
                       "suppressor_of_tumorigenicity_protein_13_st13_median", "ube2a_ubiquitin_conjugating_enzyme_e2_a_median",
                       "apex1_ap_endonuclease_1_median", "mbd4_methyl_cp_g_binding_domain_protein_4_median",
                       "hmgb1_high_mobility_group_protein_b1_median", "sodium_dependent_multivitamin_transporter_slc5a6_median",
                       "sodium_dependent_phosphate_cotransporters_slc34a_mfs_transporter_median", "copper_transporter_slc31a1_median",
                       "sodium_hydrogen_exchanger_slc9a10_median", "mitochondrial_folate_transporter_slc25a32_median",
                       "birc2_baculoviral_iap_repeat_containing_protein_2_3_median", "cytochrome_family_3_subfamily_a4_median",
                       "membrane_transport_protein_xk_median", "tn_fs_neuroblastoma_suppressor_of_tumorigenicity_1_nbl1_median",
                       "dna_endonuclease_rbbp8_median", "donson_protein_downstream_neighbour_of_son_median"
)

clean_variable_name <- function(var_name) {
  var_name %>%
    # Remove common suffixes
    gsub("_median$", "", .) %>%
    gsub("_prevalence$", "", .) %>%
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

# Select columns that contain 'median' or 'prevalence' in their names (and keep identifiers like 'treatment_group' and 'species')
#data <- data %>%
#  select(treatment_group, species, contains("median"), contains("prevalence"))%>%
#  filter(species == "Horse mussel")

# Select only named columns, clean data, and filter for species
#data <- data %>%
#  select(all_of(variables_to_keep)) 

#data <- data %>%
#  clean_names()

data <- data %>%
  select(all_of(variables_to_keep)) %>%
  filter(species == "Horse mussel")

#correlation plots##############################################
# Step 1: Compute correlation matrix (excluding non-numeric columns)
numeric_data <- data %>% select(where(is.numeric))
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")

# Step 2: Convert to long format and filter high correlations (e.g., > 0.7 and < 1)
high_corr_pairs <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
  filter(var1 != var2, abs(correlation) > 0.9) %>%
  distinct()

# Step 3: Get the list of variables involved in strong correlations
high_corr_vars <- unique(c(high_corr_pairs$var1, high_corr_pairs$var2))

# Step 4: Subset numeric data to only these variables
subset_data <- numeric_data %>% select(all_of(high_corr_vars))
subset_cor_matrix <- cor(subset_data, use = "pairwise.complete.obs")

# Step 5: Plot using ggcorrplot
subset_cor_matrix[abs(subset_cor_matrix) < 0.99] <- NA
ggcorrplot(subset_cor_matrix,
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2.5,           # Size of correlation value text
           title = "Highly Correlated Variables (|r| > 0.99)") +
  theme(
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),  # smaller X labels
    axis.text.y = element_text(size = 6),                         # smaller Y labels
    plot.title = element_text(size = 14, face = "bold")           # title formatting
  )

#Listing highly correlated pairs######################################################################
# Step 1: Select numeric columns
numeric_data <- data %>% select(where(is.numeric))

# Step 2: Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Step 3: Convert to a long (tidy) format and filter for high correlations
high_corr_pairs <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2, abs(Freq) > 0.99) %>%
  arrange(desc(abs(Freq))) %>%
  distinct(pmin(Var1, Var2), pmax(Var1, Var2), .keep_all = TRUE) %>%
  rename(Variable_1 = Var1, Variable_2 = Var2, Correlation = Freq)

# View the list
view(high_corr_pairs)

#Group highly correlated variables###################################################################
library(tidyverse)
library(igraph)

# 1. Create correlation matrix
numeric_data <- data %>% select(where(is.numeric))
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# 2. Get variable pairs with high correlation (excluding self-correlations)
high_corr_edges <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2, abs(Freq) > 0.90) %>%
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

library(writexl)

# Convert the list to a data frame
correlation_groups_df <- stack(correlation_groups)
colnames(correlation_groups_df) <- c("Variable", "Cluster")

# Define full path
output_path <- "C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Field data/results/Horse mussels/correlation_groups_HM.xlsx"

# Save
write_xlsx(correlation_groups_df, output_path)

#plot highly correlated variables for visualization#########################################################################
library(igraph)
library(ggraph)
library(tidygraph)

# Convert igraph object to tidygraph
g_tidy <- as_tbl_graph(g)

# Add cluster information to each node
g_tidy <- g_tidy %>%
  mutate(cluster = as.factor(clusters$membership[name]))

# Replace node names using rename_lookup
g_tidy <- g_tidy %>%
  mutate(name = ifelse(name %in% names(rename_lookup), rename_lookup[name], name))


correlation_network_plot_HM_field<-ggraph(g_tidy, layout = "fr") +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = cluster), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "Highly Correlated Variable Groups (Horse mussels) (r > 0.90)")

ggsave("results/Horse mussels/correlation_network_plot_HM_field.svg", plot = correlation_network_plot_HM_field, width = 10, height = 8, dpi = 300, bg="white")

#find variables that were not highly correlated with any other variable##########################################################
# Flatten and get unique variable names from the list of clusters
clustered_variables <- unique(unlist(correlation_groups))

#get all column names
colnames(numeric_data)

# Get all variable names
all_var_names <- colnames(cor_matrix)

# Identify variables not in any cluster
unclustered_vars <- setdiff((colnames(numeric_data)), clustered_variables)

# View the result
unclustered_vars

# Convert to a data frame for writing to Excel
unclustered_vars_df <- data.frame(Variable = unclustered_vars)

# Save to Excel
write_xlsx(unclustered_vars_df,
           "C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Field data/results/Horse mussels/unclustered_variables_HM.xlsx"
)


#selection of a subset of variables from each cluster to reduce multicollinearity and for inclusion in future GLMM######################
# 1. Best variables based on lowest average correlation
best_low_corr <- sapply(correlation_groups, function(group_vars) {
  other_vars <- setdiff(colnames(cor_matrix), group_vars)
  avg_corrs <- sapply(group_vars, function(var) mean(abs(cor_matrix[var, other_vars]), na.rm = TRUE))
  group_vars[which.min(avg_corrs)]
})

# 2. Best variables based on highest variance
variances <- sapply(numeric_data, var, na.rm = TRUE)
best_high_var <- sapply(correlation_groups, function(group_vars) {
  group_vars[which.max(variances[group_vars])]
})

# Combine into a data frame
best_vars_df <- data.frame(
  Cluster = names(best_low_corr),
  Best_by_low_corr = best_low_corr,
  Best_by_high_variance = best_high_var,
  stringsAsFactors = FALSE
)

# Save to Excel
write_xlsx(best_vars_df, "C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Field data/results/Horse mussels/best_vars_comparison.xlsx")

#compute p-values#############################################################
cor.mtest <- function(mat, method = "pearson") {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test <- cor.test(mat[, i], mat[, j], method = method, use = "pairwise.complete.obs")
      p.mat[i, j] <- p.mat[j, i] <- test$p.value
    }
  }
  diag(p.mat) <- 0
  return(p.mat)
}

# Calculate the matrix of p-values
p_matrix <- cor.mtest(numeric_data)

# Optional: View structure or save
head(p_matrix)

# Convert matrix to data frame and add row names as a column
p_matrix_df <- as.data.frame(p_matrix)
p_matrix_df <- tibble::rownames_to_column(p_matrix_df, var = "Variable")

# Save to Excel
write_xlsx(p_matrix_df, "C:/Users/rebeccamc/OneDrive - Cawthron/General - Environmental Health Measures for Open Ocean Aquaculture/Objective 3/Data analysis/Obj3_analysis/Lab data/results/p_value_matrix.xlsx")

#disply plot
ggcorrplot(cor_matrix,
           type = "lower",
           method = "circle",
           lab = TRUE,
           p.mat = p_matrix,
           sig.level = 0.05,
           insig = "blank")


# targeted correlations 
corr_var(numeric_data, c18_1n9c_oleic_acid_Gonad_median, top = 40) + labs(title = "Atrina – oleic acid (gonad)")
corr_var(numeric_data, c18_1n9c_oleic_acid_Gonad_median, top = 50, plot= F) |> 
  write_csv('tables/correlations_atrina_oleics_acid_gonad.csv')
