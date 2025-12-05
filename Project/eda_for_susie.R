# ============================================================================
# Exploratory Data Analysis for susieR Variable Selection
# ============================================================================
# This script performs EDA on the filtered NHANES dataset to:
# 1. Show high correlations among X variables
# 2. Demonstrate sparsity in the data (missing patterns, effect sparsity)
# 3. Generate publication-quality plots for LaTeX
# ============================================================================

library(tidyverse)
library(RColorBrewer)

# Load optional packages (install if needed)
has_corrplot <- FALSE
if (!require(corrplot, quietly = TRUE)) {
  cat("Note: corrplot package not available. Using ggplot2 for heatmaps.\n")
} else {
  has_corrplot <- TRUE
}

# For VIF calculation
if (!require(car, quietly = TRUE)) {
  cat("Note: car package not available. Will calculate VIF manually.\n")
  has_car <- FALSE
} else {
  has_car <- TRUE
}

# ============================================================================
# SETUP
# ============================================================================

base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"
data_file <- file.path(base_dir, "nhanes_filtered_for_susie.rds")
output_dir <- file.path(base_dir, "EDA_plots")
var_analysis_file <- file.path(base_dir, "variable_analysis_report.csv")

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("============================================================================\n")
cat("Exploratory Data Analysis for susieR\n")
cat("============================================================================\n\n")

# Set high-quality plot parameters for LaTeX
theme_set(theme_bw(base_size = 12))
theme_update(
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 12, hjust = 0.5),
  axis.title = element_text(size = 11),
  axis.text = element_text(size = 10),
  legend.title = element_text(size = 11),
  legend.text = element_text(size = 10),
  strip.text = element_text(size = 10)
)

# ============================================================================
# Step 1: Load Data
# ============================================================================

cat("Step 1: Loading data...\n")
data <- readRDS(data_file)

# Remove SEQN (ID variable)
data <- data %>%
  select(-SEQN)

cat("  Dataset dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("  Memory usage:", round(object.size(data) / (1024^2), 2), "MB\n\n")

# ============================================================================
# Step 2: Data Overview and Missing Data Analysis
# ============================================================================

cat("Step 2: Analyzing missing data patterns...\n")

# Calculate missing data percentage per variable
missing_summary <- data %>%
  summarise_all(~ sum(is.na(.)) / length(.) * 100) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
  arrange(desc(missing_pct))

cat("  Variables with >50% missing:", sum(missing_summary$missing_pct > 50), "\n")
cat("  Variables with >90% missing:", sum(missing_summary$missing_pct > 90), "\n")
cat("  Variables with 0% missing:", sum(missing_summary$missing_pct == 0), "\n")

# Plot 1: Distribution of missing data percentages
p1 <- missing_summary %>%
  ggplot(aes(x = missing_pct)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of Missing Data Across Variables",
    x = "Percentage of Missing Values (%)",
    y = "Number of Variables"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 50, y = max(table(cut(missing_summary$missing_pct, 50))), 
           label = "50% threshold", vjust = -1, hjust = 1.1, color = "red", size = 3.5)

ggsave(file.path(output_dir, "01_missing_data_distribution.pdf"), 
       p1, width = 8, height = 6, device = "pdf")
ggsave(file.path(output_dir, "01_missing_data_distribution.png"), 
       p1, width = 8, height = 6, dpi = 300, device = "png")
cat("  ✓ Saved: 01_missing_data_distribution.pdf/png\n")

# ============================================================================
# Step 3: Select Numeric Variables for Correlation Analysis
# ============================================================================

cat("\nStep 3: Selecting numeric variables for correlation analysis...\n")

# Identify numeric variables (exclude categorical for now)
numeric_vars <- data %>%
  select_if(is.numeric) %>%
  names()

cat("  Found", length(numeric_vars), "numeric variables\n")

# Filter to variables with reasonable completeness (< 50% missing)
# and sufficient variance
complete_numeric <- data %>%
  select(all_of(numeric_vars)) %>%
  summarise_all(~ sum(!is.na(.)) / length(.) * 100) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "complete_pct") %>%
  filter(complete_pct >= 50) %>%
  pull(variable)

cat("  Variables with >=50% completeness:", length(complete_numeric), "\n")

# Further filter: remove variables with very low variance (near constant)
if (length(complete_numeric) > 0) {
  data_numeric <- data %>%
    select(all_of(complete_numeric))
  
  # Calculate variance for each variable
  var_summary <- data_numeric %>%
    summarise_all(~ var(., na.rm = TRUE)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "variance") %>%
    filter(!is.na(variance), variance > 1e-10)  # Remove near-zero variance
  
  high_var_vars <- var_summary$variable
  cat("  Variables with sufficient variance:", length(high_var_vars), "\n")
  
  # Limit to top variables if too many (for computational efficiency)
  MAX_VARS_CORR <- 500
  if (length(high_var_vars) > MAX_VARS_CORR) {
    cat("  Limiting to", MAX_VARS_CORR, "variables with highest variance for correlation analysis\n")
    high_var_vars <- var_summary %>%
      arrange(desc(variance)) %>%
      head(MAX_VARS_CORR) %>%
      pull(variable)
  }
} else {
  stop("No suitable numeric variables found for correlation analysis")
}

# ============================================================================
# Step 4: Correlation Analysis
# ============================================================================

cat("\nStep 4: Computing correlation matrix...\n")
cat("  This may take a few minutes...\n")

# Compute correlation matrix
data_corr <- data %>%
  select(all_of(high_var_vars))

# Use pairwise complete observations
cor_matrix <- cor(data_corr, use = "pairwise.complete.obs")

cat("  Correlation matrix computed:", nrow(cor_matrix), "x", ncol(cor_matrix), "\n")

# Extract upper triangle (excluding diagonal)
cor_values <- cor_matrix[upper.tri(cor_matrix, diag = FALSE)]
cat("  Total pairwise correlations:", length(cor_values), "\n")
cat("  Mean absolute correlation:", round(mean(abs(cor_values), na.rm = TRUE), 4), "\n")
cat("  Max absolute correlation:", round(max(abs(cor_values), na.rm = TRUE), 4), "\n")
cat("  Correlations > 0.7:", sum(abs(cor_values) > 0.7, na.rm = TRUE), "\n")
cat("  Correlations > 0.9:", sum(abs(cor_values) > 0.9, na.rm = TRUE), "\n")

# Plot 2: Distribution of correlation coefficients
p2 <- data.frame(correlation = cor_values) %>%
  ggplot(aes(x = correlation)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of Pairwise Correlations",
    subtitle = paste("Mean |r| =", round(mean(abs(cor_values), na.rm = TRUE), 3)),
    x = "Correlation Coefficient",
    y = "Frequency"
  ) +
  geom_vline(xintercept = c(-0.7, 0.7), linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 0.75, y = max(table(cut(cor_values, 100))), 
           label = "|r| > 0.7", vjust = -1, color = "red", size = 3.5) +
  xlim(-1, 1)

ggsave(file.path(output_dir, "02_correlation_distribution.pdf"), 
       p2, width = 8, height = 6, device = "pdf")
ggsave(file.path(output_dir, "02_correlation_distribution.png"), 
       p2, width = 8, height = 6, dpi = 300, device = "png")
cat("  ✓ Saved: 02_correlation_distribution.pdf/png\n")

# Plot 3: Distribution of absolute correlations (showing sparsity)
p3 <- data.frame(abs_correlation = abs(cor_values)) %>%
  ggplot(aes(x = abs_correlation)) +
  geom_histogram(bins = 100, fill = "darkgreen", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of Absolute Correlations (Sparsity Pattern)",
    subtitle = paste("Most correlations are near zero, showing sparsity"),
    x = "Absolute Correlation |r|",
    y = "Frequency"
  ) +
  geom_vline(xintercept = 0.7, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 0.75, y = max(table(cut(abs(cor_values), 100))), 
           label = "High correlation\nthreshold", vjust = -1, hjust = 0, color = "red", size = 3.5) +
  scale_x_continuous(breaks = seq(0, 1, 0.2))

ggsave(file.path(output_dir, "03_absolute_correlation_sparsity.pdf"), 
       p3, width = 8, height = 6, device = "pdf")
ggsave(file.path(output_dir, "03_absolute_correlation_sparsity.png"), 
       p3, width = 8, height = 6, dpi = 300, device = "png")
cat("  ✓ Saved: 03_absolute_correlation_sparsity.pdf/png\n")

# ============================================================================
# Step 5: Correlation Heatmap (Sample of Highly Correlated Variables)
# ============================================================================

cat("\nStep 5: Creating correlation heatmap...\n")

# Find variables with high correlations
high_corr_pairs <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
if (nrow(high_corr_pairs) > 0) {
  high_corr_vars <- unique(c(rownames(cor_matrix)[high_corr_pairs[, 1]], 
                              colnames(cor_matrix)[high_corr_pairs[, 2]]))
  
  # Limit to reasonable number for visualization
  MAX_HEATMAP_VARS <- 100
  if (length(high_corr_vars) > MAX_HEATMAP_VARS) {
    # Select variables with most high correlations
    var_corr_counts <- table(c(rownames(cor_matrix)[high_corr_pairs[, 1]], 
                               colnames(cor_matrix)[high_corr_pairs[, 2]]))
    high_corr_vars <- names(sort(var_corr_counts, decreasing = TRUE)[1:MAX_HEATMAP_VARS])
  }
  
  cat("  Selected", length(high_corr_vars), "variables with high correlations for heatmap\n")
  
  # Create correlation submatrix
  cor_subset <- cor_matrix[high_corr_vars, high_corr_vars]
  
  # For very large matrices, limit to top variables
  if (length(high_corr_vars) > 100) {
    # Sample variables for visualization
    high_corr_vars <- sample(high_corr_vars, 100)
    cor_subset <- cor_matrix[high_corr_vars, high_corr_vars]
  }
  
  # Convert to long format for ggplot2
  cor_df <- as.data.frame(cor_subset) %>%
    rownames_to_column("Var1") %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")
  
  # Create heatmap
  p4_heatmap <- cor_df %>%
    ggplot(aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, limits = c(-1, 1),
                        name = "Correlation") +
    labs(
      title = "Correlation Heatmap (Highly Correlated Variables)",
      subtitle = paste("Showing", length(high_corr_vars), "variables with |r| > 0.7"),
      x = "",
      y = ""
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      axis.text.y = element_text(size = 6),
      legend.position = "right"
    )
  
  ggsave(file.path(output_dir, "04_correlation_heatmap.pdf"), 
         p4_heatmap, width = 12, height = 12, device = "pdf")
  ggsave(file.path(output_dir, "04_correlation_heatmap.png"), 
         p4_heatmap, width = 12, height = 12, dpi = 300, device = "png")
  cat("  ✓ Saved: 04_correlation_heatmap.pdf/png\n")
} else {
  cat("  No highly correlated variable pairs found (|r| > 0.7)\n")
}

# ============================================================================
# Step 6: Sparsity Analysis - Data Completeness Matrix
# ============================================================================

cat("\nStep 6: Analyzing data sparsity patterns...\n")

# Sample variables for sparsity visualization
sample_vars <- sample(high_var_vars, min(100, length(high_var_vars)))

# Create completeness matrix (1 = present, 0 = missing)
completeness_matrix <- data %>%
  select(all_of(sample_vars)) %>%
  mutate_all(~ ifelse(is.na(.), 0, 1)) %>%
  as.matrix()

# Plot 5: Sparsity pattern (missing data heatmap)
# Sample rows for visualization
sample_rows <- sample(1:nrow(completeness_matrix), min(1000, nrow(completeness_matrix)))
sparsity_df <- completeness_matrix[sample_rows, ] %>%
  as.data.frame() %>%
  rownames_to_column("row_id") %>%
  pivot_longer(-row_id, names_to = "variable", values_to = "present") %>%
  mutate(row_id = as.numeric(row_id))

p5 <- sparsity_df %>%
  ggplot(aes(x = variable, y = row_id, fill = factor(present))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), 
                    labels = c("0" = "Missing", "1" = "Present"),
                    name = "Data") +
  labs(
    title = "Data Sparsity Pattern (Sample)",
    subtitle = "Red = Missing, Blue = Present",
    x = "Variables",
    y = "Observations (sample)"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(file.path(output_dir, "05_sparsity_pattern.pdf"), 
       p5, width = 12, height = 8, device = "pdf")
ggsave(file.path(output_dir, "05_sparsity_pattern.png"), 
       p5, width = 12, height = 8, dpi = 300, device = "png")
cat("  ✓ Saved: 05_sparsity_pattern.pdf/png\n")

# ============================================================================
# Step 7: Variable Selection - Most Complete Variables
# ============================================================================

cat("\nStep 7: Identifying most complete variables...\n")

# Calculate completeness for all numeric variables
completeness_all <- data %>%
  select_if(is.numeric) %>%
  summarise_all(~ sum(!is.na(.)) / length(.) * 100) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "completeness") %>%
  arrange(desc(completeness))

# Plot 6: Top 50 most complete variables
top_complete <- completeness_all %>%
  head(50)

p6 <- top_complete %>%
  ggplot(aes(x = reorder(variable, completeness), y = completeness)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Top 50 Most Complete Variables",
    x = "Variable",
    y = "Completeness (%)"
  ) +
  theme(axis.text.y = element_text(size = 7))

ggsave(file.path(output_dir, "06_top_complete_variables.pdf"), 
       p6, width = 10, height = 8, device = "pdf")
ggsave(file.path(output_dir, "06_top_complete_variables.png"), 
       p6, width = 10, height = 8, dpi = 300, device = "png")
cat("  ✓ Saved: 06_top_complete_variables.pdf/png\n")

# ============================================================================
# Step 7b: Variable Variance Distribution (Sparsity in Signal)
# ============================================================================

cat("\nStep 7b: Analyzing variable variance (signal sparsity)...\n")

# Calculate variance for numeric variables
variance_summary <- data %>%
  select_if(is.numeric) %>%
  summarise_all(~ var(., na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "variance") %>%
  filter(!is.na(variance), is.finite(variance)) %>%
  mutate(log_variance = log10(variance + 1e-10))

# Plot 7: Distribution of variances (showing sparsity in signal strength)
p7 <- variance_summary %>%
  ggplot(aes(x = log_variance)) +
  geom_histogram(bins = 50, fill = "purple", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of Variable Variances (Log Scale)",
    subtitle = "Most variables have low variance, indicating sparse signal",
    x = "Log10(Variance)",
    y = "Number of Variables"
  ) +
  geom_vline(xintercept = median(variance_summary$log_variance), 
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = median(variance_summary$log_variance), 
           y = max(table(cut(variance_summary$log_variance, 50))), 
           label = "Median", vjust = -1, color = "red", size = 3.5)

ggsave(file.path(output_dir, "07_variance_distribution.pdf"), 
       p7, width = 8, height = 6, device = "pdf")
ggsave(file.path(output_dir, "07_variance_distribution.png"), 
       p7, width = 8, height = 6, dpi = 300, device = "png")
cat("  ✓ Saved: 07_variance_distribution.pdf/png\n")

# ============================================================================
# Step 7c: Correlation Strength Categories
# ============================================================================

cat("\nStep 7c: Categorizing correlation strengths...\n")

# Categorize correlations
cor_categories <- data.frame(
  abs_corr = abs(cor_values)
) %>%
  mutate(
    category = case_when(
      abs_corr > 0.9 ~ "Very High (|r| > 0.9)",
      abs_corr > 0.7 ~ "High (0.7 < |r| <= 0.9)",
      abs_corr > 0.5 ~ "Moderate (0.5 < |r| <= 0.7)",
      abs_corr > 0.3 ~ "Low (0.3 < |r| <= 0.5)",
      TRUE ~ "Very Low (|r| <= 0.3)"
    )
  )

# Plot 8: Correlation strength categories
p8 <- cor_categories %>%
  count(category) %>%
  mutate(
    category = factor(category, 
                     levels = c("Very High (|r| > 0.9)", 
                               "High (0.7 < |r| <= 0.9)",
                               "Moderate (0.5 < |r| <= 0.7)",
                               "Low (0.3 < |r| <= 0.5)",
                               "Very Low (|r| <= 0.3)"))
  ) %>%
  ggplot(aes(x = category, y = n, fill = category)) +
  geom_col(alpha = 0.7) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title = "Distribution of Correlation Strengths",
    subtitle = "Most correlations are very low, demonstrating sparsity",
    x = "Correlation Category",
    y = "Number of Variable Pairs"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none") +
  scale_y_log10()

ggsave(file.path(output_dir, "08_correlation_categories.pdf"), 
       p8, width = 10, height = 6, device = "pdf")
ggsave(file.path(output_dir, "08_correlation_categories.png"), 
       p8, width = 10, height = 6, dpi = 300, device = "png")
cat("  ✓ Saved: 08_correlation_categories.pdf/png\n")

# ============================================================================
# Step 8: High Correlation Clusters
# ============================================================================

cat("\nStep 8: Identifying high correlation clusters...\n")

# Find clusters of highly correlated variables
high_corr_threshold <- 0.7
high_corr_matrix <- abs(cor_matrix) > high_corr_threshold & abs(cor_matrix) < 1

# Count high correlations per variable
var_high_corr_count <- rowSums(high_corr_matrix, na.rm = TRUE)

# Plot 9: Variables with most high correlations
top_high_corr_vars <- data.frame(
  variable = names(var_high_corr_count),
  n_high_corr = var_high_corr_count
) %>%
  arrange(desc(n_high_corr)) %>%
  head(30)

p9 <- top_high_corr_vars %>%
  ggplot(aes(x = reorder(variable, n_high_corr), y = n_high_corr)) +
  geom_col(fill = "darkred", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Top 30 Variables with Most High Correlations (|r| > 0.7)",
    subtitle = "These variables are highly correlated with many others",
    x = "Variable",
    y = "Number of High Correlations"
  ) +
  theme(axis.text.y = element_text(size = 7))

ggsave(file.path(output_dir, "09_high_correlation_variables.pdf"), 
       p9, width = 10, height = 8, device = "pdf")
ggsave(file.path(output_dir, "09_high_correlation_variables.png"), 
       p9, width = 10, height = 8, dpi = 300, device = "png")
cat("  ✓ Saved: 09_high_correlation_variables.pdf/png\n")

# ============================================================================
# Step 9: Multicollinearity Analysis using VIF
# ============================================================================

cat("\nStep 9: Calculating Variance Inflation Factor (VIF) for multicollinearity...\n")

# VIF threshold
VIF_THRESHOLD <- 10

# Select variables for VIF calculation (most complete numeric variables)
# Limit to reasonable number for computational efficiency
MAX_VIF_VARS <- 200
vif_vars <- high_var_vars[1:min(MAX_VIF_VARS, length(high_var_vars))]

cat("  Calculating VIF for", length(vif_vars), "variables...\n")
cat("  (This may take several minutes...)\n")

# Prepare data for VIF calculation (complete cases only)
data_vif <- data %>%
  select(all_of(vif_vars)) %>%
  # Remove rows with too many missing values
  filter(rowSums(is.na(.)) < length(vif_vars) * 0.5)

# Calculate completeness for each variable
var_completeness <- data_vif %>%
  summarise_all(~ sum(!is.na(.)) / length(.) * 100)

# Keep only variables with reasonable completeness
complete_vars <- names(var_completeness)[var_completeness >= 70]
data_vif <- data_vif %>%
  select(all_of(complete_vars))

cat("  Variables with >=70% completeness:", length(complete_vars), "\n")

if (length(complete_vars) < 2) {
  cat("  WARNING: Too few complete variables for VIF calculation\n")
  vif_results <- data.frame(
    variable = character(0),
    VIF = numeric(0)
  )
} else {
  # Impute missing values with median for VIF calculation
  # (VIF requires complete data)
  data_vif_imputed <- data_vif %>%
    mutate_all(~ ifelse(is.na(.), median(., na.rm = TRUE), .))
  
  # Calculate VIF using inverse correlation matrix method
  # VIF = diagonal of inverse correlation matrix
  # This is more accurate than correlation-based approximation
  cat("  Computing correlation matrix...\n")
  cor_matrix_vif <- cor(data_vif_imputed, use = "complete.obs")
  
  # Check for singular matrix (perfect correlations)
  if (any(is.na(cor_matrix_vif)) || any(!is.finite(cor_matrix_vif))) {
    cat("    WARNING: Correlation matrix contains NA or Inf values\n")
    # Replace with identity matrix for problematic variables
    cor_matrix_vif[is.na(cor_matrix_vif) | !is.finite(cor_matrix_vif)] <- 0
    diag(cor_matrix_vif) <- 1
  }
  
  cat("  Computing inverse correlation matrix for VIF...\n")
  
  # Initialize vif_values
  vif_values <- numeric(length(complete_vars))
  names(vif_values) <- complete_vars
  
  # Calculate VIF from inverse correlation matrix
  # VIF_i = (R^-1)_ii where R is the correlation matrix
  vif_calc_success <- tryCatch({
    cor_inv <- solve(cor_matrix_vif)
    vif_values <- diag(cor_inv)
    names(vif_values) <- complete_vars
    TRUE
  }, error = function(e) {
    cat("    WARNING: Could not invert correlation matrix:", e$message, "\n")
    cat("    Using correlation-based approximation instead...\n")
    FALSE
  })
  
  # Fallback: use correlation-based approximation if inversion failed
  if (!vif_calc_success) {
    for (i in 1:length(complete_vars)) {
      var_name <- complete_vars[i]
      # Get correlations with all other variables
      corrs <- cor_matrix_vif[var_name, -i]
      max_abs_corr <- max(abs(corrs), na.rm = TRUE)
      
      if (max_abs_corr < 0.999) {
        vif_values[i] <- 1 / (1 - max_abs_corr^2)
      } else {
        vif_values[i] <- 100  # Very high VIF
      }
      
      if (i %% 20 == 0) {
        cat("    Calculated VIF for", i, "/", length(complete_vars), "variables...\n")
      }
    }
  }
  
  # Create results dataframe
  vif_results <- data.frame(
    variable = complete_vars,
    VIF = vif_values
  ) %>%
    arrange(desc(VIF))
  
  # Identify variables exceeding threshold
  high_vif_vars <- vif_results %>%
    filter(VIF >= VIF_THRESHOLD)
  
  cat("\n  VIF Summary:\n")
  cat("    Variables with VIF >= ", VIF_THRESHOLD, ":", nrow(high_vif_vars), "\n")
  cat("    Maximum VIF:", round(max(vif_results$VIF, na.rm = TRUE), 2), "\n")
  cat("    Mean VIF:", round(mean(vif_results$VIF, na.rm = TRUE), 2), "\n")
  cat("    Median VIF:", round(median(vif_results$VIF, na.rm = TRUE), 2), "\n")
  
  if (nrow(high_vif_vars) > 0) {
    cat("\n  Top variables exceeding VIF threshold:\n")
    print(head(high_vif_vars, 20))
  }
}

# Plot 10: VIF distribution with threshold
n_high_vif <- sum(vif_results$VIF >= VIF_THRESHOLD, na.rm = TRUE)

p10 <- vif_results %>%
  mutate(
    exceeds_threshold = VIF >= VIF_THRESHOLD,
    variable_label = ifelse(exceeds_threshold, variable, "")
  ) %>%
  ggplot(aes(x = reorder(variable, VIF), y = VIF)) +
  geom_col(aes(fill = exceeds_threshold), alpha = 0.7) +
  geom_hline(yintercept = VIF_THRESHOLD, linetype = "dashed", 
             color = "red", linewidth = 1.5) +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "darkred"),
                    labels = c("FALSE" = paste("VIF <", VIF_THRESHOLD), 
                              "TRUE" = paste("VIF >=", VIF_THRESHOLD, "(Exclude)")),
                    name = "Status") +
  labs(
    title = "Variance Inflation Factor (VIF) for Multicollinearity Detection",
    subtitle = paste("Threshold: VIF =", VIF_THRESHOLD, 
                    "| Variables exceeding threshold:", n_high_vif,
                    "| Variables to exclude:", n_high_vif),
    x = "Variable (ordered by VIF)",
    y = "VIF Value"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7)) +
  annotate("text", x = length(complete_vars) * 0.1, y = VIF_THRESHOLD * 1.2, 
           label = paste("Threshold =", VIF_THRESHOLD, "\n(Variables above this line\nshould be excluded)"), 
           vjust = 0, hjust = 0, color = "red", size = 3.5, fontface = "bold") +
  # Add text annotation for number of excluded variables
  annotate("text", x = length(complete_vars) * 0.9, y = max(vif_results$VIF, na.rm = TRUE) * 0.9, 
           label = paste("Excluded variables:", n_high_vif), 
           vjust = 1, hjust = 1, color = "darkred", size = 4, fontface = "bold")

ggsave(file.path(output_dir, "10_vif_multicollinearity.pdf"), 
       p10, width = 14, height = 8, device = "pdf")
ggsave(file.path(output_dir, "10_vif_multicollinearity.png"), 
       p10, width = 14, height = 8, dpi = 300, device = "png")
cat("  ✓ Saved: 10_vif_multicollinearity.pdf/png\n")

# Plot 11: VIF distribution histogram
p11 <- vif_results %>%
  ggplot(aes(x = VIF)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = VIF_THRESHOLD, linetype = "dashed", 
             color = "red", linewidth = 1.5) +
  labs(
    title = "Distribution of VIF Values",
    subtitle = paste("Variables with VIF >=", VIF_THRESHOLD, 
                    "indicate multicollinearity"),
    x = "VIF Value",
    y = "Number of Variables"
  ) +
  annotate("text", x = VIF_THRESHOLD, 
           y = max(table(cut(vif_results$VIF, 50))), 
           label = paste("Threshold =", VIF_THRESHOLD), 
           vjust = -1, hjust = 1.1, color = "red", size = 4, fontface = "bold") +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30))

ggsave(file.path(output_dir, "11_vif_distribution.pdf"), 
       p11, width = 10, height = 6, device = "pdf")
ggsave(file.path(output_dir, "11_vif_distribution.png"), 
       p11, width = 10, height = 6, dpi = 300, device = "png")
cat("  ✓ Saved: 11_vif_distribution.pdf/png\n")

# Save VIF results
write_csv(vif_results, file.path(output_dir, "vif_results.csv"))
if (nrow(high_vif_vars) > 0) {
  write_csv(high_vif_vars, file.path(output_dir, "high_vif_variables.csv"))
  cat("  ✓ Saved: vif_results.csv and high_vif_variables.csv\n")
} else {
  cat("  ✓ Saved: vif_results.csv\n")
}

# ============================================================================
# Step 10: Summary Statistics
# ============================================================================

cat("\nStep 10: Computing summary statistics...\n")

# Create summary table
summary_stats <- list(
  total_variables = ncol(data),
  numeric_variables = length(numeric_vars),
  variables_50pct_complete = length(complete_numeric),
  variables_high_variance = length(high_var_vars),
  total_observations = nrow(data),
  mean_missing_pct = mean(missing_summary$missing_pct),
  median_missing_pct = median(missing_summary$missing_pct),
  mean_abs_correlation = mean(abs(cor_values), na.rm = TRUE),
  median_abs_correlation = median(abs(cor_values), na.rm = TRUE),
  high_corr_pairs = sum(abs(cor_values) > 0.7, na.rm = TRUE),
  very_high_corr_pairs = sum(abs(cor_values) > 0.9, na.rm = TRUE),
  variables_analyzed_vif = if(exists("vif_results")) nrow(vif_results) else 0,
  variables_vif_above_threshold = if(exists("high_vif_vars")) nrow(high_vif_vars) else 0,
  max_vif = if(exists("vif_results")) max(vif_results$VIF, na.rm = TRUE) else NA,
  mean_vif = if(exists("vif_results")) mean(vif_results$VIF, na.rm = TRUE) else NA
)

# Save summary
summary_df <- data.frame(
  Statistic = names(summary_stats),
  Value = unlist(summary_stats)
)

write_csv(summary_df, file.path(output_dir, "summary_statistics.csv"))
cat("  ✓ Saved: summary_statistics.csv\n")

# Print summary
cat("\nSummary Statistics:\n")
print(summary_df)

# ============================================================================
# Step 11: Create LaTeX Figure Code
# ============================================================================

cat("\nStep 11: Generating LaTeX figure code...\n")

# Prepare VIF caption text
vif_caption_text <- if(exists("high_vif_vars") && nrow(high_vif_vars) > 0) {
  paste("A total of", nrow(high_vif_vars), "variables exceed the threshold and should be considered for removal.")
} else {
  "No variables exceed the VIF threshold of 10."
}

latex_code <- paste0(
  "% LaTeX code for including EDA figures\n",
  "% Generated automatically by eda_for_susie.R\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{EDA_plots/01_missing_data_distribution.pdf}\n",
  "  \\caption{Distribution of missing data percentages across all variables. ",
  "Most variables have some missing data, demonstrating sparsity in the dataset.}\n",
  "  \\label{fig:missing_dist}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{EDA_plots/02_correlation_distribution.pdf}\n",
  "  \\caption{Distribution of pairwise correlation coefficients. ",
  "The distribution is centered near zero, showing that most variable pairs are weakly correlated.}\n",
  "  \\label{fig:corr_dist}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{EDA_plots/03_absolute_correlation_sparsity.pdf}\n",
  "  \\caption{Distribution of absolute correlations demonstrating sparsity. ",
  "Most correlations are near zero, indicating sparse relationships among variables.}\n",
  "  \\label{fig:abs_corr_sparsity}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.9\\textwidth]{EDA_plots/04_correlation_heatmap.pdf}\n",
  "  \\caption{Correlation heatmap showing highly correlated variable groups. ",
  "Some variables show strong correlations (|r| > 0.7), indicating multicollinearity.}\n",
  "  \\label{fig:corr_heatmap}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.9\\textwidth]{EDA_plots/05_sparsity_pattern.pdf}\n",
  "  \\caption{Data sparsity pattern showing missing values (red) and present values (blue). ",
  "The sparse pattern indicates that not all variables are observed for all participants.}\n",
  "  \\label{fig:sparsity_pattern}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{EDA_plots/06_top_complete_variables.pdf}\n",
  "  \\caption{Top 50 most complete variables in the dataset.}\n",
  "  \\label{fig:top_complete}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{EDA_plots/07_variance_distribution.pdf}\n",
  "  \\caption{Distribution of variable variances (log scale). ",
  "Most variables have low variance, indicating sparse signal strength.}\n",
  "  \\label{fig:variance_dist}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{EDA_plots/08_correlation_categories.pdf}\n",
  "  \\caption{Distribution of correlation strengths. ",
  "The vast majority of variable pairs have very low correlations (|r| <= 0.3), ",
  "demonstrating sparsity in relationships.}\n",
  "  \\label{fig:corr_categories}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{EDA_plots/09_high_correlation_variables.pdf}\n",
  "  \\caption{Top 30 variables with the most high correlations (|r| > 0.7). ",
  "These variables form highly correlated clusters, indicating multicollinearity.}\n",
  "  \\label{fig:high_corr_vars}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.9\\textwidth]{EDA_plots/10_vif_multicollinearity.pdf}\n",
  "  \\caption{Variance Inflation Factor (VIF) for multicollinearity detection. ",
  "Variables with VIF >= 10 (red bars) indicate high multicollinearity and should be excluded. ",
  vif_caption_text, "}\n",
  "  \\label{fig:vif_multicollinearity}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{EDA_plots/11_vif_distribution.pdf}\n",
  "  \\caption{Distribution of VIF values. Most variables have VIF < 10, ",
  "indicating acceptable levels of multicollinearity.}\n",
  "  \\label{fig:vif_distribution}\n",
  "\\end{figure}\n"
)

writeLines(latex_code, file.path(output_dir, "latex_figures.tex"))
cat("  ✓ Saved: latex_figures.tex\n")

cat("\n============================================================================\n")
cat("EDA COMPLETE\n")
cat("============================================================================\n")
cat("All plots saved to:", output_dir, "\n")
cat("LaTeX code saved to:", file.path(output_dir, "latex_figures.tex"), "\n")
cat("\nKey Findings:\n")
cat("  - Mean absolute correlation:", round(summary_stats$mean_abs_correlation, 3), "\n")
cat("  - High correlation pairs (|r| > 0.7):", summary_stats$high_corr_pairs, "\n")
cat("  - Mean missing data:", round(summary_stats$mean_missing_pct, 1), "%\n")
cat("  - Variables suitable for analysis:", summary_stats$variables_high_variance, "\n")
if (exists("high_vif_vars") && nrow(high_vif_vars) > 0) {
  cat("  - Variables with VIF >= 10 (multicollinearity):", nrow(high_vif_vars), "\n")
  cat("    These variables should be considered for removal:\n")
  if (nrow(high_vif_vars) <= 20) {
    cat("    ", paste(high_vif_vars$variable, collapse = ", "), "\n")
  } else {
    cat("    ", paste(head(high_vif_vars$variable, 20), collapse = ", "), "\n")
    cat("    ... and", nrow(high_vif_vars) - 20, "more (see high_vif_variables.csv)\n")
  }
} else {
  cat("  - Variables with VIF >= 10: 0 (no multicollinearity issues detected)\n")
}
cat("============================================================================\n")

