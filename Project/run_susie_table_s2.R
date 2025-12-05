# ============================================================================
# SuSiE-R Analysis for NHANES Data - Table S2 Variables Only
# ============================================================================
# This script:
# 1. Loads prepared data
# 2. Filters to only Table S2 (Nutrition Model) variables
# 3. Splits into training and test sets
# 4. Standardizes data
# 5. Runs susieR variable selection
# 6. Makes predictions
# 7. Calculates performance metrics
# 8. Creates plots similar to susieR fine-mapping example
# 9. Generates LaTeX-ready plots and tables
# ============================================================================

library(tidyverse)
library(pROC)
library(caret)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)

# Install and load susieR if not available
if (!require(susieR, quietly = TRUE)) {
  cat("WARNING: susieR package not found.\n")
  cat("Please install it using:\n")
  cat("  install.packages('remotes')\n")
  cat("  remotes::install_github('stephenslab/susieR')\n")
  stop("susieR package is required. Please install it first.")
}

# ============================================================================
# SETUP
# ============================================================================

base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"
data_dir <- file.path(base_dir, "susie_data")
output_dir <- file.path(base_dir, "susie_results_table_s2")

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ============================================================================
# CONFIGURATION: Choose SuSiE method
# ============================================================================

SUSIE_METHOD <- "susie_rss"  # Change to "susie" to use individual-level data method

# ============================================================================
# Define Table S2 Variables
# ============================================================================

# Table S2: Nutrition Model Variables
# Note: NHANES uses DRXT prefix for Day 1 total nutrients
table_s2_vars <- list(
  "Alc_int" = "DRXTALCO",  # Alcohol intake (g)
  "Ca_int" = "DRXTCALC",  # Calcium intake (mg)
  "Caff_int" = "DRXTCAFF",  # Caffeine intake (mg)
  "Cho_int" = "DRXTCHOL",  # Cholesterol intake (mg)
  "Cu_int" = "DRXTCOPP",  # Copper intake (mg)
  "Fe_int" = "DRXTIRON",  # Iron intake (mg)
  "Fib_int" = "DRXTFIBE",  # Dietary fiber (g)
  "Fish_int" = "DRD370",  # Fish intake
  "Fol_int" = "DRXTFOLA",  # Total folate (μg)
  "K_int" = "DRXTPOTA",  # Potassium intake (mg)
  "MFA_int" = "DRXTMFAT",  # Monounsaturated fatty acids (g)
  "Mg_int" = "DRXTMAGN",  # Magnesium intake (mg)
  "Milk_int" = "DMQMILIZ",  # Milk intake
  "Na_int" = "DRXTSODI",  # Sodium intake (mg)
  "Nia_int" = "DRXTNIAC",  # Niacin intake (mg)
  "PFA_int" = "DRXTPFAT",  # Polyunsaturated fatty acids (g)
  "Prot_int" = "DRXTPROT",  # Protein intake (g)
  "Salt_freq" = "DRQSPREP",  # Salt frequency
  "Salt_prep" = "DRQSPREP",  # Salt in preparation
  "Salt_table" = "DRQSDIET",  # Salt at table
  "SFA_int" = "DRXTSFAT",  # Saturated fatty acids (g)
  "Sug_int" = "DRXTSUGR",  # Total sugars (g)
  "Theo_int" = "DRXTTHEO",  # Theobromine (mg)
  "Vit_A_int" = "DRXTVARA",  # Vitamin A (μg)
  "Vit_B1_int" = "DRXTVB1",  # Thiamin (mg)
  "Vit_B12_int" = "DRXTVB12",  # Vitamin B12 (μg)
  "Vit_B2_int" = "DRXTVB2",  # Riboflavin (mg)
  "Vit_B6_int" = "DRXTVB6",  # Vitamin B6 (mg)
  "Vit_C_int" = "DRXTVC",  # Vitamin C (mg)
  "Vit_E_int" = "DRXTATOC",  # Vitamin E as alpha-tocopherol (μg)
  "Vit_K_int" = "DRXTVK",  # Vitamin K (μg)
  "Wat_int" = "DRX.320Z",  # Total plain water (g)
  "Zn_int" = "DRXTZINC"  # Zinc intake (mg)
)

cat("============================================================================\n")
cat("SuSiE-R Analysis for NHANES Data - Table S2 Variables Only\n")
cat("============================================================================\n")
cat("Method:", SUSIE_METHOD, "\n")
cat("Table S2 variables:", length(table_s2_vars), "\n")
cat("============================================================================\n\n")

# Set high-quality plot parameters for LaTeX
theme_set(theme_bw(base_size = 12))
theme_update(
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 12, hjust = 0.5),
  axis.title = element_text(size = 11),
  axis.text = element_text(size = 10),
  legend.title = element_text(size = 11),
  legend.text = element_text(size = 10)
)

# ============================================================================
# Step 1: Load Data and Filter to Table S2 Variables
# ============================================================================

cat("Step 1: Loading data and filtering to Table S2 variables...\n")

# Load the full dataset
full_data <- readRDS(file.path(base_dir, "nhanes_filtered_for_susie.rds"))

# Get Table S2 variable names (NHANES codes)
s2_nhanes_vars <- unlist(table_s2_vars)

# Find variables that exist in the dataset
available_s2_vars <- intersect(s2_nhanes_vars, names(full_data))
missing_s2_vars <- setdiff(s2_nhanes_vars, names(full_data))

cat("  Available Table S2 variables:", length(available_s2_vars), "/", length(s2_nhanes_vars), "\n")
if (length(missing_s2_vars) > 0) {
  cat("  Missing variables:", paste(missing_s2_vars, collapse = ", "), "\n")
}

# Identify outcome variable
outcome_candidates <- c("CVD_Death", "MORTSTAT", "UCOD_LEADING")
outcome_var <- NULL
for (candidate in outcome_candidates) {
  if (candidate %in% names(full_data)) {
    outcome_var <- candidate
    break
  }
}

if (is.null(outcome_var)) {
  stop("ERROR: No outcome variable found. Expected one of: ", paste(outcome_candidates, collapse = ", "))
}

cat("  Outcome variable:", outcome_var, "\n")

# Create dataset with only Table S2 variables and outcome
data_s2 <- full_data %>%
  select(SEQN, all_of(available_s2_vars), all_of(outcome_var))

cat("  Final dataset dimensions:", nrow(data_s2), "rows x", ncol(data_s2), "columns\n")
cat("  Table S2 variables included:", length(available_s2_vars), "\n\n")

# ============================================================================
# Step 2: Prepare Data for SuSiE (Encode, Handle Missing, Create Matrices)
# ============================================================================

cat("Step 2: Preparing data for SuSiE...\n")

# Remove rows with missing outcome
data_s2 <- data_s2 %>%
  filter(!is.na(!!sym(outcome_var)))

cat("  Complete cases for outcome:", nrow(data_s2), "\n")

# Separate predictors and outcome
X_data <- data_s2 %>%
  select(-SEQN, -all_of(outcome_var))

y_data <- data_s2[[outcome_var]]

# Handle missing values: remove variables with >50% missing, impute rest
missing_pct <- sapply(X_data, function(x) mean(is.na(x)) * 100)
vars_to_keep <- names(X_data)[missing_pct <= 50]

cat("  Variables with <=50% missing:", length(vars_to_keep), "/", ncol(X_data), "\n")

X_data <- X_data %>%
  select(all_of(vars_to_keep))

# Impute remaining missing values (median for numeric, mode for categorical)
for (var in names(X_data)) {
  if (is.numeric(X_data[[var]])) {
    X_data[[var]][is.na(X_data[[var]])] <- median(X_data[[var]], na.rm = TRUE)
  } else {
    # For categorical, use most frequent value
    mode_val <- names(sort(table(X_data[[var]]), decreasing = TRUE))[1]
    X_data[[var]][is.na(X_data[[var]])] <- mode_val
  }
}

# Convert to numeric matrix (encode categorical if needed)
# For simplicity, convert factors to numeric
X_matrix <- as.matrix(X_data)
for (i in 1:ncol(X_matrix)) {
  if (!is.numeric(X_matrix[, i])) {
    X_matrix[, i] <- as.numeric(as.factor(X_matrix[, i]))
  }
}

# Remove constant variables
var_check <- apply(X_matrix, 2, function(x) stats::var(x, na.rm = TRUE))
non_constant <- which(var_check > 1e-10)
X_matrix <- X_matrix[, non_constant, drop = FALSE]

cat("  Final X matrix:", nrow(X_matrix), "rows x", ncol(X_matrix), "columns\n")
cat("  Final y vector:", length(y_data), "values\n")
cat("  Outcome distribution:\n")
print(table(y_data))

# Save X and y matrices
saveRDS(X_matrix, file.path(output_dir, "X_matrix.rds"))
saveRDS(y_data, file.path(output_dir, "y_vector.rds"))
saveRDS(colnames(X_matrix), file.path(output_dir, "variable_names.rds"))

cat("  Saved X and y matrices to", output_dir, "\n\n")

# ============================================================================
# Step 3: Split Data into Training and Test Sets
# ============================================================================

cat("Step 3: Splitting data into training (70%) and test (30%) sets...\n")

set.seed(42)
train_indices <- createDataPartition(y_data, p = 0.7, list = FALSE)

X_train <- X_matrix[train_indices, ]
y_train <- y_data[train_indices]
X_test <- X_matrix[-train_indices, ]
y_test <- y_data[-train_indices]

cat("  Training set:", nrow(X_train), "observations\n")
cat("  Test set:", nrow(X_test), "observations\n")
cat("  Training outcome distribution:\n")
print(table(y_train))
cat("  Test outcome distribution:\n")
print(table(y_test))
cat("\n")

# ============================================================================
# Step 4: Undersample Training Data to Balance Classes
# ============================================================================

cat("Step 4: Undersampling training data to balance classes...\n")

class_counts <- table(y_train)
minority_class <- as.numeric(names(class_counts)[which.min(class_counts)])
majority_class <- as.numeric(names(class_counts)[which.max(class_counts)])
minority_count <- min(class_counts)
majority_count <- max(class_counts)

cat("  Minority class:", minority_class, "with", minority_count, "samples\n")
cat("  Majority class:", majority_class, "with", majority_count, "samples\n")

minority_indices <- which(y_train == minority_class)
majority_indices <- which(y_train == majority_class)

set.seed(42)
majority_sampled_indices <- sample(majority_indices, size = minority_count, replace = FALSE)
balanced_indices <- c(minority_indices, majority_sampled_indices)
balanced_indices <- sample(balanced_indices)

X_train_balanced <- X_train[balanced_indices, ]
y_train_balanced <- y_train[balanced_indices]

cat("  Balanced training set:", nrow(X_train_balanced), "observations\n")
cat("  Balanced class distribution:\n")
print(table(y_train_balanced))

X_train <- X_train_balanced
y_train <- y_train_balanced

cat("  Undersampling complete.\n\n")

# ============================================================================
# Step 5: Standardize Data
# ============================================================================

cat("Step 5: Standardizing data...\n")

X_train_mean <- apply(X_train, 2, function(x) mean(x, na.rm = TRUE))
X_train_sd <- apply(X_train, 2, function(x) sd(x, na.rm = TRUE))
X_train_sd[X_train_sd == 0] <- 1

X_train_scaled <- scale(X_train, center = X_train_mean, scale = X_train_sd)
X_test_scaled <- scale(X_test, center = X_train_mean, scale = X_train_sd)

cat("  Training data standardized\n")
cat("  Test data standardized using training statistics\n\n")

# ============================================================================
# Step 6: Run SuSiE-R
# ============================================================================

cat("Step 6: Running SuSiE-R variable selection...\n")
cat("  Method:", SUSIE_METHOD, "\n\n")

n <- nrow(X_train_scaled)
p <- ncol(X_train_scaled)

cat("  Sample size (n):", n, "\n")
cat("  Number of predictors (p):", p, "\n\n")

L <- 10
max_iter <- 200
tol <- 0.001

if (SUSIE_METHOD == "susie_rss") {
  cat("  Using susie_rss() with summary statistics\n")
  
  # Compute correlation matrix R
  R <- cor(X_train_scaled, use = "pairwise.complete.obs")
  R[is.na(R)] <- 0
  diag(R) <- 1
  
  # Compute z-scores from univariate regressions
  z_scores <- numeric(p)
  for (j in 1:p) {
    if (var(X_train_scaled[, j], na.rm = TRUE) < 1e-10) {
      z_scores[j] <- 0
      next
    }
    tryCatch({
      fit_univariate <- lm(y_train ~ X_train_scaled[, j])
      coef_summary <- summary(fit_univariate)$coefficients
      if (nrow(coef_summary) >= 2 && !is.na(coef_summary[2, 1]) && !is.na(coef_summary[2, 2])) {
        bhat <- coef_summary[2, 1]
        shat <- coef_summary[2, 2]
        if (abs(shat) > 1e-10) {
          z_scores[j] <- bhat / shat
        }
      }
    }, error = function(e) {
      z_scores[j] <<- 0
    })
  }
  
  cat("  Summary statistics computed\n")
  
  # Fit SuSiE-RSS model
  fit <- susie_rss(z = z_scores,
                   R = R,
                   n = n,
                   L = L,
                   estimate_residual_variance = TRUE,
                   estimate_prior_variance = TRUE,
                   scaled_prior_variance = 0.2,
                   max_iter = max_iter,
                   tol = tol,
                   verbose = TRUE,
                   refine = FALSE)
  
} else {
  cat("  Using susie() with individual-level data\n")
  fit <- susie(X_train_scaled, y_train, 
               L = L,
               estimate_residual_variance = TRUE,
               estimate_prior_variance = TRUE,
               scaled_prior_variance = 0.2,
               max_iter = max_iter,
               tol = tol,
               verbose = TRUE,
               refine = FALSE)
}

cat("\n  SuSiE-R fit completed\n")
cat("  Converged:", fit$converged, "\n")
cat("  Number of iterations:", fit$niter, "\n")
cat("  Number of credible sets:", length(fit$sets$cs), "\n")
if (length(fit$sets$cs) > 0) {
  cat("  Variables in credible sets:", length(unlist(fit$sets$cs)), "\n")
  # Get selected variables from credible sets
  selected_vars <- unique(unlist(fit$sets$cs))
  cat("  Unique selected variables:", length(selected_vars), "\n")
  
  # Also get variables with high PIP (Posterior Inclusion Probability)
  high_pip_vars <- which(fit$pip > 0.5)  # Variables with PIP > 0.5
  cat("  Variables with PIP > 0.5:", length(high_pip_vars), "\n")
  
  # Print credible sets summary (similar to example)
  cat("\n  Credible Sets Summary:\n")
  cat("  ", paste(rep("=", 60), collapse = ""), "\n")
  for (i in 1:length(fit$sets$cs)) {
    cs_vars <- fit$sets$cs[[i]]
    cat("  CS", i, ":", paste(cs_vars, collapse = ", "), "\n")
    if (nrow(fit$sets$purity) >= i) {
      cat("    Purity - Min:", round(fit$sets$purity[i, "min.abs.corr"], 4),
          ", Mean:", round(fit$sets$purity[i, "mean.abs.corr"], 4),
          ", Median:", round(fit$sets$purity[i, "median.abs.corr"], 4), "\n")
    }
    cat("    Coverage:", round(fit$sets$coverage[i], 4), "\n")
  }
  cat("  ", paste(rep("=", 60), collapse = ""), "\n")
} else {
  cat("  WARNING: No credible sets found\n")
  # Fall back to high PIP variables
  high_pip_vars <- which(fit$pip > 0.5)
  selected_vars <- high_pip_vars
  cat("  Using variables with PIP > 0.5:", length(selected_vars), "\n")
}

cat("\n")

# ============================================================================
# Step 7: Make Predictions and Calculate Metrics
# ============================================================================

cat("Step 7: Making predictions and calculating performance metrics...\n")

var_names <- colnames(X_matrix)

if (SUSIE_METHOD == "susie_rss") {
  coef_posterior <- susie_get_posterior_mean(fit)
  y_train_pred <- as.vector(X_train_scaled %*% coef_posterior)
  y_test_pred <- as.vector(X_test_scaled %*% coef_posterior)
  
  y_train_prob <- as.vector(1 / (1 + exp(-y_train_pred)))
  y_test_prob <- as.vector(1 / (1 + exp(-y_test_pred)))
  
  # Store coefficients for later use
  coef <- c(0, coef_posterior)
} else {
  coef <- coef(fit)
  if (length(coef) == p + 1) {
    y_train_pred <- as.vector(X_train_scaled %*% coef[-1] + coef[1])
    y_test_pred <- as.vector(X_test_scaled %*% coef[-1] + coef[1])
  } else {
    y_train_pred <- as.vector(X_train_scaled %*% coef)
    y_test_pred <- as.vector(X_test_scaled %*% coef)
  }
  y_train_prob <- as.vector(1 / (1 + exp(-y_train_pred)))
  y_test_prob <- as.vector(1 / (1 + exp(-y_test_pred)))
}

y_train_pred_class <- ifelse(y_train_prob > 0.5, 1, 0)
y_test_pred_class <- ifelse(y_test_prob > 0.5, 1, 0)

# Calculate metrics
calculate_metrics <- function(y_true, y_pred, y_prob) {
  cm <- confusionMatrix(factor(y_pred, levels = c(0, 1)), 
                       factor(y_true, levels = c(0, 1)), 
                       positive = "1")
  
  acc <- cm$overall["Accuracy"]
  recall <- cm$byClass["Sensitivity"]
  specificity <- cm$byClass["Specificity"]
  precision <- cm$byClass["Precision"]
  f1 <- cm$byClass["F1"]
  balanced_acc <- (recall + specificity) / 2
  
  if (length(unique(y_true)) == 2) {
    roc_obj <- roc(y_true, y_prob, quiet = TRUE)
    auc <- as.numeric(auc(roc_obj))
  } else {
    auc <- NA
  }
  
  return(list(
    Accuracy = acc,
    Balanced_Accuracy = balanced_acc,
    AUC = auc,
    Recall = recall,
    Specificity = specificity,
    Precision = precision,
    F1_Score = f1,
    ConfusionMatrix = cm
  ))
}

metrics_train <- calculate_metrics(y_train, y_train_pred_class, y_train_prob)
metrics_test <- calculate_metrics(y_test, y_test_pred_class, y_test_prob)

cat("  Training metrics calculated\n")
cat("  Test metrics calculated\n\n")

# ============================================================================
# Step 7.5: Create Performance Tables
# ============================================================================

cat("Step 7.5: Creating performance tables...\n")

# Create comparison table
metrics_table <- data.frame(
  Metric = c("Accuracy", "Balanced Accuracy", "AUC", "Recall", "Specificity", "Precision", "F1-Score"),
  Training = c(
    round(metrics_train$Accuracy, 4),
    round(metrics_train$Balanced_Accuracy, 4),
    round(metrics_train$AUC, 4),
    round(metrics_train$Recall, 4),
    round(metrics_train$Specificity, 4),
    round(metrics_train$Precision, 4),
    round(metrics_train$F1_Score, 4)
  ),
  Test = c(
    round(metrics_test$Accuracy, 4),
    round(metrics_test$Balanced_Accuracy, 4),
    round(metrics_test$AUC, 4),
    round(metrics_test$Recall, 4),
    round(metrics_test$Specificity, 4),
    round(metrics_test$Precision, 4),
    round(metrics_test$F1_Score, 4)
  )
)

# Save as CSV
write_csv(metrics_table, file.path(output_dir, "performance_metrics.csv"))

# Create LaTeX table
latex_table <- kable(metrics_table, 
                     format = "latex",
                     booktabs = TRUE,
                     caption = "Performance Metrics: Training vs Test",
                     label = "tab:performance_metrics") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

writeLines(latex_table, file.path(output_dir, "performance_metrics_table.tex"))

cat("  Performance table saved\n\n")

# ============================================================================
# Step 7.6: Create Plots Similar to susieR Fine-mapping Example
# ============================================================================
# Reference: https://stephenslab.github.io/susieR/articles/finemapping.html

cat("Step 7.6: Creating plots similar to susieR fine-mapping example...\n")

# Check if susie_plot function is available
has_susie_plot <- exists("susie_plot", where = asNamespace("susieR"), mode = "function")

# Compute z-scores if not already computed (for susie method)
if (SUSIE_METHOD == "susie" && !exists("z_scores")) {
  cat("  Computing z-scores from univariate regressions...\n")
  z_scores <- numeric(p)
  for (j in 1:p) {
    if (var(X_train_scaled[, j], na.rm = TRUE) < 1e-10) {
      z_scores[j] <- 0
      next
    }
    tryCatch({
      fit_univariate <- lm(y_train ~ X_train_scaled[, j])
      coef_summary <- summary(fit_univariate)$coefficients
      if (nrow(coef_summary) >= 2 && !is.na(coef_summary[2, 1]) && !is.na(coef_summary[2, 2])) {
        bhat <- coef_summary[2, 1]
        shat <- coef_summary[2, 2]
        if (abs(shat) > 1e-10) {
          z_scores[j] <- bhat / shat
        }
      }
    }, error = function(e) {
      z_scores[j] <<- 0
    })
  }
}

# Plot 1: Z-Scores Plot (similar to susie_plot with y="z")
if (exists("z_scores") && length(z_scores) > 0) {
  if (has_susie_plot) {
    # Try using susie_plot if available
    tryCatch({
      pdf(file.path(output_dir, "z_scores_susie_plot.pdf"), width = 10, height = 6)
      susie_plot(z_scores, y = "z")
      dev.off()
      
      png(file.path(output_dir, "z_scores_susie_plot.png"), width = 10, height = 6, units = "in", res = 300)
      susie_plot(z_scores, y = "z")
      dev.off()
      cat("  ✓ Saved: z_scores_susie_plot.pdf/png (using susie_plot)\n")
    }, error = function(e) {
      cat("  Note: susie_plot failed, using ggplot2 instead\n")
    })
  }
  
  # Always create ggplot2 version as well
  z_df <- data.frame(
    Variable = var_names[1:length(z_scores)],
    Z_Score = z_scores,
    Index = 1:length(z_scores)
  ) %>%
    arrange(Index)  # Keep original order for x-axis
  
  p_z <- ggplot(z_df, aes(x = Index, y = Z_Score)) +
    geom_point(size = 1.5, alpha = 0.6, color = "steelblue") +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red", linewidth = 1) +
    labs(
      title = "Z-Scores from Univariate Regressions - Table S2 Variables",
      subtitle = "Red dashed lines indicate |z| = 2",
      x = "Variable Index",
      y = "Z-Score"
    ) +
    theme_bw()
  
  ggsave(file.path(output_dir, "z_scores_plot.pdf"), p_z, width = 10, height = 6, device = "pdf")
  ggsave(file.path(output_dir, "z_scores_plot.png"), p_z, width = 10, height = 6, dpi = 300, device = "png")
  cat("  ✓ Saved: z_scores_plot.pdf/png\n")
}

# Plot 2: PIP Plot (similar to susie_plot with y="PIP")
if (has_susie_plot) {
  tryCatch({
    pdf(file.path(output_dir, "pip_susie_plot.pdf"), width = 10, height = 6)
    susie_plot(fit, y = "PIP")
    dev.off()
    
    png(file.path(output_dir, "pip_susie_plot.png"), width = 10, height = 6, units = "in", res = 300)
    susie_plot(fit, y = "PIP")
    dev.off()
    cat("  ✓ Saved: pip_susie_plot.pdf/png (using susie_plot)\n")
  }, error = function(e) {
    cat("  Note: susie_plot failed, using ggplot2 instead\n")
  })
}

# Always create ggplot2 version of PIP plot
pip_df <- data.frame(
  Variable = var_names[1:length(fit$pip)],
  PIP = fit$pip,
  Index = 1:length(fit$pip)
) %>%
  arrange(Index)  # Keep original order for x-axis

# Highlight credible sets
pip_df$InCS <- FALSE
pip_df$CS <- NA
if (length(fit$sets$cs) > 0) {
  for (cs_idx in 1:length(fit$sets$cs)) {
    cs_vars <- fit$sets$cs[[cs_idx]]
    pip_df$InCS[pip_df$Index %in% cs_vars] <- TRUE
    pip_df$CS[pip_df$Index %in% cs_vars] <- paste0("CS", cs_idx)
  }
}

p_pip <- ggplot(pip_df, aes(x = Index, y = PIP)) +
  geom_point(aes(color = InCS), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                     labels = c("TRUE" = "In Credible Set", "FALSE" = "Not in CS"),
                     name = "") +
  labs(
    title = "Posterior Inclusion Probabilities (PIP) - Table S2 Variables",
    subtitle = paste("Variables in credible sets highlighted in red"),
    x = "Variable Index",
    y = "Posterior Inclusion Probability (PIP)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "pip_plot.pdf"), p_pip, width = 10, height = 6, device = "pdf")
ggsave(file.path(output_dir, "pip_plot.png"), p_pip, width = 10, height = 6, dpi = 300, device = "png")
cat("  ✓ Saved: pip_plot.pdf/png\n")

# Plot 3: Credible Sets with Purity Information
if (length(fit$sets$cs) > 0) {
  cs_summary <- data.frame(
    CS = paste0("CS", 1:length(fit$sets$cs)),
    Size = sapply(fit$sets$cs, length),
    Coverage = fit$sets$coverage,
    MinAbsCorr = fit$sets$purity[, "min.abs.corr"],
    MeanAbsCorr = fit$sets$purity[, "mean.abs.corr"],
    MedianAbsCorr = fit$sets$purity[, "median.abs.corr"]
  )
  
  # Save credible sets summary table
  write_csv(cs_summary, file.path(output_dir, "credible_sets_summary.csv"))
  cat("  ✓ Saved: credible_sets_summary.csv\n")
  
  # Plot credible sets size
  p_cs_size <- ggplot(cs_summary, aes(x = CS, y = Size)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_text(aes(label = Size), vjust = -0.5, size = 3.5) +
    labs(
      title = "Credible Sets Summary - Table S2 Variables",
      subtitle = "Number of variables in each credible set",
      x = "Credible Set",
      y = "Number of Variables"
    ) +
    theme_bw() +
    ylim(0, max(cs_summary$Size) * 1.15)
  
  ggsave(file.path(output_dir, "credible_sets_size.pdf"), p_cs_size, width = 8, height = 6, device = "pdf")
  ggsave(file.path(output_dir, "credible_sets_size.png"), p_cs_size, width = 8, height = 6, dpi = 300, device = "png")
  cat("  ✓ Saved: credible_sets_size.pdf/png\n")
  
  # Plot purity metrics
  cs_purity_long <- cs_summary %>%
    select(CS, MinAbsCorr, MeanAbsCorr, MedianAbsCorr) %>%
    pivot_longer(cols = c(MinAbsCorr, MeanAbsCorr, MedianAbsCorr),
                 names_to = "Purity_Metric", values_to = "Value")
  
  p_cs_purity <- ggplot(cs_purity_long, aes(x = CS, y = Value, fill = Purity_Metric)) +
    geom_col(position = "dodge", alpha = 0.7) +
    scale_fill_brewer(palette = "Set2", 
                      labels = c("MinAbsCorr" = "Min", "MeanAbsCorr" = "Mean", "MedianAbsCorr" = "Median"),
                      name = "Purity Metric") +
    labs(
      title = "Credible Sets Purity Metrics - Table S2 Variables",
      subtitle = "Absolute correlation metrics for each credible set",
      x = "Credible Set",
      y = "Absolute Correlation"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(output_dir, "credible_sets_purity.pdf"), p_cs_purity, width = 10, height = 6, device = "pdf")
  ggsave(file.path(output_dir, "credible_sets_purity.png"), p_cs_purity, width = 10, height = 6, dpi = 300, device = "png")
  cat("  ✓ Saved: credible_sets_purity.pdf/png\n")
}

# Plot 4: Compare PIP vs Z-Score within Credible Sets (like example CS3)
if (length(fit$sets$cs) > 0 && exists("z_scores")) {
  cat("  Creating PIP vs Z-Score comparison for credible sets...\n")
  
  cs_comparison_list <- list()
  for (i in 1:length(fit$sets$cs)) {
    cs_vars <- fit$sets$cs[[i]]
    if (length(cs_vars) > 1) {  # Only create comparison if CS has multiple variables
      cs_comp <- data.frame(
        CS = paste0("CS", i),
        Variable = var_names[cs_vars],
        Index = cs_vars,
        Z_Score = z_scores[cs_vars],
        PIP = fit$pip[cs_vars]
      ) %>%
        arrange(desc(abs(Z_Score)))
      
      cs_comparison_list[[i]] <- cs_comp
    }
  }
  
  if (length(cs_comparison_list) > 0) {
    cs_comparison_df <- bind_rows(cs_comparison_list)
    write_csv(cs_comparison_df, file.path(output_dir, "pip_zscore_comparison.csv"))
    cat("  ✓ Saved: pip_zscore_comparison.csv\n")
    
    # Create plot comparing PIP and Z-scores
    if (nrow(cs_comparison_df) > 0) {
      p_comp <- ggplot(cs_comparison_df, aes(x = abs(Z_Score), y = PIP)) +
        geom_point(size = 3, alpha = 0.7, color = "steelblue") +
        geom_text(aes(label = Variable), vjust = -0.5, size = 2.5, hjust = 0.5) +
        facet_wrap(~ CS, scales = "free") +
        labs(
          title = "PIP vs Z-Score Comparison within Credible Sets - Table S2",
          subtitle = "Variables with high |z-score| may not have highest PIP",
          x = "|Z-Score|",
          y = "Posterior Inclusion Probability (PIP)"
        ) +
        theme_bw()
      
      ggsave(file.path(output_dir, "pip_zscore_comparison.pdf"), p_comp, width = 12, height = 8, device = "pdf")
      ggsave(file.path(output_dir, "pip_zscore_comparison.png"), p_comp, width = 12, height = 8, dpi = 300, device = "png")
      cat("  ✓ Saved: pip_zscore_comparison.pdf/png\n")
    }
  }
}

cat("\n")

# ============================================================================
# Step 8: Create ROC Curves
# ============================================================================

cat("Step 8: Creating ROC curves...\n")

# Training ROC
roc_train <- roc(y_train, y_train_prob, quiet = TRUE)
roc_train_df <- data.frame(
  FPR = 1 - roc_train$specificities,
  TPR = roc_train$sensitivities
)

# Test ROC
roc_test <- roc(y_test, y_test_prob, quiet = TRUE)
roc_test_df <- data.frame(
  FPR = 1 - roc_test$specificities,
  TPR = roc_test$sensitivities
)

# Combined ROC plot
p_roc <- ggplot() +
  geom_line(aes(x = FPR, y = TPR, color = "Training"), data = roc_train_df, linewidth = 1) +
  geom_line(aes(x = FPR, y = TPR, color = "Test"), data = roc_test_df, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curves: Training vs Test - Table S2 Variables",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Dataset"
  ) +
  scale_color_manual(values = c("Training" = "blue", "Test" = "red")) +
  annotate("text", x = 0.7, y = 0.2, 
           label = paste("Training AUC =", round(metrics_train$AUC, 3)), 
           color = "blue", size = 4) +
  annotate("text", x = 0.7, y = 0.1, 
           label = paste("Test AUC =", round(metrics_test$AUC, 3)), 
           color = "red", size = 4) +
  coord_fixed()

ggsave(file.path(output_dir, "roc_curves.pdf"), p_roc, width = 8, height = 6, device = "pdf")
ggsave(file.path(output_dir, "roc_curves.png"), p_roc, width = 8, height = 6, dpi = 300, device = "png")
cat("  ROC curves saved\n")

# ============================================================================
# Step 9: Create Confusion Matrix Plots
# ============================================================================

cat("Step 9: Creating confusion matrix plots...\n")

# Training confusion matrix
cm_train <- metrics_train$ConfusionMatrix$table
cm_train_df <- as.data.frame(cm_train) %>%
  mutate(
    Prediction = factor(Prediction, levels = c(1, 0)),
    Reference = factor(Reference, levels = c(0, 1)),
    Freq_scaled = Freq / sum(Freq) * 100
  )

p_cm_train <- ggplot(cm_train_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(Freq, "\n(", round(Freq_scaled, 1), "%)")), 
            color = "white", size = 5, fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Confusion Matrix: Training Set",
    x = "Actual",
    y = "Predicted",
    fill = "Count"
  ) +
  theme(legend.position = "none")

# Test confusion matrix
cm_test <- metrics_test$ConfusionMatrix$table
cm_test_df <- as.data.frame(cm_test) %>%
  mutate(
    Prediction = factor(Prediction, levels = c(1, 0)),
    Reference = factor(Reference, levels = c(0, 1)),
    Freq_scaled = Freq / sum(Freq) * 100
  )

p_cm_test <- ggplot(cm_test_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(Freq, "\n(", round(Freq_scaled, 1), "%)")), 
            color = "white", size = 5, fontface = "bold") +
  scale_fill_gradient(low = "lightcoral", high = "darkred") +
  labs(
    title = "Confusion Matrix: Test Set",
    x = "Actual",
    y = "Predicted",
    fill = "Count"
  ) +
  theme(legend.position = "none")

# Combined plot
p_cm_combined <- grid.arrange(p_cm_train, p_cm_test, ncol = 2)

ggsave(file.path(output_dir, "confusion_matrices.pdf"), p_cm_combined, 
       width = 12, height = 6, device = "pdf")
ggsave(file.path(output_dir, "confusion_matrices.png"), p_cm_combined, 
       width = 12, height = 6, dpi = 300, device = "png")
cat("  Confusion matrices saved\n")

# ============================================================================
# Step 10: Variable Importance (from SuSiE coefficients)
# ============================================================================

cat("Step 10: Creating variable importance plots...\n")

# Get coefficients (excluding intercept)
coef_df <- data.frame(
  Variable = var_names,
  Coefficient = coef[-1],
  AbsCoefficient = abs(coef[-1])
) %>%
  arrange(desc(AbsCoefficient)) %>%
  head(30)  # Top 30 variables

# Plot top variables
p_var_imp <- ggplot(coef_df, aes(x = reorder(Variable, AbsCoefficient), y = AbsCoefficient)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Top 30 Variables by Absolute Coefficient (SuSiE-R) - Table S2",
    x = "Variable",
    y = "|Coefficient|"
  ) +
  theme(axis.text.y = element_text(size = 8))

ggsave(file.path(output_dir, "variable_importance.pdf"), p_var_imp, 
       width = 10, height = 8, device = "pdf")
ggsave(file.path(output_dir, "variable_importance.png"), p_var_imp, 
       width = 10, height = 8, dpi = 300, device = "png")
cat("  Variable importance plot saved\n")

# ============================================================================
# Step 11: SHAP Values
# ============================================================================

cat("Step 11: Calculating SHAP values...\n")

# For SHAP values, we need to create a model wrapper
# We'll use the selected variables and fit a simple logistic regression
# Then calculate SHAP values for that model

if (length(selected_vars) > 0 && length(selected_vars) < 100) {
  # Create model with selected variables
  X_train_selected <- X_train_scaled[, selected_vars, drop = FALSE]
  X_test_selected <- X_test_scaled[, selected_vars, drop = FALSE]
  
  # Fit logistic regression on selected variables
  train_df <- data.frame(y = y_train, X_train_selected)
  test_df <- data.frame(y = y_test, X_test_selected)
  
  glm_fit <- glm(y ~ ., data = train_df, family = binomial)
  
  # Predictions for SHAP
  y_train_pred_glm <- predict(glm_fit, newdata = train_df, type = "response")
  y_test_pred_glm <- predict(glm_fit, newdata = test_df, type = "response")
  
  cat("  Logistic regression model fitted on selected variables\n")
  
  # Calculate SHAP-like values using a simplified approach
  coef_glm <- coef(glm_fit)[-1]  # Exclude intercept
  shap_df <- data.frame(
    Variable = names(coef_glm),
    SHAP_Value = coef_glm,
    Abs_SHAP = abs(coef_glm)
  ) %>%
    arrange(desc(Abs_SHAP)) %>%
    head(20)
  
  # SHAP summary plot
  p_shap_summary <- ggplot(shap_df, aes(x = reorder(Variable, Abs_SHAP), y = SHAP_Value)) +
    geom_col(aes(fill = SHAP_Value > 0), alpha = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"), 
                     labels = c("TRUE" = "Positive", "FALSE" = "Negative"),
                     name = "Effect") +
    labs(
      title = "SHAP-like Values: Top 20 Variables - Table S2",
      subtitle = "Based on logistic regression coefficients",
      x = "Variable",
      y = "SHAP Value (Coefficient)"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")
  
  ggsave(file.path(output_dir, "shap_summary.pdf"), p_shap_summary, 
         width = 10, height = 8, device = "pdf")
  ggsave(file.path(output_dir, "shap_summary.png"), p_shap_summary, 
         width = 10, height = 8, dpi = 300, device = "png")
  
  # SHAP waterfall plot (for a single prediction example)
  example_idx <- 1
  example_shap <- data.frame(
    Variable = names(coef_glm),
    Value = X_test_selected[example_idx, ],
    Coefficient = coef_glm,
    Contribution = X_test_selected[example_idx, ] * coef_glm
  ) %>%
    arrange(desc(abs(Contribution))) %>%
    head(15)
  
  p_shap_waterfall <- ggplot(example_shap, aes(x = reorder(Variable, Contribution), y = Contribution)) +
    geom_col(aes(fill = Contribution > 0), alpha = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                     name = "Direction") +
    labs(
      title = "SHAP Waterfall Plot (Example Prediction) - Table S2",
      subtitle = paste("Predicted probability:", round(y_test_pred_glm[example_idx], 3)),
      x = "Variable",
      y = "SHAP Contribution"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")
  
  ggsave(file.path(output_dir, "shap_waterfall.pdf"), p_shap_waterfall, 
         width = 10, height = 8, device = "pdf")
  ggsave(file.path(output_dir, "shap_waterfall.png"), p_shap_waterfall, 
         width = 10, height = 8, dpi = 300, device = "png")
  
  cat("  SHAP plots created\n")
} else {
  cat("  Too many selected variables for SHAP calculation\n")
  cat("  Skipping SHAP plots\n")
}

cat("\n")

# ============================================================================
# Step 12: Summary Statistics
# ============================================================================

cat("Step 12: Creating summary statistics...\n")

summary_stats <- list(
  method = SUSIE_METHOD,
  n_train = nrow(X_train),
  n_test = nrow(X_test),
  n_variables = ncol(X_matrix),
  n_selected_variables = length(selected_vars),
  n_credible_sets = length(fit$sets$cs),
  converged = fit$converged,
  n_iterations = fit$niter,
  train_accuracy = metrics_train$Accuracy,
  train_balanced_accuracy = metrics_train$Balanced_Accuracy,
  test_accuracy = metrics_test$Accuracy,
  test_balanced_accuracy = metrics_test$Balanced_Accuracy,
  train_auc = metrics_train$AUC,
  test_auc = metrics_test$AUC,
  train_f1 = metrics_train$F1_Score,
  test_f1 = metrics_test$F1_Score
)

summary_df <- data.frame(
  Statistic = names(summary_stats),
  Value = unlist(summary_stats)
)

write_csv(summary_df, file.path(output_dir, "summary_statistics.csv"))

cat("  Summary statistics saved\n\n")

# ============================================================================
# Step 12.5: Create Detailed Credible Sets Report
# ============================================================================

cat("Step 12.5: Creating detailed credible sets report...\n")

if (length(fit$sets$cs) > 0) {
  # Create detailed report similar to example
  cs_detailed <- list()
  for (i in 1:length(fit$sets$cs)) {
    cs_vars <- fit$sets$cs[[i]]
    cs_detailed[[i]] <- data.frame(
      CS = paste0("CS", i),
      Variable = var_names[cs_vars],
      Index = cs_vars,
      PIP = fit$pip[cs_vars],
      Coverage = fit$sets$coverage[i],
      MinAbsCorr = fit$sets$purity[i, "min.abs.corr"],
      MeanAbsCorr = fit$sets$purity[i, "mean.abs.corr"],
      MedianAbsCorr = fit$sets$purity[i, "median.abs.corr"]
    )
    
    # Add z-scores if available
    if (exists("z_scores") && length(z_scores) >= max(cs_vars)) {
      cs_detailed[[i]]$Z_Score <- z_scores[cs_vars]
    }
  }
  
  cs_detailed_df <- bind_rows(cs_detailed) %>%
    arrange(CS, desc(PIP))
  
  write_csv(cs_detailed_df, file.path(output_dir, "credible_sets_detailed.csv"))
  cat("  ✓ Saved: credible_sets_detailed.csv\n")
  
  # Create credible sets report (original format)
  cs_report <- list()
  for (i in 1:length(fit$sets$cs)) {
    cs_vars <- fit$sets$cs[[i]]
    cs_report[[i]] <- data.frame(
      CS = paste0("CS", i),
      Variable = var_names[cs_vars],
      Index = cs_vars,
      PIP = fit$pip[cs_vars],
      Coverage = fit$sets$coverage[i],
      MinAbsCorr = fit$sets$purity[i, "min.abs.corr"]
    )
  }
  cs_report_df <- bind_rows(cs_report)
  write_csv(cs_report_df, file.path(output_dir, "credible_sets_report.csv"))
  cat("  ✓ Saved: credible_sets_report.csv\n")
  
  # Print top variables from each CS (similar to example)
  cat("\n  Top variables by PIP in each Credible Set:\n")
  for (i in 1:length(fit$sets$cs)) {
    cs_data <- cs_detailed_df %>% filter(CS == paste0("CS", i))
    if (nrow(cs_data) > 0) {
      cat("  ", paste(rep("-", 60), collapse = ""), "\n")
      cat("  CS", i, ":\n")
      top_cs <- cs_data %>%
        arrange(desc(PIP)) %>%
        head(min(5, nrow(cs_data)))
      
      for (j in 1:nrow(top_cs)) {
        cat(sprintf("    %d. %s (Index %d, PIP = %.4f", 
                    j, top_cs$Variable[j], top_cs$Index[j], top_cs$PIP[j]))
        if ("Z_Score" %in% names(top_cs)) {
          cat(sprintf(", z = %.4f", top_cs$Z_Score[j]))
        }
        cat(")\n")
      }
    }
  }
  cat("  ", paste(rep("-", 60), collapse = ""), "\n")
}

# Create PIP summary
pip_summary <- pip_df %>%
  head(20) %>%
  select(Variable, PIP, InCS)
write_csv(pip_summary, file.path(output_dir, "pip_summary.csv"))
cat("  ✓ Saved: pip_summary.csv\n")

cat("\n")

# ============================================================================
# Step 13: Generate LaTeX Code
# ============================================================================

cat("Step 13: Generating LaTeX code for figures...\n")

latex_code <- paste0(
  "% LaTeX code for SuSiE-R results - Table S2 Variables\n",
  "% Generated automatically by run_susie_table_s2.R\n",
  "% Reference: https://stephenslab.github.io/susieR/articles/finemapping.html\n\n",
  "\\begin{table}[htbp]\n",
  "  \\centering\n",
  "  \\input{susie_results_table_s2/performance_metrics_table.tex}\n",
  "\\end{table}\n\n",
  "% SuSiE-R Fine-mapping Plots (similar to susieR example)\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results_table_s2/z_scores_plot.pdf}\n",
  "  \\caption{Z-scores from univariate regressions - Table S2 Variables. ",
  "Red dashed lines indicate |z| = 2.}\n",
  "  \\label{fig:z_scores_s2}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results_table_s2/pip_plot.pdf}\n",
  "  \\caption{Posterior Inclusion Probabilities (PIP) for all variables - Table S2. ",
  "Variables in credible sets are highlighted in red.}\n",
  "  \\label{fig:pip_plot_s2}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results_table_s2/credible_sets_size.pdf}\n",
  "  \\caption{Number of variables in each credible set identified by SuSiE-R - Table S2.}\n",
  "  \\label{fig:credible_sets_size_s2}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results_table_s2/credible_sets_purity.pdf}\n",
  "  \\caption{Purity metrics for each credible set - Table S2.}\n",
  "  \\label{fig:credible_sets_purity_s2}\n",
  "\\end{figure}\n\n",
  "% Performance Evaluation Plots\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results_table_s2/roc_curves.pdf}\n",
  "  \\caption{ROC curves comparing training and test performance - Table S2. ",
  "Training AUC = ", round(metrics_train$AUC, 3), 
  ", Test AUC = ", round(metrics_test$AUC, 3), ".}\n",
  "  \\label{fig:roc_curves_s2}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.9\\textwidth]{susie_results_table_s2/confusion_matrices.pdf}\n",
  "  \\caption{Confusion matrices for training (left) and test (right) sets - Table S2.}\n",
  "  \\label{fig:confusion_matrices_s2}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results_table_s2/variable_importance.pdf}\n",
  "  \\caption{Top 30 variables selected by SuSiE-R ranked by absolute coefficient magnitude - Table S2.}\n",
  "  \\label{fig:variable_importance_s2}\n",
  "\\end{figure}\n"
)

# Add SHAP plots if they exist
if (file.exists(file.path(output_dir, "shap_summary.pdf"))) {
  latex_code <- paste0(latex_code,
    "\\begin{figure}[htbp]\n",
    "  \\centering\n",
    "  \\includegraphics[width=0.8\\textwidth]{susie_results_table_s2/shap_summary.pdf}\n",
    "  \\caption{SHAP-like values for top 20 variables - Table S2.}\n",
    "  \\label{fig:shap_summary_s2}\n",
    "\\end{figure}\n\n",
    "\\begin{figure}[htbp]\n",
    "  \\centering\n",
    "  \\includegraphics[width=0.8\\textwidth]{susie_results_table_s2/shap_waterfall.pdf}\n",
    "  \\caption{SHAP waterfall plot - Table S2.}\n",
    "  \\label{fig:shap_waterfall_s2}\n",
    "\\end{figure}\n"
  )
}

writeLines(latex_code, file.path(output_dir, "latex_figures.tex"))

cat("  LaTeX code saved\n\n")

# ============================================================================
# Step 14: Save Model and Results
# ============================================================================

cat("Step 14: Saving model and results...\n")

saveRDS(fit, file.path(output_dir, "susie_fit.rds"))
saveRDS(coef, file.path(output_dir, "coefficients.rds"))
saveRDS(selected_vars, file.path(output_dir, "selected_variables.rds"))
saveRDS(X_train_mean, file.path(output_dir, "train_mean.rds"))
saveRDS(X_train_sd, file.path(output_dir, "train_sd.rds"))

cat("  Model saved\n\n")

# ============================================================================
# Final Summary
# ============================================================================

cat("============================================================================\n")
cat("SuSiE-R ANALYSIS COMPLETE - TABLE S2 VARIABLES\n")
cat("============================================================================\n")
cat("Method used:", SUSIE_METHOD, "\n")
cat("Converged:", fit$converged, "\n")
cat("Iterations:", fit$niter, "\n\n")
cat("Training Performance:\n")
cat("  Accuracy:", round(metrics_train$Accuracy, 4), "\n")
cat("  Balanced Accuracy:", round(metrics_train$Balanced_Accuracy, 4), "\n")
cat("  AUC:", round(metrics_train$AUC, 4), "\n")
cat("  Recall (Sensitivity):", round(metrics_train$Recall, 4), "\n")
cat("  Specificity:", round(metrics_train$Specificity, 4), "\n")
cat("  Precision:", round(metrics_train$Precision, 4), "\n")
cat("  F1-Score:", round(metrics_train$F1_Score, 4), "\n\n")
cat("Test Performance:\n")
cat("  Accuracy:", round(metrics_test$Accuracy, 4), "\n")
cat("  Balanced Accuracy:", round(metrics_test$Balanced_Accuracy, 4), "\n")
cat("  AUC:", round(metrics_test$AUC, 4), "\n")
cat("  Recall (Sensitivity):", round(metrics_test$Recall, 4), "\n")
cat("  Specificity:", round(metrics_test$Specificity, 4), "\n")
cat("  Precision:", round(metrics_test$Precision, 4), "\n")
cat("  F1-Score:", round(metrics_test$F1_Score, 4), "\n\n")
cat("Selected Variables:", length(selected_vars), "\n")
cat("Credible Sets:", length(fit$sets$cs), "\n\n")
cat("All results saved to:", output_dir, "\n")
cat("============================================================================\n")

