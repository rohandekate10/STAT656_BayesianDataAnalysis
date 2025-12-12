# ============================================================================
# SuSiE-R Analysis for NHANES Data
# ============================================================================
# This script:
# 1. Loads prepared data
# 2. Splits into training and test sets
# 3. Standardizes data
# 4. Runs susieR variable selection
# 5. Makes predictions
# 6. Calculates performance metrics
# 7. Creates SHAP plots
# 8. Generates LaTeX-ready plots and tables
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
  cat("Or from CRAN if available:\n")
  cat("  install.packages('susieR')\n")
  stop("susieR package is required. Please install it first.")
}

# For SHAP values (if available)
if (!require(shapr, quietly = TRUE)) {
  cat("Note: shapr package not available. SHAP plots will be skipped.\n")
  has_shapr <- FALSE
} else {
  has_shapr <- TRUE
}

# ============================================================================
# SETUP
# ============================================================================

base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"
data_dir <- file.path(base_dir, "susie_data")
output_dir <- file.path(base_dir, "susie_results")

# ============================================================================
# CONFIGURATION: Choose SuSiE method
# ============================================================================
# Options: "susie" or "susie_rss"
# - "susie": Uses individual-level data (X, y) directly
#   Reference: https://stephenslab.github.io/susieR/reference/susie.html
# - "susie_rss": Uses summary statistics (z-scores, correlation matrix R, n)
#   Reference: https://stephenslab.github.io/susieR/reference/susie_rss.html
#   Computes summary statistics from individual-level data automatically

SUSIE_METHOD <- "susie"  # Using susie() with individual-level data method

# ============================================================================

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("============================================================================\n")
cat("SuSiE-R Analysis for NHANES Data\n")
cat("============================================================================\n")
cat("Method:", SUSIE_METHOD, "\n")
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
# Step 1: Load Data
# ============================================================================

cat("Step 1: Loading prepared data...\n")
X <- readRDS(file.path(data_dir, "X_matrix.rds"))
y <- readRDS(file.path(data_dir, "y_vector.rds"))
var_names <- readRDS(file.path(data_dir, "variable_names.rds"))

cat("  X matrix:", nrow(X), "rows x", ncol(X), "columns\n")
cat("  y vector:", length(y), "values\n")
cat("  Outcome distribution:\n")
print(table(y))

# Exclude outcome-related variables (data leakage prevention)
# These are variables directly related to mortality/outcome
outcome_related_patterns <- c("MORT", "DEATH", "CVD_DEATH", "VNMORT", "VNDEATH", 
                               "VNUCOD", "PERMTH", "UCOD", "ELIGSTAT")
outcome_related_indices <- unique(unlist(lapply(outcome_related_patterns, function(pattern) {
  grep(pattern, var_names, ignore.case = TRUE)
})))
if (length(outcome_related_indices) > 0) {
  cat("  Excluding", length(outcome_related_indices), "outcome-related variables to prevent data leakage\n")
  cat("    Excluded:", paste(var_names[outcome_related_indices], collapse = ", "), "\n")
  X <- X[, -outcome_related_indices, drop = FALSE]
  var_names <- var_names[-outcome_related_indices]
  cat("  X matrix after exclusion:", nrow(X), "rows x", ncol(X), "columns\n")
}

cat("\n")

# ============================================================================
# Step 2: Split Data into Training and Test Sets
# ============================================================================

cat("Step 2: Splitting data into training (70%) and test (30%) sets...\n")

set.seed(42)  # For reproducibility
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

cat("  Training set:", nrow(X_train), "observations\n")
cat("  Test set:", nrow(X_test), "observations\n")
cat("  Training outcome distribution:\n")
print(table(y_train))
cat("  Test outcome distribution:\n")
print(table(y_test))

cat("\n")

# ============================================================================
# Step 2.5: Undersample Training Data to Balance Classes
# ============================================================================

cat("Step 2.5: Undersampling training data to balance classes...\n")

# Check class distribution
class_counts <- table(y_train)
cat("  Original class distribution:\n")
print(class_counts)

# Identify minority and majority classes
minority_class <- as.numeric(names(class_counts)[which.min(class_counts)])
majority_class <- as.numeric(names(class_counts)[which.max(class_counts)])
minority_count <- min(class_counts)
majority_count <- max(class_counts)

cat("  Minority class:", minority_class, "with", minority_count, "samples\n")
cat("  Majority class:", majority_class, "with", majority_count, "samples\n")
cat("  Imbalance ratio:", round(majority_count / minority_count, 2), ":1\n")

# Get indices for each class
minority_indices <- which(y_train == minority_class)
majority_indices <- which(y_train == majority_class)

# Randomly sample from majority class to match minority class size
set.seed(42)  # For reproducibility
majority_sampled_indices <- sample(majority_indices, size = minority_count, replace = FALSE)

# Combine indices
balanced_indices <- c(minority_indices, majority_sampled_indices)

# Shuffle the indices to randomize order
balanced_indices <- sample(balanced_indices)

# Create balanced training set
X_train_balanced <- X_train[balanced_indices, ]
y_train_balanced <- y_train[balanced_indices]

cat("  Balanced training set:", nrow(X_train_balanced), "observations\n")
cat("  Balanced class distribution:\n")
print(table(y_train_balanced))

# Update training set variables
X_train <- X_train_balanced
y_train <- y_train_balanced

cat("  Undersampling complete. Training set is now balanced.\n\n")

# ============================================================================
# Step 3: Standardize Data
# ============================================================================

cat("Step 3: Standardizing data...\n")

# Calculate mean and SD from training data
X_train_mean <- apply(X_train, 2, mean, na.rm = TRUE)
X_train_sd <- apply(X_train, 2, sd, na.rm = TRUE)

# Avoid division by zero
X_train_sd[X_train_sd == 0] <- 1

# Standardize training data
X_train_scaled <- scale(X_train, center = X_train_mean, scale = X_train_sd)

# Apply same standardization to test data
X_test_scaled <- scale(X_test, center = X_train_mean, scale = X_train_sd)

cat("  Training data standardized\n")
cat("  Test data standardized using training statistics\n")
cat("  Mean of standardized training data:", round(mean(X_train_scaled), 4), "\n")
cat("  SD of standardized training data:", round(sd(X_train_scaled), 4), "\n")

cat("\n")

# ============================================================================
# Step 4: Run SuSiE-R
# ============================================================================

cat("Step 4: Running SuSiE-R variable selection...\n")
cat("  Method:", SUSIE_METHOD, "\n")
cat("  This may take several minutes...\n\n")

n <- nrow(X_train_scaled)
p <- ncol(X_train_scaled)

cat("  Sample size (n):", n, "\n")
cat("  Number of predictors (p):", p, "\n\n")

# ============================================================================
# Load Optimal Parameters (if available from grid search)
# ============================================================================
optimal_params_file <- file.path(base_dir, "susie_parameter_optimization_full", "optimal_parameters.csv")

if (file.exists(optimal_params_file)) {
  cat("  Loading optimal parameters from grid search...\n")
  optimal_params <- read_csv(optimal_params_file, show_col_types = FALSE)
  L <- optimal_params$L
  scaled_prior_variance <- optimal_params$scaled_prior_variance
  estimate_residual_variance <- optimal_params$estimate_residual_variance
  estimate_residual_method <- optimal_params$estimate_residual_method
  estimate_prior_variance <- optimal_params$estimate_prior_variance
  estimate_prior_method <- optimal_params$estimate_prior_method
  unmappable_effects <- optimal_params$unmappable_effects
  standardize <- optimal_params$standardize
  intercept <- optimal_params$intercept
  null_weight <- optimal_params$null_weight
  refine <- optimal_params$refine
  convergence_method <- optimal_params$convergence_method
  compute_univariate_zscore <- optimal_params$compute_univariate_zscore
  na.rm <- optimal_params$na.rm
  residual_variance_lowerbound <- optimal_params$residual_variance_lowerbound
  residual_variance_upperbound <- optimal_params$residual_variance_upperbound
  check_null_threshold <- optimal_params$check_null_threshold
  n_purity <- optimal_params$n_purity
  alpha0 <- optimal_params$alpha0
  beta0 <- optimal_params$beta0
  cat("    Using optimized parameters (see optimal_parameters.csv for full list)\n\n")
} else {
  cat("  Using default parameters (run optimize_susie_parameters.R to find optimal values)\n")
  L <- min(10, p)
  scaled_prior_variance <- 0.2
  estimate_residual_variance <- TRUE
  estimate_residual_method <- "MoM"
  estimate_prior_variance <- TRUE
  estimate_prior_method <- "optim"
  unmappable_effects <- "none"
  standardize <- TRUE
  intercept <- TRUE
  null_weight <- 0
  refine <- FALSE
  convergence_method <- "elbo"
  compute_univariate_zscore <- FALSE
  na.rm <- FALSE
  residual_variance_lowerbound <- var(drop(y_train))/10000
  residual_variance_upperbound <- Inf
  check_null_threshold <- 0
  n_purity <- 100
  alpha0 <- 0.1
  beta0 <- 0.1
  cat("    Using default parameters\n\n")
}

# Fixed parameters
prior_tol <- 1e-09
coverage <- 0.95
min_abs_corr <- 0.5
max_iter <- 100
tol <- 0.001
verbose <- TRUE
track_fit <- TRUE

if (SUSIE_METHOD == "susie") {
  cat("  Using susie() with individual-level data\n")
  
  # Fit SuSiE model with optimal or default parameters
  cat("  Starting SuSiE execution...\n")
  susie_start_time <- Sys.time()
  fit <- susie(
    X = X_train_scaled,
    y = y_train,
    L = L,
    scaled_prior_variance = scaled_prior_variance,
    residual_variance = NULL,
    prior_weights = NULL,
    null_weight = null_weight,
    standardize = standardize,
    intercept = intercept,
    estimate_residual_variance = estimate_residual_variance,
    estimate_residual_method = estimate_residual_method,
    estimate_prior_variance = estimate_prior_variance,
    estimate_prior_method = estimate_prior_method,
    unmappable_effects = unmappable_effects,
    check_null_threshold = check_null_threshold,
    prior_tol = prior_tol,
    residual_variance_upperbound = residual_variance_upperbound,
    model_init = NULL,
    coverage = coverage,
    min_abs_corr = min_abs_corr,
    compute_univariate_zscore = compute_univariate_zscore,
    na.rm = na.rm,
    max_iter = max_iter,
    tol = tol,
    convergence_method = convergence_method,
    verbose = verbose,
    track_fit = track_fit,
    residual_variance_lowerbound = residual_variance_lowerbound,
    refine = refine,
    n_purity = n_purity,
    alpha0 = alpha0,
    beta0 = beta0
  )
  susie_end_time <- Sys.time()
  susie_execution_time <- as.numeric(difftime(susie_end_time, susie_start_time, units = "secs"))
  cat("  SuSiE execution time:", round(susie_execution_time, 2), "seconds\n")
  
} else {
  stop("SUSIE_METHOD must be 'susie' for this script")
}

cat("  SuSiE-R fit completed\n")
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
# Step 5: Make Predictions
# ============================================================================

cat("Step 5: Making predictions...\n")

# Get coefficients from susie fit
cat("  Extracting coefficients from susie fit...\n")
coef <- coef(fit)

# Check coefficient format
if (length(coef) == p + 1) {
  # Has intercept
  y_train_pred <- as.vector(X_train_scaled %*% coef[-1] + coef[1])
  y_test_pred <- as.vector(X_test_scaled %*% coef[-1] + coef[1])
} else if (length(coef) == p) {
  # No intercept (or intercept is separate)
  y_train_pred <- as.vector(X_train_scaled %*% coef)
  y_test_pred <- as.vector(X_test_scaled %*% coef)
} else {
  # Sparse coefficients - create full vector
  coef_vec <- numeric(p)
  if (length(coef) > 1) {
    # Assume first element is intercept
    n_coef <- min(length(coef)-1, p)
    if (n_coef > 0) {
      coef_vec[seq_len(n_coef)] <- coef[-1]
    }
    intercept <- coef[1]
  } else {
    n_coef <- min(length(coef), p)
    if (n_coef > 0) {
      coef_vec[seq_len(n_coef)] <- coef
    }
    intercept <- 0
  }
  y_train_pred <- as.vector(X_train_scaled %*% coef_vec + intercept)
  y_test_pred <- as.vector(X_test_scaled %*% coef_vec + intercept)
  coef <- c(intercept, coef_vec)
}

# Convert linear predictor to probabilities (logistic transformation)
y_train_prob <- as.vector(1 / (1 + exp(-y_train_pred)))
y_test_prob <- as.vector(1 / (1 + exp(-y_test_pred)))
y_train_pred_class <- ifelse(y_train_prob > 0.5, 1, 0)
y_test_pred_class <- ifelse(y_test_prob > 0.5, 1, 0)

cat("  Predictions generated for training and test sets\n")
cat("  Training prediction range:", round(range(y_train_prob, na.rm = TRUE), 4), "\n")
cat("  Test prediction range:", round(range(y_test_prob, na.rm = TRUE), 4), "\n")
cat("  Non-zero coefficients:", sum(abs(coef) > 1e-6), "\n\n")

# ============================================================================
# Step 6: Calculate Performance Metrics
# ============================================================================

cat("Step 6: Calculating performance metrics...\n")

calculate_metrics <- function(y_true, y_pred, y_prob) {
  # Confusion matrix
  cm <- confusionMatrix(factor(y_pred, levels = c(0, 1)), 
                       factor(y_true, levels = c(0, 1)), 
                       positive = "1")
  
  # Extract metrics
  acc <- cm$overall["Accuracy"]
  recall <- cm$byClass["Sensitivity"]  # Recall = Sensitivity = TPR
  specificity <- cm$byClass["Specificity"]  # Specificity = TNR
  precision <- cm$byClass["Precision"]
  f1 <- cm$byClass["F1"]
  
  # Balanced Accuracy = (Sensitivity + Specificity) / 2
  # This is especially important for imbalanced datasets
  balanced_acc <- (recall + specificity) / 2
  
  # AUC
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

# Calculate metrics for training
metrics_train <- calculate_metrics(y_train, y_train_pred_class, y_train_prob)

# Calculate metrics for test
metrics_test <- calculate_metrics(y_test, y_test_pred_class, y_test_prob)

cat("  Training metrics calculated\n")
cat("  Test metrics calculated\n\n")

# ============================================================================
# Step 7: Create Performance Tables
# ============================================================================

cat("Step 7: Creating performance tables...\n")

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
# Step 7.5: Create Plots Similar to susieR Fine-mapping Example
# ============================================================================
# Reference: https://stephenslab.github.io/susieR/articles/finemapping.html

cat("Step 7.5: Creating plots similar to susieR fine-mapping example...\n")

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
      title = "Z-Scores from Univariate Regressions",
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
    title = "Posterior Inclusion Probabilities (PIP)",
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
      title = "Credible Sets Summary",
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
      title = "Credible Sets Purity Metrics",
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
          title = "PIP vs Z-Score Comparison within Credible Sets",
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
    title = "ROC Curves: Training vs Test",
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
    title = "Top 30 Variables by Absolute Coefficient (SuSiE-R)",
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
  # (Full SHAP requires shapr package which may not be available)
  # We'll create feature importance plots based on coefficients
  
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
      title = "SHAP-like Values: Top 20 Variables",
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
  # Use first test observation
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
      title = "SHAP Waterfall Plot (Example Prediction)",
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
  n_variables = ncol(X),
  n_selected_variables = length(selected_vars),
  n_credible_sets = length(fit$sets$cs),
  converged = fit$converged,
  n_iterations = fit$niter,
  susie_execution_time_seconds = if(exists("susie_execution_time")) susie_execution_time else NA,
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

cat("\n")

# ============================================================================
# Step 13: Generate LaTeX Code
# ============================================================================

cat("Step 13: Generating LaTeX code for figures...\n")

latex_code <- paste0(
  "% LaTeX code for SuSiE-R results\n",
  "% Generated automatically by run_susie_analysis.R\n",
  "% Reference: https://stephenslab.github.io/susieR/articles/finemapping.html\n\n",
  "\\begin{table}[htbp]\n",
  "  \\centering\n",
  "  \\input{susie_results/performance_metrics_table.tex}\n",
  "\\end{table}\n\n",
  "% SuSiE-R Fine-mapping Plots (similar to susieR example)\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results/z_scores_plot.pdf}\n",
  "  \\caption{Z-scores from univariate regressions. ",
  "Red dashed lines indicate |z| = 2. ",
  "Similar to \\texttt{susie\\_plot(z\\_scores, y = \"z\")} from the susieR fine-mapping example.}\n",
  "  \\label{fig:z_scores}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results/pip_plot.pdf}\n",
  "  \\caption{Posterior Inclusion Probabilities (PIP) for all variables. ",
  "Variables in credible sets are highlighted in red. ",
  "Similar to \\texttt{susie\\_plot(fitted, y=\"PIP\")} from the susieR fine-mapping example.}\n",
  "  \\label{fig:pip_plot}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results/credible_sets_size.pdf}\n",
  "  \\caption{Number of variables in each credible set identified by SuSiE-R.}\n",
  "  \\label{fig:credible_sets_size}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results/credible_sets_purity.pdf}\n",
  "  \\caption{Purity metrics (minimum, mean, and median absolute correlation) for each credible set. ",
  "Higher values indicate better separation of signals.}\n",
  "  \\label{fig:credible_sets_purity}\n",
  "\\end{figure}\n\n",
  "% Performance Evaluation Plots\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results/roc_curves.pdf}\n",
  "  \\caption{ROC curves comparing training and test performance. ",
  "Training AUC = ", round(metrics_train$AUC, 3), 
  ", Test AUC = ", round(metrics_test$AUC, 3), ".}\n",
  "  \\label{fig:roc_curves}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.9\\textwidth]{susie_results/confusion_matrices.pdf}\n",
  "  \\caption{Confusion matrices for training (left) and test (right) sets.}\n",
  "  \\label{fig:confusion_matrices}\n",
  "\\end{figure}\n\n",
  "\\begin{figure}[htbp]\n",
  "  \\centering\n",
  "  \\includegraphics[width=0.8\\textwidth]{susie_results/variable_importance.pdf}\n",
  "  \\caption{Top 30 variables selected by SuSiE-R ranked by absolute coefficient magnitude.}\n",
  "  \\label{fig:variable_importance}\n",
  "\\end{figure}\n"
)

# Add SHAP plots if they exist
if (file.exists(file.path(output_dir, "shap_summary.pdf"))) {
  latex_code <- paste0(latex_code,
    "\\begin{figure}[htbp]\n",
    "  \\centering\n",
    "  \\includegraphics[width=0.8\\textwidth]{susie_results/shap_summary.pdf}\n",
    "  \\caption{SHAP-like values for top 20 variables based on logistic regression coefficients. ",
    "Positive values indicate increased risk, negative values indicate decreased risk.}\n",
    "  \\label{fig:shap_summary}\n",
    "\\end{figure}\n\n",
    "\\begin{figure}[htbp]\n",
    "  \\centering\n",
    "  \\includegraphics[width=0.8\\textwidth]{susie_results/shap_waterfall.pdf}\n",
    "  \\caption{SHAP waterfall plot showing variable contributions to a single example prediction.}\n",
    "  \\label{fig:shap_waterfall}\n",
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
cat("SuSiE-R ANALYSIS COMPLETE\n")
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

