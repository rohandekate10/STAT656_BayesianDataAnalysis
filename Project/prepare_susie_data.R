# ============================================================================
# Prepare Data for susieR Variable Selection
# ============================================================================
# This script:
# 1. Encodes categorical variables (one-hot or ordinal)
# 2. Handles missing values
# 3. Creates numeric matrices X and y for susieR
# ============================================================================

library(tidyverse)
library(data.table)

# ============================================================================
# SETUP
# ============================================================================

base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"
data_file <- file.path(base_dir, "nhanes_filtered_for_susie.rds")
var_analysis_file <- file.path(base_dir, "variable_analysis_report.csv")
output_dir <- file.path(base_dir, "susie_data")

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("============================================================================\n")
cat("Preparing Data for susieR Variable Selection\n")
cat("============================================================================\n\n")

# ============================================================================
# Step 1: Load Data and Variable Analysis
# ============================================================================

cat("Step 1: Loading data and variable analysis...\n")
data <- readRDS(data_file)
var_analysis <- read_csv(var_analysis_file, show_col_types = FALSE)

cat("  Dataset dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("  Variables analyzed:", nrow(var_analysis), "\n\n")

# ============================================================================
# Step 2: Identify Outcome Variable (y)
# ============================================================================

cat("Step 2: Identifying outcome variable...\n")

# Check for CVD death or mortality variables
outcome_candidates <- c("CVD_Death", "MORTSTAT", "UCOD_LEADING", "DIABETES", "HYPERTEN")
outcome_var <- NULL

for (candidate in outcome_candidates) {
  if (candidate %in% names(data)) {
    # Check if it's suitable as outcome (binary or can be made binary)
    unique_vals <- unique(data[[candidate]][!is.na(data[[candidate]])])
    if (length(unique_vals) <= 5) {  # Reasonable number of categories
      outcome_var <- candidate
      cat("  Found outcome variable:", outcome_var, "\n")
      cat("    Unique values:", paste(unique_vals, collapse = ", "), "\n")
      break
    }
  }
}

# If no outcome found, check for any binary mortality variable
if (is.null(outcome_var)) {
  mort_vars <- grep("MORT|CVD|DEATH", names(data), value = TRUE, ignore.case = TRUE)
  if (length(mort_vars) > 0) {
    # Check if any are binary
    for (var in mort_vars) {
      unique_vals <- unique(data[[var]][!is.na(data[[var]])])
      if (length(unique_vals) <= 2) {
        outcome_var <- var
        cat("  Found binary outcome variable:", outcome_var, "\n")
        break
      }
    }
  }
}

if (is.null(outcome_var)) {
  cat("  WARNING: No outcome variable found. Creating placeholder.\n")
  cat("  You may need to specify the outcome variable manually.\n")
  outcome_var <- "OUTCOME_PLACEHOLDER"
  data[[outcome_var]] <- NA
}

cat("\n")

# ============================================================================
# Step 3: Identify Variables to Include
# ============================================================================

cat("Step 3: Identifying variables to include...\n")

# Get usable variables from analysis
usable_vars <- var_analysis %>%
  filter(usable == TRUE) %>%
  pull(variable)

# Exclude ID variables and outcome
exclude_vars <- c("SEQN", "SEQN_new", outcome_var)
vars_to_include <- setdiff(usable_vars, exclude_vars)

# Also include calculated variables
calculated_vars <- c("Sys_BP", "Dia_BP", "Diabetic", "Smoker", "eGFR")
vars_to_include <- c(vars_to_include, intersect(calculated_vars, names(data)))

cat("  Usable variables from analysis:", length(usable_vars), "\n")
cat("  Variables to include (after exclusions):", length(vars_to_include), "\n")
cat("  Calculated variables:", sum(calculated_vars %in% names(data)), "\n\n")

# ============================================================================
# Step 4: Separate Numeric and Categorical Variables
# ============================================================================

cat("Step 4: Separating numeric and categorical variables...\n")

# Get variables that exist in dataset
available_vars <- intersect(vars_to_include, names(data))
data_subset <- data %>%
  select(all_of(c("SEQN", outcome_var, available_vars)))

# Identify categorical variables
categorical_info <- var_analysis %>%
  filter(variable %in% available_vars) %>%
  filter(type %in% c("categorical", "categorical_many")) %>%
  select(variable, type, n_unique, encoding_method)

# Also check for character/factor variables
char_vars <- data_subset %>%
  select_if(is.character) %>%
  names()
char_vars <- setdiff(char_vars, c("SEQN", outcome_var))

# Combine categorical variables
categorical_vars <- unique(c(categorical_info$variable, char_vars))
categorical_vars <- intersect(categorical_vars, available_vars)

# Numeric variables
numeric_vars <- setdiff(available_vars, categorical_vars)

cat("  Numeric variables:", length(numeric_vars), "\n")
cat("  Categorical variables:", length(categorical_vars), "\n\n")

# ============================================================================
# Step 5: Encode Categorical Variables
# ============================================================================

cat("Step 5: Encoding categorical variables...\n")

encoded_data <- data_subset %>%
  select(SEQN, all_of(outcome_var), all_of(numeric_vars))

# Track encoded variable names
encoded_var_names <- numeric_vars

for (cat_var in categorical_vars) {
  if (!cat_var %in% names(data_subset)) next
  
  cat("  Encoding", cat_var, "...\n")
  
  var_data <- data_subset[[cat_var]]
  n_unique <- length(unique(var_data[!is.na(var_data)]))
  
  # Determine encoding method
  encoding_method <- if (cat_var %in% categorical_info$variable) {
    categorical_info$encoding_method[categorical_info$variable == cat_var][1]
  } else {
    "one-hot"  # Default to one-hot for unknown categoricals
  }
  
  # For variables with <= 10 categories, use one-hot encoding
  # For variables with > 10 categories, use ordinal or group rare categories
  if (n_unique <= 10 && n_unique > 1) {
    # One-hot encoding
    var_levels <- sort(unique(var_data[!is.na(var_data)]))
    
    # Create binary indicators for each level (except reference level)
    for (i in 2:length(var_levels)) {
      level <- var_levels[i]
      new_var_name <- paste0(cat_var, "_", level)
      encoded_data[[new_var_name]] <- as.numeric(var_data == level & !is.na(var_data))
      encoded_var_names <- c(encoded_var_names, new_var_name)
    }
    
    cat("    Created", length(var_levels) - 1, "one-hot encoded variables\n")
  } else if (n_unique > 10 && n_unique <= 50) {
    # Ordinal encoding (assign numeric values)
    var_levels <- sort(unique(var_data[!is.na(var_data)]))
    var_mapping <- setNames(1:length(var_levels), var_levels)
    encoded_data[[cat_var]] <- as.numeric(var_mapping[as.character(var_data)])
    encoded_data[[cat_var]][is.na(var_data)] <- NA
    encoded_var_names <- c(encoded_var_names, cat_var)
    cat("    Ordinal encoding with", n_unique, "levels\n")
  } else {
    # Too many categories - skip or group
    cat("    WARNING: Too many categories (", n_unique, "), skipping\n", sep = "")
  }
}

cat("  Total variables after encoding:", length(encoded_var_names), "\n\n")

# ============================================================================
# Step 6: Handle Missing Values
# ============================================================================

cat("Step 6: Handling missing values...\n")

# Calculate missingness for each variable
missing_summary <- encoded_data %>%
  select(all_of(encoded_var_names)) %>%
  summarise_all(~ sum(is.na(.)) / length(.) * 100) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct")

# Strategy: 
# 1. Remove variables with > 50% missing
# 2. For remaining variables, use median imputation for numeric, mode for categorical
high_missing_vars <- missing_summary %>%
  filter(missing_pct > 50) %>%
  pull(variable)

cat("  Variables with >50% missing:", length(high_missing_vars), "\n")
if (length(high_missing_vars) > 0) {
  cat("    Removing:", paste(head(high_missing_vars, 10), collapse = ", "), "\n")
  if (length(high_missing_vars) > 10) {
    cat("    ... and", length(high_missing_vars) - 10, "more\n")
  }
}

# Remove high missing variables
encoded_var_names <- setdiff(encoded_var_names, high_missing_vars)
encoded_data <- encoded_data %>%
  select(SEQN, all_of(outcome_var), all_of(encoded_var_names))

# Impute missing values
cat("  Imputing missing values...\n")
for (var in encoded_var_names) {
  if (var %in% names(encoded_data)) {
    var_data <- encoded_data[[var]]
    n_missing <- sum(is.na(var_data))
    
    if (n_missing > 0 && n_missing < nrow(encoded_data)) {
      if (is.numeric(var_data)) {
        # Median imputation for numeric
        median_val <- median(var_data, na.rm = TRUE)
        encoded_data[[var]][is.na(var_data)] <- median_val
      } else {
        # Mode imputation for categorical
        mode_val <- names(sort(table(var_data, useNA = "no"), decreasing = TRUE))[1]
        if (!is.null(mode_val)) {
          encoded_data[[var]][is.na(var_data)] <- mode_val
        }
      }
    }
  }
}

cat("  Variables after missing value handling:", length(encoded_var_names), "\n\n")

# ============================================================================
# Step 7: Create X and y Matrices
# ============================================================================

cat("Step 7: Creating X and y matrices...\n")

# Prepare outcome variable
if (outcome_var %in% names(encoded_data)) {
  y <- encoded_data[[outcome_var]]
  
  # Convert to numeric if needed
  if (!is.numeric(y)) {
    y <- as.numeric(as.factor(y)) - 1  # Convert to 0/1 if binary
  }
  
  # Remove rows with missing outcome
  complete_outcome <- !is.na(y)
  cat("  Complete outcome values:", sum(complete_outcome), "/", length(y), "\n")
} else {
  cat("  WARNING: Outcome variable not found, creating placeholder\n")
  y <- rep(0, nrow(encoded_data))
  complete_outcome <- rep(TRUE, nrow(encoded_data))
}

# Create X matrix (predictors only)
X <- encoded_data %>%
  select(all_of(encoded_var_names)) %>%
  as.matrix()

# Remove rows with missing outcome
X <- X[complete_outcome, ]
y <- y[complete_outcome]

cat("  Final X matrix dimensions:", nrow(X), "rows x", ncol(X), "columns\n")
cat("  Final y vector length:", length(y), "\n")
cat("  Missing values in X:", sum(is.na(X)), "\n")
cat("  Missing values in y:", sum(is.na(y)), "\n\n")

# ============================================================================
# Step 8: Final Checks and Save
# ============================================================================

cat("Step 8: Final checks and saving...\n")

# Remove any remaining missing values (complete case analysis)
complete_cases <- complete.cases(X) & !is.na(y)
X_final <- X[complete_cases, ]
y_final <- y[complete_cases]

cat("  Complete cases:", sum(complete_cases), "/", nrow(X), "\n")
cat("  Final X dimensions:", nrow(X_final), "rows x", ncol(X_final), "columns\n")
cat("  Final y length:", length(y_final), "\n")

# Check for constant variables (zero variance)
var_std <- apply(X_final, 2, sd, na.rm = TRUE)
constant_vars <- which(var_std == 0 | is.na(var_std))

if (length(constant_vars) > 0) {
  cat("  Removing", length(constant_vars), "constant variables\n")
  X_final <- X_final[, -constant_vars]
  encoded_var_names <- encoded_var_names[-constant_vars]
}

cat("  Final X dimensions (after removing constants):", nrow(X_final), "rows x", ncol(X_final), "columns\n")

# Save matrices
saveRDS(X_final, file.path(output_dir, "X_matrix.rds"))
saveRDS(y_final, file.path(output_dir, "y_vector.rds"))
saveRDS(encoded_var_names, file.path(output_dir, "variable_names.rds"))

# Also save as CSV for inspection (if not too large)
if (ncol(X_final) < 1000) {
  write_csv(as.data.frame(X_final), file.path(output_dir, "X_matrix.csv"))
  write_csv(data.frame(y = y_final), file.path(output_dir, "y_vector.csv"))
  cat("  Saved CSV files for inspection\n")
}

# Save metadata
metadata <- list(
  n_observations = nrow(X_final),
  n_variables = ncol(X_final),
  outcome_variable = outcome_var,
  variables_removed_high_missing = length(high_missing_vars),
  variables_removed_constant = length(constant_vars),
  categorical_variables_encoded = length(categorical_vars),
  encoding_date = Sys.Date()
)

saveRDS(metadata, file.path(output_dir, "metadata.rds"))
write_csv(data.frame(
  variable = encoded_var_names,
  type = ifelse(encoded_var_names %in% numeric_vars, "numeric", "encoded_categorical")
), file.path(output_dir, "variable_info.csv"))

cat("\n  Files saved to:", output_dir, "\n")
cat("    - X_matrix.rds: Predictor matrix\n")
cat("    - y_vector.rds: Outcome vector\n")
cat("    - variable_names.rds: Variable names\n")
cat("    - metadata.rds: Processing metadata\n")
cat("    - variable_info.csv: Variable information\n")

cat("\n============================================================================\n")
cat("DATA PREPARATION COMPLETE\n")
cat("============================================================================\n")
cat("Final dataset ready for susieR:\n")
cat("  Observations:", nrow(X_final), "\n")
cat("  Predictors:", ncol(X_final), "\n")
cat("  Outcome:", outcome_var, "\n")
cat("\nTo use in R:\n")
cat("  X <- readRDS('", output_dir, "/X_matrix.rds')\n", sep = "")
cat("  y <- readRDS('", output_dir, "/y_vector.rds')\n", sep = "")
cat("  library(susieR)\n")
cat("  fit <- susie(X, y)\n")
cat("============================================================================\n")

