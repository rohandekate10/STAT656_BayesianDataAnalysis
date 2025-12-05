# ============================================================================
# Load and Filter Data for susieR Variable Selection
# ============================================================================
# This script:
# 1. Gets eligible SEQNs from previous filtration (age, caloric, CVD, dietary status)
# 2. Loads all usable variables identified in variable analysis
# 3. Filters data to only eligible SEQNs
# 4. Merges all datasets together
# 5. Prepares data for encoding and susieR analysis
# ============================================================================

library(tidyverse)
library(data.table)

# ============================================================================
# SETUP
# ============================================================================

base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"
nhanes_dir <- file.path(base_dir, "NHANES Clean 1988-2018")
analysis_report <- file.path(base_dir, "variable_analysis_report.csv")
output_file <- file.path(base_dir, "nhanes_filtered_for_susie.rds")
output_csv <- file.path(base_dir, "nhanes_filtered_for_susie.csv")

cat("============================================================================\n")
cat("Loading and Filtering Data for susieR\n")
cat("============================================================================\n\n")

# ============================================================================
# Step 1: Get Eligible SEQNs from Previous Filtration
# ============================================================================

cat("Step 1: Getting eligible SEQNs from previous filtration...\n")

# Re-run the filtration steps to get eligible SEQNs
# (We'll do this efficiently by only running the filtration logic)

# Load required libraries for filtration
library(tidyverse)
library(data.table)

# Parameters
AGE_MIN <- 20
CALORIC_MIN <- 800
CALORIC_MAX <- 6000

# Step 1: Age filter
cat("  Filtering by age >= ", AGE_MIN, "...\n")
demographics <- fread(
  file.path(nhanes_dir, "demographics_clean.csv"),
  select = c("SEQN", "RIDAGEYR"),
  showProgress = FALSE,
  data.table = FALSE
)
eligible_seqn_age <- demographics %>%
  filter(!is.na(RIDAGEYR) & RIDAGEYR >= AGE_MIN) %>%
  pull(SEQN) %>%
  unique()
rm(demographics)
gc(verbose = FALSE)

# Step 2: Caloric filter
cat("  Filtering by caloric intake (", CALORIC_MIN, "-", CALORIC_MAX, " kcal)...\n")
dietary <- fread(
  file.path(nhanes_dir, "dietary_clean.csv"),
  select = c("SEQN", "DRXTKCAL"),
  showProgress = FALSE,
  data.table = FALSE
) %>%
  filter(SEQN %in% eligible_seqn_age) %>%
  distinct(SEQN, .keep_all = TRUE)

eligible_seqn_caloric <- dietary %>%
  filter(!is.na(DRXTKCAL)) %>%
  filter(DRXTKCAL >= CALORIC_MIN & DRXTKCAL <= CALORIC_MAX) %>%
  pull(SEQN) %>%
  unique()
rm(dietary)
gc(verbose = FALSE)

# Step 3: CVD history filter
cat("  Filtering by prior CVD history...\n")
questionnaire <- fread(
  file.path(nhanes_dir, "questionnaire_clean.csv"),
  select = c("SEQN", "MCQ160B", "MCQ160C", "MCQ160E", "MCQ160F"),
  showProgress = FALSE,
  data.table = FALSE
) %>%
  filter(SEQN %in% eligible_seqn_caloric) %>%
  distinct(SEQN, .keep_all = TRUE)

prior_cvd <- (questionnaire$MCQ160B == 1 | questionnaire$MCQ160C == 1 | 
              questionnaire$MCQ160E == 1 | questionnaire$MCQ160F == 1)
prior_cvd[is.na(prior_cvd)] <- FALSE

eligible_seqn_cvd <- questionnaire %>%
  filter(!prior_cvd) %>%
  pull(SEQN) %>%
  unique()
rm(questionnaire)
gc(verbose = FALSE)

# Step 4: Dietary status filter
cat("  Filtering by dietary recall status...\n")
dietary_status <- fread(
  file.path(nhanes_dir, "dietary_clean.csv"),
  select = c("SEQN", "DRXDRSTZ"),
  showProgress = FALSE,
  data.table = FALSE
) %>%
  filter(SEQN %in% eligible_seqn_cvd) %>%
  distinct(SEQN, .keep_all = TRUE)

eligible_seqn <- dietary_status %>%
  filter(DRXDRSTZ == 1) %>%
  pull(SEQN) %>%
  unique()
rm(dietary_status)
gc(verbose = FALSE)

cat("  Found", length(eligible_seqn), "eligible SEQNs\n\n")

# ============================================================================
# Step 2: Load Variable Analysis Results
# ============================================================================

cat("Step 2: Loading variable analysis results...\n")

if (!file.exists(analysis_report)) {
  stop("ERROR: Variable analysis report not found. Please run analyze_variables_for_susie.R first.")
}

var_analysis <- read_csv(analysis_report, show_col_types = FALSE)

# Filter to only usable variables
usable_vars <- var_analysis %>%
  filter(usable == TRUE) %>%
  select(variable, dataset, type, encoding_method, n_unique)

cat("  Found", nrow(usable_vars), "usable variables\n")
cat("  Breakdown by type:\n")
print(table(usable_vars$type, useNA = "ifany"))
cat("\n")

# Group variables by dataset
vars_by_dataset <- usable_vars %>%
  group_by(dataset) %>%
  summarise(
    variables = list(unique(variable)),
    n_vars = length(unique(variable)),
    .groups = "drop"
  )

cat("  Variables by dataset:\n")
print(vars_by_dataset)
cat("\n")

# ============================================================================
# Step 3: Load and Filter Data for Each Dataset
# ============================================================================

cat("Step 3: Loading and filtering data for each dataset...\n")
cat("  (This will take several minutes...)\n\n")

# Store loaded datasets
loaded_datasets <- list()

for (i in 1:nrow(vars_by_dataset)) {
  dataset_name <- vars_by_dataset$dataset[i]
  variables <- vars_by_dataset$variables[[i]]
  
  csv_file <- file.path(nhanes_dir, paste0(dataset_name, "_clean.csv"))
  
  if (!file.exists(csv_file)) {
    cat("  [", i, "/", nrow(vars_by_dataset), "] Skipping", dataset_name, "- file not found\n")
    next
  }
  
  cat("  [", i, "/", nrow(vars_by_dataset), "] Loading", dataset_name, 
      "(", length(variables), "variables)...\n")
  
  # Check available variables
  header <- fread(csv_file, nrows = 0, showProgress = FALSE)
  available_vars <- names(header)
  
  # Find variables that exist
  vars_to_load <- intersect(variables, available_vars)
  missing_vars <- setdiff(variables, available_vars)
  
  if (length(missing_vars) > 0) {
    cat("    Note:", length(missing_vars), "variables not found in file\n")
  }
  
  if (length(vars_to_load) == 0) {
    cat("    No variables to load\n")
    next
  }
  
  # Ensure SEQN is included for merging
  if (!"SEQN" %in% vars_to_load && "SEQN" %in% available_vars) {
    vars_to_load <- c("SEQN", vars_to_load)
  }
  
  # Load data
  tryCatch({
    cat("    Reading data...\n")
    data <- fread(
      csv_file,
      select = vars_to_load,
      showProgress = TRUE,
      data.table = FALSE
    )
    
    cat("    Filtering to eligible SEQNs...\n")
    # Filter to eligible SEQNs
    if ("SEQN" %in% names(data)) {
      data <- data %>%
        filter(SEQN %in% eligible_seqn)
      
      # Handle duplicates by taking first record per SEQN
      n_duplicates <- sum(duplicated(data$SEQN))
      if (n_duplicates > 0) {
        cat("    Found", n_duplicates, "duplicate SEQNs - taking first record\n")
        data <- data %>%
          distinct(SEQN, .keep_all = TRUE)
      }
      
      cat("    ✓ Loaded", nrow(data), "rows with", ncol(data), "columns\n\n")
      loaded_datasets[[dataset_name]] <- data
    } else {
      cat("    ✗ WARNING: SEQN not found, cannot filter\n")
    }
    
    # Garbage collection
    gc(verbose = FALSE)
    
  }, error = function(e) {
    cat("    ✗ ERROR loading", dataset_name, ":", e$message, "\n\n")
  })
}

cat("  Successfully loaded", length(loaded_datasets), "datasets\n\n")

# ============================================================================
# Step 4: Merge All Datasets
# ============================================================================

cat("Step 4: Merging all datasets by SEQN...\n")

# Find base dataset (prefer demographics or mortality)
preferred_bases <- c("demographics", "mortality")
base_dataset_name <- NULL
for (pref in preferred_bases) {
  if (pref %in% names(loaded_datasets)) {
    base_dataset_name <- pref
    break
  }
}

if (is.null(base_dataset_name)) {
  base_dataset_name <- names(loaded_datasets)[1]
}

cat("  Using", base_dataset_name, "as base dataset\n")

merged_data <- loaded_datasets[[base_dataset_name]]

# Merge remaining datasets
for (dataset_name in names(loaded_datasets)) {
  if (dataset_name == base_dataset_name) next
  
  cat("  Merging", dataset_name, "...\n")
  
  dataset_to_merge <- loaded_datasets[[dataset_name]]
  
  if (!"SEQN" %in% names(dataset_to_merge)) {
    cat("    WARNING: No SEQN in", dataset_name, ", skipping\n")
    next
  }
  
  # Check for column name conflicts
  common_cols <- intersect(names(merged_data), names(dataset_to_merge))
  common_cols <- setdiff(common_cols, "SEQN")
  
  if (length(common_cols) > 0) {
    cat("    Renaming", length(common_cols), "conflicting columns...\n")
    names(dataset_to_merge)[names(dataset_to_merge) %in% common_cols] <- 
      paste0(common_cols, "_", dataset_name)
  }
  
  # Perform left join
  merged_data <- suppressWarnings(
    merged_data %>%
      left_join(dataset_to_merge, by = "SEQN")
  )
  
  cat("    Rows after merge:", nrow(merged_data), "\n")
  
  # Garbage collection
  gc(verbose = FALSE)
}

cat("\n  Final merged dataset:", nrow(merged_data), "rows x", ncol(merged_data), "columns\n\n")

# ============================================================================
# Step 5: Remove ID and Non-Usable Variables
# ============================================================================

cat("Step 5: Cleaning up variables...\n")

# Remove ID variables
id_vars <- grep("^SEQN_new$|_ID$|^ID_|^SEQN_new", names(merged_data), value = TRUE)
if (length(id_vars) > 0) {
  cat("  Removing ID variables:", paste(id_vars, collapse = ", "), "\n")
  merged_data <- merged_data %>%
    select(-all_of(id_vars))
}

# Keep SEQN for reference but note it won't be used in regression
cat("  Keeping SEQN for reference (will be excluded from regression matrix)\n")
cat("  Final variables:", ncol(merged_data), "\n\n")

# ============================================================================
# Step 6: Save Results
# ============================================================================

cat("Step 6: Saving filtered dataset...\n")

# Save as RDS (compressed)
cat("  Saving to", output_file, "...\n")
saveRDS(merged_data, file = output_file, compress = "xz")

# Also save as CSV (for inspection, but may be large)
cat("  Saving to", output_csv, "...\n")
# Only save CSV if dataset is not too large (< 1GB)
estimated_size_mb <- (nrow(merged_data) * ncol(merged_data) * 8) / (1024^2)
if (estimated_size_mb < 1000) {
  write_csv(merged_data, file = output_csv)
  cat("  CSV file saved\n")
} else {
  cat("  CSV file skipped (dataset too large:", round(estimated_size_mb, 1), "MB)\n")
}

file_size_mb <- file.info(output_file)$size / (1024^2)
cat("  RDS file size:", round(file_size_mb, 2), "MB\n\n")

# ============================================================================
# Summary
# ============================================================================

cat("============================================================================\n")
cat("DATA LOADING AND FILTERING COMPLETE\n")
cat("============================================================================\n")
cat("Eligible SEQNs:", length(eligible_seqn), "\n")
cat("Final dataset dimensions:", nrow(merged_data), "rows x", ncol(merged_data), "columns\n")
cat("Variables loaded:", ncol(merged_data) - 1, "(excluding SEQN)\n")
cat("\nOutput files:\n")
cat("  RDS:", output_file, "\n")
if (estimated_size_mb < 1000) {
  cat("  CSV:", output_csv, "\n")
}
cat("\nNext steps:\n")
cat("  1. Encode categorical variables (one-hot or ordinal)\n")
cat("  2. Handle missing values\n")
cat("  3. Create numeric matrix for susieR\n")
cat("  4. Run susieR variable selection\n")
cat("============================================================================\n")

