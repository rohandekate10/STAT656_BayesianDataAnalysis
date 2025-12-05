# ============================================================================
# Variable Analysis Script for susieR Variable Selection
# ============================================================================
# This script analyzes all variables from the NHANES dictionary to determine
# which can be converted to numeric form for use with susieR package.
# Variables can be:
# - Already numeric (continuous or discrete)
# - Categorical (can be encoded)
# - Not usable (text, IDs, etc.)
# ============================================================================

library(tidyverse)
library(data.table)

# ============================================================================
# SETUP
# ============================================================================

base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"
nhanes_dir <- file.path(base_dir, "NHANES Clean 1988-2018")
output_file <- file.path(base_dir, "variable_analysis_results.rds")
report_file <- file.path(base_dir, "variable_analysis_report.csv")

cat("Starting variable analysis for susieR...\n")
cat("This will analyze all variables to determine usability for regression.\n\n")

# ============================================================================
# Load Dictionary
# ============================================================================

cat("Step 1: Loading dictionary...\n")
dictionary <- read_csv(
  file.path(nhanes_dir, "dictionary_nhanes.csv"),
  show_col_types = FALSE
)

# Remove the first column (row numbers) if it exists
if (names(dictionary)[1] %in% c("...1", "V1")) {
  dictionary <- dictionary[, -1]
}

# Clean up dictionary
valid_datasets <- c("demographics", "dietary", "mortality", "questionnaire", 
                    "response", "chemicals", "comments", "medications", 
                    "occupation", "weights")

dictionary <- dictionary %>%
  filter(in_dataset %in% valid_datasets) %>%
  filter(!is.na(variable_codename_use)) %>%
  filter(variable_codename_use != "")

# Explicitly add SMQ020 and SMQ040 if not in dictionary (smoking variables)
# These are critical for calculating smoking status
smq_vars <- c("SMQ020", "SMQ040")
for (smq_var in smq_vars) {
  if (!smq_var %in% dictionary$variable_codename_use) {
    cat("  Adding", smq_var, "to dictionary (smoking variable)...\n")
    dictionary <- rbind(dictionary, data.frame(
      variable_codename_use = smq_var,
      variable_description_use = ifelse(smq_var == "SMQ020", 
                                         "Smoked at least 100 cigarettes in life",
                                         "Do you now smoke cigarettes"),
      in_dataset = "questionnaire",
      units = NA,
      cas_num = NA,
      comment_codename_use = NA,
      chemical_family = NA,
      chemical_family_shorten = NA
    ))
  }
}

cat("  Found", nrow(dictionary), "variables across", 
    length(unique(dictionary$in_dataset)), "datasets\n\n")

# ============================================================================
# Function to Analyze Variable Type
# ============================================================================

analyze_variable <- function(var_name, dataset_name, sample_data) {
  # Initialize result
  result <- list(
    variable = var_name,
    dataset = dataset_name,
    usable = FALSE,
    type = NA_character_,
    encoding_method = NA_character_,
    n_unique = NA_integer_,
    n_missing = NA_integer_,
    n_total = length(sample_data),
    sample_values = NA_character_,
    notes = NA_character_
  )
  
  if (length(sample_data) == 0) {
    result$notes <- "No data available"
    return(result)
  }
  
  # Count missing
  n_missing <- sum(is.na(sample_data))
  result$n_missing <- n_missing
  
  # Get non-missing values
  non_missing <- sample_data[!is.na(sample_data)]
  if (length(non_missing) == 0) {
    result$notes <- "All values missing"
    return(result)
  }
  
  n_unique <- length(unique(non_missing))
  result$n_unique <- n_unique
  
  # Exclude ID variables
  if (grepl("^SEQN|_ID$|ID$|^ID_", var_name, ignore.case = TRUE) && 
      n_unique > 100) {
    result$notes <- "ID variable - exclude"
    return(result)
  }
  
  # Check if already numeric
  if (is.numeric(non_missing)) {
    result$usable <- TRUE
    result$type <- "numeric"
    result$encoding_method <- "none"
    unique_vals <- head(unique(non_missing), 5)
    result$sample_values <- paste(unique_vals, collapse = ", ")
    return(result)
  }
  
  # Try to convert to numeric
  numeric_conversion <- suppressWarnings(as.numeric(non_missing))
  n_numeric <- sum(!is.na(numeric_conversion))
  
  if (n_numeric == length(non_missing)) {
    # All values can be converted to numeric
    result$usable <- TRUE
    result$type <- "numeric_string"
    result$encoding_method <- "as.numeric"
    unique_vals <- head(unique(non_missing), 5)
    result$sample_values <- paste(unique_vals, collapse = ", ")
    return(result)
  }
  
  # Check if it's a character/factor that can be encoded
  if (is.character(non_missing) || is.factor(non_missing)) {
    # Check for ID-like variables (should exclude)
    if (grepl("SEQN|ID|code", var_name, ignore.case = TRUE) && 
        n_unique > 1000) {
      result$notes <- "ID variable - too many unique values"
      return(result)
    }
    
    # Check if it's categorical (reasonable number of categories)
    if (n_unique <= 50) {
      result$usable <- TRUE
      result$type <- "categorical"
      result$encoding_method <- "one-hot or ordinal"
      unique_vals <- head(unique(non_missing), 5)
      result$sample_values <- paste(unique_vals, collapse = ", ")
      return(result)
    } else if (n_unique <= 200) {
      # Many categories but might be ordinal
      result$usable <- TRUE
      result$type <- "categorical_many"
      result$encoding_method <- "ordinal or group rare categories"
      unique_vals <- head(unique(non_missing), 5)
      result$sample_values <- paste(unique_vals, collapse = ", ")
      return(result)
    } else {
      # Too many unique values - likely not usable
      result$notes <- paste("Too many unique values (", n_unique, ")", sep = "")
      return(result)
    }
  }
  
  # Check for logical/boolean
  if (is.logical(non_missing)) {
    result$usable <- TRUE
    result$type <- "logical"
    result$encoding_method <- "as.numeric (0/1)"
    result$sample_values <- paste(unique(non_missing), collapse = ", ")
    return(result)
  }
  
  result$notes <- "Unknown type or not convertible"
  return(result)
}

# ============================================================================
# Analyze Variables from Each Dataset
# ============================================================================

cat("Step 2: Analyzing variables from each dataset...\n")
cat("  (This will take several minutes...)\n\n")

# Group variables by dataset
vars_by_dataset <- dictionary %>%
  group_by(in_dataset) %>%
  summarise(
    variables = list(unique(variable_codename_use)),
    .groups = "drop"
  )

# Store results
all_results <- list()

# Sample size for analysis (to speed up - analyze first 20k rows)
SAMPLE_SIZE <- 20000

for (i in 1:nrow(vars_by_dataset)) {
  dataset_name <- vars_by_dataset$in_dataset[i]
  variables <- vars_by_dataset$variables[[i]]
  
  csv_file <- file.path(nhanes_dir, paste0(dataset_name, "_clean.csv"))
  
  if (!file.exists(csv_file)) {
    cat("  Skipping", dataset_name, "- file not found\n")
    next
  }
  
  cat("  [", i, "/", nrow(vars_by_dataset), "] Analyzing", dataset_name, 
      "dataset (", length(variables), "variables)...\n")
  
  # Read header to check available variables
  header <- fread(csv_file, nrows = 0, showProgress = FALSE)
  available_vars <- names(header)
  
  # Find variables that exist
  vars_to_check <- intersect(variables, available_vars)
  
  # Remove SEQN from vars_to_check if it's there (we'll add it separately)
  vars_to_check <- setdiff(vars_to_check, "SEQN")
  
  if (length(vars_to_check) == 0) {
    cat("    No variables found in file\n")
    next
  }
  
  # Read a sample of data for analysis
  cat("    Reading sample data (first", SAMPLE_SIZE, "rows)...\n")
  tryCatch({
    # Select SEQN and other variables (ensure SEQN is first if it exists)
    select_vars <- if ("SEQN" %in% available_vars) {
      c("SEQN", vars_to_check)
    } else {
      vars_to_check
    }
    
    sample_data <- fread(
      csv_file,
      select = select_vars,
      nrows = SAMPLE_SIZE,
      showProgress = FALSE,
      data.table = FALSE
    )
    
    # Analyze each variable
    cat("    Analyzing", length(vars_to_check), "variables...\n")
    for (j in 1:length(vars_to_check)) {
      var_name <- vars_to_check[j]
      if (var_name == "SEQN") next  # Skip SEQN itself
      
      var_data <- sample_data[[var_name]]
      result <- analyze_variable(var_name, dataset_name, var_data)
      all_results[[length(all_results) + 1]] <- result
      
      if (j %% 50 == 0) {
        cat("      Analyzed", j, "/", length(vars_to_check), "variables...\n")
      }
    }
    
    cat("    ✓ Completed", dataset_name, "- analyzed", length(vars_to_check), "variables\n\n")
  }, error = function(e) {
    cat("    ✗ ERROR processing", dataset_name, ":", e$message, "\n\n")
  })
}

# ============================================================================
# Compile Results
# ============================================================================

cat("Step 3: Compiling results...\n")

if (length(all_results) == 0) {
  stop("No variables were analyzed. Check for errors above.")
}

results_df <- bind_rows(all_results)

# Summary statistics
cat("\n============================================================================\n")
cat("ANALYSIS SUMMARY\n")
cat("============================================================================\n")
cat("Total variables analyzed:", nrow(results_df), "\n")
cat("Usable variables:", sum(results_df$usable, na.rm = TRUE), "\n")
cat("Not usable variables:", sum(!results_df$usable, na.rm = TRUE), "\n")

cat("\nVariable types:\n")
type_table <- table(results_df$type, useNA = "ifany")
print(type_table)

cat("\nEncoding methods:\n")
encoding_table <- table(results_df$encoding_method, useNA = "ifany")
print(encoding_table)

# Show breakdown by dataset
cat("\nUsable variables by dataset:\n")
usable_by_dataset <- results_df %>%
  filter(usable == TRUE) %>%
  group_by(dataset) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
print(usable_by_dataset)

# Save results
cat("\nSaving results...\n")
saveRDS(results_df, file = output_file)
write_csv(results_df, file = report_file)

cat("\nResults saved to:\n")
cat("  RDS file:", output_file, "\n")
cat("  CSV file:", report_file, "\n")

# Show sample of usable variables
cat("\nSample usable variables (first 20):\n")
usable_vars <- results_df %>%
  filter(usable == TRUE) %>%
  select(variable, dataset, type, encoding_method, n_unique) %>%
  head(20)
print(usable_vars)

cat("\n============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================================\n")
cat("Next steps:\n")
cat("  1. Review the CSV report file for detailed variable information\n")
cat("  2. Filter variables to eligible SEQNs\n")
cat("  3. Encode categorical variables as needed\n")
cat("  4. Prepare data matrix for susieR\n")
cat("============================================================================\n")
