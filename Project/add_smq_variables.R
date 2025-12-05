# ============================================================================
# Add SMQ Variables (Smoking) to Final Dataset
# ============================================================================
# This script checks for SMQ020 and SMQ040 in source files and adds them
# to the final dataset if they exist
# ============================================================================

library(tidyverse)
library(data.table)

# ============================================================================
# SETUP
# ============================================================================

base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"
nhanes_dir <- file.path(base_dir, "NHANES Clean 1988-2018")
data_file <- file.path(base_dir, "nhanes_filtered_for_susie.rds")

cat("============================================================================\n")
cat("Adding SMQ Variables (Smoking) to Final Dataset\n")
cat("============================================================================\n\n")

# ============================================================================
# Step 1: Load Final Dataset
# ============================================================================

cat("Step 1: Loading final dataset...\n")
data <- readRDS(data_file)

# Get eligible SEQNs
eligible_seqn <- data$SEQN

cat("  Dataset dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("  Eligible SEQNs:", length(eligible_seqn), "\n\n")

# ============================================================================
# Step 2: Check for SMQ Variables in Source Files
# ============================================================================

cat("Step 2: Checking for SMQ variables in source files...\n")

smq_vars <- c("SMQ020", "SMQ040")
found_smq <- list()

# Check questionnaire file
q_file <- file.path(nhanes_dir, "questionnaire_clean.csv")
if (file.exists(q_file)) {
  cat("  Checking questionnaire_clean.csv...\n")
  header_q <- fread(q_file, nrows = 0, showProgress = FALSE)
  available_smq_q <- intersect(smq_vars, names(header_q))
  if (length(available_smq_q) > 0) {
    cat("    Found:", paste(available_smq_q, collapse = ", "), "\n")
    found_smq[["questionnaire"]] <- available_smq_q
  }
}

# Check response file
r_file <- file.path(nhanes_dir, "response_clean.csv")
if (file.exists(r_file)) {
  cat("  Checking response_clean.csv...\n")
  header_r <- fread(r_file, nrows = 0, showProgress = FALSE)
  available_smq_r <- intersect(smq_vars, names(header_r))
  if (length(available_smq_r) > 0) {
    cat("    Found:", paste(available_smq_r, collapse = ", "), "\n")
    found_smq[["response"]] <- available_smq_r
  }
}

# Check all other files
other_files <- list.files(nhanes_dir, pattern = "_clean.csv", full.names = TRUE)
for (f in other_files) {
  if (basename(f) %in% c("questionnaire_clean.csv", "response_clean.csv")) next
  
  header <- tryCatch({
    fread(f, nrows = 0, showProgress = FALSE)
  }, error = function(e) NULL)
  
  if (!is.null(header)) {
    available_smq <- intersect(smq_vars, names(header))
    if (length(available_smq) > 0) {
      dataset_name <- gsub("_clean.csv", "", basename(f))
      cat("  Found in", basename(f), ":", paste(available_smq, collapse = ", "), "\n")
      found_smq[[dataset_name]] <- available_smq
    }
  }
}

cat("\n")

# ============================================================================
# Step 3: Load SMQ Variables and Merge
# ============================================================================

if (length(found_smq) > 0) {
  cat("Step 3: Loading SMQ variables and merging...\n")
  
  # Collect all SMQ variables found
  all_smq_found <- unique(unlist(found_smq))
  cat("  SMQ variables to add:", paste(all_smq_found, collapse = ", "), "\n")
  
  # Load from first source that has them
  smq_data <- NULL
  for (dataset_name in names(found_smq)) {
    if (length(found_smq[[dataset_name]]) > 0) {
      csv_file <- file.path(nhanes_dir, paste0(dataset_name, "_clean.csv"))
      if (file.exists(csv_file)) {
        cat("  Loading from", basename(csv_file), "...\n")
        smq_data <- fread(
          csv_file,
          select = c("SEQN", found_smq[[dataset_name]]),
          showProgress = TRUE,
          data.table = FALSE
        ) %>%
          filter(SEQN %in% eligible_seqn) %>%
          distinct(SEQN, .keep_all = TRUE)
        
        cat("    Loaded", nrow(smq_data), "rows\n")
        break
      }
    }
  }
  
  if (!is.null(smq_data)) {
    # Check if variables already exist in dataset
    existing_smq <- intersect(all_smq_found, names(data))
    if (length(existing_smq) > 0) {
      cat("  WARNING: Variables already exist:", paste(existing_smq, collapse = ", "), "\n")
      cat("    Skipping merge for existing variables\n")
      all_smq_found <- setdiff(all_smq_found, existing_smq)
    }
    
    if (length(all_smq_found) > 0) {
      # Merge new SMQ variables
      data <- data %>%
        left_join(
          smq_data %>% select(SEQN, all_of(all_smq_found)),
          by = "SEQN"
        )
      
      cat("  ✓ Merged SMQ variables into dataset\n")
      cat("    New dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
    }
  }
} else {
  cat("Step 3: No SMQ variables found in source files\n")
  cat("  SMQ020 and SMQ040 are not available in the cleaned data files\n")
  cat("  These variables may need to be added to the source data files first\n")
}

cat("\n")

# ============================================================================
# Step 4: Save Updated Dataset
# ============================================================================

cat("Step 4: Saving updated dataset...\n")

saveRDS(data, file = data_file, compress = "xz")

file_size_mb <- file.info(data_file)$size / (1024^2)
cat("  ✓ Dataset saved to:", data_file, "\n")
cat("  File size:", round(file_size_mb, 2), "MB\n")

cat("\n============================================================================\n")
cat("SMQ VARIABLES ADDITION COMPLETE\n")
cat("============================================================================\n")
if (length(found_smq) > 0) {
  cat("SMQ variables added:", paste(all_smq_found, collapse = ", "), "\n")
  cat("Next step: Run add_calculated_variables.R to calculate Smoker status\n")
} else {
  cat("No SMQ variables found. They may need to be added to source files.\n")
}
cat("============================================================================\n")

