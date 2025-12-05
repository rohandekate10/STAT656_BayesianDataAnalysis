# ============================================================================
# NHANES Filtration Script
# ============================================================================
# This script filters by:
# 1. Age >= 20
# 2. Caloric intake 800-6000 kcal
# 3. Exclude prior CVD history (heart failure, coronary disease, heart attack, stroke)
# to get eligible SEQN list
# ============================================================================

# Load required libraries
library(tidyverse)
library(data.table)  # For faster CSV reading of large files

# ============================================================================
# SETUP: Define paths and parameters
# ============================================================================

# Base directory containing the NHANES Clean 1988-2018 folder
base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"
nhanes_dir <- file.path(base_dir, "NHANES Clean 1988-2018")

# Age threshold
AGE_MIN <- 20

# Caloric intake thresholds
CALORIC_MIN <- 800
CALORIC_MAX <- 6000

cat("Starting NHANES filtration...\n")
cat("Data directory:", nhanes_dir, "\n\n")

# ============================================================================
# Filter demographics by age >= 20 to get eligible SEQN list
# ============================================================================

cat("Filtering demographics by age >= ", AGE_MIN, " to get eligible SEQN list...\n")

# Load demographics file to get age and SEQN
demographics_file <- file.path(nhanes_dir, "demographics_clean.csv")

if (!file.exists(demographics_file)) {
  stop("ERROR: Demographics file not found: ", demographics_file)
}

cat("  Loading demographics file to filter by age...\n")
demographics <- fread(
  demographics_file,
  select = c("SEQN", "RIDAGEYR"),
  showProgress = TRUE,
  data.table = FALSE
)

# Get total number of unique SEQNs
total_seqn <- length(unique(demographics$SEQN))
cat("  Total unique SEQNs in demographics:", total_seqn, "\n")
cat("  Total rows in demographics:", nrow(demographics), "\n")

# Filter by age >= 20
eligible_seqn <- demographics %>%
  filter(!is.na(RIDAGEYR) & RIDAGEYR >= AGE_MIN) %>%
  pull(SEQN) %>%
  unique()

cat("\n  Eligible SEQNs (age >= ", AGE_MIN, "):", length(eligible_seqn), "\n")
cat("  This represents", round(length(eligible_seqn) / total_seqn * 100, 1), 
    "% of total participants\n\n")

# Clean up demographics
rm(demographics)
gc(verbose = FALSE)

# ============================================================================
# Filter by caloric intake (800-6000 kcal)
# ============================================================================

cat("Filtering by caloric intake (", CALORIC_MIN, " - ", CALORIC_MAX, " kcal)...\n")

# Load dietary file to get caloric intake
dietary_file <- file.path(nhanes_dir, "dietary_clean.csv")

if (!file.exists(dietary_file)) {
  stop("ERROR: Dietary file not found: ", dietary_file)
}

cat("  Loading dietary file to filter by caloric intake...\n")
cat("  Using caloric variable: DRXTKCAL\n")

# Load dietary data with SEQN and DRXTKCAL, filtered to eligible SEQNs
dietary <- fread(
  dietary_file,
  select = c("SEQN", "DRXTKCAL"),
  showProgress = TRUE,
  data.table = FALSE
) %>%
  filter(SEQN %in% eligible_seqn)

cat("  Total rows in dietary (after age filter):", nrow(dietary), "\n")

# Handle duplicate SEQNs by taking the first record
n_duplicates <- sum(duplicated(dietary$SEQN))
if (n_duplicates > 0) {
  cat("  Found", n_duplicates, "duplicate SEQNs - taking first record per SEQN\n")
  dietary <- dietary %>%
    distinct(SEQN, .keep_all = TRUE)
  cat("  Rows after removing duplicates:", nrow(dietary), "\n")
}

# Filter by caloric intake range (800-6000 kcal)
eligible_seqn_caloric <- dietary %>%
  filter(!is.na(DRXTKCAL)) %>%
  filter(DRXTKCAL >= CALORIC_MIN & DRXTKCAL <= CALORIC_MAX) %>%
  pull(SEQN) %>%
  unique()

cat("\n  Eligible SEQNs (caloric intake ", CALORIC_MIN, "-", CALORIC_MAX, " kcal):", 
    length(eligible_seqn_caloric), "\n")
cat("  This represents", round(length(eligible_seqn_caloric) / length(eligible_seqn) * 100, 1), 
    "% of age-eligible participants\n\n")

# Clean up
rm(dietary)
gc(verbose = FALSE)

# ============================================================================
# Filter by prior CVD history (exclude if any CVD condition = 1)
# ============================================================================

cat("Filtering by prior CVD history (exclude participants with prior CVD)...\n")

# Load questionnaire file to get CVD history variables
questionnaire_file <- file.path(nhanes_dir, "questionnaire_clean.csv")

if (!file.exists(questionnaire_file)) {
  stop("ERROR: Questionnaire file not found: ", questionnaire_file)
}

cat("  Loading questionnaire file to check for prior CVD history...\n")

# Check what CVD variables are available
header <- fread(questionnaire_file, nrows = 0, showProgress = FALSE)
cvd_vars <- c("MCQ160B", "MCQ160C", "MCQ160E", "MCQ160F")
available_cvd_vars <- intersect(cvd_vars, names(header))

if (length(available_cvd_vars) == 0) {
  cat("  WARNING: No CVD history variables found in questionnaire file\n")
  cat("  Looking for variables with similar names...\n")
  # Try to find similar variable names
  cvd_vars_found <- grep("MCQ160|heart.*failure|coronary|heart.*attack|stroke", 
                          names(header), ignore.case = TRUE, value = TRUE)
  cat("  Found variables:", paste(head(cvd_vars_found, 10), collapse = ", "), "\n")
  eligible_seqn_final <- eligible_seqn_caloric
} else {
  cat("  Found CVD history variables:", paste(available_cvd_vars, collapse = ", "), "\n")
  
  # Load questionnaire data with CVD history variables, filtered to eligible SEQNs
  questionnaire <- fread(
    questionnaire_file,
    select = c("SEQN", available_cvd_vars),
    showProgress = TRUE,
    data.table = FALSE
  ) %>%
    filter(SEQN %in% eligible_seqn_caloric)
  
  cat("  Total rows in questionnaire (after previous filters):", nrow(questionnaire), "\n")
  
  # Handle duplicate SEQNs by taking the first record
  n_duplicates <- sum(duplicated(questionnaire$SEQN))
  if (n_duplicates > 0) {
    cat("  Found", n_duplicates, "duplicate SEQNs - taking first record per SEQN\n")
    questionnaire <- questionnaire %>%
      distinct(SEQN, .keep_all = TRUE)
    cat("  Rows after removing duplicates:", nrow(questionnaire), "\n")
  }
  
  # Check for prior CVD: exclude if any condition = 1 (Yes)
  cat("  Checking for prior CVD conditions...\n")
  prior_cvd <- rep(FALSE, nrow(questionnaire))
  
  for (cvd_var in available_cvd_vars) {
    # Check if value equals 1 (Yes)
    has_condition <- !is.na(questionnaire[[cvd_var]]) & questionnaire[[cvd_var]] == 1
    n_cases <- sum(has_condition, na.rm = TRUE)
    if (n_cases > 0) {
      cat("    Found", n_cases, "cases with", cvd_var, "\n")
    }
    prior_cvd <- prior_cvd | has_condition
  }
  
  questionnaire$Has_Prior_CVD <- prior_cvd
  total_prior_cvd <- sum(prior_cvd, na.rm = TRUE)
  cat("  Total participants with prior CVD:", total_prior_cvd, "\n")
  
  # Filter to exclude participants with prior CVD
  eligible_seqn_final <- questionnaire %>%
    filter(!Has_Prior_CVD) %>%
    pull(SEQN) %>%
    unique()
  
  cat("\n  Eligible SEQNs (no prior CVD history):", length(eligible_seqn_final), "\n")
  cat("  This represents", round(length(eligible_seqn_final) / length(eligible_seqn_caloric) * 100, 1), 
      "% of caloric-eligible participants\n\n")
  
  # Clean up
  rm(questionnaire)
  gc(verbose = FALSE)
}

# ============================================================================
# Filter by dietary recall status (exclude incomplete or unreliable data)
# ============================================================================

cat("Filtering by dietary recall status (exclude incomplete or unreliable data)...\n")

# Load dietary file again to check dietary recall status
cat("  Loading dietary file to check dietary recall status...\n")

# Check if DRXDRSTZ variable exists
header <- fread(dietary_file, nrows = 0, showProgress = FALSE)
if (!"DRXDRSTZ" %in% names(header)) {
  cat("  WARNING: DRXDRSTZ (Dietary Recall Status) not found in dietary file\n")
  cat("  Skipping dietary status filter\n")
  eligible_seqn_dietary_status <- eligible_seqn_final
} else {
  cat("  Using DRXDRSTZ (Dietary Recall Status) variable\n")
  cat("  Status codes: 1=Complete/Reliable, 2=Incomplete, 4=Not Reliable, 9=Don't know/Refused\n")
  
  # Load dietary data with SEQN and DRXDRSTZ, filtered to eligible SEQNs
  dietary_status <- fread(
    dietary_file,
    select = c("SEQN", "DRXDRSTZ"),
    showProgress = TRUE,
    data.table = FALSE
  ) %>%
    filter(SEQN %in% eligible_seqn_final)
  
  cat("  Total rows in dietary (after previous filters):", nrow(dietary_status), "\n")
  
  # Handle duplicate SEQNs by taking the first record
  n_duplicates <- sum(duplicated(dietary_status$SEQN))
  if (n_duplicates > 0) {
    cat("  Found", n_duplicates, "duplicate SEQNs - taking first record per SEQN\n")
    dietary_status <- dietary_status %>%
      distinct(SEQN, .keep_all = TRUE)
    cat("  Rows after removing duplicates:", nrow(dietary_status), "\n")
  }
  
  # Check status distribution
  status_table <- table(dietary_status$DRXDRSTZ, useNA = "ifany")
  cat("  Dietary recall status distribution:\n")
  print(status_table)
  
  # Filter to keep only complete and reliable dietary data (DRXDRSTZ = 1)
  # Exclude: 2 (incomplete), 4 (not reliable), 9 (don't know/refused), NA (missing)
  eligible_seqn_dietary_status <- dietary_status %>%
    filter(DRXDRSTZ == 1) %>%
    pull(SEQN) %>%
    unique()
  
  n_excluded <- nrow(dietary_status) - length(eligible_seqn_dietary_status)
  cat("\n  Excluded", n_excluded, "participants with incomplete or unreliable dietary data\n")
  cat("  Eligible SEQNs (complete and reliable dietary data):", length(eligible_seqn_dietary_status), "\n")
  cat("  This represents", round(length(eligible_seqn_dietary_status) / length(eligible_seqn_final) * 100, 1), 
      "% of CVD-eligible participants\n\n")
  
  # Clean up
  rm(dietary_status)
  gc(verbose = FALSE)
}

cat("============================================================================\n")
cat("FILTRATION COMPLETE\n")
cat("============================================================================\n")
cat("Total SEQNs:", total_seqn, "\n")
cat("SEQNs remaining after age filtration (age >= ", AGE_MIN, "):", length(eligible_seqn), "\n")
cat("SEQNs remaining after caloric intake filtration (", CALORIC_MIN, "-", CALORIC_MAX, " kcal):", 
    length(eligible_seqn_caloric), "\n")
cat("SEQNs remaining after CVD history exclusion:", length(eligible_seqn_final), "\n")
cat("SEQNs remaining after dietary status filter (complete/reliable only):", length(eligible_seqn_dietary_status), "\n")
cat("============================================================================\n")
