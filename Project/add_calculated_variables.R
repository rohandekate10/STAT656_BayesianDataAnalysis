# ============================================================================
# Add Calculated Variables to Final Dataset
# ============================================================================
# This script adds calculated/derived variables as specified in README.md:
# 1. Average Blood Pressure (Systolic and Diastolic)
# 2. Diabetes Status (binary)
# 3. Smoking Status (if variables available)
# ============================================================================

library(tidyverse)

# ============================================================================
# SETUP
# ============================================================================

base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"
input_file <- file.path(base_dir, "nhanes_filtered_for_susie.rds")
output_file <- file.path(base_dir, "nhanes_filtered_for_susie.rds")
backup_file <- file.path(base_dir, "nhanes_filtered_for_susie_backup.rds")

cat("============================================================================\n")
cat("Adding Calculated Variables to Final Dataset\n")
cat("============================================================================\n\n")

# ============================================================================
# Step 1: Load Dataset
# ============================================================================

cat("Step 1: Loading dataset...\n")
data <- readRDS(input_file)

cat("  Original dimensions:", nrow(data), "rows x", ncol(data), "columns\n")

# Create backup
cat("  Creating backup...\n")
file.copy(input_file, backup_file, overwrite = TRUE)
cat("  Backup saved to:", backup_file, "\n\n")

# ============================================================================
# Step 2: Calculate Average Blood Pressure
# ============================================================================

cat("Step 2: Calculating average blood pressure...\n")

# Check for BP variables
bp_sys_vars <- c("BPXSY1", "BPXSY2", "BPXSY3")
bp_dia_vars <- c("BPXDI1", "BPXDI2", "BPXDI3")

available_sys <- intersect(bp_sys_vars, names(data))
available_dia <- intersect(bp_dia_vars, names(data))

cat("  Available systolic BP variables:", paste(available_sys, collapse = ", "), "\n")
cat("  Available diastolic BP variables:", paste(available_dia, collapse = ", "), "\n")

if (length(available_sys) > 0) {
  # Calculate average systolic BP
  data <- data %>%
    rowwise() %>%
    mutate(
      Sys_BP = mean(c_across(all_of(available_sys)), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Convert Inf to NA (when all values are NA)
  data$Sys_BP[is.infinite(data$Sys_BP)] <- NA
  
  n_complete_sys <- sum(!is.na(data$Sys_BP))
  cat("  ✓ Created Sys_BP:", n_complete_sys, "complete values (", 
      round(n_complete_sys / nrow(data) * 100, 1), "%)\n")
} else {
  cat("  ✗ WARNING: No systolic BP variables found\n")
}

if (length(available_dia) > 0) {
  # Calculate average diastolic BP
  data <- data %>%
    rowwise() %>%
    mutate(
      Dia_BP = mean(c_across(all_of(available_dia)), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Convert Inf to NA (when all values are NA)
  data$Dia_BP[is.infinite(data$Dia_BP)] <- NA
  
  n_complete_dia <- sum(!is.na(data$Dia_BP))
  cat("  ✓ Created Dia_BP:", n_complete_dia, "complete values (", 
      round(n_complete_dia / nrow(data) * 100, 1), "%)\n")
} else {
  cat("  ✗ WARNING: No diastolic BP variables found\n")
}

cat("\n")

# ============================================================================
# Step 3: Calculate Diabetes Status
# ============================================================================

cat("Step 3: Calculating diabetes status...\n")

if ("DIQ010" %in% names(data)) {
  # Diabetes Status: 1 = Yes/Borderline, 0 = No
  # DIQ010: 1 = Yes, 2 = No, 3 = Borderline
  data <- data %>%
    mutate(
      Diabetic = case_when(
        DIQ010 == 1 ~ 1,      # Yes
        DIQ010 == 3 ~ 1,      # Borderline (treated as Yes)
        DIQ010 == 2 ~ 0,      # No
        TRUE ~ NA_real_       # Missing/Unknown
      )
    )
  
  n_complete_diab <- sum(!is.na(data$Diabetic))
  n_diabetic <- sum(data$Diabetic == 1, na.rm = TRUE)
  cat("  ✓ Created Diabetic:", n_complete_diab, "complete values (", 
      round(n_complete_diab / nrow(data) * 100, 1), "%)\n")
  cat("    Diabetic cases:", n_diabetic, "(", 
      round(n_diabetic / n_complete_diab * 100, 1), "% of complete)\n")
} else {
  cat("  ✗ WARNING: DIQ010 variable not found\n")
}

cat("\n")

# ============================================================================
# Step 4: Calculate Smoking Status (if variables available)
# ============================================================================

cat("Step 4: Checking for smoking variables...\n")

smoking_vars <- c("SMQ020", "SMQ040")
available_smoking <- intersect(smoking_vars, names(data))

# Also check for variables with similar names (case-insensitive, partial match)
if (length(available_smoking) < 2) {
  all_vars <- names(data)
  # Try to find SMQ variables with different case or naming
  smq020_alt <- grep("SMQ020|smq020", all_vars, value = TRUE, ignore.case = TRUE)
  smq040_alt <- grep("SMQ040|smq040", all_vars, value = TRUE, ignore.case = TRUE)
  
  if (length(smq020_alt) > 0 && !"SMQ020" %in% available_smoking) {
    available_smoking <- c(available_smoking, smq020_alt[1])
    cat("  Found alternative name for SMQ020:", smq020_alt[1], "\n")
  }
  if (length(smq040_alt) > 0 && !"SMQ040" %in% available_smoking) {
    available_smoking <- c(available_smoking, smq040_alt[1])
    cat("  Found alternative name for SMQ040:", smq040_alt[1], "\n")
  }
}

if (length(available_smoking) >= 2) {
  # Get the actual variable names
  smq020_var <- available_smoking[grep("020|SMQ020|smq020", available_smoking, ignore.case = TRUE)[1]]
  smq040_var <- available_smoking[grep("040|SMQ040|smq040", available_smoking, ignore.case = TRUE)[1]]
  
  cat("  Found smoking variables:", smq020_var, "and", smq040_var, "\n")
  
  # Smoking Status: 1 = Current smoker, 2 = Former smoker, 3 = Never smoked
  # SMQ020: Smoked >100 cigarettes in life? (1=Yes, 2=No)
  # SMQ040: Do you now smoke? (1=Every day, 2=Some days, 3=Not at all)
  data <- data %>%
    mutate(
      Smoker = case_when(
        !!sym(smq020_var) == 2 ~ 3,                                    # Never (never smoked >100)
        !!sym(smq020_var) == 1 & !!sym(smq040_var) == 3 ~ 2,          # Former (smoked but quit)
        !!sym(smq020_var) == 1 & (!!sym(smq040_var) == 1 | !!sym(smq040_var) == 2) ~ 1,  # Current (smokes every day or some days)
        TRUE ~ NA_real_                                                # Missing/Unknown
      )
    )
  
  n_complete_smoke <- sum(!is.na(data$Smoker))
  n_current <- sum(data$Smoker == 1, na.rm = TRUE)
  n_former <- sum(data$Smoker == 2, na.rm = TRUE)
  n_never <- sum(data$Smoker == 3, na.rm = TRUE)
  
  cat("  ✓ Created Smoker:", n_complete_smoke, "complete values (", 
      round(n_complete_smoke / nrow(data) * 100, 1), "%)\n")
  cat("    Current smokers:", n_current, "\n")
  cat("    Former smokers:", n_former, "\n")
  cat("    Never smoked:", n_never, "\n")
} else {
  cat("  ✗ WARNING: Smoking variables not found (SMQ020, SMQ040)\n")
  cat("    Found only:", paste(available_smoking, collapse = ", "), "\n")
  cat("    Smoking status cannot be calculated\n")
  cat("    NOTE: These variables may need to be added to the source data files first\n")
}

cat("\n")

# ============================================================================
# Step 5: Calculate eGFR (Estimated Glomerular Filtration Rate)
# ============================================================================

cat("Step 5: Calculating eGFR (Estimated Glomerular Filtration Rate)...\n")

# Check for required variables
required_vars <- c("RIDAGEYR", "RIAGENDR", "RIDRETH1", "LBXSCR")
available_vars <- intersect(required_vars, names(data))
missing_vars <- setdiff(required_vars, available_vars)

if (length(missing_vars) == 0) {
  cat("  All required variables found\n")
  
  # eGFR calculation using CKD-EPI equation
  # Formula: eGFR = 141 × (min(Scr/k, 1)^alpha) × (max(Scr/k, 1)^(-1.209)) × (0.993^Age) × is_female × is_black
  # Where:
  #   k: 0.7 for females, 0.9 for males
  #   alpha: -0.329 for females, -0.411 for males
  #   is_female: 1.018 for females, 1.0 for males
  #   is_black: 1.159 for Non-Hispanic Black, 1.0 otherwise
  #   RIAGENDR: 1 = Male, 2 = Female
  #   RIDRETH1: 4 = Non-Hispanic Black
  
  # Calculate eGFR using CKD-EPI equation
  # Formula: eGFR = 141 × (min(Scr/k, 1)^alpha) × (max(Scr/k, 1)^(-1.209)) × (0.993^Age) × is_female × is_black
  data <- data %>%
    mutate(
      # Calculate parameters based on sex
      k = if_else(RIAGENDR == 2, 0.7, 0.9),                    # Female=0.7, Male=0.9
      alpha = if_else(RIAGENDR == 2, -0.329, -0.411),         # Female=-0.329, Male=-0.411
      is_female = if_else(RIAGENDR == 2, 1.018, 1.0),         # Female=1.018, Male=1.0
      is_black = if_else(RIDRETH1 == 4, 1.159, 1.0),          # Non-Hispanic Black=1.159, else=1.0
      Scr_k = LBXSCR / k,
      min_Scr_k = pmin(Scr_k, 1),
      max_Scr_k = pmax(Scr_k, 1),
      eGFR = 141 * (min_Scr_k^alpha) * (max_Scr_k^(-1.209)) * (0.993^RIDAGEYR) * is_female * is_black
    ) %>%
    # Remove intermediate variables
    select(-k, -alpha, -is_female, -is_black, -Scr_k, -min_Scr_k, -max_Scr_k)
  
  # Handle cases where calculation results in invalid values
  data$eGFR[!is.finite(data$eGFR)] <- NA
  data$eGFR[data$eGFR < 0] <- NA  # eGFR should be positive
  
  n_complete_egfr <- sum(!is.na(data$eGFR))
  n_with_creat <- sum(!is.na(data$LBXSCR))
  
  cat("  ✓ Created eGFR:", n_complete_egfr, "complete values (", 
      round(n_complete_egfr / nrow(data) * 100, 1), "%)\n")
  cat("    Participants with creatinine:", n_with_creat, "\n")
  cat("    eGFR range:", round(min(data$eGFR, na.rm = TRUE), 2), "-", 
      round(max(data$eGFR, na.rm = TRUE), 2), "mL/min/1.73m²\n")
  cat("    Mean eGFR:", round(mean(data$eGFR, na.rm = TRUE), 2), "mL/min/1.73m²\n")
} else {
  cat("  ✗ WARNING: Missing required variables:", paste(missing_vars, collapse = ", "), "\n")
  cat("    eGFR cannot be calculated\n")
}

cat("\n")

# ============================================================================
# Step 6: Summary of New Variables
# ============================================================================

cat("Step 5: Summary of new variables...\n")

new_vars <- c("Sys_BP", "Dia_BP", "Diabetic", "Smoker", "eGFR")
new_vars_found <- intersect(new_vars, names(data))

cat("  New variables created:", length(new_vars_found), "\n")
for (var in new_vars_found) {
  n_complete <- sum(!is.na(data[[var]]))
  pct_complete <- round(n_complete / nrow(data) * 100, 1)
  cat("    -", var, ":", n_complete, "complete (", pct_complete, "%)\n")
}

cat("\n")

# ============================================================================
# Step 7: Save Updated Dataset
# ============================================================================

cat("Step 7: Saving updated dataset...\n")

cat("  Final dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("  New variables added:", length(new_vars_found), "\n")

# Save updated dataset
saveRDS(data, file = output_file, compress = "xz")

file_size_mb <- file.info(output_file)$size / (1024^2)
cat("  ✓ Dataset saved to:", output_file, "\n")
cat("  File size:", round(file_size_mb, 2), "MB\n")

cat("\n============================================================================\n")
cat("CALCULATED VARIABLES ADDED SUCCESSFULLY\n")
cat("============================================================================\n")
cat("Backup of original dataset:", backup_file, "\n")
cat("Updated dataset:", output_file, "\n")
cat("\nNew variables:\n")
for (var in new_vars_found) {
  cat("  -", var, "\n")
}
cat("\n============================================================================\n")
cat("NOTE: Run verify_and_add_table_variables.R to ensure all Table S1 and S2\n")
cat("      variables are included in the final dataset.\n")
cat("============================================================================\n")

