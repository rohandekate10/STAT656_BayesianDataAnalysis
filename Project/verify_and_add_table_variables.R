# ============================================================================
# Verify and Add Table S1 and S2 Variables
# ============================================================================
# This script ensures the final dataset contains all variables from:
# - Table S1: Health Model variables
# - Table S2: Nutrition Model variables
# ============================================================================

library(tidyverse)

# ============================================================================
# SETUP
# ============================================================================

base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"
nhanes_dir <- file.path(base_dir, "NHANES Clean 1988-2018")
input_file <- file.path(base_dir, "nhanes_filtered_for_susie.rds")
output_file <- file.path(base_dir, "nhanes_filtered_for_susie.rds")

cat("============================================================================\n")
cat("Verifying and Adding Table S1 and S2 Variables\n")
cat("============================================================================\n\n")

# ============================================================================
# Define Variable Mappings
# ============================================================================

# Table S1: Health Model Variables
table_s1_vars <- list(
  "Age" = "RIDAGEYR",
  "Alcohol" = "ALQ120U",  # Alcohol habits - need to check actual variable
  "BMI" = "BMXBMI",
  "Ca" = "LBXSCA",  # Blood calcium
  "Dia_BP" = "Dia_BP",  # Calculated
  "Diabetic" = "Diabetic",  # Calculated
  "Education" = "DMDEDUC2",
  "eGFR" = "eGFR",  # Calculated
  "Fe" = "LBXSIR",  # Blood iron
  "GlycoHb" = "LBXGH",  # Glycohemoglobin
  "Hb" = "LBXHGB",  # Hemoglobin
  "HDL" = "LBDHDD",  # HDL cholesterol
  "Health_ins" = "HIQ011",  # Health insurance
  "K" = "LBXSKSI",  # Blood potassium
  "LDH" = "LBXSLDSI",  # Lactate dehydrogenase
  "Men_Health" = "HSQ480",  # Mental health days
  "Na" = "LBXSNASI",  # Blood sodium
  "P" = "LBXSPH",  # Blood phosphorus
  "Phy_Health" = "HSQ470",  # Physical health days
  "Race" = "RIDRETH1",  # Race/ethnicity
  "Sex" = "RIAGENDR",
  "Smoker" = "Smoker",  # Calculated
  "Sys_BP" = "Sys_BP",  # Calculated
  "Total_Cho" = "LBXTC",  # Total cholesterol
  "Uric_acid" = "LBXSUA",  # Uric acid
  "Waist" = "BMXWAIST"
)

# Table S2: Nutrition Model Variables
# Note: NHANES uses DRXT prefix for Day 1 total nutrients, not DR1T
table_s2_vars <- list(
  "Alc_int" = "DRXTALCO",  # Alcohol intake (g) - DRXTALCO
  "Ca_int" = "DRXTCALC",  # Calcium intake (mg) - DRXTCALC
  "Caff_int" = "DRXTCAFF",  # Caffeine intake (mg) - DRXTCAFF
  "Cho_int" = "DRXTCHOL",  # Cholesterol intake (mg) - DRXTCHOL
  "Cu_int" = "DRXTCOPP",  # Copper intake (mg) - DRXTCOPP
  "Fe_int" = "DRXTIRON",  # Iron intake (mg) - DRXTIRON
  "Fib_int" = "DRXTFIBE",  # Dietary fiber (g) - DRXTFIBE
  "Fish_int" = "DRD370",  # Fish intake - need to check
  "Fol_int" = "DRXTFOLA",  # Total folate (μg) - DRXTFOLA
  "K_int" = "DRXTPOTA",  # Potassium intake (mg) - DRXTPOTA
  "MFA_int" = "DRXTMFAT",  # Monounsaturated fatty acids (g) - DRXTMFAT
  "Mg_int" = "DRXTMAGN",  # Magnesium intake (mg) - DRXTMAGN
  "Milk_int" = "DMQMILIZ",  # Milk intake - DMQMILIZ
  "Na_int" = "DRXTSODI",  # Sodium intake (mg) - DRXTSODI
  "Nia_int" = "DRXTNIAC",  # Niacin intake (mg) - DRXTNIAC
  "PFA_int" = "DRXTPFAT",  # Polyunsaturated fatty acids (g) - DRXTPFAT
  "Prot_int" = "DRXTPROT",  # Protein intake (g) - DRXTPROT
  "Salt_freq" = "DRQSPREP",  # Salt frequency - need to check
  "Salt_prep" = "DRQSPREP",  # Salt in preparation - DRQSPREP
  "Salt_table" = "DRQSDIET",  # Salt at table - DRQSDIET
  "SFA_int" = "DRXTSFAT",  # Saturated fatty acids (g) - DRXTSFAT
  "Sug_int" = "DRXTSUGR",  # Total sugars (g) - DRXTSUGR
  "Theo_int" = "DRXTTHEO",  # Theobromine (mg) - DRXTTHEO
  "Vit_A_int" = "DRXTVARA",  # Vitamin A (μg) - DRXTVARA
  "Vit_B1_int" = "DRXTVB1",  # Thiamin (mg) - DRXTVB1
  "Vit_B12_int" = "DRXTVB12",  # Vitamin B12 (μg) - DRXTVB12
  "Vit_B2_int" = "DRXTVB2",  # Riboflavin (mg) - DRXTVB2
  "Vit_B6_int" = "DRXTVB6",  # Vitamin B6 (mg) - DRXTVB6
  "Vit_C_int" = "DRXTVC",  # Vitamin C (mg) - DRXTVC
  "Vit_E_int" = "DRXTATOC",  # Vitamin E as alpha-tocopherol (μg) - DRXTATOC
  "Vit_K_int" = "DRXTVK",  # Vitamin K (μg) - DRXTVK
  "Wat_int" = "DRX.320Z",  # Total plain water (g) - DRX.320Z or DRXCWATR
  "Zn_int" = "DRXTZINC"  # Zinc intake (mg) - DRXTZINC
)

# ============================================================================
# Step 1: Load Current Dataset
# ============================================================================

cat("Step 1: Loading current dataset...\n")
data <- readRDS(input_file)
cat("  Current dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("  Current variables:", length(names(data)), "\n\n")

# ============================================================================
# Step 2: Check Which Variables Are Present
# ============================================================================

cat("Step 2: Checking variable availability...\n\n")

check_variables <- function(var_list, var_type) {
  cat("  ", var_type, " Variables:\n")
  present_vars <- c()
  missing_vars <- c()
  
  for (display_name in names(var_list)) {
    nhanes_var <- var_list[[display_name]]
    
    # Check if variable exists in dataset
    if (nhanes_var %in% names(data)) {
      present_vars <- c(present_vars, display_name)
      n_complete <- sum(!is.na(data[[nhanes_var]]))
      pct_complete <- round(n_complete / nrow(data) * 100, 1)
      cat("    ✓", display_name, "->", nhanes_var, 
          "(", n_complete, "complete,", pct_complete, "%)\n")
    } else {
      missing_vars <- c(missing_vars, display_name)
      cat("    ✗", display_name, "->", nhanes_var, "(MISSING)\n")
    }
  }
  
  cat("\n  Summary:\n")
  cat("    Present:", length(present_vars), "/", length(var_list), "\n")
  cat("    Missing:", length(missing_vars), "/", length(var_list), "\n")
  
  if (length(missing_vars) > 0) {
    cat("    Missing variables:", paste(missing_vars, collapse = ", "), "\n")
  }
  
  return(list(present = present_vars, missing = missing_vars))
}

s1_check <- check_variables(table_s1_vars, "Table S1 (Health Model)")
s2_check <- check_variables(table_s2_vars, "Table S2 (Nutrition Model)")

cat("\n")

# ============================================================================
# Step 3: Load Source Data to Add Missing Variables
# ============================================================================

cat("Step 3: Loading source data to add missing variables...\n")

# Get eligible SEQNs from current dataset
eligible_seqn <- unique(data$SEQN)
cat("  Eligible SEQNs:", length(eligible_seqn), "\n")

# Identify which source files we need
needed_files <- c()

# Check which files contain missing variables
missing_s1_vars <- unlist(table_s1_vars[s1_check$missing])
missing_s2_vars <- unlist(table_s2_vars[s2_check$missing])

all_missing <- unique(c(missing_s1_vars, missing_s2_vars))

cat("  Missing NHANES variables:", paste(all_missing, collapse = ", "), "\n\n")

# Load source files that might contain missing variables
source_datasets <- list()

# Demographics
if (any(c("RIDAGEYR", "RIAGENDR", "RIDRETH1", "DMDEDUC2", "DMQMILIZ", "HIQ011") %in% all_missing)) {
  cat("  Loading demographics...\n")
  demo_file <- file.path(nhanes_dir, "demographics_clean.csv")
  if (file.exists(demo_file)) {
    source_datasets$demographics <- read_csv(demo_file, show_col_types = FALSE) %>%
      filter(SEQN %in% eligible_seqn)
    cat("    Loaded", nrow(source_datasets$demographics), "rows\n")
  }
}

# Health Insurance (HIQ011) - check in questionnaire or response
if ("HIQ011" %in% all_missing) {
  cat("  Checking for HIQ011 (health insurance)...\n")
  quest_file <- file.path(nhanes_dir, "questionnaire_clean.csv")
  if (file.exists(quest_file)) {
    quest_header <- read_csv(quest_file, n_max = 0, show_col_types = FALSE)
    if ("HIQ011" %in% names(quest_header)) {
      if (!"questionnaire" %in% names(source_datasets)) {
        source_datasets$questionnaire <- read_csv(quest_file, show_col_types = FALSE) %>%
          filter(SEQN %in% eligible_seqn)
        cat("    Loaded questionnaire for HIQ011\n")
      }
    }
  }
}

# Questionnaire
if (any(c("HSQ470", "HSQ480", "ALQ120U", "DRQSPREP", "DRQSDIET", "DRD370", "SMQ020", "SMQ040") %in% all_missing)) {
  cat("  Loading questionnaire...\n")
  quest_file <- file.path(nhanes_dir, "questionnaire_clean.csv")
  if (file.exists(quest_file)) {
    if (!"questionnaire" %in% names(source_datasets)) {
      source_datasets$questionnaire <- read_csv(quest_file, show_col_types = FALSE) %>%
        filter(SEQN %in% eligible_seqn)
      cat("    Loaded", nrow(source_datasets$questionnaire), "rows\n")
    }
  }
}

# Response
if (any(c("ALQ120U", "DRQSPREP", "DRQSDIET", "DRD370") %in% all_missing)) {
  cat("  Loading response...\n")
  resp_file <- file.path(nhanes_dir, "response_clean.csv")
  if (file.exists(resp_file)) {
    source_datasets$response <- read_csv(resp_file, show_col_types = FALSE) %>%
      filter(SEQN %in% eligible_seqn)
    cat("    Loaded", nrow(source_datasets$response), "rows\n")
  }
}

# Dietary
if (any(grepl("^DRXT", all_missing) | all_missing %in% c("DR1_320Z", "DRX.320Z", "DRXCWATR", "DRD370"))) {
  cat("  Loading dietary...\n")
  diet_file <- file.path(nhanes_dir, "dietary_clean.csv")
  if (file.exists(diet_file)) {
    if (!"dietary" %in% names(source_datasets)) {
      source_datasets$dietary <- read_csv(diet_file, show_col_types = FALSE) %>%
        filter(SEQN %in% eligible_seqn)
      cat("    Loaded", nrow(source_datasets$dietary), "rows\n")
    }
  }
}

# Chemicals (lab variables)
if (any(grepl("^LBX", all_missing))) {
  cat("  Loading chemicals...\n")
  chem_file <- file.path(nhanes_dir, "chemicals_clean.csv")
  if (file.exists(chem_file)) {
    source_datasets$chemicals <- read_csv(chem_file, show_col_types = FALSE) %>%
      filter(SEQN %in% eligible_seqn)
    cat("    Loaded", nrow(source_datasets$chemicals), "rows\n")
  }
}

cat("\n")

# ============================================================================
# Step 4: Merge Missing Variables
# ============================================================================

cat("Step 4: Merging missing variables...\n")

vars_added <- 0

# Merge from each source dataset
for (source_name in names(source_datasets)) {
  source_data <- source_datasets[[source_name]]
  
  # Find variables in source that are missing in target
  vars_to_add <- intersect(all_missing, names(source_data))
  
  if (length(vars_to_add) > 0) {
    cat("  Adding from", source_name, ":", paste(vars_to_add, collapse = ", "), "\n")
    
    # Select only SEQN and variables to add
    source_subset <- source_data %>%
      select(SEQN, all_of(vars_to_add))
    
    # Handle duplicates by taking first record
    source_subset <- source_subset %>%
      group_by(SEQN) %>%
      slice(1) %>%
      ungroup()
    
    # Merge
    data <- data %>%
      left_join(source_subset, by = "SEQN")
    
    vars_added <- vars_added + length(vars_to_add)
  }
}

cat("  Total variables added:", vars_added, "\n\n")

# ============================================================================
# Step 5: Create Derived Variables (if needed)
# ============================================================================

cat("Step 5: Creating derived variables...\n")

# Alcohol habits (if ALQ120U exists, need to derive Alcohol)
if ("ALQ120U" %in% names(data) && !"Alcohol" %in% names(data)) {
  # ALQ120U: How often drink alcohol (1=Every day, 2=5-6 times/week, etc.)
  # Need to map to: 1=Drink, 2=Quit, 3=Never
  # This is a simplified mapping - may need adjustment based on actual values
  cat("  Creating Alcohol from ALQ120U...\n")
  data <- data %>%
    mutate(
      Alcohol = case_when(
        is.na(ALQ120U) ~ NA_real_,
        ALQ120U == 0 ~ 3,  # Never
        ALQ120U > 0 ~ 1,   # Drink (simplified)
        TRUE ~ NA_real_
      )
    )
  vars_added <- vars_added + 1
}

# Fish intake (DRD370)
if ("DRD370" %in% names(data) && !"Fish_int" %in% names(data)) {
  cat("  Creating Fish_int from DRD370...\n")
  data <- data %>%
    mutate(
      Fish_int = case_when(
        DRD370 == 1 ~ 1,  # Yes
        DRD370 %in% c(2, 7, 9) ~ 2,  # No/Refused/Unknown
        TRUE ~ NA_real_
      )
    )
  vars_added <- vars_added + 1
}

# Water intake (DRX.320Z or DRXCWATR)
if (("DRX.320Z" %in% names(data) || "DRXCWATR" %in% names(data)) && !"Wat_int" %in% names(data)) {
  cat("  Creating Wat_int from water variable...\n")
  if ("DRX.320Z" %in% names(data)) {
    data <- data %>%
      mutate(Wat_int = DRX.320Z)
  } else if ("DRXCWATR" %in% names(data)) {
    data <- data %>%
      mutate(Wat_int = DRXCWATR)
  }
  vars_added <- vars_added + 1
}

# Smoking status (Smoker) - recalculate if SMQ variables are present
if (all(c("SMQ020", "SMQ040") %in% names(data)) && !"Smoker" %in% names(data)) {
  cat("  Creating Smoker from SMQ020 and SMQ040...\n")
  data <- data %>%
    mutate(
      Smoker = case_when(
        SMQ020 == 2 ~ 3,                                    # Never (never smoked >100)
        SMQ020 == 1 & SMQ040 == 3 ~ 2,                      # Former (smoked but quit)
        SMQ020 == 1 & (SMQ040 == 1 | SMQ040 == 2) ~ 1,      # Current (smokes every day or some days)
        TRUE ~ NA_real_                                     # Missing/Unknown
      )
    )
  vars_added <- vars_added + 1
}

# Milk intake (DMQMILIZ)
if ("DMQMILIZ" %in% names(data) && !"Milk_int" %in% names(data)) {
  cat("  Creating Milk_int from DMQMILIZ...\n")
  # DMQMILIZ: Type of milk usually drink (1=Whole, 2=2%, 3=1%, 4=Skim, 5=None/very low)
  data <- data %>%
    mutate(
      Milk_int = case_when(
        DMQMILIZ == 1 ~ 1,  # Whole
        DMQMILIZ == 2 ~ 2,  # Low fat (2%)
        DMQMILIZ == 3 ~ 2,  # Low fat (1%)
        DMQMILIZ == 4 ~ 3,  # Skim
        DMQMILIZ == 5 ~ 4,  # No or very low
        TRUE ~ NA_real_
      )
    )
  vars_added <- vars_added + 1
}

# Salt variables
if ("DRQSPREP" %in% names(data) && !"Salt_prep" %in% names(data)) {
  cat("  Creating Salt_prep from DRQSPREP...\n")
  data <- data %>%
    mutate(
      Salt_prep = case_when(
        DRQSPREP %in% c(1, 2) ~ 1,  # Never or rarely
        DRQSPREP == 3 ~ 2,           # Occasionally
        DRQSPREP == 4 ~ 3,           # Very often
        TRUE ~ NA_real_
      )
    )
  vars_added <- vars_added + 1
}

if ("DRQSDIET" %in% names(data) && !"Salt_table" %in% names(data)) {
  cat("  Creating Salt_table from DRQSDIET...\n")
  data <- data %>%
    mutate(
      Salt_table = case_when(
        DRQSDIET == 1 ~ 1,  # Yes
        DRQSDIET == 2 ~ 0,  # No
        TRUE ~ NA_real_
      )
    )
  vars_added <- vars_added + 1
}

# Salt frequency (may need DRQSALTQ or similar)
# This might need to be derived from other variables

# Vitamin A (DRXTVARA)
if ("DRXTVARA" %in% names(data) && !"Vit_A_int" %in% names(data)) {
  cat("  Creating Vit_A_int from DRXTVARA...\n")
  data <- data %>%
    mutate(Vit_A_int = DRXTVARA)
  vars_added <- vars_added + 1
}

cat("  Derived variables created:", vars_added, "\n\n")

# ============================================================================
# Step 6: Final Verification
# ============================================================================

cat("Step 6: Final verification...\n\n")

# Re-check all variables
s1_final <- check_variables(table_s1_vars, "Table S1 (Health Model) - Final")
s2_final <- check_variables(table_s2_vars, "Table S2 (Nutrition Model) - Final")

cat("\n")

# ============================================================================
# Step 7: Save Updated Dataset
# ============================================================================

cat("Step 7: Saving updated dataset...\n")

cat("  Final dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("  Total variables:", length(names(data)), "\n")

# Save updated dataset
saveRDS(data, file = output_file, compress = "xz")

file_size_mb <- file.info(output_file)$size / (1024^2)
cat("  ✓ Dataset saved to:", output_file, "\n")
cat("  File size:", round(file_size_mb, 2), "MB\n")

cat("\n============================================================================\n")
cat("VERIFICATION COMPLETE\n")
cat("============================================================================\n")
cat("Table S1 variables present:", length(s1_final$present), "/", length(table_s1_vars), "\n")
cat("Table S2 variables present:", length(s2_final$present), "/", length(table_s2_vars), "\n")
cat("============================================================================\n")

