# ============================================================================
# Full Pipeline Script for NHANES Data Preparation and SuSiE Analysis
# ============================================================================
# This script runs all necessary scripts in the correct order to:
# 1. Filter NHANES data
# 2. Add calculated variables
# 3. Verify and add Table S1/S2 variables
# 4. Prepare data for susieR
# 5. Run SuSiE analysis
# ============================================================================

cat("============================================================================\n")
cat("NHANES Data Pipeline: Full Execution\n")
cat("============================================================================\n\n")

base_dir <- "/Users/rohandekate/Documents/STAT 656 BDA/Project"

# ============================================================================
# Step 1: Process and Filter NHANES Data
# ============================================================================

cat("Step 1: Processing and filtering NHANES data...\n")
cat("  Running process_nhanes_data.R...\n")
system(paste("cd", shQuote(base_dir), "&& Rscript process_nhanes_data.R"), 
       ignore.stdout = FALSE, ignore.stderr = FALSE)
cat("  ✓ Step 1 complete\n\n")

# ============================================================================
# Step 2: Load and Filter Data for SuSiE
# ============================================================================

cat("Step 2: Loading and filtering data for SuSiE...\n")
cat("  Running load_and_filter_data_for_susie.R...\n")
system(paste("cd", shQuote(base_dir), "&& Rscript load_and_filter_data_for_susie.R"), 
       ignore.stdout = FALSE, ignore.stderr = FALSE)
cat("  ✓ Step 2 complete\n\n")

# ============================================================================
# Step 3: Add Calculated Variables
# ============================================================================

cat("Step 3: Adding calculated variables...\n")
cat("  Running add_calculated_variables.R...\n")
system(paste("cd", shQuote(base_dir), "&& Rscript add_calculated_variables.R"), 
       ignore.stdout = FALSE, ignore.stderr = FALSE)
cat("  ✓ Step 3 complete\n\n")

# ============================================================================
# Step 4: Add SMQ Variables (if available)
# ============================================================================

cat("Step 4: Adding SMQ variables (smoking)...\n")
cat("  Running add_smq_variables.R...\n")
system(paste("cd", shQuote(base_dir), "&& Rscript add_smq_variables.R"), 
       ignore.stdout = FALSE, ignore.stderr = FALSE)
cat("  ✓ Step 4 complete\n\n")

# ============================================================================
# Step 5: Verify and Add Table S1/S2 Variables
# ============================================================================

cat("Step 5: Verifying and adding Table S1/S2 variables...\n")
cat("  Running verify_and_add_table_variables.R...\n")
system(paste("cd", shQuote(base_dir), "&& Rscript verify_and_add_table_variables.R"), 
       ignore.stdout = FALSE, ignore.stderr = FALSE)
cat("  ✓ Step 5 complete\n\n")

# ============================================================================
# Step 6: Analyze Variables for SuSiE
# ============================================================================

cat("Step 6: Analyzing variables for SuSiE...\n")
cat("  Running analyze_variables_for_susie.R...\n")
system(paste("cd", shQuote(base_dir), "&& Rscript analyze_variables_for_susie.R"), 
       ignore.stdout = FALSE, ignore.stderr = FALSE)
cat("  ✓ Step 6 complete\n\n")

# ============================================================================
# Step 7: Prepare Data for SuSiE
# ============================================================================

cat("Step 7: Preparing data for SuSiE...\n")
cat("  Running prepare_susie_data.R...\n")
system(paste("cd", shQuote(base_dir), "&& Rscript prepare_susie_data.R"), 
       ignore.stdout = FALSE, ignore.stderr = FALSE)
cat("  ✓ Step 7 complete\n\n")

# ============================================================================
# Step 8: Run SuSiE Analysis
# ============================================================================

cat("Step 8: Running SuSiE analysis...\n")
cat("  Running run_susie_analysis.R...\n")
system(paste("cd", shQuote(base_dir), "&& Rscript run_susie_analysis.R"), 
       ignore.stdout = FALSE, ignore.stderr = FALSE)
cat("  ✓ Step 8 complete\n\n")

# ============================================================================
# Final Summary
# ============================================================================

cat("============================================================================\n")
cat("PIPELINE EXECUTION COMPLETE\n")
cat("============================================================================\n")
cat("All scripts have been executed in the correct order.\n")
cat("Final results are available in:\n")
cat("  - Dataset: nhanes_filtered_for_susie.rds\n")
cat("  - SuSiE data: susie_data/\n")
cat("  - SuSiE results: susie_results/\n")
cat("============================================================================\n")

