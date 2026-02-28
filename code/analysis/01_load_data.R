################################################################################
# STEP 1: LOAD DATA
# 
# Loads USPS zipcode reference data and NYC 311 service request data
# Performs initial data validation and prints coverage summary
################################################################################

message(strrep("=", 80))
message("STEP 1: LOADING DATA FILES")
message(strrep("=", 80))

# Get directory paths
dirs <- get_directory_paths()

# -----------------------------------------------------------------------------
# Load USPS Zipcode Reference Data
# -----------------------------------------------------------------------------

USPSzipcodes <- load_usps_zipcodes(
  data_dir = dirs$data,
  filename = USPS_ZIPCODE_FILE
)

cat(sprintf("\n✓ USPS zipcodes loaded: %s rows\n", 
            format(nrow(USPSzipcodes), big.mark = ",")))

# -----------------------------------------------------------------------------
# Load Main 311 Service Request Data
# -----------------------------------------------------------------------------

d311_qa <- load_311_data(
  data_dir = dirs$data,
  filename = MAIN_DATA_FILE
)

cat(sprintf("\n✓ 311 data loaded: %s rows, %d columns\n", 
            format(nrow(d311_qa), big.mark = ","),
            ncol(d311_qa)))

# Print initial coverage
print_data_coverage(d311_qa)

# -----------------------------------------------------------------------------
# Calculate Durations (needed for quality metrics)
# -----------------------------------------------------------------------------

message("\nCalculating service request durations...")

# This assumes calculate_durations() is available from functions/
# If not, this will need to be sourced
d311_qa <- calculate_durations(
  d311_qa, 
  created_col = "created_date",
  closed_col = "closed_date",
  tz = ANALYSIS_TIMEZONE, 
  in_place = FALSE
)

# Clean up duration columns
# Keep only duration_days, rename to duration
if ("duration_sec" %in% names(d311_qa)) {
  d311_qa[, duration_sec := NULL]
}

cat("✓ Durations calculated\n")

# -----------------------------------------------------------------------------
# Optimize Memory: Remove Unnecessary Columns
# -----------------------------------------------------------------------------

d311_qa <- reduce_columns(
  dt = d311_qa,
  columns_to_keep = COLUMNS_TO_KEEP,
  verbose = TRUE
)

# Final coverage check
print_data_coverage(d311_qa)

# -----------------------------------------------------------------------------
# Verify Data Integrity
# -----------------------------------------------------------------------------

cat("\n=== DATA INTEGRITY CHECKS ===\n")

# Check for NA in critical columns
critical_cols <- c("created_date", "closed_date")
for (col in critical_cols) {
  if (col %in% names(d311_qa)) {
    n_na <- sum(is.na(d311_qa[[col]]))
    pct_na <- 100 * n_na / nrow(d311_qa)
    cat(sprintf("  %s: %s NA (%.2f%%)\n", 
                col, format(n_na, big.mark = ","), pct_na))
  }
}

# Check duration distribution
if ("duration" %in% names(d311_qa)) {
  n_negative <- sum(d311_qa$duration < 0, na.rm = TRUE)
  n_zero <- sum(d311_qa$duration == 0, na.rm = TRUE)
  n_positive <- sum(d311_qa$duration > 0, na.rm = TRUE)
  
  cat(sprintf("\nDuration distribution:\n"))
  cat(sprintf("  Negative: %s (%.2f%%)\n", 
              format(n_negative, big.mark = ","),
              100 * n_negative / nrow(d311_qa)))
  cat(sprintf("  Zero:     %s (%.2f%%)\n", 
              format(n_zero, big.mark = ","),
              100 * n_zero / nrow(d311_qa)))
  cat(sprintf("  Positive: %s (%.2f%%)\n", 
              format(n_positive, big.mark = ","),
              100 * n_positive / nrow(d311_qa)))
}

message("\n✓ Step 1 complete: Data loaded and validated")
message(strrep("=", 80), "\n")

# Objects created for downstream use:
# - USPSzipcodes: Reference data for zipcode validation
# - d311_qa: Main analysis dataset
