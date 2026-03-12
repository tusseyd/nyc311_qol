# =============================================================================
# 01_data_prep.R
# NYC 311 Quality of Life Index
#
# PURPOSE: Ingests raw NYC 311 CSV files, applies standardization and quality
# filters, maps complaint types to families and bundles, and saves prepared
# RDS files for downstream analysis.
#
# Processes two files in sequence using a shared prep_311_file() function:
#   Phase I  (Baseline) : 2021 data  -> prepared_baseline.RDS
#   Phase II (Monitor)  : 2022-2025  -> prepared_monitor.RDS
#
# PROCESSING STEPS (applied to each file):
#   1.  Selective column read — only required fields loaded from CSV
#   2.  Column name verification — confirms complaint type column present
#   3.  Missing value standardization
#   4.  Text columns converted to uppercase
#   5.  Date parsing — created_date converted to Date type
#   6.  Borough validation — invalid values flagged and removed
#   7.  Mandatory field check — rows missing complaint type or date dropped
#   8.  Complaint type consolidation and QoL filter (via apply_classification)
#   9.  Bundle label assignment
#   10. Analytical columns derived (year, month, year_month)
#   11. Final summary printed to console
#   12. Output saved and verified
#
# COLUMN NAME POLICY:
#   modify_column_names() applies mechanical cleanup only (spaces->underscores,
#   lowercase, remove parentheses). No aliasing. Raw column names are preserved
#   in their cleaned form for traceability back to the source data.
#   Key column names after cleanup:
#     "Problem (formerly Complaint Type)" -> problem_formerly_complaint_type
#     "Created Date"                      -> created_date
#     "Borough"                           -> borough
#     "Agency"                            -> agency
#
# DEPENDENCIES:
#   R/prep_functions.R       — parse_date_column(), standardize_missing_chars(),
#                              modify_column_names(), save_and_verify()
#   R/apply_classification.R — complaint type -> family -> bundle mapping
#
# Inputs:  data/raw/1-year_311SR_*.csv   (baseline)
#          data/raw/4-year_311SR_*.csv   (monitor)
# Outputs: output/data/prepared_baseline.RDS
#          output/data/prepared_monitor.RDS
# =============================================================================
# -----------------------------------------------------------------------------
# STARTUP
# -----------------------------------------------------------------------------

message("\n", strrep("=", 80))
message("NYC 311 QUALITY OF LIFE INDEX - DATA PREPARATION")
message(strrep("=", 80))
message("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

prog_start <- proc.time()

# Load configuration and functions
source("config.R")
source(file.path(PATHS$r_dir, "prep_functions.R"))
source(file.path(PATHS$r_dir, "apply_classification.R"))

# Required packages
suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

# Column name used throughout this script (after modify_column_names cleanup)
CT_COL <- "problem_formerly_complaint_type"

# -----------------------------------------------------------------------------
# HELPER: process one raw CSV file end-to-end
# -----------------------------------------------------------------------------

prep_311_file <- function(raw_path, output_path, file_label) {

  message("\n", strrep("-", 80))
  message("PROCESSING: ", file_label)
  message(strrep("-", 80))

  # ---- STEP 1: Read raw CSV (selective columns only) ------------------------
  message("\nStep 1: Reading raw CSV...")

  if (!file.exists(raw_path))
    stop("Raw file not found: ", raw_path)

  # Peek at headers to resolve raw column names before full read
  raw_headers       <- fread(raw_path, nrows = 0) |> names()
  raw_headers_clean <- modify_column_names(
                         setNames(data.table(matrix(nrow=0,
                                   ncol=length(raw_headers))),
                                   raw_headers)) |> names()

  # Map clean names -> original names for select =
  needed_clean <- c(CT_COL, "created_date", "borough", "agency")

  needed_orig <- raw_headers[raw_headers_clean %in% needed_clean]

  missing_needed <- setdiff(needed_clean, raw_headers_clean)
  if (length(missing_needed) > 0)
    stop("Required columns missing from raw data: ",
         paste(missing_needed, collapse = ", "))

  raw <- fread(
    raw_path,
    select       = needed_orig,
    nThread      = max(1L, parallel::detectCores() - 1L),
    strip.white  = TRUE,
    showProgress = TRUE,
    colClasses   = "character"
  )

  # Apply clean column names
  setnames(raw, needed_orig, raw_headers_clean[raw_headers_clean %in% needed_clean])

  n_raw <- nrow(raw)
  cat(sprintf("  Loaded : %s rows, %d columns (selective read)\n",
              format(n_raw, big.mark = ","), ncol(raw)))
  cat(sprintf("  Columns: %s\n", paste(names(raw), collapse = ", ")))

  # ---- STEP 2: Verify complaint type column ---------------------------------
  message("\nStep 2: Verifying column names...")

  if (!CT_COL %in% names(raw))
    stop("Expected column '", CT_COL, "' not found.\n",
         "Columns present: ", paste(names(raw), collapse = ", "))

  cat(sprintf("  Complaint type column: '%s' ✓\n", CT_COL))

  # ---- STEP 3: Standardize missing values -----------------------------------
  message("\nStep 3: Standardizing missing values...")
  standardize_missing_chars(raw)

  # ---- STEP 4: Convert text columns to uppercase ----------------------------
  message("\nStep 4: Converting text columns to uppercase...")

  text_cols <- c(CT_COL, "borough", "agency")
  text_cols <- intersect(text_cols, names(raw))

  for (col in text_cols) {
    raw[, (col) := toupper(get(col))]
  }
  cat(sprintf("  Uppercased: %s\n", paste(text_cols, collapse = ", ")))

  # ---- STEP 5: Parse created_date -------------------------------------------
  message("\nStep 5: Parsing created_date...")

  result <- parse_date_column(
    temp_raw_data      = raw,
    valid_date_columns = "created_date"
  )
  raw <- result$parsed_data
  rm(result); gc()

  # ---- STEP 6: Validate boroughs --------------------------------------------
  message("\nStep 6: Validating boroughs...")

    # Show invalid borough values before removing
  invalid_rows <- raw[!borough %in% VALID_BOROUGHS & !is.na(borough)]
  if (nrow(invalid_rows) > 0) {
    cat("\n  Invalid borough values found:\n")
    print(invalid_rows[, .N, by = borough][order(-N)])
  }
  
  n_before  <- nrow(raw)
  raw       <- raw[borough %in% VALID_BOROUGHS | is.na(borough)]
  n_removed <- n_before - nrow(raw)

  cat(sprintf("  Valid boroughs: %s\n", paste(VALID_BOROUGHS, collapse = ", ")))
  if (n_removed > 0)
    cat(sprintf("  Rows removed (invalid borough): %s\n",
                format(n_removed, big.mark = ",")))
  else
    cat("  All borough values valid\n")

  # Borough distribution
  cat("\n  Borough distribution:\n")
  print(raw[, .N, by = borough][order(-N)][
    , .(Borough = borough,
        Count   = format(N, big.mark = ","),
        Pct     = sprintf("%.1f%%", 100 * N / sum(N)))])

  # ---- STEP 7: Drop rows missing mandatory fields ---------------------------
  message("\nStep 7: Checking mandatory fields...")

  mandatory <- c(CT_COL, "created_date")
  n_before  <- nrow(raw)

  for (field in mandatory) {
    if (is.character(raw[[field]])) {
      raw <- raw[!is.na(get(field)) & trimws(get(field)) != ""]
    } else {
      raw <- raw[!is.na(get(field))]
    }
  }

  n_removed <- n_before - nrow(raw)
  if (n_removed > 0)
    cat(sprintf("  Rows removed (missing mandatory fields): %s\n",
                format(n_removed, big.mark = ",")))
  else
    cat("  No rows removed\n")

  # ---- STEP 8: Apply complaint type consolidation and QoL filter ------------
  message("\nStep 8: Applying complaint type consolidation and QoL filter...")

  setnames(raw, CT_COL, "complaint_type")

  raw <- apply_classification(
    dt        = raw,
    ref_path  = PATHS$ref_classification,
    hier_path = PATHS$complaint_hierarchy,
    verbose   = TRUE
  )

  # Rename complaint_type back to project-standard column name
  setnames(raw, "complaint_type", CT_COL)

  cat(sprintf("  Families retained : %d\n", uniqueN(raw$family)))
  cat(sprintf("  Bundles retained  : %d\n", uniqueN(raw$bundle)))

  # ---- STEP 9: Add bundle label ---------------------------------------------
  message("\nStep 9: Adding bundle labels...")

  bundle_labels_dt <- as.data.table(BUNDLE_LABELS)
  raw <- merge(raw, bundle_labels_dt, by = "bundle", all.x = TRUE)

  unmatched <- sum(is.na(raw$label))
  if (unmatched > 0)
    warning(unmatched, " rows have no bundle label - check BUNDLE_LABELS in config.R")
  else
    cat("  All rows have a bundle label ✓\n")

  # Bundle distribution
  cat("\n  Bundle distribution:\n")
  print(raw[, .N, by = .(bundle, label)][order(-N)][
    , .(Bundle = label,
        Count  = format(N, big.mark = ","),
        Pct    = sprintf("%.1f%%", 100 * N / sum(N)))])

  # ---- STEP 10: Derive analytical columns -----------------------------------
  message("\nStep 10: Deriving analytical columns...")

  raw[, year       := year(created_date)]
  raw[, month      := month(created_date)]
  raw[, year_month := as.Date(sprintf("%d-%02d-01", year, month))]

  cat(sprintf("  Year range: %d to %d\n", min(raw$year), max(raw$year)))
  cat(sprintf("  Months covered: %d\n",   uniqueN(raw$year_month)))

  # ---- STEP 11: Final summary -----------------------------------------------
  message("\nStep 11: Final dataset summary...")

  cat(sprintf("\n  Final row count  : %s\n", format(nrow(raw), big.mark = ",")))
  cat(sprintf("  Columns          : %s\n",  paste(names(raw), collapse = ", ")))
  cat(sprintf("  Date range       : %s to %s\n",
              format(min(raw$created_date), "%Y-%m-%d"),
              format(max(raw$created_date), "%Y-%m-%d")))

  # ---- STEP 12: Save output -------------------------------------------------
  message("\nStep 12: Saving prepared data...")

  save_and_verify(dt = raw, path = output_path)

  rm(raw); gc()

  message("COMPLETE: ", file_label, "\n")
  invisible(output_path)
}

# =============================================================================
# MAIN: Process both files
# =============================================================================

# --- Baseline: 2020-2021 -----------------------------------------------------
prep_311_file(
  raw_path    = PATHS$raw_baseline,
  output_path = PATHS$processed_baseline,
  file_label  = "PHASE 1 BASELINE (2021)"
)

# --- Monitor: 2022-2025 ------------------------------------------------------
prep_311_file(
  raw_path    = PATHS$raw_monitor,
  output_path = PATHS$processed_monitor,
  file_label  = "PHASE 2 MONITOR (2022-2025)"
)

# =============================================================================
# WRAP UP
# =============================================================================

elapsed <- proc.time() - prog_start

message("\n", strrep("=", 80))
message("DATA PREPARATION COMPLETE")
message(strrep("=", 80))
message("Baseline RDS : ", PATHS$processed_baseline)
message("Monitor RDS  : ", PATHS$processed_monitor)
message(sprintf("Elapsed time : %.1f minutes", elapsed["elapsed"] / 60))
message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
