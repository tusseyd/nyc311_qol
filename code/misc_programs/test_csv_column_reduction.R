#!/usr/bin/env Rscript
# ================================================================
# Program: test_csv_column_reduction.R
# Purpose: Measure on-disk file size change (Excel-style reduction)
# Author:  David Tussey
# ================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# ---- PATHS & FILE SETTINGS ----
base_dir <- file.path(
  "C:", 
  "Users", 
  "David", 
  "OneDrive", 
  "Documents",
  "datacleaningproject", 
  "journal_of_data_science"
)
raw_data_dir  <- file.path(base_dir, "data", "raw_data")
main_data_file <- "5-year_311SR_01-01-2020_thru_12-31-2024_AS_OF_09-23-2025.csv"
main_data_path <- file.path(raw_data_dir, main_data_file)

# ---- CONFIGURATION ----
redundant_columns <- c(
  "agency_name",
  "park_borough",
  "intersection_street_1",
  "intersection_street_2",
  "location"
)

# ---- READ FULL CSV ----
cat("\n=== Reading full dataset ===\n")
cat(sprintf("File: %s\n", main_data_path))

original_file_size <- file.size(main_data_path)
original_file_size_kb <- original_file_size / 1000

cat(sprintf("Original file size: %s KB (Windows style)\n",
            format(original_file_size_kb, big.mark = ",")))

raw_data <- fread(
  main_data_path,
  nThread = max(1, parallel::detectCores() - 1),
  check.names = FALSE,
  strip.white = TRUE,
  showProgress = TRUE,
  colClasses = "character"
)

copy_raw_data <- raw_data

cat(sprintf("\nRows read: %s  |  Columns: %d\n",
            format(nrow(raw_data), big.mark = ","), ncol(raw_data)))

# ---- REMOVE SELECTED COLUMNS ----
cols_to_drop <- intersect(redundant_columns, names(raw_data))
if (length(cols_to_drop) == 0) {
  cat("\nNo matching redundant columns found. Nothing dropped.\n")
} else {
  cat("\nDropping redundant columns:\n")
  print(cols_to_drop)
  raw_data[, (cols_to_drop) := NULL]
  cat(sprintf("Remaining columns: %d\n", ncol(raw_data)))
}

# ---- WRITE REDUCED DATASET ----
tmp_dir <- file.path(tempdir(), paste0("reduction_test_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
reduced_csv_path <- file.path(tmp_dir, "reduced_dataset.csv")

cat("\nWriting reduced dataset...\n")
fwrite(raw_data, reduced_csv_path, quote = TRUE, showProgress = TRUE)

# ---- MEASURE REDUCED FILE SIZE ----
reduced_file_size <- file.size(reduced_csv_path)
reduced_file_size_kb <- reduced_file_size / 1000
pct_reduction <- 100 * (1 - reduced_file_size / original_file_size)

# ---- RELOAD REDUCED DATA FOR TRUE SIZE ----
cat("\nReloading reduced dataset for accurate parsed size...\n")
reloaded_data <- fread(
  reduced_csv_path,
  nThread = max(1, parallel::detectCores() - 1),
  check.names = FALSE,
  strip.white = TRUE,
  colClasses = "character"
)

reloaded_mem_size <- object.size(reloaded_data)

# ---- REPORT RESULTS ----
cat("\n=== CSV FILE SIZE COMPARISON (Windows-style KB) ===\n")
cat(sprintf("Original file size: %s KB\n",
            format(original_file_size_kb, big.mark = ",")))
cat(sprintf("Reduced  file size: %s KB\n",
            format(reduced_file_size_kb, big.mark = ",")))
cat(sprintf("Size reduction: %.2f%%\n", pct_reduction))

cat(sprintf("\nReloaded memory footprint in R: %s\n",
            format(structure(reloaded_mem_size, class = "object_size"),
                   units = "auto", digits = 2)))

cat("\nTemporary output directory:\n")
cat(tmp_dir, "\n")

cat("\n=== TEST COMPLETE ===\n")
