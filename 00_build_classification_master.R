# =============================================================================
# 00_build_classification_master.R
# NYC 311 Quality of Life Index
#
# PURPOSE: Reads complaint_hierarchy.csv (the master reference file defining
# complaint_type -> family -> bundle) and the raw 311 monitor data, computes
# complaint counts, applies the volume threshold, and generates a reference
# Excel workbook with four sheets:
#   Classification  — living document for include_in_analysis decisions (edit here)
#   Hierarchy       — full complaint_type -> family -> bundle detail with counts
#   Bundle_Summary  — families and complaint totals by bundle
#   Instructions    — field definitions and workflow guidance
#
# Flags are preserved across re-runs via carry-forward merge.
#
# Re-run this script when:
#   - complaint_hierarchy.csv changes (new types, families, or bundles)
#   - Raw data is refreshed with a new year of data
#
# To change the hierarchy (complaint_type -> family -> bundle):
#   Edit: data/reference/complaint_hierarchy.csv
#   Then re-run this script
#
# To change Y/N include decisions only:
#   Edit: output/reference/complaint_classification.xlsx directly
#   No re-run needed — 01_data_prep.R reads it on every run
#
# Inputs:  data/reference/complaint_hierarchy.csv
#          data/raw/4-year_311SR_*.csv
# Output:  output/reference/complaint_classification.xlsx
# =============================================================================

message("\n", strrep("=", 80))
message("NYC 311 - BUILD CLASSIFICATION MASTER")
message(strrep("=", 80))
message("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

prog_start <- proc.time()

source("config.R")

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(openxlsx)
})

# =============================================================================
# PARAMETERS
# Edit VOLUME_THRESHOLD here if the threshold changes
# =============================================================================
VOLUME_THRESHOLD <- 10000L   # Minimum 4-year count for family inclusion
                              # Override per-family via mayoral_interest = Y
                              # in complaint_hierarchy.csv

# =============================================================================
# HELPERS
# =============================================================================
clean_names <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_{2,}", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# =============================================================================
# STEP 1: LOAD COMPLAINT HIERARCHY
# =============================================================================
message("Step 1: Loading complaint hierarchy...")

if (!file.exists(PATHS$complaint_hierarchy))
  stop("complaint_hierarchy.csv not found: ", PATHS$complaint_hierarchy,
       "\nCreate it and place it in data/reference/")

hier <- fread(PATHS$complaint_hierarchy, encoding = "UTF-8")
hier[, complaint_type   := toupper(trimws(complaint_type))]
hier[, family           := toupper(trimws(family))]
hier[, bundle           := tolower(trimws(bundle))]
hier[, mayoral_interest := toupper(trimws(mayoral_interest))]

cat(sprintf("  Complaint types  : %d\n", nrow(hier)))
cat(sprintf("  Families         : %d\n", uniqueN(hier$family)))
cat(sprintf("  Bundles          : %d\n", uniqueN(hier$bundle)))
cat(sprintf("  Mayoral interest : %d\n", hier[mayoral_interest == "Y", .N]))

# =============================================================================
# HIERARCHY VALIDATION
# All checks run before any data loading. Errors are collected and printed
# together so you can fix everything in one pass. Script stops if any
# errors are found.
# =============================================================================
message("Validating hierarchy integrity...")

validation_errors   <- character(0)
validation_warnings <- character(0)

# --- Check 1: Blank complaint_type ---
blank_ct <- hier[is.na(complaint_type) | complaint_type == "", which = TRUE]
if (length(blank_ct) > 0)
  validation_errors <- c(validation_errors,
    sprintf("CHECK 1 FAIL: %d row(s) with blank complaint_type (rows: %s)",
            length(blank_ct), paste(blank_ct, collapse = ", ")))

# --- Check 2: Blank family ---
blank_fam <- hier[is.na(family) | family == "", complaint_type]
if (length(blank_fam) > 0)
  validation_errors <- c(validation_errors,
    sprintf("CHECK 2 FAIL: %d complaint_type(s) with blank family:\n    %s",
            length(blank_fam), paste(blank_fam, collapse = "\n    ")))

# --- Check 3: Blank bundle ---
blank_bun <- hier[is.na(bundle) | bundle == "", complaint_type]
if (length(blank_bun) > 0)
  validation_errors <- c(validation_errors,
    sprintf("CHECK 3 FAIL: %d complaint_type(s) with blank bundle:\n    %s",
            length(blank_bun), paste(blank_bun, collapse = "\n    ")))

# --- Check 4: Duplicate complaint_type rows ---
dup_ct <- hier[, .N, by = complaint_type][N > 1]
if (nrow(dup_ct) > 0)
  validation_errors <- c(validation_errors,
    sprintf("CHECK 4 FAIL: %d complaint_type(s) appear more than once:\n    %s",
            nrow(dup_ct),
            paste(sprintf("%-50s (%dx)", dup_ct$complaint_type, dup_ct$N),
                  collapse = "\n    ")))

# --- Check 5: Family assigned to more than one bundle (THE CARTESIAN KILLER) ---
fam_bundle <- unique(hier[, .(family, bundle)])
dup_fam_bundle <- fam_bundle[, .N, by = family][N > 1]
if (nrow(dup_fam_bundle) > 0) {
  detail <- fam_bundle[family %in% dup_fam_bundle$family][order(family, bundle)]
  detail_str <- detail[, paste0("    ", family, " -> ", bundle,
                                 collapse = "\n")]
  validation_errors <- c(validation_errors,
    sprintf(paste0("CHECK 5 FAIL: %d family(s) assigned to multiple bundles",
                   " (causes cartesian join error in 03_):\n%s"),
            nrow(dup_fam_bundle), detail_str))
}

# --- Check 6: Bundle key not in config.R BUNDLES list ---
known_bundles <- names(BUNDLES)
unknown_bundles <- setdiff(unique(hier$bundle), known_bundles)
if (length(unknown_bundles) > 0)
  validation_errors <- c(validation_errors,
    sprintf("CHECK 6 FAIL: %d bundle(s) in hierarchy not defined in config.R:\n    %s",
            length(unknown_bundles), paste(unknown_bundles, collapse = "\n    ")))

# --- Check 7: Bundles in config.R with no families assigned (warning only) ---
unassigned_bundles <- setdiff(known_bundles, unique(hier$bundle))
if (length(unassigned_bundles) > 0)
  validation_warnings <- c(validation_warnings,
    sprintf("CHECK 7 WARN: %d bundle(s) in config.R have no families assigned:\n    %s",
            length(unassigned_bundles), paste(unassigned_bundles, collapse = "\n    ")))

# --- Check 8: Invalid mayoral_interest values ---
valid_mi <- hier[!mayoral_interest %in% c("Y", "N", ""), complaint_type]
if (length(valid_mi) > 0)
  validation_errors <- c(validation_errors,
    sprintf("CHECK 8 FAIL: %d complaint_type(s) with invalid mayoral_interest (must be Y, N, or blank):\n    %s",
            length(valid_mi), paste(valid_mi, collapse = "\n    ")))

# --- Report ---
cat(sprintf("  Complaint types  : %d\n", nrow(hier)))
cat(sprintf("  Families         : %d\n", uniqueN(hier$family)))
cat(sprintf("  Bundles          : %d\n", uniqueN(hier$bundle)))
cat(sprintf("  Mayoral interest : %d\n", hier[mayoral_interest == "Y", .N]))

if (length(validation_warnings) > 0) {
  cat("\n  ⚠️  WARNINGS (non-fatal):\n")
  for (w in validation_warnings)
    cat(sprintf("  %s\n\n", w))
}

if (length(validation_errors) > 0) {
  cat("\n")
  cat(strrep("!", 80), "\n")
  cat("  HIERARCHY VALIDATION FAILED —",
      length(validation_errors), "error(s) found\n")
  cat("  Fix complaint_hierarchy.csv and rerun.\n")
  cat(strrep("!", 80), "\n\n")
  for (e in validation_errors)
    cat(sprintf("  %s\n\n", e))
  stop("Hierarchy validation failed. See errors above.", call. = FALSE)
} else {
  cat("  ✅ All hierarchy checks passed.\n")
}

# =============================================================================
# STEP 2: LOAD RAW DATA (complaint_type column only — fast read)
# =============================================================================
message("\nStep 2: Loading raw 311 data...")

raw_dir   <- dirname(PATHS$raw_monitor)
raw_files <- list.files(raw_dir, pattern = "4-year.*\\.csv$",
                        full.names = TRUE, ignore.case = TRUE)

if (length(raw_files) == 0)
  stop("No 4-year CSV file found in: ", raw_dir)

raw_file <- if (length(raw_files) > 1)
  raw_files[which.max(file.mtime(raw_files))] else raw_files[1]

cat(sprintf("  File: %s\n", basename(raw_file)))

raw_headers       <- fread(raw_file, nrows = 0) |> names()
raw_headers_clean <- clean_names(raw_headers)
ct_clean  <- intersect(c("complaint_type",
                          "problem_formerly_complaint_type",
                          "problem_type"), raw_headers_clean)[1]
orig_ct   <- raw_headers[raw_headers_clean == ct_clean][1]

message("  Reading complaint_type column only...")
dt <- fread(raw_file, select = orig_ct, showProgress = TRUE)
setnames(dt, orig_ct, "complaint_type")
dt[, complaint_type := toupper(trimws(complaint_type))]
cat(sprintf("  Rows loaded      : %s\n", format(nrow(dt), big.mark = ",")))

# =============================================================================
# STEP 3: COUNT BY COMPLAINT TYPE
# =============================================================================
message("\nStep 3: Computing 4-year complaint counts...")

ct_counts <- dt[, .(count_4yr = .N), by = complaint_type]
setorder(ct_counts, -count_4yr)
cat(sprintf("  Unique types in raw data: %d\n", nrow(ct_counts)))

# =============================================================================
# STEP 4: MERGE HIERARCHY ONTO COUNTS
# =============================================================================
message("\nStep 4: Merging hierarchy...")

result <- merge(hier, ct_counts, by = "complaint_type", all.x = TRUE)
result[is.na(count_4yr), count_4yr := 0L]

# Report types in raw data but not in hierarchy
in_raw_not_hier <- ct_counts[!complaint_type %in% hier$complaint_type,
                               .(complaint_type, count_4yr)][order(-count_4yr)]
if (nrow(in_raw_not_hier) > 0) {
  cat(sprintf("\n  %d raw types not in hierarchy (excluded from analysis):\n",
              nrow(in_raw_not_hier)))
  print(in_raw_not_hier[count_4yr >= 1000], nrows = 20)
}

# Report hierarchy types with zero raw records
missing_in_raw <- result[count_4yr == 0, complaint_type]
if (length(missing_in_raw) > 0) {
  cat(sprintf("\n  %d hierarchy types not found in raw data (zero counts):\n",
              length(missing_in_raw)))
  cat(paste(" ", missing_in_raw, collapse = "\n"), "\n")
}

# =============================================================================
# STEP 5: AGGREGATE TO FAMILY LEVEL
# =============================================================================
message("\nStep 5: Aggregating to family level...")

family_totals <- result[, .(
  family_total     = sum(count_4yr),
  bundle           = unique(bundle)[1],
  mayoral_interest = fifelse(any(mayoral_interest == "Y"), "Y", "N"),
  n_orig_types     = .N,
  orig_types       = paste(sort(complaint_type), collapse = " | ")
), by = family]

setorder(family_totals, -family_total)
family_totals[, rank := .I]

cat(sprintf("  Families total              : %d\n", nrow(family_totals)))
cat(sprintf("  Families >= %s complaints  : %d\n",
            format(VOLUME_THRESHOLD, big.mark = ","),
            family_totals[family_total >= VOLUME_THRESHOLD, .N]))
cat(sprintf("  Mayoral interest families   : %d\n",
            family_totals[mayoral_interest == "Y", .N]))

# =============================================================================
# STEP 6: CARRY FORWARD EXISTING FLAGS OR PRE-POPULATE
# =============================================================================
message("\nStep 6: Applying include_in_analysis flags...")

ref_dir  <- file.path(dirname(PATHS$tables), "reference")
ref_path <- file.path(ref_dir, "complaint_classification.xlsx")
if (!dir.exists(ref_dir)) dir.create(ref_dir, recursive = TRUE)

if (file.exists(ref_path)) {
  message("  Existing file found — carrying forward flags...")
  existing <- as.data.table(read.xlsx(ref_path, sheet = "Classification"))
  if ("family" %in% names(existing) &&
      "include_in_analysis" %in% names(existing)) {
    keep <- existing[, .(
      family,
      include_in_analysis,
      notes = if ("notes" %in% names(existing)) notes else ""
    )]
    family_totals <- merge(family_totals, keep, by = "family", all.x = TRUE)
    cat(sprintf("  Flags carried    : %d\n",
                family_totals[!is.na(include_in_analysis) &
                                include_in_analysis != "", .N]))
    cat(sprintf("  New families     : %d\n",
                family_totals[is.na(include_in_analysis) |
                                include_in_analysis == "", .N]))
  }
} else {
  message("  No existing file — pre-populating...")
  family_totals[, include_in_analysis := ""]
  family_totals[, notes               := ""]
}

# Ensure columns exist
for (col in c("include_in_analysis", "notes"))
  if (!col %in% names(family_totals))
    family_totals[, (col) := ""]

# Fill blanks using threshold + mayoral interest logic
family_totals[is.na(include_in_analysis) | include_in_analysis == "",
              include_in_analysis := fcase(
                mayoral_interest == "Y",           "Y",  # always include
                family_total      >= VOLUME_THRESHOLD, "Y",  # above threshold
                default =                          "N"   # below threshold
              )]

# Summary
n_y      <- family_totals[include_in_analysis == "Y", .N]
n_n      <- family_totals[include_in_analysis == "N", .N]
n_thresh <- family_totals[include_in_analysis == "N" &
                             family_total < VOLUME_THRESHOLD &
                             mayoral_interest != "Y", .N]

cat(sprintf("\n  Volume threshold : %s\n", format(VOLUME_THRESHOLD, big.mark=",")))
cat(sprintf("  Included (Y)     : %d families\n", n_y))
cat(sprintf("  Excluded (N)     : %d families\n", n_n))
cat(sprintf("    Below threshold: %d\n",           n_thresh))

# Print below-threshold list for review
below <- family_totals[include_in_analysis == "N" &
                         family_total < VOLUME_THRESHOLD,
                        .(family, bundle, family_total, mayoral_interest)]
if (nrow(below) > 0) {
  message("\n  Below-threshold families (flip to Y in Excel to override):")
  print(below[order(-family_total)], nrows = 30)
}

# Final column order
setcolorder(family_totals, c(
  "rank", "family", "bundle", "family_total", "mayoral_interest",
  "n_orig_types", "orig_types", "include_in_analysis", "notes"
))

# =============================================================================
# STEP 7: WRITE EXCEL OUTPUT (4 sheets)
# =============================================================================
message("\nStep 7: Writing reference Excel...")

wb <- createWorkbook()

# ---- Sheet 1: Classification (living document — edit include_in_analysis here)
addWorksheet(wb, "Classification")
writeDataTable(wb, "Classification",
               as.data.frame(family_totals),
               tableStyle = "TableStyleMedium2")
freezePane(wb, "Classification", firstRow = TRUE)

col_out  <- names(family_totals)
inc_col  <- which(col_out == "include_in_analysis")
n_rows   <- nrow(family_totals)

green_style  <- createStyle(fgFill = "#C6EFCE", fontColour = "#276221")
yellow_style <- createStyle(fgFill = "#FFEB9C", fontColour = "#9C6500")

conditionalFormatting(wb, "Classification",
                      cols = 1:length(col_out), rows = 2:(n_rows + 1),
                      rule = paste0('$', LETTERS[inc_col], '2="Y"'),
                      style = green_style, type = "expression")
conditionalFormatting(wb, "Classification",
                      cols = 1:length(col_out), rows = 2:(n_rows + 1),
                      rule = paste0('$', LETTERS[inc_col], '2="N"'),
                      style = yellow_style, type = "expression")
setColWidths(wb, "Classification", cols = 1:length(col_out), widths = "auto")

# ---- Sheet 2: Full hierarchy (complaint_type -> family -> bundle)
hier_out <- merge(
  result[, .(complaint_type, family, bundle, mayoral_interest, count_4yr)],
  family_totals[, .(family, family_total, include_in_analysis)],
  by = "family", all.x = TRUE
)
setorder(hier_out, bundle, family, -count_4yr)

addWorksheet(wb, "Hierarchy")
writeDataTable(wb, "Hierarchy", as.data.frame(hier_out),
               tableStyle = "TableStyleMedium7")
freezePane(wb, "Hierarchy", firstRow = TRUE)
setColWidths(wb, "Hierarchy", cols = 1:ncol(hier_out), widths = "auto")

# ---- Sheet 3: Bundle summary
bundle_summary <- family_totals[, .(
  n_families_total    = .N,
  n_families_included = sum(include_in_analysis == "Y"),
  total_complaints    = sum(family_total),
  included_complaints = sum(family_total[include_in_analysis == "Y"])
), by = bundle]
setorder(bundle_summary, -total_complaints)

addWorksheet(wb, "Bundle_Summary")
writeDataTable(wb, "Bundle_Summary", as.data.frame(bundle_summary),
               tableStyle = "TableStyleMedium3")
freezePane(wb, "Bundle_Summary", firstRow = TRUE)
setColWidths(wb, "Bundle_Summary", cols = 1:ncol(bundle_summary), widths = "auto")

# ---- Sheet 4: Instructions
addWorksheet(wb, "Instructions")
instr <- data.frame(
  Field = c(
    "CLASSIFICATION SHEET — one row per Complaint Family",
    "rank", "family", "bundle", "family_total", "mayoral_interest",
    "n_orig_types", "orig_types", "include_in_analysis", "notes",
    "", "WORKFLOW",
    "Step 1", "Step 2", "Step 3", "Step 4",
    "", "TO CHANGE THE HIERARCHY",
    "Edit", "Then re-run"
  ),
  Description = c(
    "",
    "Rank by 4-year complaint volume (highest = 1)",
    "Complaint Family name — the unit of analysis in the QoL Index",
    "Bundle this family belongs to (7 bundles)",
    "Total 4-year complaints across all original types in this family",
    "Y = exempt from volume threshold — always included if QoL relevant",
    "Number of original 311 complaint types rolled into this family",
    "All original complaint types in this family (pipe-separated)",
    "Y = include in QoL Index | N = exclude  <-- EDIT THIS COLUMN",
    "Optional rationale or notes",
    "", "How to use this file:",
    "Run 00_build_classification_master.R to generate/refresh",
    "Green = included | Yellow = excluded. Edit include_in_analysis as needed",
    "Save this file — 01_data_prep.R reads it automatically on every run",
    "No need to re-run 00_ just to change Y/N flags",
    "", "To change complaint types, families, or bundles:",
    "data/reference/complaint_hierarchy.csv",
    "00_build_classification_master.R"
  )
)
writeData(wb, "Instructions", instr)
setColWidths(wb, "Instructions", cols = 1:2, widths = c(30, 75))

saveWorkbook(wb, ref_path, overwrite = TRUE)
cat(sprintf("  Saved: %s\n", ref_path))

# =============================================================================
# CONSOLE SUMMARY
# =============================================================================
message("\nIncluded families by bundle:")
included <- family_totals[include_in_analysis == "Y"][order(bundle, -family_total)]
print(included[, .(family, bundle, family_total, mayoral_interest)], nrows = 60)

elapsed <- proc.time() - prog_start
message("\n", strrep("=", 80))
message("CLASSIFICATION MASTER COMPLETE")
message(strrep("=", 80))
message(sprintf("  Hierarchy : %d complaint types -> %d families -> %d bundles",
                nrow(hier), nrow(family_totals), uniqueN(family_totals$bundle)))
message(sprintf("  Included  : %d of %d families",
                family_totals[include_in_analysis == "Y", .N],
                nrow(family_totals)))
message(sprintf("  Output    : %s", ref_path))
message(sprintf("  Elapsed   : %.1f seconds", elapsed["elapsed"]))
message(sprintf("  Next      : Review Excel, adjust Y/N if needed,"))
message(sprintf("              then run 01_data_prep.R"))
message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
