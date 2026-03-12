# =============================================================================
# apply_classification.R
# NYC 311 Quality of Life Index
#
# PURPOSE: Single-function file that maps raw 311 complaint types to their
# analytical family and bundle, then filters the dataset to only those
# families designated for inclusion in the QoL Index.
#
# Called by 01_data_prep.R once per raw CSV file (baseline and monitor).
#
# FUNCTION: apply_classification(dt, ref_path, hier_path, verbose = TRUE)
#
# ARGUMENTS:
#   dt        — data.table with a column named "complaint_type" (uppercased)
#   ref_path  — path to complaint_classification.xlsx (output of 00_)
#   hier_path — path to complaint_hierarchy.csv (data/reference/)
#   verbose   — if TRUE, prints row counts and filter summary to console
#
# PROCESSING:
#   1. Loads complaint_hierarchy.csv: complaint_type -> family -> bundle
#   2. Loads complaint_classification.xlsx (Classification sheet):
#      reads include_in_analysis column to get the list of included families
#   3. Merges hierarchy onto dt: assigns family and bundle to every row.
#      Complaint types not present in the hierarchy receive NA family.
#   4. Filters dt to included families only (include_in_analysis == "Y").
#      Rows with NA family (types not in hierarchy) are also excluded.
#
# RETURNS:
#   The input data.table filtered to QoL-included families, with two
#   new columns added:
#     family — complaint family name (uppercase)
#     bundle — bundle key (lowercase, matches config.R BUNDLES)
#
# DEPENDENCIES:
#   data.table, openxlsx
#   00_build_classification_master.R must have been run to produce ref_path
#
# NOTE: This function executes inclusion decisions — it does not make them.
#   To change which families are included, edit include_in_analysis flags
#   in complaint_classification.xlsx. No code changes needed.
# =============================================================================

apply_classification <- function(dt, ref_path, hier_path, verbose = TRUE) {

  if (!file.exists(ref_path))
    stop("Classification master not found: ", ref_path,
         "\nRun 00_build_classification_master.R first.")

  if (!file.exists(hier_path))
    stop("Complaint hierarchy not found: ", hier_path,
         "\nExpected: data/reference/complaint_hierarchy.csv")

  if (verbose) message("  Loading complaint hierarchy...")

  # Load hierarchy: complaint_type -> family -> bundle
  hier <- fread(hier_path, encoding = "UTF-8")
  hier[, complaint_type := toupper(trimws(complaint_type))]
  hier[, family         := toupper(trimws(family))]
  hier[, bundle         := tolower(trimws(bundle))]

  # Load classification: family -> include_in_analysis
  if (verbose) message("  Loading classification master...")
  cls <- as.data.table(read.xlsx(ref_path, sheet = "Classification"))

  req_cols <- c("family", "include_in_analysis")
  missing  <- setdiff(req_cols, names(cls))
  if (length(missing) > 0)
    stop("Classification file missing columns: ", paste(missing, collapse = ", "))

  included_families <- cls[include_in_analysis == "Y", family]
  included_families <- toupper(trimws(included_families))

  if (verbose) {
    n_included <- length(included_families)
    n_excluded <- cls[include_in_analysis != "Y", .N]
    cat(sprintf("    Families included        : %d\n", n_included))
    cat(sprintf("    Families excluded        : %d\n", n_excluded))
  }

  # Apply hierarchy: complaint_type -> family + bundle
  dt <- merge(dt, hier[, .(complaint_type, family, bundle)],
              by = "complaint_type", all.x = TRUE)

  # Complaint types not in hierarchy get NA family — they'll be filtered out
  n_no_family <- dt[is.na(family), .N]
  if (n_no_family > 0 && verbose)
    cat(sprintf("    Types not in hierarchy   : %s rows (excluded)\n",
                format(n_no_family, big.mark = ",")))

  # Filter to included families
  n_before <- nrow(dt)
  dt <- dt[family %in% included_families]
  n_after  <- nrow(dt)

  if (verbose) {
    cat(sprintf("    Rows before filter       : %s\n",
                format(n_before, big.mark = ",")))
    cat(sprintf("    Rows after filter        : %s\n",
                format(n_after,  big.mark = ",")))
    cat(sprintf("    Rows excluded            : %s (%.1f%%)\n",
                format(n_before - n_after, big.mark = ","),
                100 * (n_before - n_after) / n_before))
    cat(sprintf("    Families retained        : %d\n",
                uniqueN(dt$family)))
    cat(sprintf("    Bundles retained         : %d\n",
                uniqueN(dt$bundle)))
  }

  dt
}
