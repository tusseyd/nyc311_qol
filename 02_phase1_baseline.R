# =============================================================================
# 02_phase1_baseline.R
# NYC 311 Quality of Life Index
#
# PURPOSE: Reads the prepared 2021 baseline data and computes Phase I baseline
# statistics used as the reference point for all Phase II index calculations.
# Output is consumed by 03_analysis.R.
#
# TWO-TIER BASELINE APPROACH:
#   Tier 1 (seasonally adjusted):
#     Complaint families with >= 9 qualifying months in 2021 receive a
#     separate baseline mean for each calendar month (Jan-Dec). Index values
#     in Phase II are compared month-to-month, removing seasonal bias.
#
#   Tier 2 (overall mean):
#     Families with < 9 qualifying months receive a single annual mean
#     computed from whatever qualifying months exist. Flagged in all outputs
#     as having limited baseline reliability. Phase II index values for these
#     families are not seasonally adjusted.
#
# MINIMUM ACTIVITY THRESHOLD:
#     100 complaints per month. Months below this threshold are excluded
#     from baseline mean calculations for both tiers.
#
# INTERPOLATION:
#     Tier 1 families may have isolated months that fell below the activity
#     threshold. Gaps of 1-3 consecutive missing months are filled by linear
#     interpolation between nearest observed neighbors, with circular
#     wrap-around logic (Dec <-> Jan). Gaps of 4+ months are left as NA.
#     Interpolated values are flagged baseline_interpolated = TRUE for
#     distinct rendering in downstream charts (hollow points).
#
# ANOMALOUS & EMERGING TYPES:
#     Types designated in complaint_hierarchy.csv via the baseline_flag column:
#       anomalous — has 2021 data but structurally invalid (e.g. migration
#                   spike). Removed from baseline stats and routed to the
#                   emerging pipeline in 03_.
#       emerging  — zero 2021 activity. Documented here and passed downstream.
#     To add or change a designation: edit complaint_hierarchy.csv and re-run.
#
# PROCESSING STEPS:
#   1.  Load prepared baseline data
#   2.  Aggregate to monthly counts (family level)
#   2b. Aggregate to monthly counts (original complaint type level)
#   3.  Classify families into Tier 1 / Tier 2
#   3b. Classify original types into Tier 1 / Tier 2
#   4.  Compute baseline means (family level)
#   4-interp. Interpolate missing Tier 1 family baseline months
#   4b. Compute baseline means (original type level)
#   4b-interp. Interpolate missing Tier 1 detail baseline months
#   4c. Load anomalous/emerging designations from hierarchy
#   5.  Print baseline summary to console
#   6.  Save all outputs
#
# Input:  output/data/prepared_baseline.RDS
# Outputs: output/data/baseline_stats.RDS          (family level)
#          output/data/baseline_stats_detail.RDS    (original type level,
#                                                    anomalous removed)
#          output/data/anomalous_baseline_types.RDS (flags: anomalous + emerging)
#          output/data/baseline_monthly.RDS
#          output/tables/baseline_monthly_counts.csv
#          output/tables/baseline_summary.csv
# =============================================================================

message("\n", strrep("=", 80))
message("NYC 311 QUALITY OF LIFE INDEX - PHASE I BASELINE")
message(strrep("=", 80))
message("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

prog_start <- proc.time()

source("config.R")

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

CT_COL            <- "problem_formerly_complaint_type"
MIN_THRESHOLD     <- 100L   # minimum monthly complaints to include in baseline
SPARSE_THRESHOLD  <- 9L     # 75% of 12 baseline months — types below this go Tier 2

# Anomalous/emerging designations are read from complaint_hierarchy.csv
# (baseline_flag column). See Step 4c.

# -----------------------------------------------------------------------------
# STEP 1: Load prepared baseline data
# -----------------------------------------------------------------------------
message("Step 1: Loading prepared baseline data...")

if (!file.exists(PATHS$processed_baseline))
  stop("Baseline RDS not found: ", PATHS$processed_baseline,
       "\nRun 01_data_prep.R first.")

baseline <- readRDS(PATHS$processed_baseline)

cat(sprintf("  Rows loaded      : %s\n", format(nrow(baseline), big.mark = ",")))
cat(sprintf("  Complaint types  : %d\n", uniqueN(baseline[[CT_COL]])))
cat(sprintf("  Year-months      : %d\n", uniqueN(baseline$year_month)))
cat(sprintf("  Date range       : %s to %s\n",
            format(min(baseline$created_date), "%Y-%m-%d"),
            format(max(baseline$created_date), "%Y-%m-%d")))

years_present <- sort(unique(baseline$year))
cat(sprintf("  Years present    : %s\n", paste(years_present, collapse = ", ")))

unexpected_years <- setdiff(years_present, PHASE1_YEARS)
if (length(unexpected_years) > 0)
  warning("Unexpected years in baseline data: ",
          paste(unexpected_years, collapse = ", "))

# -----------------------------------------------------------------------------
# STEP 2: Aggregate to monthly counts per complaint type
# -----------------------------------------------------------------------------
message("\nStep 2: Aggregating to monthly counts (family level)...")

monthly <- baseline[, .(monthly_count = .N),
                    by = .(complaint_type = family,
                           year_month,
                           month = month(created_date))]

# Fill in zero-count months for complete grid — use FAMILY names not orig types
all_months      <- sort(unique(baseline$year_month))
all_families    <- sort(unique(baseline$family))

full_grid <- CJ(complaint_type = all_families,
                year_month     = all_months)
full_grid[, month := month(year_month)]

monthly <- merge(full_grid, monthly,
                 by  = c("complaint_type", "year_month", "month"),
                 all = TRUE)
monthly[is.na(monthly_count), monthly_count := 0L]
setorder(monthly, complaint_type, year_month)

cat(sprintf("  Months in baseline period : %d\n", length(all_months)))
cat(sprintf("  Families                  : %d\n", length(all_families)))

# -----------------------------------------------------------------------------
# STEP 2b: Aggregate to monthly counts (original complaint type level)
# Used for detail charts — each original type gets its own baseline
# -----------------------------------------------------------------------------
message("\nStep 2b: Aggregating to monthly counts (original type level)...")

monthly_detail <- baseline[, .(monthly_count = .N),
                            by = .(complaint_type = get(CT_COL),
                                   year_month,
                                   month = month(created_date))]

all_orig_types <- sort(unique(baseline[[CT_COL]]))

full_grid_detail <- CJ(complaint_type = all_orig_types,
                       year_month     = all_months)
full_grid_detail[, month := month(year_month)]

monthly_detail <- merge(full_grid_detail, monthly_detail,
                        by  = c("complaint_type", "year_month", "month"),
                        all = TRUE)
monthly_detail[is.na(monthly_count), monthly_count := 0L]
setorder(monthly_detail, complaint_type, year_month)

cat(sprintf("  Original complaint types  : %d\n", length(all_orig_types)))

# -----------------------------------------------------------------------------
# STEP 3: Classify complaint types into Tier 1 / Tier 2
# -----------------------------------------------------------------------------
message("\nStep 3: Classifying complaint types by baseline data quality...")

# Count qualifying months per complaint type (above threshold)
qualifying <- monthly[, .(
  n_qualifying = sum(monthly_count >= MIN_THRESHOLD),
  n_total      = .N
), by = complaint_type]

qualifying[, tier := ifelse(n_qualifying >= SPARSE_THRESHOLD, 1L, 2L)]

tier1_types <- qualifying[tier == 1L, complaint_type]
tier2_types <- qualifying[tier == 2L, complaint_type]

cat(sprintf("\n  Minimum activity threshold : %d complaints/month\n", MIN_THRESHOLD))
cat(sprintf("  Sparse threshold           : %d qualifying months\n", SPARSE_THRESHOLD))
cat(sprintf("\n  Tier 1 (seasonally adjusted) : %d complaint types\n", length(tier1_types)))
cat(sprintf("  Tier 2 (overall mean)        : %d complaint types\n", length(tier2_types)))

if (length(tier2_types) > 0) {
  cat("\n  Tier 2 complaint types (limited baseline reliability):\n")
  print(qualifying[tier == 2L, .(complaint_type, n_qualifying, n_total)],
        row.names = FALSE)
}

# -----------------------------------------------------------------------------
# STEP 4: Compute baseline means
# -----------------------------------------------------------------------------
message("\nStep 4: Computing baseline means...")

# --- Tier 1: seasonal baseline (mean per complaint type per calendar month) ---
tier1_baseline <- monthly[complaint_type %in% tier1_types &
                            monthly_count >= MIN_THRESHOLD,
                          .(
                            baseline_mean  = mean(monthly_count),
                            baseline_sd    = sd(monthly_count),
                            n_months_used  = .N
                          ),
                          by = .(complaint_type, month)]

# --- Tier 2: overall baseline (single mean per complaint type) ---
tier2_baseline <- monthly[complaint_type %in% tier2_types &
                            monthly_count >= MIN_THRESHOLD,
                          .(
                            baseline_mean  = mean(monthly_count),
                            baseline_sd    = sd(monthly_count),
                            n_months_used  = .N
                          ),
                          by = complaint_type]

# Add month = NA placeholder and tier flags
tier1_baseline[, tier := 1L]
tier1_baseline[, baseline_interpolated := FALSE]
tier2_baseline[, tier := 2L]
tier2_baseline[, month := NA_integer_]
tier2_baseline[, baseline_interpolated := FALSE]

# -----------------------------------------------------------------------------
# STEP 4-interp: Interpolate missing baseline months for Tier 1 family types
# Same logic as Step 4b-interp at detail level.
# -----------------------------------------------------------------------------
message("\nStep 4-interp: Interpolating missing Tier 1 family baseline months...")

MAX_INTERP_GAP <- 3L  # max consecutive missing months to interpolate

fam_interp_added <- 0L
all_tier1_fam    <- unique(tier1_baseline$complaint_type)

fam_interp_rows <- rbindlist(lapply(all_tier1_fam, function(ct) {

  observed        <- tier1_baseline[complaint_type == ct, .(month, baseline_mean)]
  observed_months <- observed$month
  missing_months  <- setdiff(1:12, observed_months)

  if (length(missing_months) == 0) return(NULL)

  result <- rbindlist(lapply(missing_months, function(m) {

    prev_candidates <- observed_months[observed_months < m]
    if (length(prev_candidates) == 0) prev_candidates <- observed_months
    prev_m <- max(prev_candidates)

    next_candidates <- observed_months[observed_months > m]
    if (length(next_candidates) == 0) next_candidates <- observed_months
    next_m <- min(next_candidates)

    dist_prev <- ifelse(prev_m < m, m - prev_m, m + 12 - prev_m)
    dist_next <- ifelse(next_m > m, next_m - m, next_m + 12 - m)
    gap_size  <- dist_prev + dist_next - 1L

    if (gap_size > MAX_INTERP_GAP) return(NULL)

    val_prev   <- observed[month == prev_m, baseline_mean]
    val_next   <- observed[month == next_m, baseline_mean]
    interp_val <- val_prev + (val_next - val_prev) * (dist_prev / (dist_prev + dist_next))

    data.table(
      complaint_type        = ct,
      month                 = m,
      baseline_mean         = round(interp_val, 2),
      baseline_sd           = NA_real_,
      n_months_used         = 0L,
      tier                  = 1L,
      baseline_interpolated = TRUE
    )
  }))
  result
}))

if (!is.null(fam_interp_rows) && nrow(fam_interp_rows) > 0) {
  fam_interp_added <- nrow(fam_interp_rows)
  tier1_baseline   <- rbindlist(list(tier1_baseline, fam_interp_rows),
                                use.names = TRUE, fill = TRUE)
  setorder(tier1_baseline, complaint_type, month)
}
cat(sprintf("  Interpolated family baseline months added : %d\n", fam_interp_added))

# Combine
baseline_stats <- rbindlist(
  list(tier1_baseline, tier2_baseline),
  use.names = TRUE,
  fill      = TRUE
)

# -----------------------------------------------------------------------------
# STEP 3b: Classify original complaint types into Tier 1 / Tier 2
# -----------------------------------------------------------------------------
message("\nStep 3b: Classifying original types by baseline data quality...")

qualifying_detail <- monthly_detail[, .(
  n_qualifying = sum(monthly_count >= MIN_THRESHOLD),
  n_total      = .N
), by = complaint_type]

qualifying_detail[, tier := ifelse(n_qualifying >= SPARSE_THRESHOLD, 1L, 2L)]

tier1_types_detail <- qualifying_detail[tier == 1L, complaint_type]
tier2_types_detail <- qualifying_detail[tier == 2L, complaint_type]

cat(sprintf("  Tier 1 original types : %d\n", length(tier1_types_detail)))
cat(sprintf("  Tier 2 original types : %d\n", length(tier2_types_detail)))

# -----------------------------------------------------------------------------
# STEP 4b: Compute baseline means (original complaint type level)
# -----------------------------------------------------------------------------
message("\nStep 4b: Computing detail baseline means...")

tier1_baseline_detail <- monthly_detail[
  complaint_type %in% tier1_types_detail & monthly_count >= MIN_THRESHOLD,
  .(baseline_mean = mean(monthly_count),
    baseline_sd   = sd(monthly_count),
    n_months_used = .N),
  by = .(complaint_type, month)]

# -----------------------------------------------------------------------------
# STEP 4b-interp: Interpolate missing baseline months for Tier 1 detail types
#
# Some Tier 1 types qualify overall (9+ months) but have gaps — months where
# activity fell below MIN_THRESHOLD (e.g. Encampment Jan=9, Feb=5).
# For gaps of 1-3 consecutive months we linearly interpolate between the
# nearest observed neighbors, wrapping around Dec<->Jan.
# Interpolated rows are flagged baseline_interpolated = TRUE so 04_ can
# render them as hollow points in a muted color.
# Gaps of 4+ consecutive months are left as NA (too large to interpolate).
# -----------------------------------------------------------------------------
message("\nStep 4b-interp: Interpolating missing Tier 1 detail baseline months...")

interp_added <- 0L

tier1_baseline_detail[, baseline_interpolated := FALSE]

all_tier1_types <- unique(tier1_baseline_detail$complaint_type)

interp_rows <- rbindlist(lapply(all_tier1_types, function(ct) {

  observed <- tier1_baseline_detail[complaint_type == ct, .(month, baseline_mean)]
  observed_months <- observed$month
  missing_months  <- setdiff(1:12, observed_months)

  if (length(missing_months) == 0) return(NULL)

  # Find runs of consecutive missing months (circular: month 12 neighbors month 1)
  # Tag each missing month with its run length
  runs <- rle(diff(c(missing_months, missing_months[1] + 12)))
  # Simple approach: just check each missing month individually for gap size
  # by finding nearest observed neighbors in circular month space

  result <- rbindlist(lapply(missing_months, function(m) {

    # Find nearest observed month below (going back, wrapping)
    prev_candidates <- observed_months[observed_months < m]
    if (length(prev_candidates) == 0)
      prev_candidates <- observed_months  # wrap: use all (take max = Dec)
    prev_m <- max(prev_candidates)
    if (prev_m >= m) prev_m <- prev_m  # already wrapped

    # Find nearest observed month above (going forward, wrapping)
    next_candidates <- observed_months[observed_months > m]
    if (length(next_candidates) == 0)
      next_candidates <- observed_months  # wrap: use all (take min = Jan)
    next_m <- min(next_candidates)

    # Circular distance to each neighbor
    dist_prev <- ifelse(prev_m < m, m - prev_m, m + 12 - prev_m)
    dist_next <- ifelse(next_m > m, next_m - m, next_m + 12 - m)
    gap_size  <- dist_prev + dist_next - 1L  # total gap including this month

    if (gap_size > MAX_INTERP_GAP) return(NULL)

    # Linear interpolation: weight by circular distance
    val_prev <- observed[month == prev_m, baseline_mean]
    val_next <- observed[month == next_m, baseline_mean]
    interp_val <- val_prev + (val_next - val_prev) * (dist_prev / (dist_prev + dist_next))

    data.table(
      complaint_type        = ct,
      month                 = m,
      baseline_mean         = round(interp_val, 2),
      baseline_sd           = NA_real_,
      n_months_used         = 0L,
      tier                  = 1L,
      baseline_interpolated = TRUE
    )
  }))

  result
}))

if (!is.null(interp_rows) && nrow(interp_rows) > 0) {
  interp_added <- nrow(interp_rows)
  tier1_baseline_detail <- rbindlist(
    list(tier1_baseline_detail, interp_rows),
    use.names = TRUE, fill = TRUE
  )
  setorder(tier1_baseline_detail, complaint_type, month)
}

cat(sprintf("  Interpolated baseline months added : %d\n", interp_added))
if (interp_added > 0) {
  interp_summary <- tier1_baseline_detail[baseline_interpolated == TRUE,
                                           .(months = paste(sort(month), collapse = ",")),
                                           by = complaint_type]
  print(interp_summary, row.names = FALSE)
}

tier2_baseline_detail <- monthly_detail[
  complaint_type %in% tier2_types_detail & monthly_count >= MIN_THRESHOLD,
  .(baseline_mean = mean(monthly_count),
    baseline_sd   = sd(monthly_count),
    n_months_used = .N),
  by = complaint_type]

tier1_baseline_detail[, tier := 1L]
tier2_baseline_detail[, tier := 2L]
tier2_baseline_detail[, month                 := NA_integer_]
tier2_baseline_detail[, baseline_interpolated := FALSE]

baseline_stats_detail <- rbindlist(
  list(tier1_baseline_detail, tier2_baseline_detail),
  use.names = TRUE,
  fill      = TRUE
)

cat(sprintf("  Detail Tier 1 rows : %d\n", nrow(tier1_baseline_detail)))
cat(sprintf("  Detail Tier 2 rows : %d\n", nrow(tier2_baseline_detail)))

# -----------------------------------------------------------------------------
# STEP 4c: Load manually designated anomalous/emerging types from hierarchy
#
# baseline_flag column in complaint_hierarchy.csv:
#   anomalous — type has baseline-period data but it is structurally invalid
#               (e.g. data migration spike, ramp from near-zero). Removed from
#               baseline_stats_detail and routed to the emerging pipeline in 03_.
#   emerging  — type had zero baseline-period activity; already absent from
#               baseline_stats_detail naturally. Listed here for documentation
#               and to pass the emerging_source tag downstream.
#   (blank)   — normal; index as usual.
#
# To add or change a designation: edit complaint_hierarchy.csv and re-run 02_.
# -----------------------------------------------------------------------------
message("\nStep 4c: Loading baseline flag designations from hierarchy...")

hier_flags <- fread(PATHS$complaint_hierarchy, encoding = "UTF-8",
                    select = c("complaint_type", "baseline_flag"))
hier_flags[, complaint_type := toupper(trimws(complaint_type))]
hier_flags[, baseline_flag  := tolower(trimws(baseline_flag))]
hier_flags <- hier_flags[baseline_flag != ""]

anomalous_types_vec <- hier_flags[baseline_flag == "anomalous", complaint_type]
emerging_types_vec  <- hier_flags[baseline_flag == "emerging",  complaint_type]

cat(sprintf("  anomalous designations : %d\n", length(anomalous_types_vec)))
cat(sprintf("  emerging  designations : %d\n", length(emerging_types_vec)))

if (length(anomalous_types_vec) > 0 || length(emerging_types_vec) > 0) {
  print(hier_flags[, .(complaint_type, baseline_flag)], row.names = FALSE)
}

if (length(anomalous_types_vec) > 0) {

  # Verify all designated types actually exist in baseline_stats_detail
  # (warn if a name is misspelled or not present in data)
  missing_in_data <- setdiff(anomalous_types_vec,
                              unique(baseline_stats_detail$complaint_type))
  if (length(missing_in_data) > 0) {
    warning("anomalous types not found in baseline_stats_detail (check spelling): ",
            paste(missing_in_data, collapse = ", "))
  }

  present_anomalous <- intersect(anomalous_types_vec,
                                  unique(baseline_stats_detail$complaint_type))
  cat(sprintf("  Removing %d anomalous type(s) from baseline_stats_detail\n",
              length(present_anomalous)))

  baseline_stats_detail <- baseline_stats_detail[
    !complaint_type %in% anomalous_types_vec
  ]

  # Also remove from family-level baseline_stats.
  # baseline_stats uses FAMILY names as complaint_type, but anomalous_types_vec
  # contains ORIGINAL type names. Build a lookup from original type -> family,
  # then translate to get the family names to remove.
  hier_full <- fread(PATHS$complaint_hierarchy, encoding = "UTF-8",
                     select = c("complaint_type", "family"))
  hier_full[, complaint_type := toupper(trimws(complaint_type))]
  hier_full[, family         := toupper(trimws(family))]

  anomalous_families <- unique(
    hier_full[complaint_type %in% anomalous_types_vec, family]
  )

  present_anomalous_fam <- intersect(anomalous_families,
                                      unique(baseline_stats$complaint_type))
  if (length(present_anomalous_fam) > 0) {
    cat(sprintf("  Removing %d anomalous family/families from baseline_stats\n",
                length(present_anomalous_fam)))
    cat(paste0("    ", present_anomalous_fam, collapse = "\n"), "\n")
    baseline_stats <- baseline_stats[!complaint_type %in% anomalous_families]
  }
}

# Save all flagged types for 03_ to consume (both anomalous and emerging)
anomalous_baseline_dt   <- hier_flags
anomalous_baseline_path <- file.path(PATHS$data_out, "anomalous_baseline_types.RDS")
saveRDS(anomalous_baseline_dt, anomalous_baseline_path)
cat(sprintf("  Saved: %s\n", basename(anomalous_baseline_path)))

# -----------------------------------------------------------------------------
# STEP 5: Print summary to console
# -----------------------------------------------------------------------------
# Provides: complaint_type, bundle (key), label
hier_dt   <- fread(PATHS$complaint_hierarchy, encoding = "UTF-8")
hier_dt[, complaint_type := toupper(trimws(complaint_type))]
hier_dt[, bundle         := tolower(trimws(bundle))]
hier_dt[, family         := toupper(trimws(family))]

bundle_labels_dt <- as.data.table(BUNDLE_LABELS)

lookup_dt <- merge(
  hier_dt[, .(complaint_type, family, bundle)],
  bundle_labels_dt,
  by = "bundle", all.x = TRUE
)
# Rename 'label' to 'bundle_label' for backward compatibility in this script
setnames(lookup_dt, c("bundle", "label"), c("bundle_key", "bundle_label"))

baseline_stats <- merge(baseline_stats, 
                        lookup_dt[, .(complaint_type, bundle_key, bundle_label)],
                        by    = "complaint_type",
                        all.x = TRUE)

setorder(baseline_stats, bundle_key, complaint_type, month)

cat(sprintf("  Tier 1 rows (type x month) : %d\n", nrow(tier1_baseline)))
cat(sprintf("  Tier 2 rows (type only)    : %d\n", nrow(tier2_baseline)))
cat(sprintf("  Total baseline_stats rows  : %d\n", nrow(baseline_stats)))

# -----------------------------------------------------------------------------
# STEP 5: Print summary to console
# -----------------------------------------------------------------------------
message("\nStep 5: Baseline summary...\n")

cat(strrep("=", 80), "\n")
cat("PHASE I BASELINE STATISTICS (2021)\n")
cat(sprintf("Minimum activity threshold: %d complaints/month\n", MIN_THRESHOLD))
cat(strrep("=", 80), "\n\n")

for (bk in unique(lookup_dt$bundle_key)) {

  bundle_label <- lookup_dt[bundle_key == bk, bundle_label][1]
  types_in_bundle <- lookup_dt[bundle_key == bk, complaint_type]

  cat(strrep("-", 80), "\n")
  cat(sprintf("Bundle: %s\n", bundle_label))
  cat(strrep("-", 80), "\n")

  for (ct in types_in_bundle) {

    ct_tier <- qualifying[complaint_type == ct, tier]
    ct_qual <- qualifying[complaint_type == ct, n_qualifying]

    # Skip if family not present in baseline data (e.g. below threshold,
    # or family only appears in monitor period)
    if (length(ct_tier) == 0L) {
      cat(sprintf("  %-40s [NOT IN BASELINE — skipped]\n", ct))
      next
    }

    if (ct_tier == 1L) {
      # Show annual summary (mean of monthly means)
      ct_stats <- baseline_stats[complaint_type == ct,
                                 .(overall_mean   = round(mean(baseline_mean), 1),
                                   min_month_mean = round(min(baseline_mean, na.rm = TRUE),  1),
                                   max_month_mean = round(max(baseline_mean, na.rm = TRUE),  1),
                                   n_months_used  = sum(n_months_used))]
      cat(sprintf("  %-40s [Tier 1] Mean: %7.1f  Min month: %7.1f  Max month: %7.1f  Months used: %d\n",
                  ct,
                  ct_stats$overall_mean,
                  ct_stats$min_month_mean,
                  ct_stats$max_month_mean,
                  ct_stats$n_months_used))
    } else {
      ct_stats <- baseline_stats[complaint_type == ct,
                                 .(baseline_mean = round(baseline_mean, 1),
                                   n_months_used)]
      cat(sprintf("  %-40s [Tier 2] Mean: %7.1f  Qualifying months: %d/24  ** LIMITED BASELINE **\n",
                  ct,
                  ct_stats$baseline_mean,
                  ct_qual))
    }
  }
  cat("\n")
}

# -----------------------------------------------------------------------------
# STEP 6: Save outputs
# -----------------------------------------------------------------------------
message("Step 6: Saving outputs...")

# baseline_stats.RDS (family level)
saveRDS(baseline_stats, PATHS$baseline_stats)
cat(sprintf("  Saved: %s\n", basename(PATHS$baseline_stats)))

# baseline_stats_detail.RDS (original complaint type level)
baseline_stats_detail_path <- file.path(PATHS$data_out, "baseline_stats_detail.RDS")
saveRDS(baseline_stats_detail, baseline_stats_detail_path)
cat(sprintf("  Saved: %s\n", basename(baseline_stats_detail_path)))

# baseline_monthly.RDS
baseline_monthly_path <- file.path(dirname(PATHS$baseline_stats),
                                   "baseline_monthly.RDS")
saveRDS(monthly, baseline_monthly_path)
cat(sprintf("  Saved: %s\n", basename(baseline_monthly_path)))

# baseline_monthly_counts.CSV - wide format for Excel review
monthly_wide <- dcast(monthly,
                      complaint_type ~ year_month,
                      value.var = "monthly_count",
                      fill      = 0L)

monthly_wide <- merge(monthly_wide,
                      lookup_dt[, .(complaint_type, bundle_key, bundle_label)],
                      by    = "complaint_type",
                      all.x = TRUE)

setcolorder(monthly_wide,
            c("bundle_key", "bundle_label", "complaint_type",
              setdiff(names(monthly_wide),
                      c("bundle_key", "bundle_label", "complaint_type"))))
setorder(monthly_wide, bundle_key, complaint_type)

monthly_csv_path <- file.path(PATHS$tables, "baseline_monthly_counts.csv")
fwrite(monthly_wide, monthly_csv_path)
cat(sprintf("  Saved: %s\n", basename(monthly_csv_path)))

# baseline_summary.CSV - one row per complaint type summary
summary_csv <- qualifying[, .(complaint_type, tier, n_qualifying, n_total)]
summary_csv <- merge(summary_csv,
                     baseline_stats[, .(
                       overall_mean = round(mean(baseline_mean), 1)
                     ), by = complaint_type],
                     by = "complaint_type")
summary_csv <- merge(summary_csv,
                     lookup_dt[, .(complaint_type, bundle_key, bundle_label)],
                     by = "complaint_type")
setorder(summary_csv, bundle_key, complaint_type)

summary_csv_path <- file.path(PATHS$tables, "baseline_summary.csv")
fwrite(summary_csv, summary_csv_path)
cat(sprintf("  Saved: %s\n", basename(summary_csv_path)))

# Verify baseline_stats.RDS
rb <- readRDS(PATHS$baseline_stats)
cat(sprintf("  Verified: %d rows x %d cols | %.1f MB\n",
            nrow(rb), ncol(rb),
            file.size(PATHS$baseline_stats) / 1024^2))
rm(rb)

# -----------------------------------------------------------------------------
# WRAP UP
# -----------------------------------------------------------------------------
elapsed <- proc.time() - prog_start

message("\n", strrep("=", 80))
message("PHASE I BASELINE COMPLETE")
message(strrep("=", 80))
message(sprintf("Elapsed time : %.1f seconds", elapsed["elapsed"]))
message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
