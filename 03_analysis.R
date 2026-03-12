# =============================================================================
# 03_analysis.R
# NYC 311 Quality of Life Index
#
# PURPOSE: Joins the prepared 2022-2025 monitor data against the Phase I
# baseline statistics to compute monthly QoL index values, annual averages,
# directional status labels, and emerging complaint type characterizations.
# Primary output (analysis_results.RDS) feeds 04_visualizations.R and
# 05_heatmap.R.
#
# INDEX FORMULA:
#   Index = monthly_count / baseline_mean
#     > 1.0 = more complaints than baseline (worse QoL)
#     < 1.0 = fewer complaints than baseline (better QoL)
#     = 1.0 = at baseline
#
# TIER JOIN LOGIC:
#   Tier 1 (seasonally adjusted): index joins on complaint_type + month
#   Tier 2 (overall mean):        index joins on complaint_type only
#   Both tiers processed in parallel at family level and original type level.
#
# DIRECTION LABELS (based on 2025 annual average index vs baseline):
#   change = index_2025 - 1.0
#   <= -0.50                        Greatly Improved
#   > -0.50 to <= -0.25             Improved
#   > -0.25 to < -0.10              Slightly Improved
#   >= -0.10 to <= 0.10             Little Changed
#   >  0.10 to <  0.25              Slightly Worse
#   >= 0.25 to <  0.50              Worse
#   >= 0.50                         Much Worse
#
# 2026 YTD OVERLAY (optional):
#   If prepared_2026.RDS is present, complete months are indexed normally.
#   The current partial month is projected using a daily complaint rate:
#     projected_count = (complaints_so_far / days_elapsed) * days_in_month
#   Projected values are flagged is_projected = TRUE for distinct chart rendering.
#
# EMERGING COMPLAINT TYPES:
#   Types that cannot be indexed normally due to absent or invalid baselines
#   are routed to a separate emerging pipeline. Two sources:
#     Source 1 — Auto-detected: zero baseline activity + minimum volume criteria
#                (>= 12 active months AND mean > 100 in at least one year)
#     Source 2 — Manually flagged: anomalous or emerging designation in
#                complaint_hierarchy.csv (passed from 02_ via
#                anomalous_baseline_types.RDS)
#   Emerging types receive a growth index (2025 avg / first active year avg)
#   and a linear trend slope rather than a standard QoL index.
#   NOTE: the emerging baseline is endogenous — drawn from the monitor period
#   itself, not a true pre-period. Flagged clearly in chart captions.
#
# PROCESSING STEPS:
#   1.   Load all inputs
#   2.   Aggregate monitor data to monthly counts (family level)
#   2b.  Aggregate monitor data to monthly counts (original type level)
#   2c.  Load and aggregate 2026 YTD data (optional)
#   3.   Join baseline and compute index (family level)
#   3b.  Join baseline and compute index (original type level)
#   3c.  Compute 2026 index and projections (optional)
#   4.   Compute annual index averages
#   5.   Compute 2025 snapshot and direction labels (family level)
#   5b.  Compute 2025 snapshot and direction labels (original type level)
#   5c.  Detect and characterize emerging complaint types
#   6.   Print annual index summary to console
#   7.   Save all outputs
#
# Inputs:  output/data/prepared_monitor.RDS
#          output/data/baseline_stats.RDS
#          output/data/baseline_stats_detail.RDS
#          output/data/anomalous_baseline_types.RDS  (from 02_)
#          output/data/prepared_2026.RDS             (optional)
#          output/data/max_date_2026.RDS             (optional)
# Outputs: output/data/analysis_results.RDS          (family-level index)
#          output/data/index_data_detail.RDS          (original type level)
#          output/data/index_emerging.RDS             (emerging types)
#          output/data/index_2026.RDS                 (2026 family overlay, optional)
#          output/data/index_2026_detail.RDS          (2026 detail overlay, optional)
#          output/tables/index_monthly.csv
#          output/tables/index_annual.csv
#          output/tables/index_snapshot.csv
#          output/tables/index_snapshot_detail.csv
#          output/tables/emerging_snapshot.csv
# =============================================================================
message("\n", strrep("=", 80))
message("NYC 311 QUALITY OF LIFE INDEX - ANALYSIS")
message(strrep("=", 80))
message("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

prog_start <- proc.time()

source("config.R")

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

CT_COL <- "problem_formerly_complaint_type"

# Direction label function - 7 levels based on 2025 annual average vs baseline
classify_direction <- function(change) {
  fcase(
    change <= -0.50,                   "Greatly Improved",
    change >  -0.50 & change <= -0.25, "Improved",
    change >  -0.25 & change <  -0.10, "Slightly Improved",
    change >= -0.10 & change <=  0.10, "Little Changed",
    change >   0.10 & change <   0.25, "Slightly Worse",
    change >=  0.25 & change <   0.50, "Worse",
    change >=  0.50,                   "Much Worse",
    default = NA_character_
  )
}

# -----------------------------------------------------------------------------
# STEP 1: Load inputs
# -----------------------------------------------------------------------------
message("Step 1: Loading inputs...")

if (!file.exists(PATHS$processed_monitor))
  stop("Monitor RDS not found: ", PATHS$processed_monitor,
       "\nRun 01_data_prep.R first.")

if (!file.exists(PATHS$baseline_stats))
  stop("Baseline stats not found: ", PATHS$baseline_stats,
       "\nRun 02_phase1_baseline.R first.")

baseline_stats_detail_path <- file.path(PATHS$data_out, "baseline_stats_detail.RDS")
if (!file.exists(baseline_stats_detail_path))
  stop("Detail baseline stats not found: ", baseline_stats_detail_path,
       "\nRun 02_phase1_baseline.R first.")

monitor               <- readRDS(PATHS$processed_monitor)
baseline_stats        <- readRDS(PATHS$baseline_stats)
baseline_stats_detail <- readRDS(baseline_stats_detail_path)

# Build bundle lookup from hierarchy (family -> bundle -> bundle_label)
# complaint_type in monitor/baseline IS the family name after 01_data_prep
hier_dt          <- fread(PATHS$complaint_hierarchy, encoding = "UTF-8")
hier_dt[, family := toupper(trimws(family))]
hier_dt[, bundle := tolower(trimws(bundle))]
bundle_labels_dt <- as.data.table(BUNDLE_LABELS)
family_bundle_lookup <- unique(merge(
  hier_dt[, .(family, bundle)],
  bundle_labels_dt,
  by = "bundle", all.x = TRUE
))
setnames(family_bundle_lookup,
         c("family",        "bundle",     "label"),
         c("complaint_type","bundle_key",  "bundle_label"))

cat(sprintf("  Monitor rows     : %s\n", format(nrow(monitor), big.mark = ",")))
cat(sprintf("  Complaint types  : %d\n", uniqueN(monitor[[CT_COL]])))
cat(sprintf("  Date range       : %s to %s\n",
            format(min(monitor$created_date), "%Y-%m-%d"),
            format(max(monitor$created_date), "%Y-%m-%d")))

years_present <- sort(unique(monitor$year))
cat(sprintf("  Years present    : %s\n", paste(years_present, collapse = ", ")))

unexpected_years <- setdiff(years_present, PHASE2_YEARS)
if (length(unexpected_years) > 0)
  warning("Unexpected years in monitor data: ",
          paste(unexpected_years, collapse = ", "))

# -----------------------------------------------------------------------------
# STEP 2: Aggregate monitor data to monthly counts
# -----------------------------------------------------------------------------
message("\nStep 2: Aggregating monitor data to monthly counts...")

monthly_monitor <- monitor[, .(monthly_count = .N),
                            by = .(complaint_type = family,
                                   year,
                                   month,
                                   year_month)]

setorder(monthly_monitor, complaint_type, year_month)

cat(sprintf("  Year-months in monitor : %d\n", uniqueN(monthly_monitor$year_month)))
cat(sprintf("  Rows in monthly        : %s\n", format(nrow(monthly_monitor), big.mark = ",")))

# Step 2b: Detail aggregation — original complaint type level
# Each original complaint type is indexed against its FAMILY baseline mean.
# This preserves comparability: an original type is measured against the same
# baseline as the family it belongs to.
message("\nStep 2b: Aggregating detail data at original complaint type level...")

monthly_detail <- monitor[, .(monthly_count = .N),
                            by = .(original_type = get(CT_COL),
                                   family,
                                   year,
                                   month,
                                   year_month)]

setorder(monthly_detail, original_type, year_month)

cat(sprintf("  Original types in detail : %d\n", uniqueN(monthly_detail$original_type)))
cat(sprintf("  Rows in detail monthly   : %s\n", format(nrow(monthly_detail), big.mark = ",")))

# -----------------------------------------------------------------------------
# STEP 3: Join baseline means and compute index
# -----------------------------------------------------------------------------
message("\nStep 3: Computing index values...")

tier1_stats <- baseline_stats[tier == 1L,
                               .(complaint_type, month,
                                 baseline_mean, n_months_used,
                                 baseline_interpolated)]

tier2_stats <- baseline_stats[tier == 2L,
                               .(complaint_type,
                                 baseline_mean, n_months_used)]
tier2_stats[, baseline_interpolated := FALSE]

tier1_types <- tier1_stats[, unique(complaint_type)]
tier2_types <- tier2_stats[, unique(complaint_type)]

# Tier 1: join on complaint_type + month
tier1_monitor <- monthly_monitor[complaint_type %in% tier1_types]
tier1_monitor <- merge(tier1_monitor, tier1_stats,
                       by    = c("complaint_type", "month"),
                       all.x = TRUE)

# Tier 2: join on complaint_type only
tier2_monitor <- monthly_monitor[complaint_type %in% tier2_types]
tier2_monitor <- merge(tier2_monitor, tier2_stats,
                       by    = "complaint_type",
                       all.x = TRUE)

# Combine
index_data <- rbindlist(list(tier1_monitor, tier2_monitor),
                        use.names = TRUE, fill = TRUE)

index_data[, index := round(monthly_count / baseline_mean, 4)]
index_data[, tier  := fifelse(complaint_type %in% tier1_types, 1L, 2L)]

# Attach bundle labels from hierarchy lookup
index_data <- merge(index_data, family_bundle_lookup,
                    by = "complaint_type", all.x = TRUE)

setorder(index_data, bundle_key, complaint_type, year_month)

cat(sprintf("  Tier 1 rows : %s\n", format(nrow(tier1_monitor), big.mark = ",")))
cat(sprintf("  Tier 2 rows : %s\n", format(nrow(tier2_monitor), big.mark = ",")))
cat(sprintf("  Total rows  : %s\n", format(nrow(index_data),    big.mark = ",")))

missing_baseline <- index_data[is.na(baseline_mean), .N]
if (missing_baseline > 0) {
  cat(sprintf("\n  ⚠️  DATA CONTINUITY NOTE: %d row(s) have no baseline mean.\n", missing_baseline))
  cat("  These families had insufficient activity during 2021 to compute\n")
  cat("  a reliable baseline (fewer qualifying months than MIN_THRESHOLD).\n")
  cat("  Index values will be NA for these rows — charts will show gaps.\n")
  cat("  Affected families:\n")
  affected <- index_data[is.na(baseline_mean),
                          .(n_rows     = .N,
                            years      = paste(sort(unique(year)), collapse = ", "),
                            n_months   = uniqueN(month)),
                          by = complaint_type]
  print(affected, row.names = FALSE)
}

# Step 3b: Compute detail index — each original type uses its OWN baseline
# from baseline_stats_detail.RDS (computed in 02_ at original type level).
# Tier assignment is per original type, independent of the family tier.
message("\nStep 3b: Computing detail index values (original type baselines)...")

detail_tier1_stats <- baseline_stats_detail[tier == 1L,
                                             .(complaint_type, month,
                                               baseline_mean, n_months_used,
                                               baseline_interpolated)]
detail_tier2_stats <- baseline_stats_detail[tier == 2L,
                                             .(complaint_type,
                                               baseline_mean, n_months_used,
                                               baseline_interpolated)]

detail_tier1_types <- detail_tier1_stats[, unique(complaint_type)]
detail_tier2_types <- detail_tier2_stats[, unique(complaint_type)]

# Tier 1 detail: join on original_type + month
tier1_detail <- monthly_detail[original_type %in% detail_tier1_types]
tier1_detail <- merge(tier1_detail,
                      detail_tier1_stats,
                      by.x = c("original_type", "month"),
                      by.y = c("complaint_type", "month"),
                      all.x = TRUE)

# Tier 2 detail: join on original_type only
tier2_detail <- monthly_detail[original_type %in% detail_tier2_types]
tier2_detail <- merge(tier2_detail,
                      detail_tier2_stats,
                      by.x = "original_type",
                      by.y = "complaint_type",
                      all.x = TRUE)

index_detail <- rbindlist(list(tier1_detail, tier2_detail),
                          use.names = TRUE, fill = TRUE)

index_detail[, index := round(monthly_count / baseline_mean, 4)]
index_detail[, tier  := fifelse(original_type %in% detail_tier1_types, 1L, 2L)]

# Attach bundle labels via family
index_detail <- merge(index_detail,
                      family_bundle_lookup[, .(complaint_type, bundle_key, bundle_label)],
                      by.x = "family",
                      by.y = "complaint_type",
                      all.x = TRUE)

setorder(index_detail, bundle_key, original_type, year_month)

cat(sprintf("  Detail Tier 1 rows : %s\n", format(nrow(tier1_detail), big.mark = ",")))
cat(sprintf("  Detail Tier 2 rows : %s\n", format(nrow(tier2_detail), big.mark = ",")))
cat(sprintf("  Detail total rows  : %s\n", format(nrow(index_detail), big.mark = ",")))

missing_detail <- index_detail[is.na(baseline_mean), .N]
if (missing_detail > 0) {
  cat(sprintf("\n  ⚠️  DATA CONTINUITY NOTE (detail): %d row(s) have no baseline mean.\n", missing_detail))
  cat("  These original complaint types had insufficient activity during 2021.\n")
  cat("  Detail charts for these types will show gaps where index cannot be computed.\n")
  cat("  Affected original types:\n")
  affected_detail <- index_detail[is.na(baseline_mean),
                                   .(family     = unique(family),
                                     n_rows     = .N,
                                     years      = paste(sort(unique(year)), collapse = ", "),
                                     n_months   = uniqueN(month)),
                                   by = original_type]
  print(affected_detail, row.names = FALSE)
}

# -----------------------------------------------------------------------------
# STEP 2c / 3c: 2026 YTD overlay (conditional — skipped if file not present)
# Computes actual monthly index for complete 2026 months, plus a projection
# for the current (partial) month based on daily complaint rate.
#
# Projection formula:
#   daily_rate       = complaints_so_far / days_elapsed_in_month
#   projected_count  = daily_rate * days_in_month
#   projected_index  = projected_count / baseline_mean
#
# Output columns:
#   is_projected  : FALSE for complete months, TRUE for the partial month
#   projected_index : NA for complete months, projected value for partial month
# -----------------------------------------------------------------------------

has_2026 <- file.exists(PATHS$processed_2026)

if (has_2026) {

  message("\nStep 2c: Loading and aggregating 2026 YTD data...")

  ytd_2026     <- readRDS(PATHS$processed_2026)
  max_date_path <- file.path(PATHS$data_out, "max_date_2026.RDS")
  max_date_2026 <- if (file.exists(max_date_path))
                     readRDS(max_date_path)
                   else max(ytd_2026$created_date, na.rm = TRUE)

  cat(sprintf("  2026 rows      : %s\n", format(nrow(ytd_2026), big.mark = ",")))
  cat(sprintf("  Data through   : %s\n", format(max_date_2026, "%Y-%m-%d")))

  # Partial month detection
  last_month      <- month(max_date_2026)
  last_month_year <- year(max_date_2026)
  days_in_month   <- days_in_month(max_date_2026)
  days_elapsed    <- as.integer(format(max_date_2026, "%d"))
  partial_ym      <- as.Date(sprintf("%d-%02d-01", last_month_year, last_month))

  cat(sprintf("  Partial month  : %s (%d of %d days)\n",
              format(partial_ym, "%B %Y"), days_elapsed, days_in_month))

  # --- Family-level 2026 aggregation ---
  monthly_2026 <- ytd_2026[, .(monthly_count = .N),
                             by = .(complaint_type = family,
                                    year  = year(created_date),
                                    month = month(created_date),
                                    year_month = as.Date(sprintf("%d-%02d-01",
                                                  year(created_date),
                                                  month(created_date))))]
  setorder(monthly_2026, complaint_type, year_month)

  # --- Detail-level 2026 aggregation ---
  monthly_2026_detail <- ytd_2026[, .(monthly_count = .N),
                                   by = .(original_type = get(CT_COL),
                                          family,
                                          year  = year(created_date),
                                          month = month(created_date),
                                          year_month = as.Date(sprintf("%d-%02d-01",
                                                        year(created_date),
                                                        month(created_date))))]
  setorder(monthly_2026_detail, original_type, year_month)

  message("\nStep 3c: Computing 2026 index and projections...")

  # Helper: compute index + projection for a monthly table joined to baseline
  compute_2026_index <- function(monthly_dt, bl_tier1, bl_tier2,
                                 type_col, partial_ym, days_elapsed, days_in_month) {

    t1_types <- bl_tier1[, unique(complaint_type)]
    t2_types <- bl_tier2[, unique(complaint_type)]

    # Tier 1
    t1 <- monthly_dt[get(type_col) %in% t1_types]
    t1 <- merge(t1, bl_tier1,
                by.x = c(type_col, "month"),
                by.y = c("complaint_type", "month"),
                all.x = TRUE)

    # Tier 2
    t2 <- monthly_dt[get(type_col) %in% t2_types]
    t2 <- merge(t2, bl_tier2,
                by.x = type_col,
                by.y = "complaint_type",
                all.x = TRUE)

    out <- rbindlist(list(t1, t2), use.names = TRUE, fill = TRUE)
    out[, tier := fifelse(get(type_col) %in% t1_types, 1L, 2L)]
    out[, index := round(monthly_count / baseline_mean, 4)]

    # Mark partial month and compute projection
    out[, is_projected   := (year_month == partial_ym)]
    out[, projected_index := fifelse(
      is_projected & !is.na(baseline_mean),
      round((monthly_count / days_elapsed * days_in_month) / baseline_mean, 4),
      NA_real_
    )]

    out
  }

  # Family-level 2026 index
  index_2026 <- compute_2026_index(
    monthly_dt    = monthly_2026,
    bl_tier1      = tier1_stats,
    bl_tier2      = tier2_stats,
    type_col      = "complaint_type",
    partial_ym    = partial_ym,
    days_elapsed  = days_elapsed,
    days_in_month = days_in_month
  )

  index_2026 <- merge(index_2026, family_bundle_lookup,
                      by = "complaint_type", all.x = TRUE)
  setorder(index_2026, bundle_key, complaint_type, year_month)

  # Detail-level 2026 index
  index_2026_detail <- compute_2026_index(
    monthly_dt    = monthly_2026_detail,
    bl_tier1      = detail_tier1_stats,
    bl_tier2      = detail_tier2_stats,
    type_col      = "original_type",
    partial_ym    = partial_ym,
    days_elapsed  = days_elapsed,
    days_in_month = days_in_month
  )

  index_2026_detail <- merge(
    index_2026_detail,
    family_bundle_lookup[, .(complaint_type, bundle_key, bundle_label)],
    by.x = "family", by.y = "complaint_type", all.x = TRUE
  )
  setorder(index_2026_detail, bundle_key, original_type, year_month)

  cat(sprintf("  2026 family rows  : %s\n", format(nrow(index_2026),        big.mark = ",")))
  cat(sprintf("  2026 detail rows  : %s\n", format(nrow(index_2026_detail), big.mark = ",")))
  cat(sprintf("  Projected month   : %s (%.0f%% of month elapsed)\n",
              format(partial_ym, "%B %Y"),
              100 * days_elapsed / days_in_month))

  rm(ytd_2026); gc()

} else {
  message("\nStep 2c/3c: No 2026 YTD file found — skipping overlay.")
  index_2026        <- NULL
  index_2026_detail <- NULL
}

# -----------------------------------------------------------------------------
# STEP 4: Compute annual average index
# -----------------------------------------------------------------------------
message("\nStep 4: Computing annual average index...")

annual_index <- index_data[, .(
  n_months     = .N,
  total_count  = sum(monthly_count),
  mean_monthly = round(mean(monthly_count), 1),
  mean_index   = round(mean(index, na.rm = TRUE), 4),
  median_index = round(median(index, na.rm = TRUE), 4),
  min_index    = round(min(index, na.rm = TRUE), 4),
  max_index    = round(max(index, na.rm = TRUE), 4)
), by = .(complaint_type, bundle_key, bundle_label, tier, year)]

setorder(annual_index, bundle_key, complaint_type, year)

# -----------------------------------------------------------------------------
# STEP 5: Compute snapshot comparison (baseline vs 2025)
# -----------------------------------------------------------------------------
message("\nStep 5: Computing snapshot comparison...")

# 2025 snapshot
snap_2025 <- index_data[year == 2025, .(
  index_2025 = round(mean(index, na.rm = TRUE), 4)
), by = .(complaint_type, bundle_key, bundle_label, tier)]

snap_2025[, change_from_baseline := round(index_2025 - 1.0, 4)]
snap_2025[, direction := classify_direction(change_from_baseline)]

setorder(snap_2025, bundle_key, complaint_type)

# Step 5b: Detail snapshot — original complaint type level
message("\nStep 5b: Computing detail snapshot comparison...")

snap_2025_detail <- index_detail[year == 2025, .(
  index_2025 = round(mean(index, na.rm = TRUE), 4)
), by = .(original_type, family, bundle_key, bundle_label, tier)]

snap_2025_detail[, change_from_baseline := round(index_2025 - 1.0, 4)]
snap_2025_detail[, direction := classify_direction(change_from_baseline)]

setorder(snap_2025_detail, bundle_key, family, original_type)

# -----------------------------------------------------------------------------
# STEP 5c: Detect and characterize emerging complaint types
#
# Two sources feed the emerging pipeline:
#   Source 1 — True emerging: original types with zero activity in BASELINE_YEARS.
#              Must meet volume/activity criteria (EMERGING_MIN_MONTHS,
#              EMERGING_MIN_MEAN) to be included.
#   Source 2 — Anomalous baseline: types that qualified for Tier 1 in 02_ but
#              whose baseline period is structurally invalid for indexing (extreme
#              seasonal ratio or single-month spike). These are passed through
#              unconditionally since they already have monitor-period volume.
#              Written to anomalous_baseline_types.RDS by 02_.
#
# Both sources receive the same treatment: monthly raw counts, linear trend
# slope, growth index relative to earliest active year.
# -----------------------------------------------------------------------------
message("\nStep 5c: Detecting emerging complaint types...")

EMERGING_MIN_MONTHS <- 12L    # min active months in monitor period (Source 1 only)
EMERGING_MIN_MEAN   <- 100L   # min mean count in at least one calendar year (Source 1 only)

# --- Source 2: manually designated types from complaint_hierarchy.csv (via 02_) ---
# anomalous_baseline_types.RDS now contains complaint_type + baseline_flag columns.
# baseline_flag = "anomalous" : had baseline data but it is structurally invalid
# baseline_flag = "emerging"  : zero baseline-period activity (documented designation)
anomalous_path <- file.path(PATHS$data_out, "anomalous_baseline_types.RDS")
if (file.exists(anomalous_path)) {
  flagged_dt          <- readRDS(anomalous_path)
  anomalous_types_vec <- flagged_dt[baseline_flag == "anomalous", complaint_type]
  emerging_flag_vec   <- flagged_dt[baseline_flag == "emerging",  complaint_type]
} else {
  flagged_dt          <- data.table(complaint_type = character(0),
                                    baseline_flag  = character(0))
  anomalous_types_vec <- character(0)
  emerging_flag_vec   <- character(0)
}
cat(sprintf("  anomalous baseline types (from 02_) : %d\n",
            length(anomalous_types_vec)))
cat(sprintf("  emerging  flag types    (from 02_) : %d\n",
            length(emerging_flag_vec)))

# --- Source 1: true emerging (zero baseline activity, not manually designated) ---
all_orig_types_monitor <- unique(monitor[[CT_COL]])
baseline_active        <- unique(readRDS(PATHS$processed_baseline)[[CT_COL]])

# Exclude anomalous and explicitly-flagged emerging types from auto-detection:
# anomalous types have baseline data (just invalid); emerging_flag types are
# already accounted for via the manual designation.
no_baseline_types <- setdiff(all_orig_types_monitor,
                              union(baseline_active,
                                    union(anomalous_types_vec, emerging_flag_vec)))

# Monthly counts for true-emerging candidates
emerging_monthly_s1 <- monitor[get(CT_COL) %in% no_baseline_types,
                                .(monthly_count = .N),
                                by = .(original_type = get(CT_COL),
                                       family,
                                       bundle,
                                       bundle_label = label,
                                       year,
                                       month,
                                       year_month)]

# Criterion 2: at least EMERGING_MIN_MONTHS active months
active_months_s1 <- emerging_monthly_s1[monthly_count > 0,
                                         .(n_active_months = .N),
                                         by = original_type]

# Criterion 3: at least one year with mean > EMERGING_MIN_MEAN
annual_means_s1  <- emerging_monthly_s1[, .(year_mean = mean(monthly_count)),
                                         by = .(original_type, year)]
has_robust_year  <- annual_means_s1[year_mean > EMERGING_MIN_MEAN,
                                     .(has_robust = TRUE),
                                     by = original_type]

emerging_pass_s1 <- active_months_s1[n_active_months >= EMERGING_MIN_MONTHS,
                                      .(original_type, n_active_months)]
emerging_pass_s1 <- merge(emerging_pass_s1, has_robust_year,
                           by = "original_type", all = FALSE)

cat(sprintf("  Types with no baseline activity       : %d\n",
            length(no_baseline_types)))
cat(sprintf("  True emerging (pass criteria)         : %d\n",
            nrow(emerging_pass_s1)))

# --- Monthly counts for Source 2 (anomalous + manually-flagged emerging types) ---
all_flagged_vec <- union(anomalous_types_vec, emerging_flag_vec)

emerging_monthly_s2 <- monitor[get(CT_COL) %in% all_flagged_vec,
                                .(monthly_count = .N),
                                by = .(original_type = get(CT_COL),
                                       family,
                                       bundle,
                                       bundle_label = label,
                                       year,
                                       month,
                                       year_month)]

# --- Combine both sources ---
all_emerging_types <- union(emerging_pass_s1$original_type, all_flagged_vec)

emerging_monthly <- rbindlist(
  list(
    emerging_monthly_s1[original_type %in% emerging_pass_s1$original_type],
    emerging_monthly_s2
  ),
  use.names = TRUE
)

# Tag source using baseline_flag value directly for transparency
# auto_emerging = passed auto-detection criteria (no baseline activity + volume)
emerging_monthly[, emerging_source := fcase(
  original_type %in% anomalous_types_vec, "anomalous",
  original_type %in% emerging_flag_vec,   "emerging",
  default = "auto_emerging"
)]

cat(sprintf("  Total emerging (both sources)         : %d\n",
            length(all_emerging_types)))

if (length(all_emerging_types) > 0) {

  setorder(emerging_monthly, original_type, year_month)

  # Compute linear trend slope per type
  # x = sequential month number (1, 2, 3...), y = monthly_count
  # slope in units of complaints/month — positive = growing
  emerging_monthly[, month_seq := as.integer(factor(year_month,
                                                      levels = sort(unique(year_month)))),
                   by = original_type]

  slopes <- emerging_monthly[, {
    fit <- lm(monthly_count ~ month_seq)
    .(slope        = round(coef(fit)[2], 2),
      slope_pct_mo = round(coef(fit)[2] / mean(monthly_count) * 100, 2),
      mean_monthly = round(mean(monthly_count), 1),
      total_count  = sum(monthly_count),
      n_months     = .N)
  }, by = original_type]

  # Carry emerging_source through (take first value per type — consistent within type)
  source_tag <- emerging_monthly[, .(emerging_source = emerging_source[1]),
                                  by = original_type]

  index_emerging <- merge(emerging_monthly, slopes,     by = "original_type", all.x = TRUE)
  index_emerging <- merge(index_emerging,   source_tag, by = "original_type", all.x = TRUE)

  # Annual averages for chart annotation
  annual_emerging <- index_emerging[, .(
    mean_monthly = round(mean(monthly_count), 1),
    total_count  = sum(monthly_count)
  ), by = .(original_type, year)]

  # Emerging snapshot: one row per type with 2025 mean and trend slope
  emerging_snapshot <- index_emerging[year == 2025, .(
    mean_2025    = round(mean(monthly_count), 1),
    total_2025   = sum(monthly_count)
  ), by = .(original_type, family, bundle, bundle_label)]

  emerging_snapshot <- merge(emerging_snapshot, slopes,
                              by = "original_type", all.x = TRUE)

  emerging_snapshot <- merge(emerging_snapshot, source_tag,
                              by = "original_type", all.x = TRUE)

  # Earliest-year self-baseline: mean monthly count in first active calendar year.
  # Used to compute a growth index comparable to the standard heatmap index.
  # Index = mean_2025 / earliest_year_mean  (1.0 = same as first year)
  # NOTE: this is an endogenous baseline — drawn from the monitor period itself,
  # not a true pre-period. Caption in 05_ flags this clearly.
  # For anomalous-baseline types: first_year is the first MONITOR year (2022+),
  # since their baseline-period data is invalid.
  first_year_per_type <- annual_emerging[, .(first_year = min(year[mean_monthly > 0])),
                                          by = original_type]
  first_year_means <- merge(annual_emerging, first_year_per_type,
                             by = "original_type")
  first_year_means <- first_year_means[year == first_year,
                                        .(original_type,
                                          first_year,
                                          first_year_mean = round(mean_monthly, 1))]

  emerging_snapshot <- merge(emerging_snapshot, first_year_means,
                              by = "original_type", all.x = TRUE)

  emerging_snapshot[, growth_index := round(mean_2025 / first_year_mean, 4)]
  emerging_snapshot[, growth_change := round(growth_index - 1.0, 4)]

  # Direction label using same classify_direction thresholds as standard index
  emerging_snapshot[, direction := classify_direction(growth_change)]

  # Slope direction label for scorecard coloring
  emerging_snapshot[, slope_direction := fcase(
    slope_pct_mo >= 2.0,                          "Rapidly Growing",
    slope_pct_mo >= 0.5 & slope_pct_mo < 2.0,    "Growing",
    slope_pct_mo > -0.5 & slope_pct_mo < 0.5,    "Stable",
    slope_pct_mo <= -0.5 & slope_pct_mo > -2.0,  "Declining",
    slope_pct_mo <= -2.0,                         "Rapidly Declining",
    default = "Unknown"
  )]

  setorder(emerging_snapshot, bundle, original_type)

  cat(sprintf("\n  Emerging types detected (%d total):\n", length(all_emerging_types)))
  print(emerging_snapshot[, .(original_type, family, bundle, emerging_source,
                               mean_2025, growth_index, slope_pct_mo,
                               slope_direction)],
        row.names = FALSE)

} else {
  index_emerging    <- NULL
  emerging_snapshot <- NULL
  cat("  No emerging types detected.\n")
}
message("\nStep 6: Summary tables...\n")

cat(strrep("=", 80), "\n")
cat("ANNUAL INDEX BY COMPLAINT TYPE\n")
cat("Index = 1.0 is baseline | > 1.0 = more complaints (worse QoL)\n")
cat(strrep("=", 80), "\n\n")

for (bk in unique(annual_index$bundle_key)) {

  bundle_label_text <- annual_index[bundle_key == bk, bundle_label][1]

  cat(strrep("-", 80), "\n")
  cat(sprintf("Bundle: %s\n", bundle_label_text))
  cat(strrep("-", 80), "\n")

  bundle_dt <- annual_index[bundle_key == bk,
                             .(complaint_type, tier, year, mean_index)]

  bundle_wide <- dcast(bundle_dt,
                       complaint_type + tier ~ year,
                       value.var = "mean_index")

  # Add 2025 direction
  bundle_wide <- merge(bundle_wide,
                       snap_2025[, .(complaint_type, direction)],
                       by = "complaint_type", all.x = TRUE)

  bundle_wide[tier == 2L, direction := paste0(direction, " *")]

  print(bundle_wide, row.names = FALSE)
  cat("\n")
}

cat("* Tier 2: limited baseline reliability\n\n")

cat(strrep("=", 80), "\n")
cat("2025 SNAPSHOT vs BASELINE\n")
cat(strrep("=", 80), "\n\n")

print(snap_2025[, .(
  Bundle    = bundle_label,
  Complaint = complaint_type,
  Tier      = tier,
  Index_2025 = index_2025,
  Change    = change_from_baseline,
  Direction = direction
)][order(Change)], row.names = FALSE)

cat("\nDirection summary (7-level):\n")
print(snap_2025[, .N, by = direction][order(-N)], row.names = FALSE)

# -----------------------------------------------------------------------------
# STEP 7: Save outputs
# -----------------------------------------------------------------------------
message("\nStep 7: Saving outputs...")

saveRDS(index_data, PATHS$analysis_results)
cat(sprintf("  Saved: %s\n", basename(PATHS$analysis_results)))

saveRDS(index_detail, file.path(PATHS$data_out, "index_data_detail.RDS"))
cat(sprintf("  Saved: index_data_detail.RDS\n"))

if (!is.null(index_2026)) {
  saveRDS(index_2026,        file.path(PATHS$data_out, "index_2026.RDS"))
  saveRDS(index_2026_detail, file.path(PATHS$data_out, "index_2026_detail.RDS"))
  cat(sprintf("  Saved: index_2026.RDS\n"))
  cat(sprintf("  Saved: index_2026_detail.RDS\n"))
}

fwrite(index_data[, .(bundle_key, bundle_label, complaint_type, tier,
                      year, month, year_month, monthly_count,
                      baseline_mean, index)],
       file.path(PATHS$tables, "index_monthly.csv"))
cat(sprintf("  Saved: index_monthly.csv\n"))

fwrite(annual_index, file.path(PATHS$tables, "index_annual.csv"))
cat(sprintf("  Saved: index_annual.csv\n"))

fwrite(snap_2025, file.path(PATHS$tables, "index_snapshot.csv"))
cat(sprintf("  Saved: index_snapshot.csv\n"))

fwrite(snap_2025_detail, file.path(PATHS$tables, "index_snapshot_detail.csv"))
cat(sprintf("  Saved: index_snapshot_detail.csv\n"))

if (!is.null(index_emerging)) {
  saveRDS(index_emerging, file.path(PATHS$data_out, "index_emerging.RDS"))
  fwrite(emerging_snapshot, file.path(PATHS$tables, "emerging_snapshot.csv"))
  cat(sprintf("  Saved: index_emerging.RDS  (%d types)\n",
              uniqueN(index_emerging$original_type)))
  cat(sprintf("  Saved: emerging_snapshot.csv\n"))
}

rb <- readRDS(PATHS$analysis_results)
cat(sprintf("  Verified: %s rows x %d cols | %.1f MB\n",
            format(nrow(rb), big.mark = ","),
            ncol(rb),
            file.size(PATHS$analysis_results) / 1024^2))
rm(rb)

# -----------------------------------------------------------------------------
# WRAP UP
# -----------------------------------------------------------------------------
elapsed <- proc.time() - prog_start

message("\n", strrep("=", 80))
message("ANALYSIS COMPLETE")
message(strrep("=", 80))
message(sprintf("Elapsed time : %.1f seconds", elapsed["elapsed"]))
message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
