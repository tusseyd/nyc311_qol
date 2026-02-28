################################################################################
# STEP 2: CALCULATE QUALITY METRICS
# 
# Defines quality metrics and calculates them by period (month/week/day)
# Aggregates event counts and sample sizes for SPC analysis
################################################################################

message(strrep("=", 80))
message("STEP 2: CALCULATING QUALITY METRICS")
message(strrep("=", 80))

# Verify prerequisites from Step 1
if (!exists("d311_qa")) {
  stop("d311_qa not found. Run 01_load_data.R first.")
}

if (!exists("USPSzipcodes")) {
  stop("USPSzipcodes not found. Run 01_load_data.R first.")
}

dirs <- get_directory_paths()

# -----------------------------------------------------------------------------
# Define All Quality Metrics
# -----------------------------------------------------------------------------

message("\nDefining quality metrics...")

# Source metric definition function (from your existing code)
source(file.path(dirs$functions, "define_all_metrics.R"))

# Define all metrics
all_quality_metrics <- define_all_metrics(USPSzipcodes)

cat(sprintf("\n✓ Defined %d quality metrics\n", length(all_quality_metrics)))

# Save metric definitions for reference
metric_def_file <- file.path(dirs$analytics, "quality_metrics_definitions.rds")
saveRDS(all_quality_metrics, metric_def_file)

cat(sprintf("✓ Metric definitions saved: %s\n", metric_def_file))

# Print metric names
cat("\nMetrics defined:\n")
for (i in seq_along(all_quality_metrics)) {
  cat(sprintf("  %2d. %s\n", i, names(all_quality_metrics)[i]))
}

# -----------------------------------------------------------------------------
# Calculate Metrics by Period
# -----------------------------------------------------------------------------

message(sprintf("\nCalculating metrics by %s...", SPC_PERIOD))

# Source calculation function (from your existing code)
# This function should aggregate metrics by period
all_metrics <- calculate_all_quality_metrics(
  dt = d311_qa,
  metrics_list = all_quality_metrics,
  analytics_dir = dirs$analytics
)

# -----------------------------------------------------------------------------
# Validate Results
# -----------------------------------------------------------------------------

cat("\n=== CALCULATION RESULTS ===\n")
cat(sprintf("Total periods: %d\n", uniqueN(all_metrics$period)))
cat(sprintf("Total metrics: %d\n", uniqueN(all_metrics$metric_name)))
cat(sprintf("Total metric-period combinations: %s\n", 
            format(nrow(all_metrics), big.mark = ",")))

# Period coverage
if ("period" %in% names(all_metrics)) {
  min_period <- min(all_metrics$period)
  max_period <- max(all_metrics$period)
  cat(sprintf("Period range: %s to %s\n", 
              format(min_period, "%Y-%m-%d"),
              format(max_period, "%Y-%m-%d")))
}

# Sample size summary
if ("sample_size" %in% names(all_metrics)) {
  cat(sprintf("\nSample sizes per period:\n"))
  cat(sprintf("  Min:    %s\n", format(min(all_metrics$sample_size), big.mark = ",")))
  cat(sprintf("  Median: %s\n", format(median(all_metrics$sample_size), big.mark = ",")))
  cat(sprintf("  Mean:   %s\n", format(round(mean(all_metrics$sample_size)), big.mark = ",")))
  cat(sprintf("  Max:    %s\n", format(max(all_metrics$sample_size), big.mark = ",")))
}

# Event rate summary
if ("events" %in% names(all_metrics) && "sample_size" %in% names(all_metrics)) {
  all_metrics[, event_rate := events / sample_size]
  
  cat(sprintf("\nEvent rates (proportion defective):\n"))
  cat(sprintf("  Min:    %.6f\n", min(all_metrics$event_rate, na.rm = TRUE)))
  cat(sprintf("  Median: %.6f\n", median(all_metrics$event_rate, na.rm = TRUE)))
  cat(sprintf("  Mean:   %.6f\n", mean(all_metrics$event_rate, na.rm = TRUE)))
  cat(sprintf("  Max:    %.6f\n", max(all_metrics$event_rate, na.rm = TRUE)))
}

# -----------------------------------------------------------------------------
# Save Calculated Metrics
# -----------------------------------------------------------------------------

# Save to CSV for easy inspection
metrics_csv_file <- file.path(dirs$analytics, "quality_metrics_by_period.csv")
fwrite(all_metrics, metrics_csv_file)
cat(sprintf("\n✓ Metrics saved (CSV): %s\n", metrics_csv_file))

# Also save as RDS for faster loading
metrics_rds_file <- file.path(dirs$quality_metrics, "quality_metrics_by_period.rds")
saveRDS(all_metrics, metrics_rds_file)
cat(sprintf("✓ Metrics saved (RDS): %s\n", metrics_rds_file))

# -----------------------------------------------------------------------------
# Check for Metrics with Insufficient Data
# -----------------------------------------------------------------------------

cat("\n=== DATA SUFFICIENCY CHECK ===\n")

# Count periods per metric
periods_per_metric <- all_metrics[, .(n_periods = .N), by = metric_name]
setorder(periods_per_metric, n_periods)

min_periods_required <- if (exists("MIN_PERIODS_FOR_CHART")) {
  MIN_PERIODS_FOR_CHART
} else {
  20
}

insufficient <- periods_per_metric[n_periods < min_periods_required]

if (nrow(insufficient) > 0) {
  cat(sprintf("⚠️  %d metrics have fewer than %d periods:\n",
              nrow(insufficient), min_periods_required))
  print(insufficient, nrows = 10)
  cat("\nThese metrics will be skipped in chart generation.\n")
} else {
  cat(sprintf("✓ All metrics have sufficient data (>= %d periods)\n",
              min_periods_required))
}

message("\n✓ Step 2 complete: Quality metrics calculated")
message(strrep("=", 80), "\n")

# Objects created for downstream use:
# - all_quality_metrics: Metric definitions (list)
# - all_metrics: Calculated metrics by period (data.table)
