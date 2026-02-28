################################################################################
# STEP 3: GENERATE SPC P-CHARTS
# 
# Creates statistical process control P-charts for each quality metric
# Saves charts to PDF and compiles results
################################################################################

message(strrep("=", 80))
message("STEP 3: GENERATING SPC P-CHARTS")
message(strrep("=", 80))

# Verify prerequisites from Step 2
if (!exists("all_metrics")) {
  stop("all_metrics not found. Run 02_calculate_metrics.R first.")
}

dirs <- get_directory_paths()

# -----------------------------------------------------------------------------
# Prepare Output Directory
# -----------------------------------------------------------------------------

chart_output_dir <- dirs$spc_charts

cat(sprintf("\nChart output directory: %s\n", chart_output_dir))

# -----------------------------------------------------------------------------
# Generate All P-Charts
# -----------------------------------------------------------------------------

# Call the function from R/spc_analysis.R
chart_results <- generate_all_p_charts(
  metrics_dt = all_metrics,
  output_dir = chart_output_dir,
  display_plot = TRUE  # Show in RStudio plot pane
)

# -----------------------------------------------------------------------------
# Summary Statistics
# -----------------------------------------------------------------------------

metrics_attempted <- uniqueN(all_metrics$metric_name)
metrics_processed <- length(chart_results)
metrics_skipped <- metrics_attempted - metrics_processed

cat("\n", strrep("=", 80), "\n")
cat("P-CHART GENERATION SUMMARY\n")
cat(strrep("=", 80), "\n")
cat(sprintf("  Metrics in dataset:      %d\n", metrics_attempted))
cat(sprintf("  Charts generated:        %d\n", metrics_processed))
cat(sprintf("  Skipped (insufficient):  %d\n", metrics_skipped))
cat(sprintf("  Success rate:            %.1f%%\n", 
            100 * metrics_processed / metrics_attempted))
cat(sprintf("  Output directory:        %s\n", chart_output_dir))
cat(strrep("=", 80), "\n")

# -----------------------------------------------------------------------------
# Identify High-Violation Metrics
# -----------------------------------------------------------------------------

if (metrics_processed > 0) {
  
  cat("\n=== HIGH-VIOLATION METRICS ===\n")
  
  # Extract violation info
  violation_summary <- rbindlist(lapply(names(chart_results), function(m) {
    data.table(
      metric = m,
      n_violations = chart_results[[m]]$summary$n_violations,
      n_periods = chart_results[[m]]$summary$n_periods,
      pct_violations = round(
        100 * chart_results[[m]]$summary$n_violations / 
          chart_results[[m]]$summary$n_periods, 1
      ),
      overall_rate = chart_results[[m]]$summary$overall_rate
    )
  }))
  
  setorder(violation_summary, -pct_violations)
  
  # Show top violators
  top_violators <- violation_summary[pct_violations > 0]
  
  if (nrow(top_violators) > 0) {
    cat(sprintf("\nMetrics with control violations (%d):\n", nrow(top_violators)))
    print(top_violators[1:min(10, nrow(top_violators))], nrows = Inf)
  } else {
    cat("\n✓ No control violations detected in any metric\n")
  }
}

# -----------------------------------------------------------------------------
# Save Results Object
# -----------------------------------------------------------------------------

# Save the full results for use in summary report
results_file <- file.path(dirs$analytics, "pchart_results.rds")
saveRDS(chart_results, results_file)

cat(sprintf("\n✓ Chart results saved: %s\n", results_file))

message("\n✓ Step 3 complete: P-charts generated")
message(strrep("=", 80), "\n")

# Objects created for downstream use:
# - chart_results: Named list of P-chart results and summaries
