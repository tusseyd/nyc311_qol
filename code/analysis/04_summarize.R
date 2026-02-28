################################################################################
# STEP 4: GENERATE SUMMARY REPORT
# 
# Creates comprehensive summary of quality metrics analysis
# Identifies trends, high-violation metrics, and overall patterns
################################################################################

message(strrep("=", 80))
message("STEP 4: GENERATING SUMMARY REPORT")
message(strrep("=", 80))

# Verify prerequisites from Step 3
if (!exists("chart_results")) {
  stop("chart_results not found. Run 03_generate_charts.R first.")
}

if (length(chart_results) == 0) {
  stop("No charts were generated. Cannot create summary report.")
}

dirs <- get_directory_paths()

# -----------------------------------------------------------------------------
# Generate Summary Report
# -----------------------------------------------------------------------------

message("\nCompiling summary statistics...")

# Generate comprehensive summary using function from R/reporting.R
summary_report <- generate_summary_report(
  chart_results = chart_results,
  metrics_dt = if (exists("all_metrics")) all_metrics else NULL
)

cat(sprintf("\n✓ Summary report generated: %d metrics\n", nrow(summary_report)))

# -----------------------------------------------------------------------------
# Print Highlights
# -----------------------------------------------------------------------------

# Use function from R/reporting.R to print formatted highlights
print_summary_highlights(summary_report)

# -----------------------------------------------------------------------------
# Additional Analysis: Distribution of Defect Rates
# -----------------------------------------------------------------------------

if ("overall_rate" %in% names(summary_report)) {
  
  cat("\n=== DEFECT RATE DISTRIBUTION ===\n")
  
  # Categorize defect rates
  summary_report[, rate_category := fcase(
    overall_rate < 0.01, "Very Low (<1%)",
    overall_rate < 0.05, "Low (1-5%)",
    overall_rate < 0.10, "Medium (5-10%)",
    overall_rate < 0.20, "High (10-20%)",
    default = "Very High (>20%)"
  )]
  
  rate_dist <- summary_report[, .N, by = rate_category]
  setorder(rate_dist, rate_category)
  
  cat("\nDistribution of overall defect rates:\n")
  print(rate_dist, nrows = Inf)
}

# -----------------------------------------------------------------------------
# Statistical Summary
# -----------------------------------------------------------------------------

cat("\n=== OVERALL STATISTICS ===\n")

if ("overall_rate" %in% names(summary_report)) {
  cat(sprintf("Mean defect rate:   %.4f (%.2f%%)\n", 
              mean(summary_report$overall_rate),
              100 * mean(summary_report$overall_rate)))
  cat(sprintf("Median defect rate: %.4f (%.2f%%)\n", 
              median(summary_report$overall_rate),
              100 * median(summary_report$overall_rate)))
  cat(sprintf("Min defect rate:    %.4f (%.2f%%)\n", 
              min(summary_report$overall_rate),
              100 * min(summary_report$overall_rate)))
  cat(sprintf("Max defect rate:    %.4f (%.2f%%)\n", 
              max(summary_report$overall_rate),
              100 * max(summary_report$overall_rate)))
}

if ("pct_violations" %in% names(summary_report)) {
  cat(sprintf("\nMean violation rate: %.1f%%\n", 
              mean(summary_report$pct_violations)))
  cat(sprintf("Median violation rate: %.1f%%\n", 
              median(summary_report$pct_violations)))
}

# -----------------------------------------------------------------------------
# Save Summary Report
# -----------------------------------------------------------------------------

# Save to SPC charts directory
summary_file <- save_summary_report(
  summary_report = summary_report,
  output_dir = dirs$spc_charts,
  filename = "quality_metrics_summary.csv"
)

# Also save to analytics directory for easy access
summary_file2 <- save_summary_report(
  summary_report = summary_report,
  output_dir = dirs$analytics,
  filename = "quality_metrics_summary.csv"
)

# Save as RDS for R users
summary_rds <- file.path(dirs$analytics, "quality_metrics_summary.rds")
saveRDS(summary_report, summary_rds)
cat(sprintf("✓ Summary saved (RDS): %s\n", summary_rds))

# -----------------------------------------------------------------------------
# Create Top Metrics Lists
# -----------------------------------------------------------------------------

cat("\n=== TOP METRICS LISTS ===\n")

# Top 5 highest defect rates
if ("overall_rate" %in% names(summary_report)) {
  top_defect <- summary_report[order(-overall_rate)][1:min(5, .N)]
  
  cat("\nTop 5 metrics by defect rate:\n")
  print(top_defect[, .(metric, overall_rate, pct_violations)], nrows = Inf)
}

# Top 5 most violations
if ("n_violations" %in% names(summary_report)) {
  top_violations <- summary_report[order(-n_violations)][1:min(5, .N)]
  
  cat("\nTop 5 metrics by number of violations:\n")
  print(top_violations[, .(metric, n_violations, pct_violations)], nrows = Inf)
}

# Most stable (fewest violations)
if ("pct_violations" %in% names(summary_report)) {
  most_stable <- summary_report[order(pct_violations)][1:min(5, .N)]
  
  cat("\nTop 5 most stable metrics (fewest violations):\n")
  print(most_stable[, .(metric, pct_violations, overall_rate)], nrows = Inf)
}

message("\n✓ Step 4 complete: Summary report generated")
message(strrep("=", 80), "\n")

# Objects created:
# - summary_report: Comprehensive summary statistics (data.table)
