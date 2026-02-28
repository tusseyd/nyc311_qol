################################################################################
# REPORTING FUNCTIONS
# Summary statistics and report generation
################################################################################

#' Generate quality metrics summary report
#' 
#' Creates a comprehensive summary table from P-chart results including
#' violation rates, defect rates, and trends
#' 
#' @param chart_results Named list of P-chart results from generate_all_p_charts()
#' @param metrics_dt data.table with metric_name, period, events, sample_size
#' @return data.table with summary statistics for each metric
generate_summary_report <- function(chart_results, metrics_dt = NULL) {
  
  if (length(chart_results) == 0) {
    warning("No chart results provided - empty summary")
    return(data.table())
  }
  
  # Build summary from chart results
  summary_list <- lapply(names(chart_results), function(metric_name) {
    
    result <- chart_results[[metric_name]]
    
    # Basic statistics from chart result
    summary_row <- data.table(
      metric = metric_name,
      total_periods = result$summary$n_periods,
      total_events = result$summary$total_events,
      total_samples = result$summary$total_samples,
      overall_rate = result$summary$overall_rate,
      center_line = result$summary$center_line,
      ucl = result$summary$ucl,
      lcl = result$summary$lcl,
      n_violations = result$summary$n_violations,
      pct_violations = round(
        100 * result$summary$n_violations / result$summary$n_periods, 2
      )
    )
    
    # Add additional statistics if raw data available
    if (!is.null(metrics_dt)) {
      metric_data <- metrics_dt[metric_name == !!metric_name]
      
      if (nrow(metric_data) > 0) {
        
        # Calculate event rate per period
        metric_data[, event_rate := events / sample_size]
        
        # Add descriptive stats
        summary_row[, `:=`(
          mean_defect_rate = mean(metric_data$event_rate, na.rm = TRUE),
          median_defect_rate = median(metric_data$event_rate, na.rm = TRUE),
          min_defect_rate = min(metric_data$event_rate, na.rm = TRUE),
          max_defect_rate = max(metric_data$event_rate, na.rm = TRUE)
        )]
        
        # Calculate trend (correlation with time)
        if (nrow(metric_data) >= 3) {
          metric_data[, period_num := seq_len(.N)]
          
          trend_cor <- cor(
            metric_data$period_num, 
            metric_data$event_rate, 
            use = "complete.obs"
          )
          
          # Classify trend
          trend_threshold <- if (exists("TREND_CORRELATION_THRESHOLD")) {
            TREND_CORRELATION_THRESHOLD
          } else {
            0.1
          }
          
          trend_label <- if (trend_cor > trend_threshold) {
            "Increasing"
          } else if (trend_cor < -trend_threshold) {
            "Decreasing"
          } else {
            "Stable"
          }
          
          summary_row[, `:=`(
            trend_correlation = round(trend_cor, 3),
            trend = trend_label
          )]
        }
      }
    }
    
    return(summary_row)
  })
  
  # Combine into single data.table
  summary_report <- rbindlist(summary_list, fill = TRUE)
  
  # Sort by violation percentage (descending)
  setorder(summary_report, -pct_violations)
  
  return(summary_report)
}


#' Print summary report highlights
#' 
#' Prints formatted highlights from summary report including high violations
#' and trend information
#' 
#' @param summary_report data.table from generate_summary_report()
print_summary_highlights <- function(summary_report) {
  
  if (nrow(summary_report) == 0) {
    cat("No summary data available.\n")
    return(invisible(NULL))
  }
  
  cat("\n", strrep("=", 80), "\n")
  cat("QUALITY METRICS SUMMARY\n")
  cat(strrep("=", 80), "\n\n")
  
  # Overall statistics
  cat(sprintf("Total metrics analyzed: %d\n", nrow(summary_report)))
  cat(sprintf("Average violation rate: %.1f%%\n", 
              mean(summary_report$pct_violations, na.rm = TRUE)))
  cat(sprintf("Metrics with >10%% violations: %d\n", 
              sum(summary_report$pct_violations > 10, na.rm = TRUE)))
  
  # High violation metrics
  high_viol_threshold <- if (exists("HIGH_VIOLATION_THRESHOLD")) {
    HIGH_VIOLATION_THRESHOLD
  } else {
    10
  }
  
  high_violations <- summary_report[pct_violations > high_viol_threshold]
  
  if (nrow(high_violations) > 0) {
    cat("\n", strrep("-", 80), "\n")
    cat(sprintf("⚠️  METRICS WITH >%d%% VIOLATIONS:\n", high_viol_threshold))
    cat(strrep("-", 80), "\n")
    
    print(high_violations[, .(
      metric, 
      pct_violations, 
      overall_rate,
      trend = if ("trend" %in% names(high_violations)) trend else NA
    )], nrows = Inf)
  }
  
  # Trend analysis (if available)
  if ("trend" %in% names(summary_report)) {
    
    improving <- summary_report[trend == "Decreasing"]
    worsening <- summary_report[trend == "Increasing"]
    
    if (nrow(improving) > 0) {
      cat("\n", strrep("-", 80), "\n")
      cat("✅ IMPROVING METRICS (Decreasing defect rate):\n")
      cat(strrep("-", 80), "\n")
      
      print(improving[, .(
        metric, 
        mean_defect_rate = if ("mean_defect_rate" %in% names(improving)) {
          sprintf("%.4f", mean_defect_rate)
        } else NA,
        trend
      )], nrows = Inf)
    }
    
    if (nrow(worsening) > 0) {
      cat("\n", strrep("-", 80), "\n")
      cat("⚠️  WORSENING METRICS (Increasing defect rate):\n")
      cat(strrep("-", 80), "\n")
      
      print(worsening[, .(
        metric, 
        mean_defect_rate = if ("mean_defect_rate" %in% names(worsening)) {
          sprintf("%.4f", mean_defect_rate)
        } else NA,
        trend
      )], nrows = Inf)
    }
  }
  
  cat("\n", strrep("=", 80), "\n\n")
  
  invisible(NULL)
}


#' Save summary report
#' 
#' Saves summary report to CSV file
#' 
#' @param summary_report data.table from generate_summary_report()
#' @param output_dir Directory to save report
#' @param filename Filename for CSV (default: "quality_metrics_summary.csv")
save_summary_report <- function(summary_report, 
                               output_dir, 
                               filename = "quality_metrics_summary.csv") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  output_path <- file.path(output_dir, filename)
  fwrite(summary_report, output_path)
  
  cat(sprintf("✓ Summary report saved: %s\n", output_path))
  
  invisible(output_path)
}
