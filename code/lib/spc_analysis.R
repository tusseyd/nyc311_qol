################################################################################
# SPC ANALYSIS FUNCTIONS
# P-chart generation and statistical process control
################################################################################

library(qcc)

#' Generate P-chart for a single quality metric
#' 
#' Creates a statistical process control P-chart for proportion defective data.
#' Displays chart in RStudio plot pane and saves to PDF.
#' 
#' @param metric_data data.table with columns: period, events, sample_size
#' @param metric_name Character string for chart title
#' @param output_dir Directory to save PDF chart
#' @param display_plot Logical, whether to display in RStudio pane (default TRUE)
#' @param display_seconds Numeric, seconds to display plot (default 3)
#' @return List with qcc object, file path, and summary statistics, or NULL if insufficient data
#' 
#' @examples
#' metric_data <- data.table(
#'   period = seq.Date(as.Date("2022-01-01"), by = "month", length.out = 24),
#'   events = sample(50:150, 24),
#'   sample_size = sample(5000:8000, 24)
#' )
#' result <- generate_p_chart(metric_data, "Test Metric", tempdir())
generate_p_chart <- function(metric_data, 
                             metric_name, 
                             output_dir, 
                             display_plot = TRUE,
                             display_seconds = 3) {
  
  # Validate inputs
  required_cols <- c("period", "events", "sample_size")
  if (!all(required_cols %in% names(metric_data))) {
    stop(sprintf("metric_data must contain columns: %s", 
                paste(required_cols, collapse = ", ")))
  }
  
  # Check minimum data requirements
  min_periods <- if (exists("MIN_PERIODS_FOR_CHART")) MIN_PERIODS_FOR_CHART else 20
  
  if (nrow(metric_data) < min_periods) {
    warning(sprintf("Insufficient periods for %s (need %d, have %d)", 
                   metric_name, min_periods, nrow(metric_data)))
    return(NULL)
  }
  
  # Create safe filename
  safe_filename <- gsub("[^A-Za-z0-9_-]", "_", metric_name)
  safe_filename <- tolower(safe_filename)
  
  # Calculate summary statistics for annotation
  total_events <- sum(metric_data$events)
  total_samples <- sum(metric_data$sample_size)
  avg_sample_size <- mean(metric_data$sample_size)
  overall_rate <- total_events / total_samples
  
  summary_text <- sprintf(
    "Total Events: %s | Avg Sample Size: %s | Overall Rate: %.4f",
    format(total_events, big.mark = ","),
    format(round(avg_sample_size), big.mark = ","),
    overall_rate
  )
  
  # Generate P-chart object (display in RStudio if requested)
  pchart_obj <- qcc(
    data = metric_data$events,
    sizes = metric_data$sample_size,
    type = "p",
    title = sprintf("P-Chart: %s", metric_name),
    xlab = "Period",
    ylab = "Proportion Defective",
    labels = format(metric_data$period, "%Y-%m"),
    plot = display_plot
  )
  
  # Add summary annotation to plot pane
  if (display_plot) {
    mtext(summary_text, side = 3, line = 0.5, cex = 0.8)
    Sys.sleep(display_seconds)  # Allow time to view
  }
  
  # Save to PDF
  pdf_file <- file.path(output_dir, sprintf("pchart_%s.pdf", safe_filename))
  
  pdf(pdf_file, 
      width = if (exists("CHART_WIDTH")) CHART_WIDTH else 11, 
      height = if (exists("CHART_HEIGHT")) CHART_HEIGHT else 8.5)
  
  qcc(
    data = metric_data$events,
    sizes = metric_data$sample_size,
    type = "p",
    title = sprintf("P-Chart: %s", metric_name),
    xlab = "Period",
    ylab = "Proportion Defective",
    labels = format(metric_data$period, "%Y-%m"),
    plot = TRUE
  )
  
  mtext(summary_text, side = 3, line = 0.5, cex = 0.8)
  dev.off()
  
  # Extract control limits
  ucl <- max(pchart_obj$limits[, "UCL"])
  lcl <- min(pchart_obj$limits[, "LCL"])
  
  # Count violations
  n_violations <- length(pchart_obj$violations$beyond.limits)
  
  # Return comprehensive results
  list(
    qcc_object = pchart_obj,
    pdf_file = pdf_file,
    metric_name = metric_name,
    summary = list(
      total_events = total_events,
      total_samples = total_samples,
      avg_sample_size = avg_sample_size,
      overall_rate = overall_rate,
      center_line = pchart_obj$center,
      ucl = ucl,
      lcl = lcl,
      n_violations = n_violations,
      n_periods = nrow(metric_data)
    )
  )
}


#' Generate P-charts for all metrics
#' 
#' Processes all unique metrics in a dataset and generates P-charts for each.
#' Handles errors gracefully and returns results for successful charts.
#' 
#' @param metrics_dt data.table with columns: metric_name, period, events, sample_size
#' @param output_dir Directory to save charts
#' @param display_plot Logical, whether to display charts in RStudio (default TRUE)
#' @return Named list of results for each successfully processed metric
#' 
#' @examples
#' results <- generate_all_p_charts(all_metrics, "output/charts")
generate_all_p_charts <- function(metrics_dt, 
                                  output_dir, 
                                  display_plot = TRUE) {
  
  # Validate inputs
  required_cols <- c("metric_name", "period", "events", "sample_size")
  if (!all(required_cols %in% names(metrics_dt))) {
    stop(sprintf("metrics_dt must contain columns: %s", 
                paste(required_cols, collapse = ", ")))
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(sprintf("Created output directory: %s\n", output_dir))
  }
  
  unique_metrics <- unique(metrics_dt$metric_name)
  results <- list()
  
  cat("\n", strrep("=", 80), "\n")
  cat(sprintf("GENERATING P-CHARTS FOR %d METRICS\n", length(unique_metrics)))
  cat(strrep("=", 80), "\n")
  
  metrics_processed <- 0
  metrics_skipped <- 0
  
  for (metric in unique_metrics) {
    
    cat("\n", strrep("-", 80), "\n")
    cat(sprintf("Processing: %s\n", metric))
    cat(strrep("-", 80), "\n")
    
    # Extract data for this metric
    metric_data <- metrics_dt[metric_name == metric]
    
    # Attempt to generate chart
    result <- tryCatch({
      generate_p_chart(
        metric_data = metric_data, 
        metric_name = metric, 
        output_dir = output_dir,
        display_plot = display_plot
      )
    }, error = function(e) {
      cat(sprintf("❌ Error: %s\n", e$message))
      NULL
    })
    
    # Process result
    if (!is.null(result)) {
      # Print summary
      cat(sprintf("  Total events:      %s\n", 
                  format(result$summary$total_events, big.mark = ",")))
      cat(sprintf("  Total samples:     %s\n", 
                  format(result$summary$total_samples, big.mark = ",")))
      cat(sprintf("  Overall rate:      %.4f (%.2f%%)\n", 
                  result$summary$overall_rate,
                  result$summary$overall_rate * 100))
      cat(sprintf("  UCL:               %.4f\n", result$summary$ucl))
      cat(sprintf("  LCL:               %.4f\n", result$summary$lcl))
      cat(sprintf("  Out of control:    %d points\n", 
                  result$summary$n_violations))
      cat(sprintf("  ✓ Saved:           %s\n", basename(result$pdf_file)))
      
      results[[metric]] <- result
      metrics_processed <- metrics_processed + 1
      
    } else {
      metrics_skipped <- metrics_skipped + 1
    }
  }
  
  # Summary
  cat("\n", strrep("=", 80), "\n")
  cat("P-CHART GENERATION COMPLETE\n")
  cat(sprintf("  Processed:  %d\n", metrics_processed))
  cat(sprintf("  Skipped:    %d\n", metrics_skipped))
  cat(sprintf("  Total:      %d\n", length(unique_metrics)))
  cat(sprintf("  Output:     %s\n", output_dir))
  cat(strrep("=", 80), "\n")
  
  return(results)
}


#' Extract P-chart data for manual plotting
#' 
#' Extracts period-level data with control limits from qcc object
#' Useful for creating custom ggplot2 visualizations
#' 
#' @param metric_data data.table with period, events, sample_size
#' @param qcc_obj qcc object from qcc() call
#' @return data.table with period, proportion, cl, ucl, lcl, violation flag
extract_pchart_data <- function(metric_data, qcc_obj) {
  
  chart_data <- data.table(
    period = metric_data$period,
    events = metric_data$events,
    sample_size = metric_data$sample_size,
    proportion = qcc_obj$statistics,
    cl = qcc_obj$center,
    ucl = qcc_obj$limits[, "UCL"],
    lcl = qcc_obj$limits[, "LCL"]
  )
  
  # Flag violations
  chart_data[, violation := proportion > ucl | proportion < lcl]
  
  return(chart_data)
}
