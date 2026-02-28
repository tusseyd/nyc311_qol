################################################################################
# CREATE P-CHARTS WITH RECALCULATED CONTROL LIMITS
# File: create_phased_pchart.R
################################################################################

create_single_pchart_with_phases <- function(
    dt,
    recalc_every = 16,
    chart_title = "P-Chart with Recalculated Control Limits",
    save_path = NULL,
    use_standard_sample = TRUE
) {
  
  # Validate data
  required_cols <- c("period", "period_num", "metric_name", "events", 
                     "sample_size", "event_rate")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Choose which sample size to use
  if (use_standard_sample && "standard_sample_size" %in% names(dt)) {
    dt[, sample_size_for_chart := standard_sample_size]
    dt[, events_for_chart := standard_events]
    sample_type <- "Standardized"
  } else {
    dt[, sample_size_for_chart := sample_size]
    dt[, events_for_chart := events]
    sample_type <- "Actual"
  }
  
  # Add phase indicator
  dt[, phase := ceiling(period_num / recalc_every)]
  
  # Calculate control limits for each phase using qcc
  phase_limits <- dt[, {
    qcc_obj <- qcc(
      data = events_for_chart,
      sizes = sample_size_for_chart,
      type = "p",
      plot = FALSE
    )
    
    .(
      center = qcc_obj$center,
      ucl = qcc_obj$limits[2],
      lcl = qcc_obj$limits[1],
      n_periods = .N,
      avg_sample_size = mean(sample_size_for_chart)
    )
  }, by = phase]
  
  # Merge limits back to main data
  dt <- phase_limits[dt, on = "phase"]
  
  # Identify violations
  dt[, violation := event_rate > ucl | event_rate < lcl]
  dt[, violation_type := fcase(
    event_rate > ucl, "Above UCL",
    event_rate < lcl, "Below LCL",
    default = "In Control"
  )]
  
  # Create the plot
  p <- ggplot(dt, aes(x = period_num)) +
    
    # Control limits (step function shows phase changes)
    geom_step(aes(y = ucl, color = "UCL"), 
              linewidth = 0.8, linetype = "dashed", direction = "hv") +
    geom_step(aes(y = lcl, color = "LCL"), 
              linewidth = 0.8, linetype = "dashed", direction = "hv") +
    geom_step(aes(y = center, color = "Center"), 
              linewidth = 0.8, linetype = "solid", direction = "hv") +
    
    # Data points and line
    geom_line(aes(y = event_rate), color = "black", linewidth = 0.5, alpha = 0.7) +
    geom_point(aes(y = event_rate, fill = violation_type, shape = violation_type), 
               size = 3, color = "black", stroke = 0.5) +
    
    # Phase dividers
    geom_vline(xintercept = seq(recalc_every, max(dt$period_num), by = recalc_every),
               linetype = "dotted", color = "gray50", alpha = 0.5, linewidth = 0.3) +
    
    # Scales and labels
    scale_color_manual(
      name = "Control Limits",
      values = c("UCL" = "red", "LCL" = "red", "Center" = "blue"),
      guide = guide_legend(order = 1)
    ) +
    scale_fill_manual(
      name = "Status",
      values = c("Above UCL" = "red", "Below LCL" = "orange", "In Control" = "forestgreen"),
      guide = guide_legend(order = 2)
    ) +
    scale_shape_manual(
      name = "Status",
      values = c("Above UCL" = 24, "Below LCL" = 25, "In Control" = 21),
      guide = guide_legend(order = 2)
    ) +
    
    scale_x_continuous(breaks = seq(0, max(dt$period_num), by = 4)) +
    
    labs(
      title = chart_title,
      subtitle = sprintf("Control limits recalculated every %d periods | Sample type: %s", 
                         recalc_every, sample_type),
      x = "Period Number (Month)",
      y = "Proportion Defective (p)"
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, hjust = 0, color = "gray30"),
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      axis.text = element_text(size = 9)
    )
  
  print(p)
  Sys.sleep(3)
  
  # Print summary statistics
  cat("\n", strrep("=", 80), "\n")
  cat("P-CHART SUMMARY:", chart_title, "\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("Total periods: %d\n", nrow(dt)))
  cat(sprintf("Number of phases: %d\n", max(dt$phase)))
  cat(sprintf("Recalculation interval: %d periods\n", recalc_every))
  cat(sprintf("Sample type: %s\n", sample_type))
  
  cat("\n--- Violations Summary ---\n")
  violation_summary <- dt[, .N, by = violation_type][order(-N)]
  print(violation_summary)
  
  cat("\n--- Phase Statistics ---\n")
  print(phase_limits)
  
  cat("\n--- Event Rate Statistics ---\n")
  cat(sprintf("Overall Mean:   %.6f\n", mean(dt$event_rate, na.rm = TRUE)))
  cat(sprintf("Overall Median: %.6f\n", median(dt$event_rate, na.rm = TRUE)))
  cat(sprintf("Min:            %.6f (Period %d)\n", 
              min(dt$event_rate, na.rm = TRUE),
              dt[which.min(event_rate), period_num]))
  cat(sprintf("Max:            %.6f (Period %d)\n", 
              max(dt$event_rate, na.rm = TRUE),
              dt[which.max(event_rate), period_num]))
  
  # Save if requested
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 13, height = 8.5, dpi = 300, device = cairo_pdf)
    cat(sprintf("\n✅ Chart saved to: %s\n", save_path))
  }
  
  return(list(
    plot = p,
    data = dt,
    phase_limits = phase_limits,
    violations = dt[violation == TRUE],
    summary = list(
      total_periods = nrow(dt),
      n_phases = max(dt$phase),
      n_violations = nrow(dt[violation == TRUE]),
      pct_violations = 100 * nrow(dt[violation == TRUE]) / nrow(dt)
    )
  ))
}