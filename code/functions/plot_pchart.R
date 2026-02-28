################################################################################
# CUSTOM P-CHART FUNCTION
# File: plot_pchart.R
#
# Features:
#   - Phase I frozen baseline (p_bar from baseline_params)
#   - Variable UCL/LCL per period based on each period's n
#   - Optional recalculation after recalc_after periods using preceding window
#   - Vertical dashed line at recalculation point
#   - Western Electric Rule 1: points beyond 3-sigma (red)
#   - Western Electric Rule 2: 8+ consecutive points same side of CL (orange)
#   - Summary statistics in chart margin
#   - Returns violation data invisibly for downstream use
################################################################################

plot_pchart <- function(
    metric_data,          # data.table with columns: period, events, sample_size, event_rate
    p_bar,                # Phase I baseline p̄
    metric_name,          # metric display name for chart title
    recalc_after  = NULL, # number of periods after which to recalculate CL (NULL = no recalc)
    sigma_limit   = 3,    # control limit multiplier (default 3-sigma)
    plot_title    = NULL, # override chart title (NULL = auto)
    save_pdf      = FALSE,# whether to save to PDF
    pdf_file      = NULL, # full path for PDF output
    annotation    = NULL  # subtitle annotation text
) {
  
  n_periods <- nrow(metric_data)
  p         <- metric_data$event_rate
  n_i       <- metric_data$sample_size
  periods   <- metric_data$period
  
  # ============================================================================
  # PHASE 1 SEGMENT: Use frozen Phase I p̄
  # ============================================================================
  
  if (!is.null(recalc_after) && recalc_after < n_periods) {
    seg1_idx <- 1:recalc_after
    seg2_idx <- (recalc_after + 1):n_periods
  } else {
    seg1_idx <- 1:n_periods
    seg2_idx <- integer(0)
  }
  
  # Segment 1 limits (Phase I baseline)
  cl1  <- rep(p_bar, length(seg1_idx))
  ucl1 <- p_bar + sigma_limit * sqrt(p_bar * (1 - p_bar) / n_i[seg1_idx])
  lcl1 <- pmax(0, p_bar - sigma_limit * sqrt(p_bar * (1 - p_bar) / n_i[seg1_idx]))
  
  # Segment 2 limits (recalculated from preceding recalc_after periods)
  cl2 <- ucl2 <- lcl2 <- numeric(0)
  p_bar2 <- NULL
  
  if (length(seg2_idx) > 0) {
    # Compute new p̄ from the recalc_after periods immediately preceding the break
    p_bar2 <- sum(metric_data$events[seg1_idx]) / sum(n_i[seg1_idx])
    cl2    <- rep(p_bar2, length(seg2_idx))
    ucl2   <- p_bar2 + sigma_limit * sqrt(p_bar2 * (1 - p_bar2) / n_i[seg2_idx])
    lcl2   <- pmax(0, p_bar2 - sigma_limit * sqrt(p_bar2 * (1 - p_bar2) / n_i[seg2_idx]))
  }
  
  # Combined vectors for full series
  cl_all  <- c(cl1,  cl2)
  ucl_all <- c(ucl1, ucl2)
  lcl_all <- c(lcl1, lcl2)
  
  # ============================================================================
  # WESTERN ELECTRIC RULES
  # ============================================================================
  
  # Rule 1: Beyond 3-sigma limits
  rule1_idx <- which(p > ucl_all | p < lcl_all)
  
  # Rule 2: 8+ consecutive points on same side of centerline
  side      <- sign(p - cl_all)   # +1 above, -1 below, 0 on CL
  rule2_idx <- integer(0)
  
  run_start <- 1
  for (i in 2:n_periods) {
    if (side[i] == 0 || side[i] != side[run_start]) {
      run_start <- i
    } else {
      run_length <- i - run_start + 1
      if (run_length >= 8) {
        rule2_idx <- union(rule2_idx, run_start:i)
      }
    }
  }
  
  # Points that are rule2 only (not already flagged by rule1)
  rule2_only_idx <- setdiff(rule2_idx, rule1_idx)
  
  # ============================================================================
  # PLOTTING
  # ============================================================================
  
  do_plot <- function() {
    
    y_min <- min(0, min(lcl_all, na.rm = TRUE), min(p, na.rm = TRUE)) * 0.95
    y_max <- max(ucl_all, p, na.rm = TRUE) * 1.10
    
    title_text <- if (!is.null(plot_title)) plot_title else 
      sprintf("P-Chart: %s", metric_name)
    
    # Period labels — show every 6th to avoid crowding
    x_labels <- format(periods, "%Y-%m")
    label_at  <- seq(1, n_periods, by = 6)
    
    # Base plot
    par(mar = c(6, 5, 5, 2))
    plot(
      1:n_periods, p,
      type = "n",
      xlim = c(1, n_periods),
      ylim = c(y_min, y_max),
      xaxt = "n",
      xlab = "Period",
      ylab = "Proportion Defective",
      main = title_text,
      cex.main = 1.0,
      cex.lab  = 0.9
    )
    
    # x-axis labels
    axis(1, at = label_at, labels = x_labels[label_at], las = 2, cex.axis = 0.7)
    
    # Grid
    grid(nx = NA, ny = NULL, lty = "dotted", col = "grey85")
    
    # ── Segment 1 control lines ───────────────────────────────────────────────
    lines(seg1_idx, cl1,  col = "blue",  lwd = 1.5, lty = "solid")
    lines(seg1_idx, ucl1, col = "red",   lwd = 1.2, lty = "dashed")
    lines(seg1_idx, lcl1, col = "red",   lwd = 1.2, lty = "dashed")
    
    # Labels at right edge of segment 1
    last1 <- max(seg1_idx)
    text(last1, cl1[last1],  "  CL",  col = "blue", cex = 0.7, adj = 0)
    text(last1, ucl1[last1], "  UCL", col = "red",  cex = 0.7, adj = 0)
    text(last1, lcl1[last1], "  LCL", col = "red",  cex = 0.7, adj = 0)
    
    # ── Segment 2 control lines (if recalc) ──────────────────────────────────
    if (length(seg2_idx) > 0) {
      
      # Vertical dashed line at break point
      abline(v = recalc_after + 0.5, col = "grey40", lwd = 1.5, lty = "dashed")
      text(recalc_after + 0.5, y_max * 0.98,
           sprintf("Recalc\np\u0305=%.4f", p_bar2),
           col = "grey40", cex = 0.65, adj = 0.5)
      
      lines(seg2_idx, cl2,  col = "darkblue", lwd = 1.5, lty = "solid")
      lines(seg2_idx, ucl2, col = "darkred",  lwd = 1.2, lty = "dashed")
      lines(seg2_idx, lcl2, col = "darkred",  lwd = 1.2, lty = "dashed")
      
      last2 <- max(seg2_idx)
      text(last2, cl2[length(cl2)],  "  CL",  col = "darkblue", cex = 0.7, adj = 0)
      text(last2, ucl2[length(ucl2)],"  UCL", col = "darkred",  cex = 0.7, adj = 0)
      text(last2, lcl2[length(lcl2)],"  LCL", col = "darkred",  cex = 0.7, adj = 0)
    }
    
    # ── Data points ───────────────────────────────────────────────────────────
    # Normal points
    normal_idx <- setdiff(1:n_periods, union(rule1_idx, rule2_idx))
    points(normal_idx,      p[normal_idx],      pch = 16, col = "black",  cex = 0.8)
    
    # Rule 2 only (orange)
    if (length(rule2_only_idx) > 0)
      points(rule2_only_idx, p[rule2_only_idx], pch = 16, col = "orange", cex = 0.9)
    
    # Rule 1 (red, larger)
    if (length(rule1_idx) > 0)
      points(rule1_idx,      p[rule1_idx],      pch = 16, col = "red",    cex = 1.1)
    
    # Connect all points with line
    lines(1:n_periods, p, col = "black", lwd = 0.8)
    
    # ── Summary statistics box ────────────────────────────────────────────────
    stats_text <- sprintf(
      paste0(
        "Groups = %d  |  Phase I p\u0305 = %.4f  |  Phase II Rate = %.4f\n",
        "UCL (mean n) = %.4f  |  LCL (mean n) = %.4f\n",
        "Beyond limits (Rule 1) = %d  |  Run violations (Rule 2) = %d"
      ),
      n_periods,
      p_bar,
      sum(metric_data$events) / sum(n_i),
      mean(ucl_all),
      mean(lcl_all),
      length(rule1_idx),
      length(rule2_idx)
    )
    
    mtext(stats_text, side = 1, line = 4.5, cex = 0.65, adj = 0)
    
    # Annotation subtitle
    if (!is.null(annotation))
      mtext(annotation, side = 3, line = 0.3, cex = 0.75)
    
    # Legend
    legend_labels <- c("In control", "Rule 1: Beyond limits", "Rule 2: Run violation")
    legend_cols   <- c("black", "red", "orange")
    if (length(seg2_idx) > 0) {
      legend_labels <- c(legend_labels, "Recalculated CL")
      legend_cols   <- c(legend_cols,   "darkblue")
    }
    legend("topright",
           legend = legend_labels,
           col    = legend_cols,
           pch    = c(16, 16, 16, if (length(seg2_idx) > 0) NA),
           lty    = c(NA, NA, NA, if (length(seg2_idx) > 0) "solid"),
           cex    = 0.7,
           bty    = "n")
  }
  
  # ── Save to PDF ───────────────────────────────────────────────────────────
  if (save_pdf && !is.null(pdf_file)) {
    pdf(pdf_file, width = 11, height = 8.5)
    do_plot()
    dev.off()
    cat(sprintf("  \u2713 Chart saved: %s\n", pdf_file))
  }
  
  # ── Return violation summary invisibly ───────────────────────────────────
  invisible(list(
    n_periods      = n_periods,
    p_bar_phase1   = p_bar,
    p_bar_phase2   = p_bar2,
    rule1_idx      = rule1_idx,
    rule1_periods  = periods[rule1_idx],
    rule2_idx      = rule2_idx,
    rule2_periods  = periods[rule2_idx],
    n_rule1        = length(rule1_idx),
    n_rule2        = length(rule2_idx),
    pct_violations = 100 * length(rule1_idx) / n_periods
  ))
}
