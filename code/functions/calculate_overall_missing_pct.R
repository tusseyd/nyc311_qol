################################################################################
# SPECIAL METRIC CALCULATIONS
# File: calculate_quality_metrics_special.R
################################################################################

# Special: Overall missing data percentage
calculate_overall_missing_pct <- function(dt, period_type) {
  
  # Exclude key fields from missing data check
  cols_to_check <- setdiff(names(dt), c("unique_key", "created_date"))
  
  dt_work <- copy(dt)
  dt_work[, year := year(created_date)]
  
  # Calculate overall missing data
  total_cells <- nrow(dt_work) * length(cols_to_check)
  total_missing <- sum(sapply(dt_work[, ..cols_to_check], function(x) sum(is.na(x))))
  
  cat(sprintf("\nTOTAL DATASET SUMMARY:\n"))
  cat(sprintf("  Total cells: %s\n", format(total_cells, big.mark = ",")))
  cat(sprintf("  Missing cells: %s (%.2f%%)\n", 
              format(total_missing, big.mark = ","),
              100 * total_missing / total_cells))
  cat(sprintf("  Fields checked: %d\n\n", length(cols_to_check)))
  
  # Yearly breakdown
  yearly_summary <- dt_work[, {
    year_cells <- .N * length(cols_to_check)
    year_missing <- sum(sapply(.SD, function(x) sum(is.na(x))))
    .(
      missing = year_missing,
      total_cells = year_cells,
      pct = 100 * year_missing / year_cells
    )
  }, by = year, .SDcols = cols_to_check][order(year)]
  
  cat("YEARLY BREAKDOWN:\n")
  print(yearly_summary, nrows = Inf)
  cat("\n")
  
  if (period_type == "month") {
    dt_work[, period := floor_date(created_date, "month")]
  } else if (period_type == "day") {
    dt_work[, period := as.Date(created_date)]
  } else if (period_type == "week") {
    dt_work[, period := floor_date(created_date, "week")]
  }
  
  result <- dt_work[, {
    total_cells <- .N * length(cols_to_check)
    missing_cells <- sum(sapply(.SD, function(x) sum(is.na(x))))
    .(
      events = missing_cells,
      sample_size = total_cells,
      event_rate = missing_cells / total_cells
    )
  }, by = period, .SDcols = cols_to_check][order(period)]
  
  result[, period_num := seq_len(.N)]
  result[, metric_name := "Overall Missing Data Percentage"]
  
  cat(sprintf("PERIOD SUMMARY: %d periods calculated\n", nrow(result)))
  
  return(result)
}

# Special: Mean response time for top 100 complaint types
calculate_top100_mean <- function(dt, period_type) {
  
  cat("\n", strrep("=", 80), "\n")
  cat("CALCULATING TOP 100 COMPLAINT TYPES (by volume)\n")
  cat(strrep("=", 80), "\n\n")
  
  # Identify top 100 complaint types across ENTIRE period
  top100_summary <- dt[!is.na(duration_days), 
                       .N, by = problem_formerly_complaint_type][order(-N)][1:min(100, .N)]
  
  # Add percentage and cumulative percentage
  top100_summary[, `:=`(
    pct = 100 * N / sum(N),
    cum_pct = 100 * cumsum(N) / sum(N)
  )]
  
  cat(sprintf("Total complaints with duration data: %s\n", 
              format(sum(top100_summary$N), big.mark = ",")))
  cat(sprintf("Top 100 complaint types identified: %d\n\n", nrow(top100_summary)))
  
  # Print the top 100 list
  cat("TOP 100 COMPLAINT TYPES:\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("%-3s %-45s %12s %8s %10s\n", 
              "Rank", "Complaint Type", "Count", "Pct %", "Cum Pct %"))
  cat(strrep("-", 80), "\n")
  
  for (i in 1:nrow(top100_summary)) {
    cat(sprintf("%-3d %-45s %12s %7.2f%% %9.2f%%\n",
                i,
                substr(top100_summary$problem_formerly_complaint_type[i], 1, 45),
                format(top100_summary$N[i], big.mark = ","),
                top100_summary$pct[i],
                top100_summary$cum_pct[i]))
  }
  cat(strrep("-", 80), "\n\n")
  
  # Extract just the complaint type names for filtering
  top100_types <- top100_summary$problem_formerly_complaint_type
  
  # Filter to top 100
  dt_top100 <- dt[problem_formerly_complaint_type %in% top100_types & !is.na(duration_days)]
  
  # Calculate overall mean
  overall_mean <- mean(dt_top100$duration_days, na.rm = TRUE)
  
  cat(sprintf("OVERALL MEAN RESPONSE TIME (Top 100): %.2f days\n\n", overall_mean))
  
  # Create period column
  if (period_type == "month") {
    dt_top100[, period := floor_date(created_date, "month")]
  } else if (period_type == "day") {
    dt_top100[, period := as.Date(created_date)]
  } else if (period_type == "week") {
    dt_top100[, period := floor_date(created_date, "week")]
  }
  
  # Calculate mean by period
  result <- dt_top100[, .(
    mean_duration = mean(duration_days, na.rm = TRUE),
    sample_size = .N
  ), by = period][order(period)]
  
  result[, `:=`(
    events = as.integer(mean_duration * sample_size),
    event_rate = mean_duration,
    period_num = seq_len(.N),
    metric_name = "Mean Response Time - Top 100"
  )]
  
  # Print monthly mean response times
  cat("MEAN RESPONSE TIME BY MONTH:\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("%-15s %15s %12s\n", "Period", "Mean Days", "N Records"))
  cat(strrep("-", 80), "\n")
  
  for (i in 1:nrow(result)) {
    cat(sprintf("%-15s %15.2f %12s\n",
                format(result$period[i], "%Y-%m"),
                result$mean_duration[i],
                format(result$sample_size[i], big.mark = ",")))
  }
  cat(strrep("-", 80), "\n")
  cat(sprintf("%-15s %15.2f %12s\n",
              "OVERALL",
              overall_mean,
              format(sum(result$sample_size), big.mark = ",")))
  cat(strrep("=", 80), "\n\n")
  
  return(result)
}

# Special: Median response time for top 100 complaint types
calculate_top100_median <- function(dt, period_type) {
  
  cat("\n", strrep("=", 80), "\n")
  cat("CALCULATING MEDIAN RESPONSE TIME (using same Top 100)\n")
  cat(strrep("=", 80), "\n\n")
  
  # Use same top 100 as mean
  top100_types <- dt[!is.na(duration_days), 
                     .N, by = problem_formerly_complaint_type][order(-N)][1:min(100, .N)]$problem_formerly_complaint_type
  
  cat(sprintf("Using same %d top complaint types\n\n", length(top100_types)))
  
  # Filter to top 100
  dt_top100 <- dt[problem_formerly_complaint_type %in% top100_types & !is.na(duration_days)]
  
  # Calculate overall median
  overall_median <- median(dt_top100$duration_days, na.rm = TRUE)
  
  cat(sprintf("OVERALL MEDIAN RESPONSE TIME (Top 100): %.2f days\n\n", overall_median))
  
  # Create period column
  if (period_type == "month") {
    dt_top100[, period := floor_date(created_date, "month")]
  } else if (period_type == "day") {
    dt_top100[, period := as.Date(created_date)]
  } else if (period_type == "week") {
    dt_top100[, period := floor_date(created_date, "week")]
  }
  
  # Calculate median by period
  result <- dt_top100[, .(
    median_duration = median(duration_days, na.rm = TRUE),
    sample_size = .N
  ), by = period][order(period)]
  
  result[, `:=`(
    events = as.integer(median_duration * sample_size),
    event_rate = median_duration,
    period_num = seq_len(.N),
    metric_name = "Median Response Time - Top 100"
  )]
  
  # Print monthly median response times
  cat("MEDIAN RESPONSE TIME BY MONTH:\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("%-15s %15s %12s\n", "Period", "Median Days", "N Records"))
  cat(strrep("-", 80), "\n")
  
  for (i in 1:nrow(result)) {
    cat(sprintf("%-15s %15.2f %12s\n",
                format(result$period[i], "%Y-%m"),
                result$median_duration[i],
                format(result$sample_size[i], big.mark = ",")))
  }
  cat(strrep("-", 80), "\n")
  cat(sprintf("%-15s %15.2f %12s\n",
              "OVERALL",
              overall_median,
              format(sum(result$sample_size), big.mark = ",")))
  cat(strrep("=", 80), "\n\n")
  
  return(result)
}