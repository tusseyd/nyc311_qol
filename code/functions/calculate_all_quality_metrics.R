################################################################################
# CALCULATE ALL QUALITY METRICS
# File: calculate_all_quality_metrics.R
################################################################################

################################################################################
# HELPER FUNCTIONS
################################################################################


# Helper: Calculate condition-based metrics
calculate_condition_metric <- function(dt, metric_def, period_type, 
                                       show_examples = TRUE, n_examples = 10,
                                       analytics_dir = ".") {
  
  # Apply sample filter
  if (!is.null(metric_def$sample_filter)) {
    sample_data <- dt[eval(parse(text = metric_def$sample_filter))]
  } else {
    sample_data <- copy(dt)
  }
  
  # Add year column for yearly breakdown
  sample_data[, year := year(created_date)]
  
  # Identify anomalies
  sample_data[, is_anomaly := eval(parse(text = metric_def$condition))]
  
  # Get total counts
  total_anomalies <- sum(sample_data$is_anomaly, na.rm = TRUE)
  total_records <- nrow(sample_data)
  
  # Yearly breakdown
  yearly_summary <- sample_data[, .(
    anomalies = sum(is_anomaly, na.rm = TRUE),
    records = .N,
    pct = 100 * sum(is_anomaly, na.rm = TRUE) / .N
  ), by = year][order(year)]
  
  # Print summary
  cat(sprintf("\nTOTAL DATASET SUMMARY:\n"))
  cat(sprintf("  Total anomalies found: %s (%.2f%% of %s records)\n\n",
              format(total_anomalies, big.mark = ","),
              100 * total_anomalies / total_records,
              format(total_records, big.mark = ",")))
  
  cat("YEARLY BREAKDOWN:\n")
  print(yearly_summary, nrows = Inf)
  
  # Show random examples if requested and anomalies exist
  if (show_examples && total_anomalies > 0) {
    anomaly_records <- sample_data[is_anomaly == TRUE]
    
    display_cols <- intersect(
      c("created_date", "closed_date", "problem_formerly_complaint_type", "agency"),
      names(anomaly_records)
    )
    
    n_to_show <- min(n_examples, nrow(anomaly_records))
    
    cat(sprintf("\nRANDOM SAMPLE OF ANOMALIES (n=%d):\n", n_to_show))
    sample_indices <- sample(nrow(anomaly_records), n_to_show)
    print(anomaly_records[sample_indices, ..display_cols], nrows = Inf)
  }
  
  cat("\n")
  
  # Create period column
  if (period_type == "month") {
    sample_data[, period := floor_date(created_date, "month")]
  } else if (period_type == "day") {
    sample_data[, period := as.Date(created_date)]
  } else if (period_type == "week") {
    sample_data[, period := floor_date(created_date, "week")]
  }
  
  # Calculate defects by period
  result <- sample_data[, .(
    events = sum(is_anomaly, na.rm = TRUE),
    sample_size = .N
  ), by = period][order(period)]
  
  result[, event_rate := events / sample_size]
  result[, period_num := seq_len(.N)]
  result[, metric_name := metric_def$name]
  
  cat(sprintf("PERIOD SUMMARY: %d periods calculated\n", nrow(result)))
  
  return(result)
}

################################################################################


# Helper: Calculate domain validation metrics
calculate_domain_metric <- function(dt, metric_def, period_type,
                                    show_examples = TRUE, n_examples = 10,
                                    analytics_dir = ".") {
  
  # Apply sample filter
  if (!is.null(metric_def$sample_filter)) {
    sample_data <- dt[eval(parse(text = metric_def$sample_filter))]
  } else {
    sample_data <- copy(dt)
  }
  
  # Add year column
  sample_data[, year := year(created_date)]
  
  # Check if value is in valid domain
  sample_data[, is_invalid := !(get(metric_def$field) %in% metric_def$valid_values)]
  
  # Get total counts
  total_invalid <- sum(sample_data$is_invalid)
  total_records <- nrow(sample_data)
  
  # Yearly breakdown
  yearly_summary <- sample_data[, .(
    invalid = sum(is_invalid),
    records = .N,
    pct = 100 * sum(is_invalid) / .N
  ), by = year][order(year)]
  
  # Print summary
  cat(sprintf("\nTOTAL DATASET SUMMARY:\n"))
  cat(sprintf("  Total invalid values found: %s (%.2f%% of %s records)\n",
              format(total_invalid, big.mark = ","),
              100 * total_invalid / total_records,
              format(total_records, big.mark = ",")))
  cat(sprintf("  Field: %s\n", metric_def$field))
  cat(sprintf("  Valid values: %d defined\n", length(metric_def$valid_values)))
  
  # Count unique invalid values
  if (total_invalid > 0) {
    invalid_records <- sample_data[is_invalid == TRUE]
    n_unique_invalid <- uniqueN(invalid_records[[metric_def$field]], na.rm = TRUE)
    cat(sprintf("  Unique invalid values: %s\n\n", format(n_unique_invalid, big.mark = ",")))
  } else {
    cat("\n")
  }
  
  cat("YEARLY BREAKDOWN:\n")
  print(yearly_summary, nrows = Inf)
  
  # Save invalid records to CSV if any exist
  if (show_examples && total_invalid > 0) {
    invalid_records <- sample_data[is_invalid == TRUE]
    
    output_filename <- paste0(
      "invalid_",
      gsub("[^[:alnum:]_]", "_", tolower(metric_def$name)),
      "_",
      format(Sys.Date(), "%Y%m%d"),
      ".csv"
    )
    
    save_cols <- intersect(
      c("created_date", "closed_date", metric_def$field,
        "problem_formerly_complaint_type", "agency"),
      names(invalid_records)
    )
    
    fwrite(invalid_records[, ..save_cols], file.path(analytics_dir, output_filename))
    
    cat(sprintf("\nINVALID RECORDS SAVED TO: %s\n", file.path(analytics_dir, output_filename)))
    cat(sprintf("  Records saved: %s\n", format(nrow(invalid_records), big.mark = ",")))
    
    display_cols <- intersect(
      c("created_date", metric_def$field, "problem_formerly_complaint_type", "agency"),
      names(invalid_records)
    )
    
    n_to_show <- min(n_examples, nrow(invalid_records))
    
    cat(sprintf("\nRANDOM SAMPLE OF INVALID VALUES (n=%d):\n", n_to_show))
    sample_indices <- sample(nrow(invalid_records), n_to_show)
    print(invalid_records[sample_indices, ..display_cols], nrows = Inf)
    
    cat("\nMOST COMMON INVALID VALUES:\n")
    invalid_value_freq <- invalid_records[, .N,
                            by = c(metric_def$field)][order(-N)][1:min(20, .N)]
    print(invalid_value_freq, nrows = Inf)
  }
  
  cat("\n")
  
  # Create period column
  if (period_type == "month") {
    sample_data[, period := floor_date(created_date, "month")]
  } else if (period_type == "day") {
    sample_data[, period := as.Date(created_date)]
  } else if (period_type == "week") {
    sample_data[, period := floor_date(created_date, "week")]
  }
  
  # Calculate by period
  result <- sample_data[, .(
    events = sum(is_invalid),
    sample_size = .N
  ), by = period][order(period)]
  
  result[, event_rate := events / sample_size]
  result[, period_num := seq_len(.N)]
  result[, metric_name := metric_def$name]
  
  cat(sprintf("PERIOD SUMMARY: %d periods calculated\n", nrow(result)))
  
  return(result)
}

################################################################################


# Helper: Calculate special metrics
calculate_special_metric <- function(dt, metric_def, period_type) {
  
  metric_name <- metric_def$name
  
  if (metric_name == "Overall Missing Data Percentage") {
    return(calculate_overall_missing_pct(dt, period_type))
  } else if (metric_name == "Mean Response Time - Top 100") {
    return(calculate_top100_mean(dt, period_type))
  } else if (metric_name == "Median Response Time - Top 100") {
    return(calculate_top100_median(dt, period_type))
  } else {
    stop(sprintf("Unknown special metric: %s", metric_name))
  }
}

################################################################################
# SPECIAL METRIC CALCULATIONS
################################################################################


# Special: Overall missing data percentage
calculate_overall_missing_pct <- function(dt, period_type) {
  
  # Exclude key fields from missing data check
  cols_to_check <- setdiff(names(dt), c("created_date"))
  
  dt_work <- copy(dt)
  dt_work[, year := year(created_date)]
  
  # Calculate overall missing data
  total_cells   <- nrow(dt_work) * length(cols_to_check)
  total_missing <- sum(sapply(dt_work[, ..cols_to_check], function(x) sum(is.na(x))))
  
  cat(sprintf("\nTOTAL DATASET SUMMARY:\n"))
  cat(sprintf("  Total cells: %s\n",   format(total_cells,   big.mark = ",")))
  cat(sprintf("  Missing cells: %s (%.2f%%)\n",
              format(total_missing, big.mark = ","),
              100 * total_missing / total_cells))
  cat(sprintf("  Fields checked: %d\n", length(cols_to_check)))
  cat(sprintf("  Rows checked: %d\n\n", nrow(dt_work)))
  
  # Yearly breakdown
  yearly_summary <- dt_work[, {
    year_cells   <- .N * length(cols_to_check)
    year_missing <- sum(sapply(.SD, function(x) sum(is.na(x))))
    .(
      missing     = year_missing,
      total_cells = year_cells,
      pct         = 100 * year_missing / year_cells
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
    total_cells  <- .N * length(cols_to_check)
    missing_cells <- sum(sapply(.SD, function(x) sum(is.na(x))))
    .(
      events      = missing_cells,
      sample_size = total_cells,
      event_rate  = missing_cells / total_cells
    )
  }, by = period, .SDcols = cols_to_check][order(period)]
  
  result[, period_num  := seq_len(.N)]
  result[, metric_name := "Overall Missing Data Percentage"]
  
  cat(sprintf("PERIOD SUMMARY: %d periods calculated\n", nrow(result)))
  
  return(result)
}

################################################################################


# Special: Mean response time for top 100 complaint types
# NOTE: Uses full population per period — no sampling
calculate_top100_mean <- function(dt, period_type) {
  
  cat("\n", strrep("=", 80), "\n")
  cat("CALCULATING MEAN RESPONSE TIME - TOP 100 COMPLAINT TYPES\n")
  cat(strrep("=", 80), "\n\n")
  
  # Identify top 100 complaint types by volume across entire period
  top100_types <- dt[!is.na(duration_days),
                     .N, by = problem_formerly_complaint_type][order(-N)][1:min(100, .N)]
  
  top100_types[, `:=`(
    pct     = 100 * N / sum(N),
    cum_pct = 100 * cumsum(N) / sum(N)
  )]
  
  cat(sprintf("Total complaints with duration data: %s\n",
              format(sum(top100_types$N), big.mark = ",")))
  cat(sprintf("Top 100 complaint types identified: %d\n\n", nrow(top100_types)))
  
  cat("TOP 10 COMPLAINT TYPES (of 100 total):\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("%-3s %-45s %12s %8s %10s\n",
              "Rank", "Complaint Type", "Count", "Pct %", "Cum Pct %"))
  cat(strrep("-", 80), "\n")
  
  for (i in 1:min(10, nrow(top100_types))) {
    cat(sprintf("%-3d %-45s %12s %7.2f%% %9.2f%%\n",
                i,
                substr(top100_types$problem_formerly_complaint_type[i], 1, 45),
                format(top100_types$N[i], big.mark = ","),
                top100_types$pct[i],
                top100_types$cum_pct[i]))
  }
  cat(sprintf("...  (%d more complaint types)\n", nrow(top100_types) - 10))
  cat(strrep("-", 80), "\n\n")
  
  # Filter to top 100 complaint types with valid durations
  top100_names <- top100_types$problem_formerly_complaint_type
  dt_top100    <- dt[problem_formerly_complaint_type %in% top100_names & !is.na(duration_days)]
  
  # Create period column
  if (period_type == "month") {
    dt_top100[, period := floor_date(created_date, "month")]
  } else if (period_type == "day") {
    dt_top100[, period := as.Date(created_date)]
  } else if (period_type == "week") {
    dt_top100[, period := floor_date(created_date, "week")]
  }
  
  # Calculate mean response time per period using full population
  result <- dt_top100[, .(
    mean_duration = mean(duration_days, na.rm = TRUE),
    sample_size   = .N
  ), by = period][order(period)]
  
  overall_mean <- mean(result$mean_duration, na.rm = TRUE)
  cat(sprintf("OVERALL MEAN RESPONSE TIME (Top 100): %.2f days\n\n", overall_mean))
  
  result[, `:=`(
    events      = sample_size,
    period_num  = seq_len(.N),
    metric_name = "Mean Response Time - Top 100"
  )]
  
  # Print period table
  cat("MEAN RESPONSE TIME BY PERIOD:\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("%-15s %15s %15s\n", "Period", "Mean Days", "Sample Size"))
  cat(strrep("-", 80), "\n")
  
  for (i in 1:nrow(result)) {
    cat(sprintf("%-15s %15.2f %15s\n",
                format(result$period[i], "%Y-%m"),
                result$mean_duration[i],
                format(result$sample_size[i], big.mark = ",")))
  }
  cat(strrep("-", 80), "\n")
  cat(sprintf("%-15s %15.2f %15s\n",
              "OVERALL", overall_mean,
              format(sum(result$sample_size), big.mark = ",")))
  cat(strrep("=", 80), "\n\n")
  
  return(result)
}


################################################################################


# Special: Median response time for top 100 complaint types
# NOTE: Uses full population per period — no sampling
calculate_top100_median <- function(dt, period_type) {
  
  cat("\n", strrep("=", 80), "\n")
  cat("CALCULATING MEDIAN RESPONSE TIME - TOP 100 COMPLAINT TYPES\n")
  cat(strrep("=", 80), "\n\n")
  
  # Use same top 100 complaint types as mean function
  top100_names <- dt[!is.na(duration_days),
                     .N, by = problem_formerly_complaint_type][order(-N)][1:min(100, .N)]$problem_formerly_complaint_type
  
  cat(sprintf("Using same %d top complaint types\n\n", length(top100_names)))
  
  # Filter to top 100 with valid durations
  dt_top100 <- dt[problem_formerly_complaint_type %in% top100_names & !is.na(duration_days)]
  
  # Create period column
  if (period_type == "month") {
    dt_top100[, period := floor_date(created_date, "month")]
  } else if (period_type == "day") {
    dt_top100[, period := as.Date(created_date)]
  } else if (period_type == "week") {
    dt_top100[, period := floor_date(created_date, "week")]
  }
  
  # Calculate median response time per period using full population
  result <- dt_top100[, .(
    median_duration = median(duration_days, na.rm = TRUE),
    sample_size     = .N
  ), by = period][order(period)]
  
  overall_median <- median(result$median_duration, na.rm = TRUE)
  cat(sprintf("OVERALL MEDIAN RESPONSE TIME (Top 100): %.2f days\n\n", overall_median))
  
  result[, `:=`(
    events      = sample_size,
    period_num  = seq_len(.N),
    metric_name = "Median Response Time - Top 100"
  )]
  
  # Print period table
  cat("MEDIAN RESPONSE TIME BY PERIOD:\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("%-15s %15s %15s\n", "Period", "Median Days", "Sample Size"))
  cat(strrep("-", 80), "\n")
  
  for (i in 1:nrow(result)) {
    cat(sprintf("%-15s %15.2f %15s\n",
                format(result$period[i], "%Y-%m"),
                result$median_duration[i],
                format(result$sample_size[i], big.mark = ",")))
  }
  cat(strrep("-", 80), "\n")
  cat(sprintf("%-15s %15.2f %15s\n",
              "OVERALL", overall_median,
              format(sum(result$sample_size), big.mark = ",")))
  cat(strrep("=", 80), "\n\n")
  
  return(result)
}

################################################################################
# MAIN FUNCTION
################################################################################

calculate_all_quality_metrics <- function(
    dt,
    metrics_list,
    period_type   = "month",
    show_examples = TRUE,
    n_examples    = 10,
    analytics_dir = "."
) {
  
  # ==========================================================================
  # CALCULATE EACH METRIC
  # ==========================================================================
  
  all_results <- list()
  
  for (metric_name in names(metrics_list)) {
    metric_def <- metrics_list[[metric_name]]
    
    cat(sprintf("\n%s\nCalculating: %s\n%s\n",
                strrep("=", 80), metric_def$name, strrep("=", 80)))
    
    result <- switch(
      metric_def$calc_type,
      "condition"         = calculate_condition_metric(dt, metric_def, period_type,
                                                       show_examples, n_examples, analytics_dir),
      "domain_validation" = calculate_domain_metric(dt, metric_def, period_type,
                                                    show_examples, n_examples, analytics_dir),
      "special"           = calculate_special_metric(dt, metric_def, period_type),
      stop(sprintf("Unknown calc_type: %s", metric_def$calc_type))
    )
    
    all_results[[metric_name]] <- result
  }
  
  # Combine all results
  all_metrics <- rbindlist(all_results, fill = TRUE)
  
  return(all_metrics)
}
