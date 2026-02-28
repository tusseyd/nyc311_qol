################################################################################
# HELPER FUNCTIONS FOR QUALITY METRICS
# File: calculate_quality_metrics_helpers.R
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
    
    # Select columns to display (customize based on what's useful)
    display_cols <- intersect(
      c("duration_days", "created_date", "closed_date", "problem_formerly_complaint_type",
        "descriptor", "agency", "borough", "incident_zip"),
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
    
    # Create output filename based on metric name
    output_filename <- paste0(
      "invalid_",
      gsub("[^[:alnum:]_]", "_", tolower(metric_def$name)),
      "_",
      format(Sys.Date(), "%Y%m%d"),
      ".csv"
    )
    
    # Select columns to save
    save_cols <- intersect(
      c( "created_date", "closed_date", metric_def$field, 
        "problem_formerly_complaint_type", "descriptor", "agency", 
        "agency_name", "borough", "incident_zip", "duration_days" ),
      names(invalid_records)
    )
    
    fwrite(invalid_records[, ..save_cols], file.path(analytics_dir, output_filename))
    
    cat(sprintf("\nINVALID RECORDS SAVED TO: %s\n", file.path(analytics_dir, output_filename)))
    cat(sprintf("  Records saved: %s\n", format(nrow(invalid_records), big.mark = ",")))
    
    # Show the field in question plus context
    display_cols <- intersect(
      c("created_date", metric_def$field, 
        "problem_formerly_complaint_type", "agency", "borough", "duration_days"),
      names(invalid_records)
    )
    
    n_to_show <- min(n_examples, nrow(invalid_records))
    
    cat(sprintf("\nRANDOM SAMPLE OF INVALID VALUES (n=%d):\n", n_to_show))
    sample_indices <- sample(nrow(invalid_records), n_to_show)
    print(invalid_records[sample_indices, ..display_cols], nrows = Inf)
    
    # Show frequency of invalid values
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