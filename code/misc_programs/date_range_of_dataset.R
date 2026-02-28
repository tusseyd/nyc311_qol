# Check exact datetime range in filtered data
d311_quality[, .(
  min_date = min(created_date),
  max_date = max(created_date),
  n_records = .N
)]

# Count months with exact start/end dates per month
d311_quality[, month := floor_date(created_date, "month")]
month_counts <- d311_quality[, .(
  n_records = .N,
  min_datetime = min(created_date),
  max_datetime = max(created_date)
), by = month][order(month)]

cat(sprintf("\nTotal months: %d\n\n", nrow(month_counts)))
print(month_counts)

# Show first 3 and last 3 months with full timestamps
cat("\n=== First 3 Months ===\n")
print(month_counts[1:3])

cat("\n=== Last 3 Months ===\n")
print(month_counts[(.N-2):.N])

# Also check the config dates vs actual filter
cat("\n=== Filter Configuration ===\n")
cat(sprintf("Config start: %s\n", quality_start_date))
cat(sprintf("Config end:   %s\n", quality_end_date))
cat(sprintf("Filter start: %s\n", as.POSIXct(quality_start_date, tz = "America/New_York")))
cat(sprintf("Filter end:   %s\n", as.POSIXct(quality_end_date, tz = "America/New_York")))