# --- 1. Define your date fields ---
valid_date_columns <- c(
  "Created Date",
  "Closed Date", 
  "Due Date",
  "Resolution Action Updated Date"
)

# --- 2. Read only those columns ---
raw_data_dir <- file.path(data_dir, "raw_data")
raw_data <- fread(
  file.path(raw_data_dir, "raw_data_5_years_AS_OF_10-10-2025.csv"),
  select = valid_date_columns,
  nThread = parallel::detectCores() - 1,
  strip.white = TRUE,
  showProgress = TRUE
)
cat("Rows read:", nrow(raw_data), "\n")

# --- 3. Initialize an empty results list ---
summary_list <- list()

# --- 4. Loop through each date column ---
for (col in valid_date_columns) {
  
  vals <- trimws(raw_data[[col]])
  vals <- vals[!is.na(vals) & vals != ""]
  
  # Character-length summary
  len_table <- data.table(nchar = nchar(vals))[
    , .N, by = nchar
  ][order(nchar)]
  
  # Identify date-only records (length 10 for MM/DD/YYYY or no time part)
  date_only <- nchar(vals) == 10 | !grepl(" ", vals)
  count_date_only <- sum(date_only, na.rm = TRUE)
  
  # Filter to only records with time component
  vals_with_time <- vals[!date_only]
  
  # Extract HH, MM, AM/PM (only from records with time)
  hour_vals   <- substr(vals_with_time, 12, 13)
  minute_vals <- substr(vals_with_time, 15, 16)
  ampm_vals   <- substr(vals_with_time, 21, 22)
  time_part   <- substr(vals_with_time, 12, 22)  # "HH:MM:SS AM/PM"
  
  # Extract just "HH:MM:SS" portion and count 00:00:00
  hhmmss_vals <- substr(vals_with_time, 12, 19)

  # Hour table
  hour_table <- data.table(hour = hour_vals)[
    !is.na(hour) & hour != "",
    .N, by = hour
  ][order(as.integer(hour))]
  hour_table[, pct := round(100 * N / sum(N), 2)]
  
  # Minute table
  minute_table <- data.table(minute = minute_vals)[
    !is.na(minute) & minute != "",
    .N, by = minute
  ][order(as.integer(minute))]
  minute_table[, pct := round(100 * N / sum(N), 2)]
  
  # Count specific times (in 12-hour format)
  count_midnight <- sum(time_part == "12:00:00 AM", na.rm = TRUE)
  count_noon     <- sum(time_part == "12:00:00 PM", na.rm = TRUE)
  total_non_na   <- length(vals)
  
  # Combine main summary
  result <- data.table(
    column = col,
    nchar_summary = paste(
      paste0(len_table$nchar, " (", len_table$N, ")"),
      collapse = ", "
    ),
    total_non_na    = total_non_na,
    count_date_only = count_date_only,
    pct_date_only   = round(100 * count_date_only / total_non_na, 4),
    count_midnight  = count_midnight,
    pct_midnight    = round(100 * count_midnight / total_non_na, 4),
    count_noon      = count_noon,
    pct_noon        = round(100 * count_noon / total_non_na, 4),
  )
  
  # ---- Print detailed results ----
  cat("\n============================================================\n")
  cat("Field:", col, "\n")
  cat("============================================================\n")
  
  cat("\n", col, " — Date-Only Records (no time component)\n", sep = "")
  cat(sprintf("Count: %d (%.2f%%)\n", count_date_only, 100 * count_date_only / total_non_na))
  
  if (length(vals_with_time) > 0) {
    cat("\n", col, " — Hour Distribution (records with time only)\n", sep = "")
    print(hour_table)
    
    cat("\n", col, " — Minute Distribution (records with time only)\n", sep = "")
    print(minute_table)
    
    cat("\n", col, " — Counts of Special Times\n", sep = "")
    cat(sprintf("Midnight (12:00:00 AM): %d (%.2f%% of records with time)\n", 
                count_midnight, 100 * count_midnight / length(vals_with_time)))
    cat(sprintf("Noon (12:00:00 PM):     %d (%.2f%% of records with time)\n", 
                count_noon, 100 * count_noon / length(vals_with_time)))
    cat(sprintf("00:00:00 (HH:MM:SS only): %d (%.2f%% of total)\n", 
                count_all_zeros, 100 * count_all_zeros / total_non_na))
  } else {
    cat("\nNo records with time component found.\n")
  }
  
  # Append this result to list
  summary_list[[col]] <- result
}

# --- 5. Combine and print final summary ---
summary_dt <- rbindlist(summary_list, fill = TRUE)
cat("\n--- Date Field Character-Length, Date-Only, 'Midnight', 'Noon', and '00:00:00' Summary ---\n")
print(summary_dt)
