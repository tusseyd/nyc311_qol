#########################################################################

main_data_file <- 
  "5-year_311SR_01-01-2020_thru_12-31-2024_AS_OF_09-23-2025.rds"

# Boolean flag. TRUE to redirect console output to text file
# FALSE to display console output on the screen
enable_sink <- FALSE   

#The "as of" date in "YYYY-MM-DD" format
projection_date <- "2025-10-01"  

#Number of SRs for the year through the projection_date  
projection_SR_count <- 2577514  

# Okabe-Ito palette for colorblind safe
palette(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
          "#0072B2", "#D55E00", "#CC79A7", "#999999"))

#########################################################################
# ------------------------------------------------------ -------
# 📦 INSTALL AND LOAD REQUIRED PACKAGES
# -------------------------------------------------------------
load_required_packages <- function(verbose = TRUE) {
  # Ordered to avoid common masking issues
  required_packages <- c(
    # Core data manipulation (load first - fundamental infrastructure)
    "data.table",
    "arrow",
    
    # Date/time handling
    "fasttime",
    "lubridate",
    "zoo",
    
    # Spatial data
    "sf",
    
    # Tidyverse core (dplyr, ggplot2, stringr are part of tidyverse)
    "tidyverse",
    
    # Additional ggplot2 extensions and formatting
    "scales",
    "ggpmisc",
    "ggpattern",
    
    # Quality control and statistical visualization
    "qcc",
    "qicharts2",
    
    # Shiny and web components
    "bslib",
    "shiny",
    "DT",
    
    # Table formatting and display
    "gt",
    "gridExtra",
    "grid",
    
    # Utilities
    "httr",
    "stringdist",
    "styler",
    "rlang",
    "renv"
  )
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (verbose) message(sprintf("📦 Installing missing package: %s", pkg))
      tryCatch(
        install.packages(pkg),
        error = function(e) message(sprintf("❌ Failed to install %s: %s", 
                                            pkg, e$message))
      )
    }
    
    # Try loading the package
    tryCatch({
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      if (verbose) message(sprintf("✅ Loaded: %s", pkg))
    }, error = function(e) {
      message(sprintf("❌ Failed to load %s: %s", pkg, e$message))
    })
  }
}

# Default verbose output
load_required_packages()

# -------------------------------------------------------------
# 📁 Set Working Directory for the Project.
# -------------------------------------------------------------
# Set working directory to the location of the initialization script
# Manually set the base directory
base_dir <- file.path(
  "C:",
  "Users",
  "David",
  "OneDrive",
  "Documents", 
  "datacleaningproject", 
  "nyc311clean",
  "Journal_of_Data_Science"
)

# Define the path for the main data file (RDS file)
data_dir <- file.path(base_dir, "data")

# Define the path for the charts
chart_dir <- file.path(base_dir, "charts", "timeline_charts")

# Define the path to the directory containing your function scripts
functions_dir <- file.path(base_dir, "code", "functions")

# Define the path to the directory containing the console output
console_dir <- file.path(base_dir, "console_output")

# Create the directory for the reduced size file following shrinkage code.
write_dir <- file.path(base_dir, "data")

# Create the directory for the raw data
raw_data_dir <- file.path(data_dir, raw_data)

# Get all .R files in the "functions" sub-directory
function_files <- list.files(functions_dir, pattern = "\\.R$", 
                             full.names = TRUE)

# More robust sourcing with verification
source_functions_safely <- function(functions_dir) {
  function_files <- list.files(functions_dir, pattern = "\\.R$", full.names = TRUE)
  
  sourced_count <- 0
  failed_count <- 0
  
  for (file in function_files) {
    tryCatch({
      # Get function count before sourcing
      functions_before <- sum(sapply(ls(.GlobalEnv), function(x) is.function(get(x))))
      
      # Source the file
      source(file, local = FALSE)
      
      # Verify sourcing worked
      functions_after <- sum(sapply(ls(.GlobalEnv), function(x) is.function(get(x))))
      
      message("Successfully sourced: ", basename(file), 
              " (added ", functions_after - functions_before, " functions)")
      sourced_count <- sourced_count + 1
      
    }, error = function(e) {
      message("ERROR sourcing: ", basename(file), " - ", e$message)
      failed_count <- failed_count + 1
    })
  }
  
  message("\nSourcing complete: ", sourced_count, " files sourced, ", failed_count, " failed")
}

# Usage
source_functions_safely(functions_dir)

#########################################################################
options(scipen = 999) # Set scipen option to a large value.
options(digits = 15) # Set the number of decimal places to 15, the max observed.
options(datatable.print.class = FALSE)

# Extract the date after "AS_OF_"
extracted_date <- sub(".*AS_OF_([0-9-]+).*", "\\1", main_data_file)
as_of_date <- as.POSIXct(
  paste0(extracted_date, " 00:00:00"),
  format = "%m-%d-%Y %H:%M:%S",
  tz = "America/New_York"
)

# Earliest possible genesis date as POSIXct with America/New_York timezone
genesis_date <- as.POSIXct("2003-01-01 00:00:00", tz = "America/New_York")

# Convert to POSIXct format
max_closed_date <- as.POSIXct(extracted_date, format = "%m-%d-%Y", tz = "America/New_York")
#print(paste("Parsed date:", max_closed_date))

# Add time to end of day
max_closed_date <- max_closed_date + (23*3600 + 59*60 + 59)
#print(paste("Final datetime:", max_closed_date))

#########################################################################
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")
cat("\n***** Program initialization *****")

message("\nExecution begins at:", formattedStartTime)

# Define the console output directory and file name.
console_output_file <- file.path(console_dir, "JDS_timeline_console_output.txt")

if (isTRUE(enable_sink)) {
  sink(console_output_file)
}

if (sink.number(type = "output") > 0L) {
  cat("\nExecution begins at:", formattedStartTime)
}

#########################################################################
cat("\n\n**********DATA INPUT AND PREPARATION**********\n")

# Construct file path
main_data_file <- file.path(data_dir, main_data_file)

# Read RDS file as data.table
cat("Reading RDS file:", main_data_file, "\n")
d311 <- readRDS(main_data_file)

# Convert to data.table if not already
if (!data.table::is.data.table(d311)) {
  cat("Converting to data.table format...\n")
  setDT(d311)
}

# Specify the columns to keep
desired_columns <- c("created_date", "closed_date", "agency")

# Check which desired columns exist
existing_columns <- names(d311)
missing_columns <- setdiff(desired_columns, existing_columns)
available_columns <- intersect(desired_columns, existing_columns)

if (length(missing_columns) > 0) {
  cat("Warning: Missing columns:", paste(missing_columns, collapse = ", "), "\n")
}

cat("Keeping columns:", paste(available_columns, collapse = ", "), "\n")

# Keep only the desired columns
d311 <- d311[, ..available_columns]

# Get row count
num_rows <- nrow(d311)
cat("Total rows:", prettyNum(num_rows, big.mark = ","), "\n")

# Verify date column formats (already in POSIX format)
date_columns <- c("created_date", "closed_date")
existing_date_columns <- intersect(date_columns, names(d311))

cat("Data preparation complete.\n")

#########################################################################

# For multiple date columns
adjust_feb_29_to_28(d311, "created_date")
adjust_feb_29_to_28(d311, "closed_date")

#########################################################################
# Collect macro statistics from the dataset
# Extract the year(s) from the created_date column
years <- year(d311$created_date)
num_years <- unique(years)

cat("\nTotal rows:", format(num_rows, big.mark = ","), "covering", length(num_years), "years")

year_digits <- 5
file_name_prefix <- "5-year"
#########################################################################
# Calculate the earliest and latest dates directly
cat("\n=== Date Range Analysis ===\n")

earliest_date <- min(d311$created_date, na.rm = TRUE)
latest_date <- max(d311$created_date, na.rm = TRUE)

# Calculate time span
time_span <- as.numeric(difftime(latest_date, earliest_date, units = "days"))

# Print the formatted date range with additional context
cat(sprintf("Data contains SRs created from %s through %s\n",
            format(earliest_date, "%Y-%m-%d %H:%M:%S"),
            format(latest_date, "%Y-%m-%d %H:%M:%S")))

cat(sprintf("Time span: %.0f days (%.1f years)\n", 
            time_span, 
            time_span / 365.25))

# Convert to yyyy-mm-dd format for titles
earliest_title <- format(earliest_date, "%Y-%m-%d")
latest_title <- format(latest_date, "%Y-%m-%d")

# Count records with missing created_date
missing_dates <- sum(is.na(d311$created_date))
if (missing_dates > 0) {
  cat(sprintf("Warning: %s records have missing created_date (%.2f%%)\n",
              prettyNum(missing_dates, big.mark = ","),
              100 * missing_dates / nrow(d311)))
}
#########################################################################
cat("\n=== Creating DateTime Aggregations ===\n")

# Aggregate created_date by second (precise timestamps)
cat("Creating second-level aggregation...\n")
second_level_created_summary <- d311[!is.na(created_date), 
                                     .(count = .N), 
                                     by = .(created_second = floor_date(created_date, "second"))
][order(created_second)]

# Aggregate by minute using second-level data
cat("Creating minute-level aggregation...\n")
minute_level_created_summary <- second_level_created_summary[,
                                                             .(count = sum(count)),
                                                             by = .(created_minute = floor_date(created_second, "minute"))
][order(created_minute)]

# Aggregate by hour using minute-level data
cat("Creating hour-level aggregation...\n")
hour_level_created_summary <- minute_level_created_summary[,
                                                           .(count = sum(count)),
                                                           by = .(created_hour = floor_date(created_minute, "hour"))
][order(created_hour)]

# Hour of day summary (0-23)
cat("Creating hour-of-day aggregation...\n")
created_hour_of_day <- second_level_created_summary[,
                                                    .(count = sum(count)),
                                                    by = .(created_hour = hour(created_second))
][order(created_hour)]

# Aggregate by day using hour-level data
cat("Creating day-level aggregation...\n")
day_level_summary <- hour_level_created_summary[,
                                                .(count = sum(count)),
                                                by = .(created_day = as.Date(format(created_hour, "%Y-%m-%d")))
][order(created_day)]

# Regenerate monthly summary
monthly_summary <- day_level_summary[,
                                     .(count = sum(count)),
                                     by = .(YearMonth = floor_date(created_day, "month"))
][order(YearMonth)]

# Regenerate yearly summary  
yearly_summary <- monthly_summary[,
                                  .(count = sum(count)),
                                  by = .(Year = year(YearMonth))
][order(Year)]

# Calendar month summary (Jan-Dec across all years)
cat("Creating calendar month aggregation...\n")
calendar_month_summary <- day_level_summary[,
                                            .(count = sum(count)),
                                            by = .(Month = format(created_day, "%B"))
]

# Convert to factor with proper month ordering
calendar_month_summary[, Month := factor(Month, levels = month.name)]
setorder(calendar_month_summary, Month)

# Day of week summary
cat("Creating day-of-week aggregation...\n")
day_of_week_summary <- day_level_summary[,
                                         .(count = sum(count)),
                                         by = .(day_of_week = weekdays(created_day))
]
# Convert to factor with proper ordering and labels
day_of_week_summary[, day_of_week := factor(
  day_of_week,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
  labels = c("1-Monday", "2-Tuesday", "3-Wednesday", "4-Thursday", "5-Friday", "6-Saturday", "7-Sunday")
)]
setorder(day_of_week_summary, day_of_week)

# Day of year summary (handles leap years properly)
cat("Creating day-of-year aggregation...\n")
day_of_year_summary <- day_level_summary[,
                                         .(count = sum(count)),
                                         by = .(day_of_year = format(created_day, "%m/%d"))
]

# Create proper ordering by converting to date (using non-leap year)
day_of_year_summary[, sort_date := as.Date(paste0("2023/", day_of_year), format = "%Y/%m/%d")]
setorder(day_of_year_summary, sort_date)

# Add day number for reference and remove sort helper
day_of_year_summary[, day_number := .I]
day_of_year_summary[, sort_date := NULL]

# Convert to character and reorder by count for analysis
day_of_year_summary[, day_of_year := as.character(day_of_year)]
# Keep both versions - chronological and by count
day_of_year_by_count <- copy(day_of_year_summary)
setorder(day_of_year_by_count, -count)

# Print summary of aggregations created
cat("\nAggregation Summary:\n")
cat(sprintf("  Second-level: %s records\n", prettyNum(nrow(second_level_created_summary), big.mark = ",")))
cat(sprintf("  Minute-level: %s records\n", prettyNum(nrow(minute_level_created_summary), big.mark = ",")))
cat(sprintf("  Hour-level: %s records\n", prettyNum(nrow(hour_level_created_summary), big.mark = ",")))
cat(sprintf("  Day-level: %s records\n", prettyNum(nrow(day_level_summary), big.mark = ",")))
cat(sprintf("  Monthly: %s records\n", prettyNum(nrow(monthly_summary), big.mark = ",")))
cat(sprintf("  Yearly: %s records\n", prettyNum(nrow(yearly_summary), big.mark = ",")))
cat(sprintf("  Hour-of-day: %s records (0-23)\n", nrow(created_hour_of_day)))
cat(sprintf("  Day-of-week: %s records\n", nrow(day_of_week_summary)))
cat(sprintf("  Day-of-year: %s records\n", nrow(day_of_year_summary)))

cat("\nDateTime aggregations complete.\n")
#########################################################################
cat("\n=== Creating Closed Date Aggregations ===\n")

# Aggregate closed_date by second (precise timestamps)
cat("Creating closed second-level aggregation...\n")
second_level_closed_summary <- d311[!is.na(closed_date), 
                                    .(count = .N), 
                                    by = .(closed_second = floor_date(closed_date, "second"))
][order(closed_second)]

# Aggregate by minute using second-level closed data
cat("Creating closed minute-level aggregation...\n")
minute_level_closed_summary <- second_level_closed_summary[,
                                                           .(count = sum(count)),
                                                           by = .(closed_minute = floor_date(closed_second, "minute"))
][order(closed_minute)]

# Hour of day summary for closed dates (0-23)
cat("Creating closed hour-of-day aggregation...\n")
closed_hour_of_day <- second_level_closed_summary[,
                                                  .(count = sum(count)),
                                                  by = .(closed_hour = hour(closed_second))
][order(closed_hour)]

# Print summary of closed date aggregations
cat("\nClosed Date Aggregation Summary:\n")
cat(sprintf("  Records with closed_date: %s (%.1f%% of total)\n", 
            prettyNum(sum(second_level_closed_summary$count), big.mark = ","),
            100 * sum(second_level_closed_summary$count) / nrow(d311)))
cat(sprintf("  Second-level: %s unique timestamps\n", prettyNum(nrow(second_level_closed_summary), big.mark = ",")))
cat(sprintf("  Minute-level: %s unique minutes\n", prettyNum(nrow(minute_level_closed_summary), big.mark = ",")))
cat(sprintf("  Hour-of-day: %s records (0-23)\n", nrow(closed_hour_of_day)))

# Show date range for closed dates
if (nrow(second_level_closed_summary) > 0) {
  earliest_closed <- min(second_level_closed_summary$closed_second, na.rm = TRUE)
  latest_closed <- max(second_level_closed_summary$closed_second, na.rm = TRUE)
  cat(sprintf("  Closed date range: %s to %s\n",
              format(earliest_closed, "%Y-%m-%d"),
              format(latest_closed, "%Y-%m-%d")))
}

cat("\nClosed date aggregations complete.\n")
#########################################################################
cat("\n=== Preparing Data for Charting ===\n")

# Add combined column for day_number and day_of_year for charting
day_of_year_summary[, day_info := paste(day_number, "-", day_of_year)]

# Create data for charting (select relevant columns)
days_to_chart <- day_of_year_summary[, .(count, day_info)]

cat("\n=== Busiest Day Analysis ===\n")

# Step 1: Find the busiest day
busiest_day_info <- day_level_summary[which.max(count)]
busiest_day <- busiest_day_info$created_day
busiest_count <- busiest_day_info$count

cat(sprintf("Busiest day: %s with %s service requests\n",
            format(busiest_day, "%Y-%m-%d (%A)"),
            prettyNum(busiest_count, big.mark = ",")))

# Step 2: Filter entries for the busiest day
entries_on_busiest_day <- d311[as.Date(created_date) == busiest_day]

cat(sprintf("Records on busiest day: %s\n", 
            prettyNum(nrow(entries_on_busiest_day), big.mark = ",")))

# Step 3: Aggregate by hour for the busiest day
cat("Creating hourly breakdown for busiest day...\n")
hourly_summary_busiest_day <- entries_on_busiest_day[,
                                                     .(count = .N),
                                                     by = .(created_hour = hour(created_date))
][order(created_hour)]

# Step 4: Aggregate by minute for the busiest day
cat("Creating minute-level breakdown for busiest day...\n")
minute_summary_busiest_day <- entries_on_busiest_day[,
                                                     .(count = .N),
                                                     by = .(created_minute = floor_date(created_date, "minute"))
][order(created_minute)]

# Step 5: Filter records with closed_date on busiest day
entries_with_closed_date <- entries_on_busiest_day[!is.na(closed_date)]

cat(sprintf("Records with closed_date on busiest day: %s (%.1f%%)\n",
            prettyNum(nrow(entries_with_closed_date), big.mark = ","),
            100 * nrow(entries_with_closed_date) / nrow(entries_on_busiest_day)))

# Step 6: Aggregate closed dates by minute for the busiest day
if (nrow(entries_with_closed_date) > 0) {
  cat("Creating closed date minute-level breakdown for busiest day...\n")
  minute_summary_closed_busiest_day <- entries_with_closed_date[,
                                                                .(count = .N),
                                                                by = .(closed_minute = floor_date(closed_date, "minute"))
  ][order(closed_minute)]
  
  # Step 7: Aggregate closed dates by second for the busiest day
  cat("Creating closed date second-level breakdown for busiest day...\n")
  second_summary_closed_busiest_day <- entries_with_closed_date[,
                                                                .(count = .N),
                                                                by = .(closed_second = floor_date(closed_date, "second"))
  ][order(closed_second)]
} else {
  cat("No closed dates found on busiest day.\n")
  minute_summary_closed_busiest_day <- data.table()
  second_summary_closed_busiest_day <- data.table()
}

# Print summary of busiest day aggregations
cat("\nBusiest Day Aggregation Summary:\n")
cat(sprintf("  Hourly breakdown: %s records (0-23 hours)\n", nrow(hourly_summary_busiest_day)))
cat(sprintf("  Minute breakdown: %s unique minutes\n", prettyNum(nrow(minute_summary_busiest_day), big.mark = ",")))

if (nrow(entries_with_closed_date) > 0) {
  cat(sprintf("  Closed minute breakdown: %s unique minutes\n", prettyNum(nrow(minute_summary_closed_busiest_day), big.mark = ",")))
  cat(sprintf("  Closed second breakdown: %s unique seconds\n", prettyNum(nrow(second_summary_closed_busiest_day), big.mark = ",")))
  
  # Show peak hour for both created and closed
  peak_created_hour <- hourly_summary_busiest_day[which.max(count)]
  cat(sprintf("  Peak creation hour: %02d:00 with %s requests\n", 
              peak_created_hour$created_hour, 
              prettyNum(peak_created_hour$count, big.mark = ",")))
  
  if (nrow(entries_with_closed_date) > 0) {
    closed_hourly <- entries_with_closed_date[, .(count = .N), by = .(closed_hour = hour(closed_date))]
    peak_closed_hour <- closed_hourly[which.max(count)]
    cat(sprintf("  Peak closure hour: %02d:00 with %s closures\n", 
                peak_closed_hour$closed_hour, 
                prettyNum(peak_closed_hour$count, big.mark = ",")))
  }
}

cat("\nBusiest day analysis complete.\n")

#########################################################################
cat("\n=== Data Preparation Complete ===\n")
cat("All aggregations created as data.table objects for optimal performance.\n")

# Optional: Verify all objects are data.tables
dt_objects <- c("second_level_closed_summary", "closed_hour_of_day", 
                "minute_summary_closed_busiest_day", "second_summary_closed_busiest_day",
                "minute_summary_busiest_day", "hourly_summary_busiest_day",
                "yearly_summary", "monthly_summary", "day_level_summary",
                "calendar_month_summary", "day_of_year_summary", 
                "day_of_week_summary", "created_hour_of_day")

for (obj_name in dt_objects) {
  if (exists(obj_name)) {
    if (!data.table::is.data.table(get(obj_name))) {
      cat("Converting", obj_name, "to data.table...\n")
      assign(obj_name, setDT(get(obj_name)), envir = .GlobalEnv)
    }
  }
}

cat("Data preparation workflow complete.\n")
#########################################################################
cat("\n=== Creating Yearly Trend Chart ===\n")

# Find max and min years using data.table syntax
max_year <- yearly_summary[which.max(count)]
min_year <- yearly_summary[which.min(count)]

# Get earliest and latest year counts
earliest_year_count <- yearly_summary[Year == min(Year), count]
latest_year_count <- yearly_summary[Year == max(Year), count]

# Compute the percentage growth
percentage_growth <- round(((latest_year_count - earliest_year_count) / earliest_year_count) * 100, 1)

# Calculate number of years in dataset
num_years <- nrow(yearly_summary)
year_span <- max(yearly_summary$Year) - min(yearly_summary$Year) + 1

cat(sprintf("\nGrowth over %d years is %.1f%%\n", num_years, percentage_growth))
cat(sprintf("Year range: %d to %d (%d year span)\n", 
            min(yearly_summary$Year), max(yearly_summary$Year), year_span))

  # Create yearly chart directly with ggplot (bypassing plot_barchart)
  yearly_chart_data <- copy(yearly_summary)
  
  # Create the growth annotation (adjust x position for discrete axis)
  extra_line <- annotate("text",
                         x = 1,  # Position at first bar for discrete axis
                         y = max_year$count,
                         label = paste0(year_span, "-yr growth: ", percentage_growth, "%"),
                         size = 3.7, 
                         color = "#661100", 
                         vjust = -0.7, 
                         hjust = 0.1
  )
  
 # Create plot directly
p_yearly <- ggplot(yearly_chart_data, aes(x = factor(Year), y = count)) +
  geom_col(fill = "#44AA99", width = 0.7) +
  geom_hline(yintercept = mean(yearly_chart_data$count), 
             linetype = "dotted", color = "indianred") +
  geom_smooth(aes(group = 1, x = as.numeric(factor(Year))), 
              method = "lm", se = FALSE, color = "red") +
  extra_line +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Yearly Service Requests (w/trendline)",
    subtitle = paste0("Total: ", prettyNum(num_rows, big.mark = ","), " requests"),  # Move subtitle here
    x = "Year",
    y = "Count"
  ) +
  david_theme(text_size = 12, x_axis_text_size = 9, x_axis_angle = 10)
  
  print(p_yearly)
  Sys.sleep(2)
  
  ggsave(file.path(chart_dir, paste0(file_name_prefix, "-trend_SRs_yearly.pdf")), 
         plot = p_yearly, width = 13, height = 8.5)
  
  cat("Yearly trend chart created successfully.\n")

#########################################################################
cat("\n=== Creating Monthly Trend Chart ===\n")

# Prepare monthly data for charting
monthly_chart_data <- copy(monthly_summary)

# Ensure YearMonth is a Date object (add day component if needed)
if (!inherits(monthly_chart_data$YearMonth, "Date")) {
  monthly_chart_data[, YearMonth := as.Date(YearMonth)]
}

# Calculate summary statistics
max_count <- max(monthly_chart_data$count)
total_count <- sum(monthly_chart_data$count)
max_month <- monthly_chart_data[which.max(count)]
min_month <- monthly_chart_data[which.min(count)]

cat(sprintf("Monthly data: %d months, total SRs: %s\n", 
            nrow(monthly_chart_data), 
            prettyNum(total_count, big.mark = ",")))
cat(sprintf("Busiest month: %s with %s SRs\n", 
            format(max_month$YearMonth, "%Y-%m"), 
            prettyNum(max_month$count, big.mark = ",")))
cat(sprintf("Quietest month: %s with %s SRs\n", 
            format(min_month$YearMonth, "%Y-%m"), 
            prettyNum(min_month$count, big.mark = ",")))

# Create growth annotation (reuse from yearly analysis)
if (exists("percentage_growth") && exists("year_span")) {
  extra_line <- annotate("text",
                         x = min_month$YearMonth, 
                         y = max_month$count,
                         label = paste0(year_span, "-yr growth: ", percentage_growth, "%"),
                         size = 4, 
                         color = "#661100", 
                         vjust = 1, 
                         hjust = -1
  )
} else {
  extra_line <- NULL
}

# Create the monthly chart
SR_monthly <- plot_barchart(
  DT = monthly_chart_data,
  bar_width = 20, 
  x_col = "YearMonth",
  y_col = "count",
  title = "Service Requests by Month",
  subtitle = paste0("Total: ", prettyNum(total_count, big.mark = ","), " requests"),
  console_print_title = "Monthly SR Count",
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_mean = TRUE,
  add_median = FALSE,
  add_trendline = FALSE,
  trendline_method = "lm",
  extra_line = extra_line,
  x_label_every = 6,  # Show every 6 months instead of 180 days
  x_axis_angle = 45,  # Angle labels for better readability
  rows_to_print = 24, # Show 2 years worth
  filename = "SRs_monthly.pdf",
  chart_dir = chart_dir,
  chart_width = 13,   # Wider for time series
  chart_height = 8.5
)

cat("Monthly trend chart created successfully.\n")

#########################################################################
cat("\n=== Creating Daily Trend Chart ===\n")

# Prepare daily data for charting
daily_chart_data <- copy(day_level_summary)

# Ensure created_day is in Date format and sort chronologically
daily_chart_data[, created_day := as.Date(created_day)]
setorder(daily_chart_data, created_day)

# Calculate summary statistics
max_date <- daily_chart_data[which.max(count), created_day]
min_date <- daily_chart_data[which.min(count), created_day]
earliest_day <- min(daily_chart_data$created_day)
latest_day <- max(daily_chart_data$created_day)
max_count <- max(daily_chart_data$count)
min_count <- min(daily_chart_data$count)
total_days <- nrow(daily_chart_data)

cat(sprintf("Daily data: %d days from %s to %s\n", 
            total_days,
            format(earliest_day, "%Y-%m-%d"),
            format(latest_day, "%Y-%m-%d")))
cat(sprintf("Busiest day: %s (%s) with %s SRs\n", 
            format(max_date, "%Y-%m-%d"),
            format(max_date, "%A"),
            prettyNum(max_count, big.mark = ",")))
cat(sprintf("Quietest day: %s (%s) with %s SRs\n", 
            format(min_date, "%Y-%m-%d"),
            format(min_date, "%A"),
            prettyNum(min_count, big.mark = ",")))



subtitle_text <- paste0("(", format(earliest_day, "%Y-%m-%d"), " to ", format(latest_day, "%Y-%m-%d"), ")")

# Create the daily chart
SR_daily <- plot_barchart(
  DT = daily_chart_data,
  x_col = "created_day",
  y_col = "count",
  title = "Daily Service Request Count over 5 Years",
  subtitle = subtitle_text,
  console_print_title = "Daily SR Count",
  add_maximum = TRUE,
  add_minimum = TRUE,
  add_median = TRUE,
  add_mean = FALSE,
  x_label_every = 90,  # Show every 30 days instead of 180
  x_axis_angle = 45,   # Angle labels for readability
  rows_to_print = 31,  # Show 31 days (about a month)
  filename = paste0(file_name_prefix, "-trend_SRs_daily.pdf"),
  chart_dir = chart_dir,
  chart_width = 13,    # Extra wide for daily time series
  chart_height = 8.5
)

cat("Daily trend chart created successfully.\n")

#########################################################################
cat("\n=== Creating Hour-of-Day Chart (workday View) ===\n")

# Prepare hour-of-day data for charting
hour_chart_data <- copy(created_hour_of_day)

# Add formatted hour column for display
hour_chart_data[, created_hour_formatted := sprintf("%02d:00", created_hour)]

# Reorder hours to start at 08:00 (workday perspective)
# Create a new ordering: 8,9,10...23,0,1,2...7
work_day_order <- c(8:23, 0:7)
hour_chart_data[, work_day_order := match(created_hour, work_day_order)]

# Sort by workday order instead of chronological order
setorder(hour_chart_data, work_day_order)

# Create a workday position for x-axis
hour_chart_data[, work_day_position := .I]

# Calculate summary statistics (same as before)
max_hour_info <- hour_chart_data[which.max(count)]
min_hour_info <- hour_chart_data[which.min(count)]
total_requests <- sum(hour_chart_data$count)

cat(sprintf("Hour-of-day analysis (workday view): 24 hours, total SRs: %s\n", 
            prettyNum(total_requests, big.mark = ",")))
cat(sprintf("Peak hour: %s with %s SRs\n", 
            max_hour_info$created_hour_formatted, 
            prettyNum(max_hour_info$count, big.mark = ",")))
cat(sprintf("Quietest hour: %s with %s SRs\n", 
            min_hour_info$created_hour_formatted, 
            prettyNum(min_hour_info$count, big.mark = ",")))

# Create subtitle
subtitle_text <- "Service request creation patterns by hour (workday view: 08:00-00:00)"

# Create custom extra annotation for maximum
extra_line <- annotate("text",
                       x = max_hour_info$work_day_position, 
                       y = max_hour_info$count,
                       label = paste0("Peak: ", max_hour_info$created_hour_formatted, "\n", 
                                      format(max_hour_info$count, big.mark = ",")),
                       size = 4, 
                       color = "#999999", 
                       vjust = -0.7, 
                       hjust = -0.1
)

#Before calling plot_barchart, convert to factor
hour_chart_data[, work_day_hour_factor := factor(created_hour_formatted, levels = created_hour_formatted)]


# Create the workday hour-of-day chart
SR_created_time_of_day_workday <- plot_barchart(
  DT = hour_chart_data,
  x_col = "work_day_hour_factor",  # Use the factor with proper labels
  y_col = "count",
  title = "Service Requests Created by Hour of Day (workday View)",
  subtitle = subtitle_text,
  console_print_title = "SRs Created by Hour-of-the-Day (workday: 08:00-00:00)",
  add_mean = TRUE,
  add_maximum = FALSE,  # Using custom annotation instead
  extra_line = extra_line,
  x_label_every = 4,    # Show every 4th position
  x_axis_angle = 45,    # Angle lab   els for readability
  rows_to_print = 24,   # Show all 24 hours
  filename = paste0(file_name_prefix, "-trend_SRs_created_by_hour_workday.pdf"),
  chart_dir = chart_dir,
  chart_width = 13,     # Slightly wider for better readability
  chart_height = 8.5
)

# Print formatted summary table in workday order
cat("\nSummary of SRs created by hour-of-the-day (workday order):\n")
print(hour_chart_data[, .(created_hour_formatted, count)], row.names = FALSE, right = FALSE)

cat("workday hour-of-day chart created successfully.\n")
#########################################################################
cat("\n=== Creating Closed Hour-of-Day Chart (workday View) ===\n")

# Prepare closed hour-of-day data for charting
closed_hour_chart_data <- copy(closed_hour_of_day)

# Add formatted hour column for display
closed_hour_chart_data[, closed_hour_formatted := sprintf("%02d:00", closed_hour)]

# Reorder hours to start at 08:00 (workday perspective)
work_day_order <- c(8:23, 0:7)
closed_hour_chart_data[, work_day_order := match(closed_hour, work_day_order)]

# Sort by workday order
setorder(closed_hour_chart_data, work_day_order)

# Convert to factor with workday ordering for proper x-axis labels
closed_hour_chart_data[, work_day_hour_factor := factor(closed_hour_formatted, levels = closed_hour_formatted)]

# Calculate summary statistics
max_hour_info <- closed_hour_chart_data[which.max(count)]
min_hour_info <- closed_hour_chart_data[which.min(count)]
total_closed <- sum(closed_hour_chart_data$count)

cat(sprintf("Closed hour-of-day analysis (workday view): 24 hours, total closures: %s\n", 
            prettyNum(total_closed, big.mark = ",")))
cat(sprintf("Peak closure hour: %s with %s closures\n", 
            max_hour_info$closed_hour_formatted, 
            prettyNum(max_hour_info$count, big.mark = ",")))
cat(sprintf("Quietest closure hour: %s with %s closures\n", 
            min_hour_info$closed_hour_formatted, 
            prettyNum(min_hour_info$count, big.mark = ",")))

# Create subtitle
subtitle_text <- "Service request closure patterns by hour (workday view: 08:00-00:00)"

# Create custom extra annotation for maximum
extra_line <- annotate("text",
                       x = which.max(closed_hour_chart_data$count),  # Use position in reordered data
                       y = max_hour_info$count,
                       label = paste0("Peak: ", max_hour_info$closed_hour_formatted, "\n", 
                                      format(max_hour_info$count, big.mark = ",")),
                       size = 3.7, 
                       color = "#999999", 
                       vjust = -0.7, 
                       hjust = -0.5
)

# Create the closed hour-of-day chart (workday view)
SR_closed_time_of_day_workday <- plot_barchart(
  DT = closed_hour_chart_data,
  x_col = "work_day_hour_factor",  # Use the factor with proper labels
  y_col = "count",
  title = "Service Requests Closed by Hour-of-the-Day (Workday View)",
  subtitle = subtitle_text,
  console_print_title = "SRs Closed by Hour-of-the-Day (Workday: 08:00-00:00)",
  add_mean = TRUE,
  add_maximum = FALSE,  # Using custom annotation instead
  extra_line = extra_line,
  x_label_every = 4,    # Show every 4th hour
  x_axis_angle = 45,    # Angle labels for readability
  rows_to_print = 24,   # Show all 24 hours
  filename = paste0(file_name_prefix, "-trend-SRs_closed_by_hour_workday.pdf"),
  chart_dir = chart_dir,
  chart_width = 13,     # Wide enough for 24 hours
  chart_height = 8.5
)

# Print formatted summary table in workday order
cat("\nSummary of SRs closed by hour-of-the-day (workday order):\n")
print(closed_hour_chart_data[, .(closed_hour_formatted, count)], row.names = FALSE, right = FALSE)

cat("Closed hour-of-day chart (workday view) created successfully.\n")

#########################################################################
cat("\n=== Creating Calendar Month Chart ===\n")

# Prepare calendar month data for charting
calendar_month_chart_data <- copy(calendar_month_summary)

# Number of days in each month, with February as 28.2 days (accounting for leap years)
days_in_month <- c(
  "January" = 31, "February" = 28.2, "March" = 31, "April" = 30,
  "May" = 31, "June" = 30, "July" = 31, "August" = 31,
  "September" = 30, "October" = 31, "November" = 30, "December" = 31
)

# Add the count_per_day column using data.table syntax
calendar_month_chart_data[, count_per_day := round(count / days_in_month[Month], 0)]

# Order by month (should already be ordered due to factor levels)
setorder(calendar_month_chart_data, Month)

# Calculate summary statistics
max_month_info <- calendar_month_chart_data[which.max(count)]
min_month_info <- calendar_month_chart_data[which.min(count)]
max_per_day_info <- calendar_month_chart_data[which.max(count_per_day)]
total_requests <- sum(calendar_month_chart_data$count)

cat(sprintf("Calendar month analysis: 12 months, total SRs: %s\n", 
            prettyNum(total_requests, big.mark = ",")))
cat(sprintf("Busiest month (total): %s with %s SRs\n", 
            max_month_info$Month, 
            prettyNum(max_month_info$count, big.mark = ",")))
cat(sprintf("Busiest month (per day): %s with %s SRs/day\n", 
            max_per_day_info$Month, 
            prettyNum(max_per_day_info$count_per_day, big.mark = ",")))

# Create subtitle if it exists from previous analysis
  subtitle_text <- "Seasonal patterns across calendar months"

# Print the summary table
cat("\nCalendar Month with total count and count_per_day:\n")
print(calendar_month_chart_data[, .(Month, count, count_per_day)], row.names = FALSE, right = FALSE)

# Create the calendar month chart
SR_calendar_month <- plot_barchart(
  DT = calendar_month_chart_data,
  x_col = "Month",
  y_col = "count",
  title = "Service Requests by Calendar Month",
  subtitle = subtitle_text,
  console_print_title = "SRs by Calendar Month",
  add_maximum = TRUE,
  add_mean = FALSE,
  x_label_every = 1,    # Show all months
  x_axis_angle = 45,    # Angle month names for readability
  rows_to_print = 12,   # Show all 12 months
  filename = paste0(file_name_prefix, "-trend_SRs_by_calendar_month.pdf"),
  chart_dir = chart_dir,
  chart_width = 13,
  chart_height = 8.5
)

cat("Calendar month chart created successfully.\n")

#########################################################################

cat("\n=== Creating Day-of-Year Chart ===\n")

# Prepare day-of-year data for charting (should already exist from earlier processing)
day_of_year_chart_data <- copy(days_to_chart)

# Calculate summary statistics
max_day_info <- day_of_year_chart_data[which.max(count)]
min_day_info <- day_of_year_chart_data[which.min(count)]

# Find second maximum
sorted_data <- day_of_year_chart_data[order(-count)]
second_max_info <- sorted_data[2]

total_days <- nrow(day_of_year_chart_data)
total_requests <- sum(day_of_year_chart_data$count)

cat(sprintf("Day-of-year analysis: %d days, total SRs: %s\n", 
            total_days, 
            prettyNum(total_requests, big.mark = ",")))
cat(sprintf("Busiest day: %s with %s SRs\n", 
            max_day_info$day_info, 
            prettyNum(max_day_info$count, big.mark = ",")))
cat(sprintf("Second busiest: %s with %s SRs\n", 
            second_max_info$day_info, 
            prettyNum(second_max_info$count, big.mark = ",")))
cat(sprintf("Quietest day: %s with %s SRs\n", 
            min_day_info$day_info, 
            prettyNum(min_day_info$count, big.mark = ",")))

# Create subtitle if it exists from previous analysis
subtitle_text <- "Daily patterns across the calendar year"


# Create the day-of-year chart
SR_day_of_the_year <- plot_barchart(
  DT = day_of_year_chart_data,
  x_col = "day_info",
  y_col = "count",
  title = "Service Requests by Day-of-the-Year",
  subtitle = subtitle_text,
  console_print_title = "SRs by Day_of_the_year",
  add_mean = TRUE,
  add_maximum = TRUE,
  x_label_every = 10,   # Show every 10th day as requested
  x_axis_angle = 70,    # Steep angle for day_info labels
  rows_to_print = 30,   # Show first 30 days
  filename = paste0(file_name_prefix, "-trend_SRs_by_day_of_the_year.pdf"),
  chart_dir = chart_dir,
  chart_width = 13,     # Extra wide for 365 days
  chart_height = 8.5,     # Slightly taller for better proportion
  show_summary = TRUE   # Will print the table as part of the function
)

cat("Day-of-year chart created successfully.\n")
#########################################################################
cat("\n=== Creating Day-of-Week Chart ===\n")

# Prepare day-of-week data for charting
day_of_week_chart_data <- copy(day_of_week_summary)

# Convert to character and back to factor to ensure proper ordering
day_of_week_chart_data[, day_of_week := as.character(day_of_week)]
day_of_week_chart_data[, week_day := .I]  # Add row number equivalent

# Convert back to factor to maintain ordering
day_of_week_chart_data[, day_of_week := factor(day_of_week, 
                                               levels = c("1-Monday", "2-Tuesday", "3-Wednesday", "4-Thursday", 
                                                          "5-Friday", "6-Saturday", "7-Sunday"))]

# Calculate summary statistics
max_day_info <- day_of_week_chart_data[which.max(count)]
min_day_info <- day_of_week_chart_data[which.min(count)]
total_requests <- sum(day_of_week_chart_data$count)

# Calculate weekday vs weekend totals
weekday_total <- day_of_week_chart_data[week_day <= 5, sum(count)]
weekend_total <- day_of_week_chart_data[week_day > 5, sum(count)]

cat(sprintf("Day-of-week analysis: 7 days, total SRs: %s\n", 
            prettyNum(total_requests, big.mark = ",")))
cat(sprintf("Busiest day: %s with %s SRs\n", 
            max_day_info$day_of_week, 
            prettyNum(max_day_info$count, big.mark = ",")))
cat(sprintf("Quietest day: %s with %s SRs\n", 
            min_day_info$day_of_week, 
            prettyNum(min_day_info$count, big.mark = ",")))
cat(sprintf("Weekdays (Mon-Fri): %s SRs (%.1f%%)\n", 
            prettyNum(weekday_total, big.mark = ","),
            100 * weekday_total / total_requests))
cat(sprintf("Weekends (Sat-Sun): %s SRs (%.1f%%)\n", 
            prettyNum(weekend_total, big.mark = ","),
            100 * weekend_total / total_requests))

# Create subtitle if it exists from previous analysis
subtitle_text <- "Weekly patterns in service request volume"

# Create the day-of-week chart
SR_day_of_the_week <- plot_barchart(
  DT = day_of_week_chart_data,
  x_col = "day_of_week",
  y_col = "count",
  title = "Service Requests by Day-of-the-Week",
  subtitle = subtitle_text,
  console_print_title = "SRs by Day-of-the-Week",
  add_maximum = TRUE,
  add_mean = FALSE,
  x_label_every = 1,    # Show all 7 days
  x_axis_angle = 45,    # Angle labels for readability
  rows_to_print = 7,    # Show all 7 days
  filename = paste0(file_name_prefix, "-trend_SRs_by_day_of_the_week.pdf"),
  chart_dir = chart_dir,
  chart_width = 13,
  chart_height = 8.5
)

cat("Day-of-week chart created successfully.\n")

#########################################################################
cat("\n=== Creating Top-of-Hour Chart ===\n")

# Use second-level aggregated data and filter for top of the hour entries
filtered_by_hour <- second_level_created_summary[
  minute(created_second) == 0 & second(created_second) == 0
]

cat(sprintf("Records created exactly on the hour: %s (%.2f%% of all timestamps)\n",
            prettyNum(nrow(filtered_by_hour), big.mark = ","),
            100 * nrow(filtered_by_hour) / nrow(second_level_created_summary)))

# Group the data by the hour using data.table
grouped_by_hour <- filtered_by_hour[,
                                    .(count = sum(count)),
                                    by = .(created_hour = hour(created_second))
][order(created_hour)]

# Calculate summary statistics
max_hour_info <- grouped_by_hour[which.max(count)]
min_hour_info <- grouped_by_hour[which.min(count)]

# Find second maximum
sorted_data <- grouped_by_hour[order(-count)]
second_max_info <- if(nrow(sorted_data) >= 2) sorted_data[2] else NULL

total_on_hour <- sum(grouped_by_hour$count)

cat(sprintf("Top-of-hour analysis: %d hours, total SRs on the hour: %s\n", 
            nrow(grouped_by_hour), 
            prettyNum(total_on_hour, big.mark = ",")))
cat(sprintf("Peak hour: %02d:00 with %s SRs\n", 
            max_hour_info$created_hour, 
            prettyNum(max_hour_info$count, big.mark = ",")))

if (!is.null(second_max_info)) {
  cat(sprintf("Second peak: %02d:00 with %s SRs\n", 
              second_max_info$created_hour, 
              prettyNum(second_max_info$count, big.mark = ",")))
}

cat(sprintf("Quietest hour: %02d:00 with %s SRs\n", 
            min_hour_info$created_hour, 
            prettyNum(min_hour_info$count, big.mark = ",")))

# Create subtitle if it exists from previous analysis
subtitle_text <- "Service requests created exactly at HH:00:00"

# Create custom annotation for maximum
extra_line <- annotate("text",
                       x = max_hour_info$created_hour, 
                       y = max_hour_info$count,
                       label = paste0("Max: ", format(max_hour_info$count, big.mark = ",")),
                       size = 3.7, 
                       color = "#999999", 
                       vjust = -0.4, 
                       hjust = 0.1
)

# Create the top-of-hour chart
SR_created_by_top_of_hour <- plot_barchart(
  DT = grouped_by_hour,
  x_col = "created_hour",
  y_col = "count",
  title = "SRs Created Exactly on the Hour",
  subtitle = subtitle_text,
  console_print_title = "SRs Created Exactly on the Hour (HH:00:00)",
  add_median = TRUE,
  add_maximum = FALSE,  # Using custom annotation instead
  extra_line = extra_line,
  x_label_every = 2,    # Show every 2 hours
  x_axis_angle = 0,     # Keep horizontal for hour labels
  rows_to_print = 24,   # Show all hours if available
  filename = paste0(file_name_prefix, "-trend_SRs_created_on_the_hour.pdf"),
  chart_dir = chart_dir,
  chart_width = 13,
  chart_height = 8.5
)

cat("Top-of-hour chart created successfully.\n")

#########################################################################
# 
# cat("\n=== Creating Minute-Level Chart for Busiest Day ===\n")
# 
# # Use pre-aggregated data for the minute-level summary
# minute_counts_busiest_day <- minute_level_created_summary[
#   as.Date(created_minute) == as.Date(busiest_day)  # Use busiest_day from earlier analysis
# ]
# 
# cat(sprintf("Analyzing minute-level patterns for busiest day: %s\n", 
#             format(busiest_day, "%Y-%m-%d (%A)")))
# cat(sprintf("Total minute-level records for this day: %s\n", 
#             prettyNum(nrow(minute_counts_busiest_day), big.mark = ",")))
# 
# # Group by hour and minute using data.table
# minute_counts_busiest_day[, `:=`(
#   hour = hour(created_minute),
#   minute = minute(created_minute)
# )]
# 
# # Aggregate by hour and minute
# minute_summary <- minute_counts_busiest_day[,
#                                             .(count = sum(count)),
#                                             by = .(hour, minute)
# ][order(hour, minute)]
# 
# # Create formatted hour:minute column
# minute_summary[, hour_minute := sprintf("%02d:%02d", hour, minute)]
# 
# # Convert to POSIXct for proper time axis (using today's date as base)
# base_date <- as.Date(Sys.Date())
# minute_summary[, hour_minute_time := as.POSIXct(
#   paste(base_date, hour_minute), 
#   format = "%Y-%m-%d %H:%M"
# )]
# 
# # Calculate summary statistics
# max_minute_info <- minute_summary[which.max(count)]
# min_minute_info <- minute_summary[which.min(count)]
# total_requests <- sum(minute_summary$count)
# 
# cat(sprintf("Peak minute: %s with %s SRs\n", 
#             max_minute_info$hour_minute, 
#             prettyNum(max_minute_info$count, big.mark = ",")))
# cat(sprintf("Quietest minute: %s with %s SRs\n", 
#             min_minute_info$hour_minute, 
#             prettyNum(min_minute_info$count, big.mark = ",")))
# cat(sprintf("Total SRs on busiest day: %s\n", 
#             prettyNum(total_requests, big.mark = ",")))
# 
# # Create subtitle
# subtitle_text <- paste0("Minute-by-minute patterns on ", format(busiest_day, "%Y-%m-%d"))
# 
# # Create the minute-level chart
# SR_created_by_minute_of_busiest_day <- plot_barchart(
#   DT = minute_summary,
#   x_col = "hour_minute_time",
#   y_col = "count",
#   title = paste("Service Requests by Minute on Busiest Day"),
#   subtitle = subtitle_text,
#   console_print_title = "SRs Created by Minute (HH:MM:00) on Busiest Day",
#   add_maximum = TRUE,
#   add_mean = FALSE,
#   x_label_every = 120,  # Show every 2 hours (120 minutes)
#   x_axis_angle = 45,    # Angle labels for readability
#   y_axis_labels = scales::comma,
#   rows_to_print = 60,   # Show first hour worth of data
#   filename = paste0(file_name_prefix, "-trend_SRs_created_by_minute_of_busiest_day.pdf"),
#   chart_dir = chart_dir,
#   chart_width = 16,     # Extra wide for minute-level detail
#   chart_height = 8
# )
# 
# cat("Minute-level busiest day chart created successfully.\n")
#########################################################################
cat("\n=== Creating Closed Top-of-Hour Chart ===\n")

# Use second-level closed summary and filter for top of the hour entries
filtered_by_hour <- second_level_closed_summary[
  minute(closed_second) == 0 & second(closed_second) == 0
]

cat(sprintf("Records closed exactly on the hour: %s (%.2f%% of all closed timestamps)\n",
            prettyNum(nrow(filtered_by_hour), big.mark = ","),
            100 * nrow(filtered_by_hour) / nrow(second_level_closed_summary)))

# Group the data by the hour using data.table
grouped_by_hour <- filtered_by_hour[,
                                    .(count = sum(count)),
                                    by = .(closed_hour = hour(closed_second))
][order(closed_hour)]

# Calculate summary statistics
max_hour_info <- grouped_by_hour[which.max(count)]
min_hour_info <- grouped_by_hour[which.min(count)]

# Find second maximum
sorted_data <- grouped_by_hour[order(-count)]
second_max_info <- if(nrow(sorted_data) >= 2) sorted_data[2] else NULL

total_on_hour <- sum(grouped_by_hour$count)

cat(sprintf("Closed top-of-hour analysis: %d hours, total closures on the hour: %s\n", 
            nrow(grouped_by_hour), 
            prettyNum(total_on_hour, big.mark = ",")))
cat(sprintf("Peak closure hour: %02d:00 with %s closures\n", 
            max_hour_info$closed_hour, 
            prettyNum(max_hour_info$count, big.mark = ",")))

if (!is.null(second_max_info)) {
  cat(sprintf("Second peak: %02d:00 with %s closures\n", 
              second_max_info$closed_hour, 
              prettyNum(second_max_info$count, big.mark = ",")))
}

cat(sprintf("Quietest closure hour: %02d:00 with %s closures\n", 
            min_hour_info$closed_hour, 
            prettyNum(min_hour_info$count, big.mark = ",")))

# Create subtitle if it exists from previous analysis
subtitle_text <- "Service Requests closed exactly at HH:00:00"

# Create custom annotation for maximum
extra_line <- annotate("text",
                       x = max_hour_info$closed_hour, 
                       y = max_hour_info$count,
                       label = paste0("Max: ", format(max_hour_info$count, big.mark = ",")),
                       size = 3.7, 
                       color = "#999999", 
                       vjust = -0.7, 
                       hjust = 0.1
)

# Create the closed top-of-hour chart
SR_closed_by_top_of_hour <- plot_barchart(
  DT = grouped_by_hour,
  x_col = "closed_hour",
  y_col = "count",
  title = "SRs Closed Exactly on the Hour",
  subtitle = subtitle_text,
  console_print_title = "SRs Closed Exactly on the Hour (HH:00:00)",
  add_maximum = FALSE,  # Using custom annotation instead
  add_mean = FALSE,
  extra_line = extra_line,
  x_label_every = 2,    # Show every 2 hours
  x_axis_angle = 0,     # Keep horizontal for hour labels
  rows_to_print = 24,   # Show all hours if available
  filename = paste0(file_name_prefix, "-trend_SRs_closed_on_the_hour.pdf"),
  chart_dir = chart_dir,
  chart_width = 13,
  chart_height = 8.5
)

cat("Closed top-of-hour chart created successfully.\n")

#########################################################################
# cat("\n=== Creating Closed Minute-Level Chart for Busiest Day ===\n")
# 
# # Use pre-aggregated minute-level closed summary and filter for the busiest day
# minute_counts_busiest_day <- minute_level_closed_summary[
#   as.Date(closed_minute) == as.Date(busiest_day)  # Use busiest_day from earlier analysis
# ]
# 
# cat(sprintf("Analyzing closure minute-level patterns for busiest day: %s\n", 
#             format(busiest_day, "%Y-%m-%d (%A)")))
# cat(sprintf("Total minute-level closure records for this day: %s\n", 
#             prettyNum(nrow(minute_counts_busiest_day), big.mark = ",")))
# 
# if (nrow(minute_counts_busiest_day) == 0) {
#   cat("No closure data available for the busiest day. Skipping chart creation.\n")
# } else {
#   
#   # Group by hour and minute using data.table
#   minute_counts_busiest_day[, `:=`(
#     hour = hour(closed_minute),
#     minute = minute(closed_minute)
#   )]
#   
#   # Aggregate by hour and minute
#   minute_counts <- minute_counts_busiest_day[,
#                                              .(count = sum(count)),
#                                              by = .(hour, minute)
#   ][order(hour, minute)]
#   
#   # Create formatted hour:minute column
#   minute_counts[, hour_minute := sprintf("%02d:%02d", hour, minute)]
#   
#   # Convert to POSIXct for proper time axis (using today's date as base)
#   base_date <- as.Date(Sys.Date())
#   minute_counts[, hour_minute_time := as.POSIXct(
#     paste(base_date, hour_minute), 
#     format = "%Y-%m-%d %H:%M"
#   )]
#   
#   # Calculate summary statistics
#   max_minute_info <- minute_counts[which.max(count)]
#   min_minute_info <- minute_counts[which.min(count)]
#   
#   # Find second maximum
#   sorted_data <- minute_counts[order(-count)]
#   second_max_info <- if(nrow(sorted_data) >= 2) sorted_data[2] else NULL
#   
#   total_closures <- sum(minute_counts$count)
#   
#   cat(sprintf("Peak closure minute: %s with %s closures\n", 
#               max_minute_info$hour_minute, 
#               prettyNum(max_minute_info$count, big.mark = ",")))
#   
#   if (!is.null(second_max_info)) {
#     cat(sprintf("Second peak: %s with %s closures\n", 
#                 second_max_info$hour_minute, 
#                 prettyNum(second_max_info$count, big.mark = ",")))
#   }
#   
#   cat(sprintf("Quietest closure minute: %s with %s closures\n", 
#               min_minute_info$hour_minute, 
#               prettyNum(min_minute_info$count, big.mark = ",")))
#   cat(sprintf("Total closures on busiest day: %s\n", 
#               prettyNum(total_closures, big.mark = ",")))
#   
#   # Create subtitle
#   subtitle_text <- paste0("Minute-by-minute closure patterns on ", format(busiest_day, "%Y-%m-%d"))
#   
#   # Create the minute-level closure chart
#   SR_closed_by_minute_of_busiest_day <- plot_barchart(
#     DT = minute_counts,
#     x_col = "hour_minute_time",
#     y_col = "count",
#     title = "Service Requests Closed by Minute on Busiest Day",
#     subtitle = subtitle_text,
#     console_print_title = "SRs Closed by Minute (HH:MM:00) on Busiest Day",
#     add_maximum = TRUE,
#     add_mean = FALSE,
#     x_label_every = 120,  # Show every 2 hours (120 minutes)
#     x_axis_angle = 45,    # Angle labels for readability
#     y_axis_labels = scales::comma,
#     rows_to_print = 60,   # Show first hour worth of data
#     filename = paste0(file_name_prefix, "-trend-SRs_closed_by_minute_of_busiest_day.pdf"),
#     chart_dir = chart_dir,
#     chart_width = 16,     # Extra wide for minute-level detail
#     chart_height = 8
#   )
#   
#   cat("Closed minute-level busiest day chart created successfully.\n")
# }
# 

########################################################################
cat("\n=== Analyzing Midnight and Noon Creation Patterns ===\n")

# Extract time components from second-level created summary using data.table
second_level_created_summary[, `:=`(
  hour = hour(created_second),
  minute = minute(created_second),
  second = second(created_second)
)]

# Identify rows with time exactly at midnight (00:00:00) and noon (12:00:00)
midnight_indices <- second_level_created_summary[hour == 0 & minute == 0 & second == 0, which = TRUE]
noon_indices <- second_level_created_summary[hour == 12 & minute == 0 & second == 0, which = TRUE]

# Get the actual records from the main dataset
created_at_midnight <- if(length(midnight_indices) > 0) {
  d311[midnight_indices, .(created_date, agency)]
} else {
  data.table()
}

created_at_noon <- if(length(noon_indices) > 0) {
  d311[noon_indices, .(created_date, agency)]
} else {
  data.table()
}

# Count totals
midnight_created_count <- nrow(created_at_midnight)
noon_created_count <- nrow(created_at_noon)
total_requests <- nrow(d311)

cat(sprintf("Total service requests in dataset: %s\n", 
            prettyNum(total_requests, big.mark = ",")))
cat(sprintf("Created exactly at midnight (00:00:00): %s (%.3f%%)\n", 
            prettyNum(midnight_created_count, big.mark = ","),
            100 * midnight_created_count / total_requests))
cat(sprintf("Created exactly at noon (12:00:00): %s (%.3f%%)\n", 
            prettyNum(noon_created_count, big.mark = ","),
            100 * noon_created_count / total_requests))

# Process SRs created at midnight
if (midnight_created_count > 0) {
  cat("\n=== Midnight Creation Analysis ===\n")
  
  # Check if plot_pareto_combo function exists
  if (exists("plot_pareto_combo", mode = "function")) {
    plot_pareto_combo(
      DT = created_at_midnight,
      x_col = "agency",
      chart_dir = chart_dir,
      filename = "SRs_created_at_midnight_by_Agency.pdf",
      title = "SRs Created Exactly at Midnight (00:00:00) by Agency",
      subtitle = paste0("Total: ", prettyNum(midnight_created_count, big.mark = ","), " requests"),
      top_n = 20,
      include_na = FALSE
    )
  } else {
    # Fallback: create agency summary manually
    midnight_by_agency <- created_at_midnight[, .(count = .N), by = agency][order(-count)]
    cat("Top agencies for midnight creation:\n")
    print(head(midnight_by_agency, 10), row.names = FALSE)
    cat("Note: plot_pareto_combo function not found. Manual summary provided.\n")
  }
} else {
  cat("\nNo service requests were created exactly at midnight (00:00:00).\n")
}

# Process SRs created at noon
if (noon_created_count > 0) {
  cat("\n=== Noon Creation Analysis ===\n")
  
  # Check if plot_pareto_combo function exists
  if (exists("plot_pareto_combo", mode = "function")) {
    plot_pareto_combo(
      DT = created_at_noon,
      x_col = "agency",
      chart_dir = chart_dir,
      filename = "SRs_created_at_noon_by_Agency.pdf",
      title = "SRs Created Exactly at Noon (12:00:00) by Agency",
      subtitle = paste0("Total: ", prettyNum(noon_created_count, big.mark = ","), " requests"),
      top_n = 20,
      include_na = FALSE
    )
  } else {
    # Fallback: create agency summary manually
    noon_by_agency <- created_at_noon[, .(count = .N), by = agency][order(-count)]
    cat("Top agencies for noon creation:\n")
    print(head(noon_by_agency, 10), row.names = FALSE)
    cat("Note: plot_pareto_combo function not found. Manual summary provided.\n")
  }
} else {
  cat("\nNo service requests were created exactly at noon (12:00:00).\n")
}

# Summary insights
if (midnight_created_count > 0 || noon_created_count > 0) {
  cat("\n=== Summary Insights ===\n")
  cat("High concentrations of requests at exact times (midnight/noon) may indicate:\n")
  cat("  • Automated batch processing systems\n")
  cat("  • Manual data entry with timestamp rounding\n")
  cat("  • System maintenance or scheduled operations\n")
  cat("  • Data quality issues requiring investigation\n")
}

cat("\nMidnight/noon Creation analysis complete.\n")
#########################################################################
cat("\n=== Analyzing Midnight and Noon Closure Patterns ===\n")

# Check if we have closed date data
if (nrow(second_level_closed_summary) == 0) {
  cat("No closed date data available for analysis.\n")
} else {
  
  # Extract time components from second-level closed summary
  second_level_closed_summary[, `:=`(
    hour = hour(closed_second),
    minute = minute(closed_second),
    second = second(closed_second)
  )]
  
  # Identify indices for midnight and noon closures
  midnight_indices <- second_level_closed_summary[hour == 0 & minute == 0 & second == 0, which = TRUE]
  noon_indices <- second_level_closed_summary[hour == 12 & minute == 0 & second == 0, which = TRUE]
  
  # Get the actual records from the main dataset (only records with valid closed_date)
  closed_at_midnight <- if(length(midnight_indices) > 0) {
    d311[midnight_indices][!is.na(closed_date), .(created_date, closed_date, agency)]
  } else {
    data.table()
  }
  
  closed_at_noon <- if(length(noon_indices) > 0) {
    d311[noon_indices][!is.na(closed_date), .(created_date, closed_date, agency)]
  } else {
    data.table()
  }
  
  # Count totals
  midnight_closed_count <- nrow(closed_at_midnight)
  noon_closed_count <- nrow(closed_at_noon)
  total_closed <- sum(!is.na(d311$closed_date))
  
  cat(sprintf("Total service requests with closed dates: %s\n", 
              prettyNum(total_closed, big.mark = ",")))
  cat(sprintf("Closed exactly at midnight (00:00:00): %s (%.3f%%)\n", 
              prettyNum(midnight_closed_count, big.mark = ","),
              100 * midnight_closed_count / total_closed))
  cat(sprintf("Closed exactly at noon (12:00:00): %s (%.3f%%)\n", 
              prettyNum(noon_closed_count, big.mark = ","),
              100 * noon_closed_count / total_closed))
  
  # Process SRs closed at midnight
  if (midnight_closed_count > 0) {
    cat("\n=== Midnight Closure Analysis ===\n")
    
    # Check if plot_pareto_combo function exists
    if (exists("plot_pareto_combo", mode = "function")) {
      plot_pareto_combo(
        DT = closed_at_midnight,
        x_col = "agency",
        chart_dir = chart_dir,
        filename = "SRs_closed_at_midnight_by_Agency.pdf",
        title = "SRs Closed Exactly at Midnight (00:00:00) by Agency",
        subtitle = paste0("Total: ", prettyNum(midnight_closed_count, big.mark = ","), " closures"),
        top_n = 20,
        include_na = FALSE
      )
    } else {
      # Fallback: create agency summary manually
      midnight_by_agency <- closed_at_midnight[, .(count = .N), by = agency][order(-count)]
      cat("Top agencies for midnight closures:\n")
      print(head(midnight_by_agency, 10), row.names = FALSE)
      cat("Note: plot_pareto_combo function not found. Manual summary provided.\n")
    }
  } else {
    cat("\nNo service requests were closed exactly at midnight (00:00:00).\n")
  }
  
  # Process SRs closed at noon
  if (noon_closed_count > 0) {
    cat("\n=== Noon Closure Analysis ===\n")
    
    # Check if plot_pareto_combo function exists
    if (exists("plot_pareto_combo", mode = "function")) {
      plot_pareto_combo(
        DT = closed_at_noon,
        x_col = "agency",
        chart_dir = chart_dir,
        filename = "SRs_closed_at_noon_by_Agency.pdf",
        title = "SRs Closed Exactly at Noon (12:00:00) by Agency",
        subtitle = paste0("Total: ", prettyNum(noon_closed_count, big.mark = ","), " closures"),
        top_n = 20,
        include_na = FALSE
      )
    } else {
      # Fallback: create agency summary manually
      noon_by_agency <- closed_at_noon[, .(count = .N), by = agency][order(-count)]
      cat("Top agencies for noon closures:\n")
      print(head(noon_by_agency, 10), row.names = FALSE)
      cat("Note: plot_pareto_combo function not found. Manual summary provided.\n")
    }
  } else {
    cat("\nNo service requests were closed exactly at noon (12:00:00).\n")
  }
  
  # Summary insights
  if (midnight_closed_count > 0 || noon_closed_count > 0) {
    cat("\n=== Closure Pattern Insights ===\n")
    cat("High concentrations of closures at exact times may indicate:\n")
    cat("  • Automated batch closure processes\n")
    cat("  • End-of-shift or scheduled closure workflows\n")
    cat("  • Manual data entry with timestamp rounding\n")
    cat("  • System maintenance or overnight processing\n")
    
    # Compare closure vs creation patterns if both exist
    if (exists("midnight_created_count") && exists("noon_created_count")) {
      cat("\n=== Creation vs Closure Comparison ===\n")
      cat(sprintf("Midnight - Created: %s, Closed: %s\n", 
                  prettyNum(midnight_created_count, big.mark = ","),
                  prettyNum(midnight_closed_count, big.mark = ",")))
      cat(sprintf("Noon - Created: %s, Closed: %s\n", 
                  prettyNum(noon_created_count, big.mark = ","),
                  prettyNum(noon_closed_count, big.mark = ",")))
    }
  }
}

cat("\nMidnight/noon closure analysis complete.\n")
########################################################################

#########################################################################
# Conclude program
# Store the program end time and calculate the duration
programStop <- as.POSIXct(Sys.time())
formatted_end_time <- format(programStop, "%Y-%m-%d %H:%M:%S")

# Calculate the duration of the program (in seconds)
duration_seconds <- as.numeric(difftime(programStop, programStart,  
                                        units = "secs"))

# Convert the duration to a formatted string (hrs, mins, and secs)
hours <- floor(duration_seconds / 3600)
minutes <- floor((duration_seconds %% 3600) / 60)
seconds <- round(duration_seconds %% 60, 4)  # Round to 4 decimal places

# Create the formatted duration string
duration_string <- paste0(
  if (hours > 0) paste0(hours, " hours, ") else "", 
  if (minutes > 0) paste0(minutes, " minutes, ") else "",
  seconds, " seconds"
)

# Print the final program information to the console
cat("\n\n*****END OF PROGRAM*****\n")
cat("\n📅 Execution ends at:", formatted_end_time, "\n")
cat("\n⏱️ Program run-time:", duration_string, "\n")

if (isTRUE(enable_sink)) {
  sink()  # restore normal console output
}

#########################################################################
# Call the end_program function with the formatted end time and duration string
end_program(formatted_end_time, duration_string)

#########################################################################