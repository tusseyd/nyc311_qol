  ######################################################################### 
######################################################################### 
message("\nLoading functions and settings.")

main_data_file <-  
  "4-year_311SR_01-01-2022_thru_12-31-2025_AS_OF_02-02-2026.rds"

zip_data_file <- "USPS_zipcodes.rds"

# Boolean flag. TRUE to redirect console output to text file
# FALSE to display console output on the screen
enable_sink <- TRUE       

#The "as of" date in "YYYY-MM-DD" format
projection_date <- "2025-11-30"   

#Number of SRs for the year through the projection_date  
projection_SR_count <- 3208332  

# Okabe-Ito palette for colorblind safe 
palette(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
          "#0072B2", "#D55E00", "#CC79A7", "#999999"))

#########################################################################
# ------------------------------------------------------ -------
# 📦 INSTALL AND LOAD REQUIRED PACKAGES
# -------------------------------------------------------------
load_required_packages <- function(verbose = TRUE) {
  # Ordered to avoid common masking issues
  required_packages  <- c(
    "data.table",      # Load first - commonly masked functions
    "arrow",
    "fasttime",
    "lubridate",
    "here",
    "zoo",
    "ggplot2",         # Core plotting (scales is a dependency, auto-loaded)
    "ggpmisc",
    "ggpattern",
    "ggrastr",
    "qcc",
    "qicharts2",
    "grid",
    "gridExtra",
    "sf",              # Spatial - can mask some dplyr functions
    "stringr",         # Load before dplyr/tidyverse
    "stringdist",
    "dplyr",           # Load before tidyverse
    "tidyverse",       # Load late - masks many functions
    "scales",          # Load AFTER tidyverse to mask purrr::discard and readr::col_factor
    "bslib",
    "shiny",
    "DT",              # Load after dplyr to avoid confusion
    "gt",
    "styler",
    "rlang",
    "renv",
    "remotes"
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

# STANDALONE MODE: Use your manual absolute path
base_dir <- file.path(
  "C:",
  "Users",
  "David",
  "OneDrive",
  "Documents", 
  "datacleaningproject", 
  "journal_of_data_science", 
  "nyc_311_data_quality"
)

cat("Base directory:", base_dir, "\n")

# Define all paths relative to base_dir (works in both modes)
analytics_dir  <- file.path(base_dir, "analytics")
chart_dir     <- file.path(base_dir, "charts")
code_dir      <- file.path(base_dir, "code")
console_dir   <- file.path(base_dir, "console_output")
data_dir      <- file.path(base_dir, "data")
functions_dir <- file.path(base_dir, "code", "functions")
misc_dir      <- file.path(base_dir, "misc")
raw_data_dir  <- file.path(base_dir, "data", "raw_data")


# Create directories if they don't exist
dirs_to_create <- c(data_dir, chart_dir, console_dir)
for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  }
}

cat("\nDirectory paths set:\n")
cat("  Analytics", analytics_dir, "\n")
cat("  Charts:", chart_dir, "\n")
cat("  Code:", code_dir, "\n")
cat("  Console output:", console_dir, "\n")
cat("  Data:", data_dir, "\n")
cat("  Functions:", functions_dir, "\n")
cat("  Miscellaneous:", misc_dir, "\n")
cat("  Raw data:", raw_data_dir, "\n")

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
options(max.print = 100000)
options(warn = 2)  

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

cat("\nExecution begins at:", formattedStartTime)
message("\nExecution begins at:", formattedStartTime)

# Define the console output directory and file name.
console_output_file <- file.path(console_dir, "jds_quality_console.txt")

if (isTRUE(enable_sink)) {
  sink(console_output_file)
}

#########################################################################
# Load the USPS zipcode file
message("\nReading the USPS zipcode file.")

USPS_zipcode_file_path <- file.path(data_dir, zip_data_file )

usps_zipcodes <- readRDS(USPS_zipcode_file_path)
if (!is.data.table(usps_zipcodes)) setDT(usps_zipcodes)  # converts in place

#########################################################################
# Load the main 311 SR data file. Set the read & write paths.
message("\nReading the main 311 SR data file.")

main_data_file_path <- file.path( data_dir, main_data_file)

cleaned_data <- readRDS(main_data_file_path)
if (!is.data.table(cleaned_data)) setDT(cleaned_data)  # converts in place

num_rows_cleaned_data <- nrow(cleaned_data)
num_columns_cleaned_data <- ncol(cleaned_data)

# Guard: ensure column exists
stopifnot("unique_key" %in% names(cleaned_data))
setindex(cleaned_data, unique_key)   # no reorder; speeds joins/subsets on unique_key

# Vector of columns to keep
columns_to_keep <- c("unique_key",
                     "created_date",
                     "closed_date",
                     "incident_zip",
                     "status",
                     "resolution_action_updated_date",
                     "community_board",
                     "cross_street_1",
                     "intersection_street_1",
                     "cross_street_2",
                     "intersection_street_2",
                     "street_name",
                     "landmark",
                     "borough"
)

# Retain only those columns
cleaned_data <- cleaned_data[, ..columns_to_keep]

cat("\n\nDataset row count:", format(nrow(cleaned_data), big.mark=","))

# Extract the 10-character date after "AS_OF_"
max_closed_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)

# Convert to POSIXct format
max_closed_date <- as.POSIXct(max_closed_date, format = "%m-%d-%Y", 
                              tz = "America/New_York") + (23*3600 + 59*60 + 59)

cat("\n\nDataset Created Dates span from", 
    format(min(cleaned_data$created_date), "%Y-%m-%d %H:%M:%S"), "to", 
    format(max(cleaned_data$created_date), "%Y-%m-%d %H:%M:%S"))

################################################################################      
# Extract year & year-month for grouping
message("\nCreating the year-month field.")

cat("\n\nCreating year_month grouping...")
cleaned_data[, `:=`(
  year = lubridate::year(created_date),
  year_month = substr(as.character(created_date), 1, 7)
)]

cleaned_data[, .N, by = 
             .(year = lubridate::year(closed_date))][!is.na(year)][order(year)]

################################################################################

cat("\n\n**********DATA SUMMARY**********")

# assume cleaned_data is a data.table and created_date is POSIXct
tz_out  <-  "America/New_York"                    
fmt_ts  <- "%Y-%m-%d %H:%M:%S"
fmt_day <- "%Y-%m-%d"               

# Compute once
rng <- cleaned_data[, range(created_date, na.rm = TRUE)]  # POSIXct min,max
if (any(is.infinite(rng))) stop("created_date has no non-missing values")

earliest_date <- rng[1L]
latest_date   <- rng[2L]

# Timestamp strings
earliest_date_formatted <- format(earliest_date, fmt_ts, tz = tz_out)
latest_date_formatted   <- format(latest_date,   fmt_ts, tz = tz_out)

# Date-only strings for titles
earliest_title <- format(earliest_date, fmt_day, tz = tz_out)
latest_title   <- format(latest_date,   fmt_day, tz = tz_out)

################################################################################
# Probe right before function call
range(cleaned_data$created_date)
summary(attr(cleaned_data$created_date, "tzone"))

# Plot yearly growth of 311 SRs.
message("\nCreating yearly plot and statistics.")

yearly_bar_chart <- plot_annual_counts_with_projection(
  DT = cleaned_data,
  created_col = "created_date",
  estimate_flag = TRUE,
  estimate_date = projection_date,
  estimate_value = projection_SR_count,
  chart_dir = chart_dir,
  filename = "annual_trend_with_projection_bar_chart.pdf",
  title = "NYC 311 Service Requests by Year",
  subtitle = "w/2025 projection",
  include_projection_in_growth = TRUE,
  include_projection_in_stats  = TRUE,
  show_projection_bar = TRUE    
)


##############################################################################

# Data Conditions Analysis (inline)

##############################################################################
# Calculate the minimum monthly count to use as sample size
total_per_month <- cleaned_data[, .N, by = year_month]
min_count <- min(total_per_month$N)

# Print the counts per year_month
cat("\nMonthly Record Counts:\n")
cat(sprintf("%-15s %12s\n", "Year_Month", "Count"))
cat(strrep("-", 28), "\n")
for(i in 1:nrow(total_per_month)) {
  cat(sprintf("%-15s %12s\n", 
              total_per_month$year_month[i], 
              format(total_per_month$N[i], big.mark = ",")))
}

# Calculate statistics
min_month <- total_per_month[N == min_count, year_month]
mean_count <- mean(total_per_month$N)
sd_count <- sd(total_per_month$N)

# Print summary statistics
cat("\n")
cat(sprintf("Minimum monthly count: %s (occurring in %s)\n", 
            format(min_count, big.mark = ","), 
            min_month))
cat(sprintf("Average monthly count:  %s\n", 
            format(round(mean_count), big.mark = ",")))
cat(sprintf("Standard deviation:     %s\n", 
            format(round(sd_count), big.mark = ",")))
cat("\n")
####################
# Take random samples of equal size from each month
cat("\n=== SAMPLING DATA ===\n")
cat("Taking random samples of", format(min_count, big.mark = ","), 
    "records from each month for constant sample size...\n")

sampled_data <- data.table()

# Set seed for reproducibility
set.seed(42)

# Loop through each month and collect a sample of rows
sampled_list <- vector("list", length(unique(cleaned_data$year_month)))
i <- 1
for (month in unique(cleaned_data$year_month)) {
  month_data <- cleaned_data[year_month == month]
  month_sample <- month_data[sample(.N, min_count, replace = FALSE)]
  sampled_list[[i]] <- month_sample
  i <- i + 1
}
sampled_data <- rbindlist(sampled_list)

cat("Sampled data created with", format(nrow(sampled_data), big.mark = ","), 
    "total records\n")
cat("Number of months:", length(unique(sampled_data$year_month)), "\n\n")

####################
# Create indicators for all conditions (in both sampled and full data)
cat("=== CREATING CONDITION INDICATORS ===\n")

# First for sampled data
sampled_data[, SRs_with_a_midnight_closed_date := 
               ifelse(!is.na(closed_date), 
                      format(closed_date, "%H:%M:%S") == "00:00:00", FALSE)]

sampled_data[, SRs_with_a_resolution_update_date_occurring_before_creation := 
               ifelse(!is.na(resolution_action_updated_date) & !is.na(created_date), 
                      resolution_action_updated_date < created_date, FALSE)]

sampled_data[, SRs_with_not_closed_status_but_with_a_closed_date := 
               ifelse(!is.na(closed_date) & !is.na(status), 
                      !is.na(closed_date) & status != "CLOSED", FALSE)]

sampled_data[, SRs_with_status_set_to_closed_but_no_closed_date 
             := ifelse(!is.na(status), status == "CLOSED" 
                       & is.na(closed_date), FALSE)]

sampled_data[, SRs_with_late_resolution_updates := 
               ifelse(!is.na(closed_date) & !is.na(resolution_action_updated_date), 
                      as.numeric(difftime(resolution_action_updated_date, 
                                          closed_date, units = "days")) > 90, FALSE)]

sampled_data[, SRs_with_negative_or_zero_duration := 
               ifelse(!is.na(closed_date) & !is.na(created_date), 
                      closed_date <= created_date, FALSE)]

sampled_data[, SRs_with_cross_intersection_1_matching_intersection_street_1 := 
               ifelse(cross_street_1 != intersection_street_1, FALSE, TRUE)]

sampled_data[, SRs_with_cross_intersection_2_matching_intesection_street_2 := 
               ifelse(cross_street_2 != intersection_street_2, FALSE, TRUE)]

sampled_data[, SRs_wth_street_name_matching_landmark := 
               ifelse(street_name != landmark, FALSE, TRUE)]

cat("Condition indicators created for sampled data\n")

####################
# Now for full data
cleaned_data[, SRs_with_a_midnight_closed_date := 
               ifelse(!is.na(closed_date), 
                      format(closed_date, "%H:%M:%S") == "00:00:00", FALSE)]

cleaned_data[, SRs_with_a_resolution_update_date_occurring_before_creation := 
               ifelse(!is.na(resolution_action_updated_date) & 
                        !is.na(created_date), resolution_action_updated_date 
                      < created_date, FALSE)]

cleaned_data[, SRs_with_not_closed_status_but_with_a_closed_date := 
               ifelse(!is.na(closed_date) & !is.na(status), 
                      !is.na(closed_date) & status != "CLOSED", FALSE)]

cleaned_data[, SRs_with_status_set_to_closed_but_no_closed_date := 
               ifelse(!is.na(status), status == "CLOSED" & is.na(closed_date), FALSE)]

cleaned_data[, SRs_with_late_resolution_updates := ifelse(!is.na(closed_date) & 
                                                            !is.na(resolution_action_updated_date), 
                                                          as.numeric(difftime(resolution_action_updated_date, 
                                                                              closed_date, units = "days")) >90, FALSE)]

cleaned_data[, SRs_with_negative_or_zero_duration := 
               ifelse(!is.na(closed_date) & !is.na(created_date), 
                      closed_date <= created_date, FALSE)]

cleaned_data[, SRs_with_cross_intersection_1_matching_intersection_street_1 := 
               ifelse(cross_street_1 != intersection_street_1, FALSE, TRUE)]

cleaned_data[, SRs_with_cross_intersection_2_matching_intesection_street_2  := 
               ifelse(cross_street_2 != intersection_street_2, FALSE, TRUE)]

cleaned_data[, SRs_wth_street_name_matching_landmark  := 
               ifelse(street_name != landmark, FALSE, TRUE)]

cat("Condition indicators created for full data\n\n")

####################
# Create a list to store all results
data_condition_results <- list()

# Define conditions
conditions <- c(
  "SRs_with_a_midnight_closed_date", 
  "SRs_with_a_resolution_update_date_occurring_before_creation", 
  "SRs_with_not_closed_status_but_with_a_closed_date", 
  "SRs_with_status_set_to_closed_but_no_closed_date", 
  "SRs_with_late_resolution_updates", 
  "SRs_with_negative_or_zero_duration",
  "SRs_with_cross_intersection_1_matching_intersection_street_1",
  "SRs_with_cross_intersection_2_matching_intesection_street_2",
  "SRs_wth_street_name_matching_landmark"
)

cat("=== PROCESSING CONDITIONS ===\n")
cat("Total conditions to process:", length(conditions), "\n\n")

####################
# Process each condition for both sampled and full data
for (condition in conditions) {
  
  # 1. For sampled data (for QCC and QIC)
  sampled_result <- sampled_data[, .(
    count = sum(get(condition), na.rm = TRUE),
    N = .N,
    fraction = sum(get(condition), na.rm = TRUE) / .N
  ), by = year_month]
  
  # 2. For full data (for ggplot)
  full_result <- cleaned_data[, .(
    count = sum(get(condition), na.rm = TRUE),
    N = .N,
    fraction = sum(get(condition), na.rm = TRUE) / .N
  ), by = year_month]
  
  # Make sure the data is in chronological order
  sampled_result <- sampled_result[order(year_month)]
  full_result <- full_result[order(year_month)]
  
  # Convert year_month from character to Date for proper plotting
  sampled_result[, year_month := as.Date(paste0(year_month, "-01"))]
  full_result[, year_month := as.Date(paste0(year_month, "-01"))]
  
  # Store in results list
  data_condition_results[[paste0(condition, "_sampled")]] <- copy(sampled_result)
  data_condition_results[[condition]] <- copy(full_result)
  
  cat("Processed:", condition, "\n")
}

cat("\nAll conditions processed and stored in data_condition_results\n\n")

####################
# Define subtitles for each condition
subtitles <- list(
  SRs_with_a_midnight_closed_date = "Service requests with closed date timestamp at exactly midnight",
  SRs_with_a_resolution_update_date_occurring_before_creation = "Resolution update date occurs before creation date",
  SRs_with_not_closed_status_but_with_a_closed_date = "Status is not 'CLOSED' but has a closed date",
  SRs_with_status_set_to_closed_but_no_closed_date = "Status is 'CLOSED' but missing closed date",
  SRs_with_late_resolution_updates = "Resolution updated more than 90 days after closure",
  SRs_with_negative_or_zero_duration = "Closed date is before or equal to creation date",
  SRs_with_cross_intersection_1_matching_intersection_street_1 = "Cross street 1 matches intersection street 1",
  SRs_with_cross_intersection_2_matching_intesection_street_2 = "Cross street 2 matches intersection street 2",
  SRs_wth_street_name_matching_landmark = "Street name matches landmark"
)

####################
# Initialize summary storage
condition_summary <- list()

####################
# Create charts for each condition
cat("=== CREATING CHARTS ===\n")
cat("Generating ggplot and QCC charts for", length(conditions), "conditions\n")
cat("Charts will be saved to:", chart_dir, "\n\n")

for (condition in conditions) {
  
  full_data <- copy(data_condition_results[[condition]])
  sampled_data <- copy(data_condition_results[[paste0(condition, "_sampled")]])
  
  cat("\n\n======= CONDITION:", condition, "=======\n")
  cat("Total non-conforming (full data):", 
      format(sum(full_data$count), big.mark = ","), "\n")
  cat("Average monthly count:", 
      format(round(mean(full_data$count)), big.mark = ","), "\n")
  
  # === 1. ggplot chart ===
  cat("Creating ggplot chart...\n")
  result <- create_condition_plot(
    data = full_data,
    paste("Count of", gsub("_", " ", tools::toTitleCase(condition)), 
          "w/loess Fitting"), 
    "Non-conforming Count",
    subtitle = subtitles[[condition]],
    "count"
  )
  
  # Display in RStudio Plots pane
  print(result$plot)
  
  # Save to PDF with distinctive filename
  ggsave(paste0(chart_dir, "/ggplot_", condition, ".pdf"), 
         plot = result$plot, 
         width = 13, height = 8.5)
  cat("  Saved: ggplot_", condition, ".pdf\n", sep = "")
  
  condition_summary[[condition]] <- list(
    title = result$title,
    first_year_mean = result$first_year_mean,
    last_year_mean = result$last_year_mean
  )
  
  Sys.sleep(2)
  
  # === 2. QCC np-chart ===
  cat("Creating QCC np-chart...\n")
  chart_title <- paste("QCC np-chart of", gsub("_", " ", 
                                               tools::toTitleCase(condition)))
  
  # Create and display in RStudio Plots pane
  qcc_plot <- qcc(sampled_data$count,
                  sizes = sampled_data$N[1],
                  type = "np",
                  title = chart_title,
                  xlab = "Year-Month",
                  ylab = "Non-conforming Count",
                  labels = format(sampled_data$year_month, "%Y-%m"),
                  plot = TRUE)
  
  # Save to PDF with distinctive filename
  pdf(paste0(chart_dir, "/qcc_np_chart_", condition, ".pdf"),
      width = 13, height = 8.5)
  plot(qcc_plot, title = chart_title)
  dev.off()
  cat("  Saved: qcc_np_chart_", condition, ".pdf\n", sep = "")
  
  # Print QCC statistics
  cat("  Center line (np̄):", round(qcc_plot$center, 2), "\n")
  cat("  UCL:", round(qcc_plot$limits[2], 2), "\n")
  cat("  LCL:", round(qcc_plot$limits[1], 2), "\n")
  if(length(qcc_plot$violations$beyond.limits) > 0) {
    cat("  Out-of-control points:", length(qcc_plot$violations$beyond.limits), "\n")
  } else {
    cat("  Out-of-control points: 0\n")
  }
  
  Sys.sleep(2)
  
}

cat("\n\n=== CHART GENERATION COMPLETE ===\n")
cat("Total charts created:", length(conditions) * 2, "\n\n")

####################
# Create a data frame from the summaries
cat("=== CREATING SUMMARY DATAFRAME ===\n")
summary_df <- data.frame(
  condition = names(condition_summary),
  title = sapply(condition_summary, function(x) x$title),
  first_year_mean = sapply(condition_summary, function(x) x$first_year_mean),
  last_year_mean  = sapply(condition_summary, function(x) x$last_year_mean)
)

cat("Summary dataframe created with", nrow(summary_df), "conditions\n\n")

################################################################################

########## Zip Code Evaluation ##########

################################################################################
# Check for invalid zip codes in cleaned_data$incident_zip using USPSzipcodesOnly
valid_USPS_zipcodes <- as.list(usps_zipcodes$zip)

# # Field to include "agency" in the computed dataset
# valid_agencies <- unique(cleaned_data$agency)

# check for allowable values in the 'community_board' field
valid_community_boards <-
  c(
    "01 BRONX", "01 BROOKLYN", "01 MANHATTAN", "01 QUEENS", "01 STATEN ISLAND",
    "02 BRONX", "02 BROOKLYN", "02 MANHATTAN", "02 QUEENS", "02 STATEN ISLAND",
    "03 BRONX", "03 BROOKLYN", "03 MANHATTAN", "03 QUEENS", "03 STATEN ISLAND",
    "04 BRONX", "04 BROOKLYN", "04 MANHATTAN", "04 QUEENS",
    "05 BRONX", "05 BROOKLYN", "05 MANHATTAN", "05 QUEENS",
    "06 BRONX", "06 BROOKLYN", "06 MANHATTAN", "06 QUEENS",
    "07 BRONX", "07 BROOKLYN", "07 MANHATTAN", "07 QUEENS",
    "08 BRONX", "08 BROOKLYN", "08 MANHATTAN", "08 QUEENS",
    "09 BRONX", "09 BROOKLYN", "09 MANHATTAN", "09 QUEENS",
    "10 BRONX", "10 BROOKLYN", "10 MANHATTAN", "10 QUEENS",
    "11 BRONX", "11 BROOKLYN", "11 MANHATTAN", "11 QUEENS",
    "12 BRONX", "12 BROOKLYN", "12 MANHATTAN", "12 QUEENS",
                "13 BROOKLYN", 				   "13 QUEENS",
				"14 BROOKLYN", 				   "14 QUEENS",
				"15 BROOKLYN",
				"16 BROOKLYN",
				"17 BROOKLYN",
				"18 BROOKLYN",
    
	"UNSPECIFIED BRONX", 
	"UNSPECIFIED BROOKLYN", 
	"UNSPECIFIED MANHATTAN",
    "UNSPECIFIED QUEENS", 
	"UNSPECIFIED STATEN ISLAND",
    "0 UNSPECIFIED"
  )

# --- Centralized spec: field -> allowed values --------------------------------

valid_spec <- list(
#   agency                 = valid_agencies,        # Add this line
#   address_type           = valid_address_types,
#   status                 = valid_statuses,
#   borough                = valid_boroughs,
#   park_borough           = valid_boroughs,
#   taxi_company_borough   = valid_boroughs,
#   open_data_channel_type = valid_channels,
#   vehicle_type           = valid_vehicle_types,
#   community_board        = valid_community_boards,
  incident_zip           = valid_USPS_zipcodes
)

# --- Run all validations in one pass ------------------------------------------

all_validation_results <- lapply(names(valid_spec), function(fld) {
  validate_values(
    x             = cleaned_data[[fld]],
    allowed       = valid_spec[[fld]],
    field         = fld,
    ignore_case   = TRUE,
    use_fastmatch = TRUE,
    quiet         = FALSE
  )
})

names(all_validation_results) <- names(valid_spec)

################################################################################
################################################################################

# Step 1: Copy cleaned_data to preserve original
zip_check_data <- copy(cleaned_data)

# Step 2: Extract first 5 characters of incident_zip
zip_check_data[, incident_zip_5 := 
                 substr(trimws(as.character(incident_zip)), 1, 5)]

# Step 3: Identify and write malformed ZIPs (not 5 digits)
invalid_zip_table <- zip_check_data[
  !grepl("^\\d{5}$", incident_zip_5) | is.na(incident_zip_5),
  .N,
  by = .(original_incident_zip = incident_zip)
][order(-N)]

fwrite(invalid_zip_table, file = file.path(analytics_dir, "invalid_zip_codes.csv"))

# Step 4: Keep only rows with valid 5-digit ZIPs (formatted correctly)
zip_cleaned_data <- zip_check_data[grepl("^\\d{5}$", incident_zip_5)]
zip_cleaned_data[, incident_zip := incident_zip_5]  # overwrite incident_zip
zip_cleaned_data[, incident_zip_5 := NULL]          # drop helper column

# Step 5: Derive valid ZIP reference from data itself
valid_zips_clean <- unique(zip_cleaned_data[, .(zip = incident_zip)])


# Step 6: Run analyze_invalid_values
incident_zip_results <- analyze_invalid_values(
  dataset = zip_cleaned_data,
  field_name = "incident_zip",
  valid_values_list = usps_zipcodes,
  valid_field_name = "zip"
)

# Step 6: Run analyze_invalid_values
incident_zip_results <- analyze_invalid_values(
  dataset = zip_cleaned_data,
  field_name = "incident_zip",
  valid_values_list = usps_zipcodes,
  valid_field_name = "zip"
)

# Step 7: Extract results
incident_zip_full   <- incident_zip_results[[1]]
incident_zip_sample <- incident_zip_results[[2]]

# Step 8: Extract logically invalid ZIPs (those not in reference list)
# Ensure ZIP is character and trimmed
zip_cleaned_data[, incident_zip := trimws(as.character(incident_zip))]

# Filter: ZIPs not in the USPS reference list
invalid_zip_details <- zip_cleaned_data[!incident_zip %in% usps_zipcodes$zip]

# Summary table
invalid_zip_summary <- invalid_zip_details[, .N, by = .(incident_zip)][order(-N)]

# Save to disk
fwrite(invalid_zip_details, 
       file = file.path(analytics_dir, "invalid_zip_logical_details.csv"))
fwrite(invalid_zip_summary, 
       file = file.path(analytics_dir, "invalid_zip_logical_summary.csv"))

# Step 9: Create the year-month colums
# Ensure both are data.tables
setDT(incident_zip_full)
setDT(incident_zip_sample)

# Convert year_month to Date and sort
incident_zip_full[, year_month := as.Date(paste0(year_month, "-01"))]
incident_zip_sample[, year_month := as.Date(paste0(year_month, "-01"))]

setorder(incident_zip_full, year_month)
setorder(incident_zip_sample, year_month)

# Step 10: Generate the line chart
# Create ggplot for incident ZIPs
zip_gg_plot <- create_condition_plot(
  data = incident_zip_full,
  title = "Proportion of Invalid Incident ZIPs w/loess fitting",
  y_label = "Non-conforming Proportion",
  subtitle = "Incident ZIPs not found in USPS reference list",
  value_field = "fraction"
)

# Display plot
print(zip_gg_plot$plot)  # Use $plot to access ggplot object from list

# Save plot to PDF
file_path <- file.path(chart_dir, "invalid_incident_zips.pdf")
ggsave(file_path,
       plot = zip_gg_plot$plot,
       width = 13,
       height = 8.5)

# Pause to allow rendering
cat("\nWaiting 2 seconds between charts...\n")
Sys.sleep(2)

# Step 11: Generate the qcc p-chart
# Extract metadata from ggplot object
zip_title <- zip_gg_plot$title
zip_first_mean <- zip_gg_plot$first_year_mean
zip_last_mean  <- zip_gg_plot$last_year_mean

# Define variables for QCC
zip_count_data <- incident_zip_sample$count
zip_sample_sizes <- incident_zip_sample$N
zip_chart_title <- "QCC p-chart of Invalid Incident ZIPs"
zip_labels <- format(incident_zip_sample$year_month, "%Y-%m")

# Create and display QCC chart
zip_qcc_plot <- qcc(zip_count_data,
                    sizes = zip_sample_sizes,
                    type = "np",
                    title = zip_chart_title,
                    xlab = "Year-Month",
                    ylab = "Non-conforming Proportion",
                    labels = zip_labels,
                    plot = TRUE)

# Let plot render before next chart
cat("\nWaiting 2 seconds before continuing to next chart...\n")
Sys.sleep(2)

# Save QCC chart to PDF
pdf(file.path(chart_dir, "qcc_p_chart_invalid_incident_zips.pdf"),
    width = 13, height = 8.5)
plot(zip_qcc_plot, title = zip_chart_title)
dev.off()

################################################################################

########## Community Board Evaluation ##########

################################################################################
# Define valid community boards
valid_community_boards <- c(
  "01 BRONX", "01 BROOKLYN", "01 MANHATTAN", "01 QUEENS", "01 STATEN ISLAND",
  "02 BRONX", "02 BROOKLYN", "02 MANHATTAN", "02 QUEENS", "02 STATEN ISLAND",
  "03 BRONX", "03 BROOKLYN", "03 MANHATTAN", "03 QUEENS", "03 STATEN ISLAND",
  "04 BRONX", "04 BROOKLYN", "04 MANHATTAN", "04 QUEENS",
  "05 BRONX", "05 BROOKLYN", "05 MANHATTAN", "05 QUEENS",
  "06 BRONX", "06 BROOKLYN", "06 MANHATTAN", "06 QUEENS",
  "07 BRONX", "07 BROOKLYN", "07 MANHATTAN", "07 QUEENS",
  "08 BRONX", "08 BROOKLYN", "08 MANHATTAN", "08 QUEENS",
  "09 BRONX", "09 BROOKLYN", "09 MANHATTAN", "09 QUEENS",
  "10 BRONX", "10 BROOKLYN", "10 MANHATTAN", "10 QUEENS",
  "11 BRONX", "11 BROOKLYN", "11 MANHATTAN", "11 QUEENS",
  "12 BRONX", "12 BROOKLYN", "12 MANHATTAN", "12 QUEENS",
              "13 BROOKLYN",                 "13 QUEENS",
              "14 BROOKLYN",                 "14 QUEENS",
              "15 BROOKLYN",
              "16 BROOKLYN",
              "17 BROOKLYN",
              "18 BROOKLYN",
 
  "UNSPECIFIED BRONX", "UNSPECIFIED BROOKLYN", "UNSPECIFIED MANHATTAN",
  "UNSPECIFIED QUEENS", "UNSPECIFIED STATEN ISLAND",
  "0 UNSPECIFIED"
)

valid_cb <- data.table(cb = valid_community_boards)

#######################
community_board_results <- analyze_invalid_values(
  cleaned_data, 
  "community_board", 
  valid_cb, "cb")

# Extract the data
community_board_full <- community_board_results[[1]]
community_board_sample <- community_board_results[[2]]

# Convert year_month to Date
community_board_full$year_month <- 
  as.Date(paste0(community_board_full$year_month, "-01"))
community_board_sample$year_month <- 
  as.Date(paste0(community_board_sample$year_month, "-01"))

# Arrange by year_month from oldest to newest
community_board_full <- community_board_full %>% arrange(year_month)
community_board_sample <- community_board_sample %>% arrange(year_month)

####################### 
# Create ggplot for community boards
cb_gg_plot <- create_condition_plot(
  data = community_board_full,
  title = "Proportion of Invalid Community Boards w/loess fitting",
  y_label = "Non-conforming Proportion",
  subtitle = "Community Boards that do not exist",
  value_field = "fraction"
)

print(cb_gg_plot$plot)  # Note the $plot to access the plot from the return list

# Display and save the ggplot
file_path <- paste0(chart_dir, "/invalid_community_boards.pdf")
ggsave(file_path, 
       plot = cb_gg_plot$plot, 
       width = 13, 
       height = 8.5)

# Delay between charts
cat("\nWaiting 2 seconds between charts...\n")
Sys.sleep(2)

# Extract details from the plot result
cb_title <- cb_gg_plot$title
cb_first_mean <- cb_gg_plot$first_year_mean
cb_last_mean  <- cb_gg_plot$last_year_mean

# Create QCC chart for community boards
# Define variables explicitly
cb_count_data <- community_board_sample$count
cb_sample_sizes <- community_board_sample$N
cb_chart_title <- "QCC np-chart of Invalid Community Boards"
cb_labels <- format(community_board_sample$year_month, "%Y-%m")

# Create and display QCC chart directly
cb_qcc_plot <- qcc(cb_count_data,  # Use invalid_zips from the sample data
                   sizes = cb_sample_sizes,  # Use total_records as sizes
                   type = "p",
                   title = cb_chart_title,
                   xlab = "Year-Month",
                   ylab = "Non-conforming Proportion",
                   labels = cb_labels,
                   plot = TRUE)  # This will display the plot in RStudio

# Add a 2 second delay to let plots render completely
cat("\nWaiting 2 seconds before continuing to next chart...\n")
Sys.sleep(2)

# Save QCC chart - with explicit title
pdf(paste0(chart_dir, "/qcc_p_chart_invalid_community_boards.pdf"),
    width = 13, height = 8.5)
plot(cb_qcc_plot, title = cb_chart_title)
dev.off()

####################### Add zipcode and cb values to summary table.
# New conditions data frame
new_conditions_df <- data.frame(
  condition = c("invalid_zipcodes", "invalid_community_boards"),
  title = c(zip_title, cb_title),
  first_year_mean = c(zip_first_mean, cb_first_mean),
  last_year_mean  = c(zip_last_mean, cb_last_mean)
)

# Add delta and percentage columns
new_conditions_df$delta <- new_conditions_df$last_year_mean - 
  new_conditions_df$first_year_mean
new_conditions_df$percent_change <- round(
  100 * new_conditions_df$delta / new_conditions_df$first_year_mean, 2)

# Add delta and percentage columns
summary_df$delta <- summary_df$last_year_mean - summary_df$first_year_mean
summary_df$percent_change <- round(
  100 * summary_df$delta / summary_df$first_year_mean, 2)

# new_conditions_df <- new_conditions_df %>% select(-title)
# summary_df <- summary_df %>% select(-title)

# Append new conditions to summary_df
summary_df <- rbind(summary_df, new_conditions_df)

# Check and replace values > 500 with "N/A" in the percent_change column
summary_df$percent_change <- ifelse(summary_df$percent_change > 500, 
                                    "N/A", summary_df$percent_change)

# Save the final summary
write.csv(summary_df, paste0(analytics_dir, "condition_summaries.csv"), 
          row.names = FALSE)

# ###############################################################################
# # DST end-day computations. Clock is reset from 0159 => 0100.
# # Filter for rows that meet all three conditions
# 
# # Step 1: Extract DST end days from cleaned_data
# november_data <- cleaned_data %>%
#   filter(month(created_date) == 11)
# 
# dst_end_days <- november_data %>%
#   filter(day(created_date) <= 7, wday(created_date) == 1)
# 
# # Step 2: Filter to DST hour (01:00–01:59)
# dst_hour_data <- dst_end_days %>%
#   filter(hour(created_date) >= 1 & hour(created_date) < 2) %>%
#   mutate(dst_end_date = as.Date(created_date))
# 
# # Step 3a: Create full summary
# summary_data_dst_end <- dst_hour_data %>%
#   mutate(duration_minutes = 
#            as.numeric(difftime(closed_date, created_date, units = "mins"))) %>%
#   group_by(dst_end_date) %>%
#   summarize(
#     records_0100_0200 = n(),
#     records_closed_leq_created = 
#       sum(!is.na(closed_date) & closed_date <= created_date, na.rm = TRUE),
#     avg_negative_duration_minutes = 
#       if (sum(!is.na(closed_date) & closed_date <= created_date) > 0) {
#         mean(duration_minutes[!is.na(closed_date) 
#                               & closed_date <= created_date], na.rm = TRUE)
#       } else {
#         0
#       },
#     .groups = "drop"
#   ) %>%
#   mutate(
#     fraction_closed_leq_created = 
#                       ifelse(records_0100_0200 > 0,
#                       records_closed_leq_created / records_0100_0200, 0),
#                       abs_duration_minutes = abs(avg_negative_duration_minutes)
#   ) %>% arrange(dst_end_date)
# 
# # Step 3b: Determine sampling size
# sample_size <- dst_hour_data %>%
#   count(dst_end_date) %>%
#   summarize(min_n = min(n)) %>%
#   pull(min_n)
# 
# # Step 4: Create a stratified sample by dst_end_date (equal n per group)
# set.seed(42)
# 
# sampled_dst_end_data <- dst_hour_data %>%
#   group_by(dst_end_date) %>%
#   slice_sample(n = sample_size) %>%
#   ungroup()
# 
# # Step 5: Create summary for the sampled dataset
# sample_dst_end <- sampled_dst_end_data %>%
#   mutate(duration_minutes = as.numeric(difftime(closed_date, created_date, 
#                                                 units = "mins"))) %>%
#   group_by(dst_end_date) %>%
#   summarize(
#     records_0100_0200 = n(),
#     records_closed_leq_created = 
#       sum(!is.na(closed_date) & closed_date <= created_date, na.rm = TRUE),
#     avg_negative_duration_minutes = 
#       if (sum(!is.na(closed_date) & closed_date <= created_date) > 0) {
#         mean(duration_minutes[!is.na(closed_date) 
#                               & closed_date <= created_date], na.rm = TRUE)
#       } else {
#         0
#       },
#     .groups = "drop"
#   ) %>%
#   mutate(
#     fraction_closed_leq_created = 
#                         ifelse(records_0100_0200 > 0,
#                         records_closed_leq_created / records_0100_0200,0),
#                         abs_duration_minutes = abs(avg_negative_duration_minutes)
#   ) %>% arrange(dst_end_date)
# 
# summary_data_dst_end <- summary_data_dst_end %>%
#   mutate(
#     abs_duration_minutes = abs(avg_negative_duration_minutes),
#     duration_for_plot = abs_duration_minutes
#   )
# 
# stacked_data <- summary_data_dst_end %>%
#   mutate(
#     other_records = records_0100_0200 - records_closed_leq_created
#   ) %>%
#   pivot_longer(
#     cols = c(other_records, records_closed_leq_created),  # Order matters here
#     names_to = "record_type",
#     values_to = "count"
#   ) %>%
#   mutate(
#     record_type = factor(record_type, levels = 
#                            c("other_records", "records_closed_leq_created"))
#   )
# 
# ####################
# # ---- Plot Stacked Bar ----
# # Scale the duration values to match the count scale for plotting
# # Make sure your limits are high enough for all data
# # Calculate max values for both axes
# max_count <- max(summary_data_dst_end$records_0100_0200, na.rm = TRUE)
# max_count <- ceiling(max_count/100) * 100  # Round up to nearest 100
# 
# max_duration <- max(summary_data_dst_end$abs_duration_minutes, na.rm = TRUE)
# max_duration <- ceiling(max_duration/10) * 10  # Round up to nearest 10
# 
# # Define appropriate steps
# count_step <- max(1, round(max_count/5))  # 5 breaks for count axis
# duration_step <- max(1, round(max_duration/4))  # 4 breaks for duration axis
# 
# # Calculate the scale factor between count and duration
# scale_anchor <- max_count / max_duration
# 
# # Scale the duration values to match the count scale for plotting
# summary_data_dst_end$duration_for_plot <- 
#   summary_data_dst_end$abs_duration_minutes * scale_anchor
# 
# dst_end_chart_stacked <- ggplot(stacked_data, 
#                                 aes(x = as.factor(dst_end_date), y = count, 
#                                     fill = record_type)) +
#   
#   geom_bar(stat = "identity", position = "stack", width = 0.7) +
#   
#   # Plot average duration as points (optional)
#   geom_point(
#     data = summary_data_dst_end,
#     aes(x = as.factor(dst_end_date), y = duration_for_plot),
#     inherit.aes = FALSE,
#     color = "red3",
#     shape = 18,
#     size = 4
#   ) +
#   
#   # Duration text labels above the diamond points (optional)
#   geom_text(
#     data = summary_data_dst_end,
#     aes(x = as.factor(dst_end_date), y = duration_for_plot,
#         label = paste0("-", round(abs_duration_minutes, 1), " min")),
#     inherit.aes = FALSE,
#     vjust = -1,
#     size = 3.5,
#     color = "red3"
#   ) +
#   geom_text(
#     data = stacked_data %>% filter(record_type == "records_closed_leq_created"),
#     aes(x = as.factor(dst_end_date), y = count, label = count),
#     position = position_stack(vjust = 1),  # Top of the segment
#     inherit.aes = FALSE,
#     size = 3,
#     color = "black",
#     vjust = -0.2  # Slight nudge above the bar
#   ) +
#   geom_text(
#     data = summary_data_dst_end,
#     aes(x = as.factor(dst_end_date), 
#         y = records_0100_0200, 
#         label = records_0100_0200),
#     inherit.aes = FALSE,
#     vjust = -0.5,
#     size = 3,
#     color = "black"
#   ) +
#   
#   scale_fill_manual(
#     values = c(
#       "other_records" = "steelblue3",                     # Base
#       "records_closed_leq_created" = "darkorange"         # Overlay
#     ),
#     labels = c(
#       "other_records" = "Total 0100–0159 Records",
#       "records_closed_leq_created" = "Closed <= Created"
#     )
#   ) +
#   
#   scale_y_continuous(
#     name = "# of Records: 0100–0159 on DST end day",
#     breaks = seq(0, max_count, count_step),
#     limits = c(0, max_count),
#     sec.axis = sec_axis(
#       transform = ~ . / scale_anchor,
#       name = "Avg Duration minutes (closed <= created)",
#       breaks = seq(0, max_duration, duration_step)
#     )
#   ) +
#   
#   theme(
#     plot.title = element_text(size = 14, face = "bold", color = "black", 
#                               margin = margin(b = 10)),
#     plot.subtitle = element_text(size = 11, color = "black", 
#                                  margin = margin(b = 15)),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
#     panel.grid.major = element_line(color = "gray88", linewidth = 0.5),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "gray99", color = NA),
#     plot.background = element_rect(fill = "gray90", color = NA),
#     axis.text.x = element_text(angle = 40, hjust = 1, color = "black"),
#     axis.title.y = element_text(margin = margin(r = 10), color = "black"),
#     
#     # Secondary axis styling - all red
#     axis.title.y.right = element_text(color = "red3"),  # Red title
#     axis.text.y.right = element_text(color = "red3"),   # Red tick labels
#     axis.ticks.y.right = element_line(color = "red3"),  # Red tick marks
#     axis.line.y.right = element_line(color = "red3"),   # Red axis line
#     
#     plot.caption = element_text(hjust = 1, size = 9, color = "black"),
#     legend.position = "inside",
#     legend.position.inside = c(0.02, 0.98),
#     legend.justification.inside = c(0, 1),
#     legend.background = element_rect(fill = "gray95", color = "black", 
#                                      linewidth = 0.5),
#     legend.key = element_rect(fill = NA),
#     legend.title = element_text(face = "bold")
#   ) +
#   
#   labs(
#     title = "DST Ending Days Analysis",
#     subtitle = "Comparing 0100–0159 created records to early closures",
#     y = "# of Records: 0100-0159 on DST end day",
#     fill = "Record Type",
#     x = NULL 
#   )
# 
# print(dst_end_chart_stacked)
# cat("\nWaiting 2 seconds between charts...\n")
# Sys.sleep(2)
# 
# # ---- Export ----
# output_file <- file.path(chart_dir, "paired_bar_dst.pdf")
# ggsave(
#   filename = output_file,
#   plot = dst_end_chart_stacked,
#   device = "pdf",
#   width = 13,
#   height = 8.5,
#   units = "in"
# )
# 
# dst_end_chart <- 
#   create_condition_plot( data = summary_data_dst_end,
#               title = "Proportion of affected SRs on DST 'end-day' w/trendline",
#               y_label = "Affected Proportion",
#               subtitle = "SRs where closed_date <= created_date",
#               value_field = "fraction_closed_leq_created",
#               bias_value = 1,
#               date_field = "dst_end_date"
# )
# 
# print(dst_end_chart$plot)
# 
# cat("\nWaiting 2 seconds between charts...\n")
# Sys.sleep(2)
# 
# # ---- Export ----
# output_file <- file.path(chart_dir, "Affected_SRs_on DST_end-day.pdf")
# ggsave(
#   filename = output_file,
#   plot = dst_end_chart$plot,
#   device = "pdf",
#   width = 13,
#   height = 8.5,
#   units = "in"
# )
# 
# Create QCC chart for community boards
# Define variables explicitly
# Count of "defects" — i.e., closed <= created
dst_count_data <- sample_dst_end$records_closed_leq_created

# Sample size — i.e., all records from 01:00–01:59 on DST-end date
dst_sample_sizes <- sample_dst_end$records_0100_0200

# Title for the chart
dst_chart_title <- "QCC p-chart of DST-End Negative Durations"

# X-axis labels — use full date for clarity
dst_labels <- format(sample_dst_end$dst_end_date, "%Y-%m-%d")

# Create and display QCC chart directly
dst_end_qcc_plot <- qcc(
  data = dst_count_data,
  sizes = dst_sample_sizes,
  type = "p",
  labels = dst_labels,
  title = dst_chart_title,
  plot = TRUE  # This will display the plot in RStudio
)

# Add a 2 second delay to let plots render completely
cat("\nWaiting 2 seconds before continuing to next chart...\n")
Sys.sleep(2)

# Save QCC chart - with explicit title
pdf(paste0(chart_dir, "/QCC p-chart of DST-End with negative durations.pdf"),
    width = 13, height = 8.5)
plot(dst_end_qcc_plot, title = dst_chart_title)
# dev.off()
# 
# ################################################################################
# # DST start-day computations. Clock is moved forward from 0159 => 0300.
# # Filter for rows that meet conditions for DST start (spring forward)
# 
# # Step 1: Extract DST end days from cleaned_data
# march_data <- cleaned_data %>% filter(month(created_date) == 3)
# 
# dst_start_days <- march_data %>%
#   filter(day(created_date) > 7, , day(created_date) <= 14, 
#          wday(created_date) == 1 )
# 
# # Step 2: Filter to DST hour (01:00-01:59) and identify affected records
# dst_start_affected_records <- dst_start_days %>%
#   filter(hour(created_date) >= 1 & hour(created_date) < 2) %>%
#   mutate(
#     dst_start_date = as.Date(created_date),
#     duration_minutes = as.numeric(difftime(closed_date, created_date, 
#                                            units = "mins"))
#   )
# 
# # Step 3: Create summary dataset for records affected by DST start
# summary_data_dst_start <- if (nrow(dst_start_affected_records) == 0) {
#   # Handle edge case where no records exist
#   tibble(
#     dst_start_date = as.Date(character(0)),
#     records_0100_0200 = integer(0),
#     records_affected_by_dst = integer(0),
#     fraction_affected = numeric(0)
#   )
# } else {
#   # Normal case - process the records
#   dst_start_affected_records %>%
#     mutate(
#       affected_by_dst = is.na(closed_date) | 
#         (hour(closed_date) >= 3 & as.Date(closed_date) == as.Date(created_date))
#     ) %>%
#     group_by(dst_start_date) %>%
#     summarize(
#       records_0100_0200 = n(),
#       records_affected_by_dst = sum(affected_by_dst, na.rm = TRUE),
#       fraction_affected = sum(affected_by_dst, na.rm = TRUE) / n()
#     ) %>%
#     arrange(dst_start_date)
# }
# 
# # Step 4: Create summary for the sampled DST start dataset
# # Determine the minimum count of records across all DST start dates
# sample_size <- dst_start_affected_records %>%
#   count(dst_start_date) %>%
#   summarize(min_n = min(n)) %>%
#   pull(min_n)
# 
# # Step 4b: Create a stratified sample by dst_start_date (equal n per group)
# set.seed(42)
# 
# dst_start_sampled_records <- dst_start_affected_records %>%
#   group_by(dst_start_date) %>%
#   slice_sample(n = sample_size) %>%
#   ungroup()
# 
# # Step 5: Create summary for the sampled dataset
# sample_dst_start <- dst_start_sampled_records %>%
#   group_by(dst_start_date) %>%
#   summarize(
#     records_0100_0200 = n(),
#     records_affected_by_dst = sum(is.na(closed_date) | 
#                                 (hour(closed_date) >= 3 & 
#                                 as.Date(closed_date) == as.Date(created_date)), 
#                                 na.rm = TRUE), .groups = "drop"
#   ) %>%
#   mutate(
#     fraction_affected = ifelse(records_0100_0200 > 0,
#                              records_affected_by_dst / records_0100_0200, 0)
#   ) %>%
#   arrange(dst_start_date)
# 
# # Create stacked bar data
# stacked_data <- summary_data_dst_start %>%
#   mutate(
#     unaffected_records = records_0100_0200 - records_affected_by_dst
#   ) %>%
#   pivot_longer(
#     cols = c(unaffected_records, records_affected_by_dst),
#     names_to = "record_type",
#     values_to = "count"
#   ) %>%
#   mutate(
#     record_type = factor(record_type, levels = c("unaffected_records", 
#                                                  "records_affected_by_dst"))
#   )
# 
# # ---- Final plot (stacked bar version) ----
# dst_start_stacked_chart <- ggplot(stacked_data, aes(x = 
#                                           as.factor(dst_start_date), y = count, 
#                                           fill = record_type)) +
#   geom_bar(stat = "identity", position = "stack", width = 0.7) +
#   
#   # Show count of affected records
#   geom_text(
#     data = stacked_data %>% filter(record_type == "records_affected_by_dst"),
#     aes(x = as.factor(dst_start_date), y = count, label = count),
#     position = position_stack(vjust = 1),  # Top of the segment
#     inherit.aes = FALSE,
#     size = 3,
#     color = "black",
#     vjust = -0.2  # Slight nudge above the bar
#   ) +
#   
#   # Show total count
#   geom_text(
#     data = summary_data_dst_start,
#     aes(x = as.factor(dst_start_date), y = records_0100_0200, 
#         label = records_0100_0200),
#     inherit.aes = FALSE,
#     vjust = -0.5,
#     size = 3,
#     color = "black"
#   ) +
#   
#   scale_fill_manual(
#     values = c(
#       "unaffected_records" = "steelblue3",           
#       "records_affected_by_dst" = "darkorange"       
#     ),
#     labels = c(
#       "unaffected_records" = "Total 0100–0159 Records",
#       "records_affected_by_dst" = "Affected by DST (+1hr)"
#     )
#   ) +
#   
#   scale_y_continuous(
#     name = "# of Records: 0100–0159 on DST start day"
#   ) +
#   
#   theme(
#     plot.title = element_text(size = 14, face = "bold", color = "black", 
#                               margin = margin(b = 10)),
#     plot.subtitle = element_text(size = 11, color = "black", 
#                                  margin = margin(b = 15)),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
#     panel.grid.major = element_line(color = "gray88", linewidth = 0.5),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "gray99", color = NA),
#     plot.background = element_rect(fill = "gray90", color = NA),
#     axis.text.x = element_text(angle = 40, hjust = 1, color = "black"),
#     axis.title.y = element_text(margin = margin(r = 10), color = "black"),
#     plot.caption = element_text(hjust = 1, size = 9, color = "black"),
#     legend.position = "inside",
#     legend.position.inside = c(0.02, 0.98),
#     legend.justification.inside = c(0, 1),
#     legend.background = element_rect(fill = "gray95", color = "black", 
#                                      linewidth = 0.5),
#     legend.key = element_rect(fill = NA),
#     legend.title = element_text(face = "bold")
#   ) +
#   
#   labs(
#     title = "DST Starting Days Analysis",
#     subtitle = "Comparing 0100–0159 created records to those affected by 'spring forward'",
#     y = "# of Records: 0100-0159 on DST start day",
#     fill = "Record Type",
#     x = NULL
#   )
# 
# print(dst_start_stacked_chart)
# cat("\nWaiting 2 seconds between charts...\n")
# Sys.sleep(2)
# 
# # ---- Export ----
# output_file <- file.path(chart_dir, "paired_bar_dst.pdf")
# ggsave(
#   filename = output_file,
#   plot = dst_start_stacked_chart,
#   device = "pdf",
#   width = 13,
#   height = 8.5,
#   units = "in"
# )
# 
# dst_start_chart <- create_condition_plot( data = summary_data_dst_start,
#             title = "Proportion of affected SRs on DST 'start-day' w/trendline",
#             y_label = "Affected Proportion (X 1000)",
#             subtitle = "SRs where 1 hour added to closed_date",
#             value_field = "fraction_affected",
#             bias_value = 1,
#             date_field = "dst_start_date"
# )
# 
# print(dst_start_chart$plot)
# 
# cat("\nWaiting 2 seconds between charts...\n")
# Sys.sleep(2)
# 
# # ---- Export ----
# output_file <- file.path(chart_dir, "Affected_SRs_on DST_start-day.pdf")
# ggsave(
#   filename = output_file,
#   plot = dst_start_chart$plot,
#   device = "pdf",
#   width = 13,
#   height = 8.5,
#   units = "in"
# )
# 
# # Create QCC chart for community boards
# # Count of "defects" — i.e., closed <= created
# dst_count_data <- sample_dst_start$records_affected_by_dst
# 
# # Sample size — i.e., all records from 01:00–01:59 on DST-end date
# dst_sample_sizes <- sample_dst_start$records_0100_0200
# 
# # Title for the chart
# dst_chart_title <- "QCC p-chart of DST-Start-day affected SRs"
# 
# # X-axis labels — use full date for clarity
# dst_labels <- format(sample_dst_start$dst_start_date, "%Y-%m-%d")
# 
# # Create and display QCC chart directly
# dst_qcc_plot <- qcc(
#   data = dst_count_data,
#   sizes = dst_sample_sizes,
#   type = "p",
#   labels = dst_labels,
#   title = dst_chart_title,
#   plot = TRUE  # This will display the plot in RStudio
# )
# 
# # Add a 2 second delay to let plots render completely
# cat("\nWaiting 2 seconds before continuing to next chart...\n")
# Sys.sleep(2)
# 
# # Save QCC chart - with explicit title
# pdf(paste0(chart_dir, "/QCC p-chart of DST-Start-day affected SRs.pdf"),
#     width = 13, height = 8.5)
# plot(dst_qcc_plot, title = dst_chart_title)
# dev.off()

################################################################################
# Store the program end time and calculate the duration
programStop <- as.POSIXct(Sys.time())
formatted_end_time <- format(programStop, "%Y-%m-%d %H:%M:%S")

# Calculate the duration of the program (in seconds)
duration_seconds <- as.numeric(difftime(programStop, programStart,
                                        units = "secs"))

# Convert the duration to a formatted string (hours, minutes, and seconds)
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

cat("\n\n*****END OF PROGRAM*****\n")
cat("\n📅 Execution ends at:", formatted_end_time, "\n")
cat("\n⏱️ Program run-time:", duration_string, "\n")

################################################################################  