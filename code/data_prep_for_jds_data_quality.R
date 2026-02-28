################################################################################
# Read-in raw csv data (main_data_file)
# Convert text columns to upper case
# Convert date columns to POSIXct format
# Modify and standardize column names
# Check for presence of mandatory fields
# Combine NYC Agencies to accommodate name changes
# Replace missing values with NA for standardization
# Write out two files in RDS format and one in CSV

################################################################################
main_data_file <- "2-year_311SR_01-01-2020_thru_12-31-2021_AS_OF_02-20-2026.csv"

# Boolean flag. TRUE to redirect console output to text file
# FALSE to display console output on the screen
enable_sink <- FALSE      

# Okabe-Ito palette for colorblind safe 
palette(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
          "#0072B2", "#D55E00", "#CC79A7", "#999999"))

################################################################################
# ------------------------------------------------------ -------
# 📦 CREATE REQUIRED DIRECTORY STRUCTURE
# -------------------------------------------------------------
################################################################################

message("\n", strrep("=", 80))
message("NYC 311 DATA QUALITY ANALYSIS - DATA PREPARATION")
message(strrep("=", 80), "\n")

# STEP 1: Create directory structure (inline)
message("STEP 1: Setting up directory structure...")

# Set base directory to current working directory
base_dir <- "C:\\Users\\David\\OneDrive\\Documents\\datacleaningproject\\journal_of_data_science\\nyc_311_data_quality"

cat("Base directory:", base_dir, "\n")

# Define all paths relative to base_dir
analytics_dir <- file.path(base_dir, "analytics")
chart_dir     <- file.path(base_dir, "charts")
code_dir      <- file.path(base_dir, "code")
console_dir   <- file.path(base_dir, "console_output")
data_dir      <- file.path(base_dir, "data")
functions_dir <- file.path(base_dir, "code", "functions")
raw_data_dir  <- file.path(base_dir, "data", "raw_data")

dirs_to_create <- c(analytics_dir, chart_dir, code_dir, console_dir, 
                    data_dir, functions_dir, raw_data_dir)

for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("✅ Created directory:", dir, "\n")
  } else {
    cat("📁 Directory exists:", dir, "\n")
  }
}

cat("\nDirectory paths set:\n")
cat("  Analytics:", analytics_dir, "\n")
cat("  Charts:", chart_dir, "\n")
cat("  Code:", code_dir, "\n")
cat("  Console output:", console_dir, "\n")
cat("  Data:", data_dir, "\n")
cat("  Functions:", functions_dir, "\n")
cat("  Raw data:", raw_data_dir, "\n")

message("✓ Directory structure complete\n")

################################################################################
# STEP 2: Source and run setup function
message("STEP 2: Running project setup...")

source(file.path(functions_dir, "setup_project.R"))

timing <- setup_project(
  enable_sink = enable_sink,
  console_filename = "JDS_data_prep_for_quality_console_output.txt",
  functions_dir = functions_dir,
  verbose = TRUE
)

message("✓ Project setup complete\n")

################################################################################
# STEP 3: Read raw 311 data
################################################################################

message("STEP 3: Reading raw 311 Service Request data...")

valid_date_columns <- c(
  "Created Date",
  "Closed Date",
  "Due Date",
  "Resolution Action Updated Date"
)

main_data_path <- file.path(raw_data_dir, main_data_file)

# When first reading the file
raw_data <- fread(
  main_data_path,
  nThread       = parallel::detectCores() - 1,
  check.names   = FALSE,
  strip.white   = TRUE,
  showProgress  = TRUE,
  colClasses    = "character"
)

num_rows_raw_data <- nrow(raw_data)
cat(sprintf("\n✓ Raw data loaded: %s rows, %d columns\n",
            format(num_rows_raw_data, big.mark = ","),
            ncol(raw_data)))

################################################################################
# STEP 4: Standardize column names
message("\nSTEP 4: Standardizing column names...")

raw_data <- modify_column_names(raw_data)

message("✓ Column names standardized\n")

################################################################################
# STEP 5: Standardize missing data representation
message("STEP 5: Standardizing missing data representation...")

standardize_missing_chars(raw_data)

message("✓ Missing data standardized to NA\n")

################################################################################
# STEP 6: Check and enforce mandatory fields
message("STEP 6: Checking mandatory fields...")

mandatory_fields <- c(
  "unique_key",
  "created_date",
  "agency",
  "complaint_type",
  "status"
)

# Keep only those fields that actually exist in the data
present_mandatory_fields <- intersect(mandatory_fields, names(raw_data))

# Remove rows with NA or blank values in any present mandatory field
for (field in present_mandatory_fields) {
  raw_data <- raw_data[!is.na(get(field)) & trimws(get(field)) != ""]
}

removed_rows <- num_rows_raw_data - nrow(raw_data)

if (removed_rows > 0) {
  cat(sprintf("  Rows removed: %s\n", format(removed_rows, big.mark = ",")))
  cat(sprintf("  Remaining rows: %s\n", format(nrow(raw_data), big.mark = ",")))
} else {
  cat("  No rows removed\n")
}

message("✓ Mandatory field check complete\n")

################################################################################
# STEP 7: Consolidate agencies
message("STEP 7: Consolidating agency names...")

raw_data <- consolidate_agencies(DT = raw_data, drop_agencies = NULL)

message("✓ Agencies consolidated\n")

################################################################################
# STEP 8: Check and parse date fields
message("STEP 8: Checking and parsing date fields...")

# Define date columns to be converted to POSIXct format
date_columns <- c(
  "created_date",
  "closed_date",
  "due_date",
  "resolution_action_updated_date"
)

# Check date fields
summary_dt <- date_checks_character(
  DT = raw_data,
  date_cols = date_columns
)

# Parse dates
result <- parse_date_column(
  temp_raw_data      = raw_data,
  valid_date_columns = date_columns
)

raw_data <- result$parsed_data
failed_to_parse <- result$failures
parsed_summmary <- result$summary

rm(result)
gc()

message("✓ Date fields parsed and converted\n")

################################################################################
# STEP 9: Convert text columns to uppercase
message("STEP 9: Converting text columns to uppercase...")

# Define columns containing text to convert to uppercase
columns_to_upper <- c(
  "address_type",
  "agency",
  "agency_name",
  "borough",
  "bridge_highway_direction",
  "bridge_highway_name",
  "bridge_highway_segment",
  "city",
  "community_board",
  "problem_formerly_complaint_type",
  "cross_street_1",
  "cross_street_2",
  "problem_detail_formerly_descriptor",
  "facility_type",
  "incident_address",
  "intersection_street_1",
  "intersection_street_2",
  "landmark",
  "location_type",
  "open_data_channel_type",
  "park_borough",
  "park_facility_name",
  "police_precinct",
  "resolution_description",
  "road_ramp",
  "status",
  "street_name",
  "taxi_company_borough",
  "taxi_pick_up_location",
  "vehicle_type"
)

# Step 1: Ensure columns where text to be converted actually exist in dataset
existing_cols <- columns_to_upper[columns_to_upper %in% names(raw_data)]

# Warn about any missing columns
missing_cols <- setdiff(columns_to_upper, existing_cols)
if (length(missing_cols) > 0) {
  cat("\n[WARNING] The following columns were not found in raw_data:\n",
      paste(" -", missing_cols, collapse = "\n"), "\n")
} 

# Step 2: Keep only those that are character columns
valid_cols <- existing_cols[sapply(raw_data[, ..existing_cols], is.character)]

# Step 3: Convert to uppercase
for (col in valid_cols) {
  raw_data[, (col) := toupper(get(col))]
}

cat(sprintf("  ✓ %d columns converted to uppercase\n", length(valid_cols)))
message("✓ Text conversion complete\n")

################################################################################
# STEP 10: Extract data coverage information
message("STEP 10: Analyzing data coverage...")

# Extract AS_OF date from filename
as_of_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)
local_tz <- "America/New_York"

# Coverage from the data
min_date <- suppressWarnings(min(raw_data$created_date, na.rm = TRUE))
max_date <- suppressWarnings(max(raw_data$created_date, na.rm = TRUE))

if (!is.finite(min_date) || !is.finite(max_date)) 
  stop("created_date has no finite values.")

min_date <- as.POSIXct(min_date, tz = local_tz)
max_date <- as.POSIXct(max_date, tz = local_tz)

cat("\n", strrep("=", 80), "\n")
cat("DATA COVERAGE SUMMARY\n")
cat(strrep("=", 80), "\n")
cat(sprintf("Full dataset range: %s to %s\n", 
            format(min_date, "%Y-%m-%d %H:%M:%S %Z"), 
            format(max_date, "%Y-%m-%d %H:%M:%S %Z")))

cat(sprintf("Data AS_OF date: %s\n", as_of_date))

# Spans to generate
selected_year_spans <- c(2, 4)
cat(sprintf("Selected year spans: %s\n", paste(selected_year_spans, collapse = ", ")))

# Ensure output directory exists
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

# Find max complete year (exclude current partial year)
current_year <- year(Sys.Date())
max_complete_year <- raw_data[year(created_date) < current_year, 
                              year(max(created_date, na.rm = TRUE))]
date_tz <- attr(raw_data$created_date, "tzone")

cat(sprintf("Current year: %d\n", current_year))
cat(sprintf("Max complete year: %d\n", max_complete_year))
cat(sprintf("Timezone: %s\n", date_tz))

message("\n✓ Data coverage analysis complete\n")

################################################################################
# STEP 11: Process and save year-span datasets
message("STEP 11: Processing year-span datasets...")

# Collect results for summary
summary_list <- list()

cat("\n", strrep("=", 80), "\n")
cat("PROCESSING YEAR SPANS\n")
cat(strrep("=", 80), "\n")

for (span in selected_year_spans) {
  start_date <- as.POSIXct(
    sprintf("%d-01-01 00:00:00", max_complete_year - span + 1), tz = date_tz
  )
  end_date <- as.POSIXct(
    sprintf("%d-01-01 00:00:00", max_complete_year + 1), tz = date_tz
  )
  
  cat("\n", strrep("-", 80), "\n")
  cat(sprintf("Processing %d-year span\n", span))
  cat(strrep("-", 80), "\n")
  cat(sprintf("Year range: %d-%d\n", 
              max_complete_year - span + 1, 
              max_complete_year))
  cat(sprintf("Date range: %s to %s\n",
              format(start_date, "%Y-%m-%d %H:%M:%S %Z"),
              format(end_date, "%Y-%m-%d %H:%M:%S %Z")))
  
  filtered_data <- raw_data[created_date >= start_date & created_date < end_date]
  
  if (nrow(filtered_data) == 0L) {
    cat(sprintf("WARNING: No data found for %d-year span [%s to %s]\n",
                span,
                format(start_date, "%Y-%m-%d"),
                format(end_date, "%Y-%m-%d")))
    next
  }
  
  cat(sprintf("Records filtered: %s\n", format(nrow(filtered_data), big.mark = ",")))
  
  s_date <- format(start_date, "%m-%d-%Y")
  e_date <- format(max(filtered_data$created_date), "%m-%d-%Y")
  
  file_name <- sprintf(
    "%d-year_311SR_%s_thru_%s_AS_OF_%s.rds",
    span, s_date, e_date, as_of_date
  )
  full_path <- file.path(data_dir, file_name)
  
  cat(sprintf("Output file: %s\n", file_name))
  
  # Save and verify
  save_and_verify(dt = filtered_data, path = full_path)
  
  # Collect summary
  summary_list[[length(summary_list) + 1]] <- data.table(
    span_years = span,
    years_included = sprintf("%d–%d", max_complete_year - span + 1, 
                             max_complete_year),
    start_date = format(start_date, "%Y-%m-%d"),
    end_date   = format(end_date - 1, "%Y-%m-%d"),
    rows       = nrow(filtered_data),
    file       = basename(full_path)
  )
}

# Summary
if (length(summary_list) > 0) {
  summary_dt <- rbindlist(summary_list)
  summary_dt[, rows := prettyNum(rows, big.mark = ",")]
  
  cat("\n", strrep("=", 80), "\n")
  cat("SUMMARY OF GENERATED DATASETS\n")
  cat(strrep("=", 80), "\n")
  print(summary_dt)
}

message("\n✓ Year-span datasets complete\n")

################################################################################
# STEP 12: Process USPS zipcode data
message("STEP 12: Processing USPS zipcode data...")

usps_data_file <- "zip_code_database.csv"
usps_path      <- file.path(raw_data_dir, usps_data_file)
usps_rds_file  <- file.path(data_dir, "USPS_zipcodes.rds")

if (!file.exists(usps_path)) {
  stop("USPS CSV not found at: ", usps_path)
}

# Read zipcode data
zipcode_data <- tryCatch(
  fread(
    usps_path,
    select      = "zip",
    colClasses  = c(zip = "character"),
    nThread     = max(1L, parallel::detectCores() - 1L),
    check.names = FALSE,
    strip.white = TRUE,
    showProgress = TRUE
  ),
  error = function(e) {
    hdr <- names(fread(usps_path, nrows = 0, check.names = FALSE, 
                       showProgress = TRUE))
    cand <- grep("^zip(code)?$", hdr, ignore.case = TRUE, value = TRUE)
    if (length(cand) == 0L) 
      stop("No 'zip' column found (case-insensitive) in USPS CSV.")
    fread(
      usps_path,
      select      = cand[1],
      colClasses  = setNames("character", cand[1]),
      nThread     = max(1L, parallel::detectCores() - 1L),
      check.names = FALSE,
      strip.white = TRUE,
      showProgress = TRUE
    )
  }
)

# Ensure the column is named exactly 'zip'
if (!identical(names(zipcode_data), "zip")) {
  setnames(zipcode_data, 1L, "zip")
}

# Light cleanup
zipcode_data[, zip := trimws(zip)]

# Save
saveRDS(zipcode_data[, .(zip)], usps_rds_file)

cat(sprintf("  ✓ USPS zipcodes saved: %s records\n", 
            format(nrow(zipcode_data), big.mark = ",")))
cat(sprintf("  Output file: %s\n", basename(usps_rds_file)))

message("✓ USPS zipcode processing complete\n")

################################################################################
# STEP 13: Close program
message(strrep("=", 80))
message("DATA PREPARATION COMPLETE")
message(strrep("=", 80), "\n")

close_program(
  program_start = timing$program_start,
  enable_sink = enable_sink,
  verbose = TRUE
)

################################################################################