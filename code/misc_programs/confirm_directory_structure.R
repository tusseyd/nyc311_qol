# Set working directory to your main project folder
setwd("C:/Users/David/OneDrive/Documents/datacleaningproject/journal_of_data_science/nyc_311_data_quality")

# Test 1: Load configuration
cat("=== TEST 1: Loading Configuration ===\n")
source("config/parameters.R")
cat("✓ Configuration loaded\n")
cat("Working directory:", WD_PATH, "\n\n")

# Test 2: Create directory structure
cat("=== TEST 2: Setting Up Directories ===\n")
source("code/lib/setup.R")
setup_directory_structure()
cat("\n")

# Test 3: Get directory paths
cat("=== TEST 3: Directory Paths ===\n")
dirs <- get_directory_paths()
print_directory_summary()

# Test 4: Load function libraries
cat("=== TEST 4: Loading Function Libraries ===\n")
source("code/lib/data_loading.R")
cat("✓ data_loading.Rdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC loaded\n")
source("code/lib/spc_analysis.R")
cat("✓ spc_analysis.R loaded\n")
source("code/lib/reporting.R")
cat("✓ reporting.R loaded\n\n")

# Test 5: Check that your existing functions are accessible
cat("=== TEST 5: Checking Existing Functions ===\n")
if (file.exists(file.path(dirs$functions, "setup_project.R"))) {
  cat("✓ setup_project.R found\n")
} else {
  cat("✗ setup_project.R NOT FOUND\n")
}

if (file.exists(file.path(dirs$functions, "define_all_metrics.R"))) {
  cat("✓ define_all_metrics.R found\n")
} else {
  cat("✗ define_all_metrics.R NOT FOUND\n")
}

if (file.exists(file.path(dirs$functions, "calculate_durations.R"))) {
  cat("✓ calculate_durations.R found\n")
} else {
  cat("✗ calculate_durations.R NOT FOUND\n")
}

if (file.exists(file.path(dirs$functions, "close_program.R"))) {
  cat("✓ close_program.R found\n")
} else {
  cat("✗ close_program.R NOT FOUND\n")
}

# Test 6: Check data files
cat("\n=== TEST 6: Checking Data Files ===\n")
if (file.exists(file.path(dirs$data, MAIN_DATA_FILE))) {
  cat("✓ Main 311 data file found:", MAIN_DATA_FILE, "\n")
} else {
  cat("✗ Main 311 data file NOT FOUND:", MAIN_DATA_FILE, "\n")
  cat("  Expected location:", file.path(dirs$data, MAIN_DATA_FILE), "\n")
}

if (file.exists(file.path(dirs$data, USPS_ZIPCODE_FILE))) {
  cat("✓ USPS zipcode file found:", USPS_ZIPCODE_FILE, "\n")
} else {
  cat("✗ USPS zipcode file NOT FOUND:", USPS_ZIPCODE_FILE, "\n")
  cat("  Expected location:", file.path(dirs$data, USPS_ZIPCODE_FILE), "\n")
}

cat("\n=== SETUP VERIFICATION COMPLETE ===\n")
cat("If all tests passed, you're ready to run the analysis!\n")


