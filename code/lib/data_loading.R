################################################################################
# DATA LOADING FUNCTIONS
# Functions for loading and preparing 311 data
################################################################################

#' Load USPS zipcode reference data
#' 
#' @param data_dir Directory containing the zipcode file
#' @param filename Name of the zipcode RDS file
#' @return data.table of USPS zipcodes
load_usps_zipcodes <- function(data_dir = NULL, filename = USPS_ZIPCODE_FILE) {
  
  if (is.null(data_dir)) {
    dirs <- get_directory_paths()
    data_dir <- dirs$data
  }
  
  file_path <- file.path(data_dir, filename)
  
  message("Loading USPS zipcode reference data...")
  
  if (!file.exists(file_path)) {
    stop(sprintf("Zipcode file not found: %s", file_path))
  }
  
  usps_zips <- readRDS(file_path)
  
  # Ensure it's a data.table
  if (!is.data.table(usps_zips)) {
    setDT(usps_zips)
  }
  
  cat(sprintf("  Rows: %s, Columns: %d\n", 
              format(nrow(usps_zips), big.mark = ","), 
              ncol(usps_zips)))
  
  return(usps_zips)
}


#' Load main 311 service request data
#' 
#' @param data_dir Directory containing the 311 data file
#' @param filename Name of the 311 data RDS file
#' @return data.table of 311 service requests
load_311_data <- function(data_dir = NULL, filename = MAIN_DATA_FILE) {
  
  if (is.null(data_dir)) {
    dirs <- get_directory_paths()
    data_dir <- dirs$data
  }
  
  file_path <- file.path(data_dir, filename)
  
  message("Loading 311 service request data...")
  
  if (!file.exists(file_path)) {
    stop(sprintf("311 data file not found: %s", file_path))
  }
  
  d311 <- readRDS(file_path)
  
  # Ensure it's a data.table
  if (!is.data.table(d311)) {
    setDT(d311)
  }
  
  cat(sprintf("  Rows: %s, Columns: %d\n", 
              format(nrow(d311), big.mark = ","), 
              ncol(d311)))
  
  # Verify required columns
  if (!"unique_key" %in% names(d311)) {
    stop("Required column 'unique_key' not found in 311 data")
  }
  
  # Set index for faster lookups
  setindex(d311, unique_key)
  
  return(d311)
}


#' Print data coverage summary
#' 
#' @param dt data.table with created_date column
print_data_coverage <- function(dt) {
  
  if (!"created_date" %in% names(dt)) {
    warning("created_date column not found")
    return(invisible(NULL))
  }
  
  min_date <- suppressWarnings(min(dt$created_date, na.rm = TRUE))
  max_date <- suppressWarnings(max(dt$created_date, na.rm = TRUE))
  
  if (!is.finite(min_date) || !is.finite(max_date)) {
    warning("created_date has no finite values")
    return(invisible(NULL))
  }
  
  cat("\n", strrep("=", 80), "\n")
  cat("DATA COVERAGE SUMMARY\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("Date range: %s to %s\n", 
              format(min_date, "%Y-%m-%d %H:%M:%S %Z"), 
              format(max_date, "%Y-%m-%d %H:%M:%S %Z")))
  cat(sprintf("Total records: %s\n", format(nrow(dt), big.mark = ",")))
  cat(sprintf("Time span: %.1f years\n", 
              as.numeric(difftime(max_date, min_date, units = "days")) / 365.25))
  cat(strrep("=", 80), "\n\n")
  
  invisible(NULL)
}


#' Reduce dataset columns for memory efficiency
#' 
#' Keeps only columns specified in COLUMNS_TO_KEEP config parameter
#' 
#' @param dt data.table to filter
#' @param columns_to_keep Character vector of column names (default from config)
#' @param verbose Logical, whether to print memory savings
#' @return data.table with only specified columns (modified in place)
reduce_columns <- function(dt, columns_to_keep = COLUMNS_TO_KEEP, verbose = TRUE) {
  
  if (verbose) {
    message("\nReducing columns for memory efficiency...")
    
    # Memory before
    mem_before <- as.numeric(object.size(dt)) / 1024^2  # MB
    cols_before <- ncol(dt)
  }
  
  # Verify columns exist
  missing_cols <- setdiff(columns_to_keep, names(dt))
  if (length(missing_cols) > 0) {
    warning(sprintf("Requested columns not found: %s", 
                   paste(missing_cols, collapse = ", ")))
    columns_to_keep <- intersect(columns_to_keep, names(dt))
  }
  
  # Remove columns not in keep list
  cols_to_remove <- setdiff(names(dt), columns_to_keep)
  dt[, (cols_to_remove) := NULL]
  
  # Force garbage collection
  gc(verbose = FALSE)
  
  if (verbose) {
    mem_after <- as.numeric(object.size(dt)) / 1024^2  # MB
    cols_after <- ncol(dt)
    
    cat(sprintf("  Columns: %d → %d (removed %d)\n", 
                cols_before, cols_after, cols_before - cols_after))
    cat(sprintf("  Memory:  %.1f MB → %.1f MB (saved %.1f MB)\n", 
                mem_before, mem_after, mem_before - mem_after))
  }
  
  invisible(dt)
}
