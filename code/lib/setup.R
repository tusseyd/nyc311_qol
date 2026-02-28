################################################################################
# SETUP FUNCTIONS
# Directory creation and project initialization utilities
# Adapted for existing directory structure
################################################################################

#' Create project directory structure
#' 
#' Creates any missing directories for the analysis.
#' Works with your existing analytics/, charts/, code/, console_output/, data/ structure.
#' Only creates new subdirectories as needed (spc/, quality_metrics/, lib/, analysis/)
#' 
#' @return Invisible NULL
setup_directory_structure <- function() {
  
  # Ensure working directory is set
  if (!exists("WD_PATH")) {
    stop("WD_PATH not defined. Source config/parameters.R first.")
  }
  
  setwd(WD_PATH)
  base_dir <- getwd()
  
  # Build full paths - includes both existing and new directories
  dirs_to_create <- c(
    file.path(base_dir, DIR_ANALYTICS),         # EXISTING
    file.path(base_dir, DIR_CHARTS),            # EXISTING
    file.path(base_dir, DIR_SPC_CHARTS),        # NEW: charts/spc/
    file.path(base_dir, DIR_CODE),              # EXISTING
    file.path(base_dir, DIR_CONSOLE),           # EXISTING
    file.path(base_dir, DIR_DATA),              # EXISTING
    file.path(base_dir, DIR_QUALITY_DATA),      # NEW: data/quality_data/
    file.path(base_dir, DIR_QUALITY_METRICS),   # NEW: data/quality_metrics/
    file.path(base_dir, DIR_FUNCTIONS),         # EXISTING: code/functions/
    file.path(base_dir, DIR_LIB),               # NEW: code/lib/
    file.path(base_dir, DIR_ANALYSIS)           # NEW: code/analysis/
  )
  
  # Create directories
  cat("\n=== VERIFYING DIRECTORY STRUCTURE ===\n")
  
  n_created <- 0
  n_existing <- 0
  
  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("✅ Created:   ", dir, "\n")
      n_created <- n_created + 1
    } else {
      cat("📁 Existing:  ", dir, "\n")
      n_existing <- n_existing + 1
    }
  }
  
  cat(sprintf("\nSummary: %d created, %d already existed\n", n_created, n_existing))
  cat("✓ Directory structure verified\n")
  
  invisible(NULL)
}


#' Get full directory paths
#' 
#' Returns a named list of full directory paths for use in analysis
#' 
#' @return Named list of directory paths
#' @examples
#' dirs <- get_directory_paths()
#' fwrite(data, file.path(dirs$analytics, "output.csv"))
get_directory_paths <- function() {
  
  base_dir <- getwd()
  
  list(
    base = base_dir,
    analytics = file.path(base_dir, DIR_ANALYTICS),
    charts = file.path(base_dir, DIR_CHARTS),
    spc_charts = file.path(base_dir, DIR_SPC_CHARTS),
    code = file.path(base_dir, DIR_CODE),
    console = file.path(base_dir, DIR_CONSOLE),
    data = file.path(base_dir, DIR_DATA),
    quality_data = file.path(base_dir, DIR_QUALITY_DATA),
    quality_metrics = file.path(base_dir, DIR_QUALITY_METRICS),
    functions = file.path(base_dir, DIR_FUNCTIONS),
    lib = file.path(base_dir, DIR_LIB),
    analysis = file.path(base_dir, DIR_ANALYSIS)
  )
}


#' Print directory summary
#' 
#' Displays all project directories for verification
print_directory_summary <- function() {
  
  dirs <- get_directory_paths()
  
  cat("\n=== PROJECT DIRECTORY STRUCTURE ===\n")
  cat(sprintf("Base:          %s\n", dirs$base))
  cat("\nExisting directories:\n")
  cat(sprintf("  Analytics:   %s\n", dirs$analytics))
  cat(sprintf("  Charts:      %s\n", dirs$charts))
  cat(sprintf("  Code:        %s\n", dirs$code))
  cat(sprintf("  Console:     %s\n", dirs$console))
  cat(sprintf("  Data:        %s\n", dirs$data))
  cat(sprintf("  Functions:   %s\n", dirs$functions))
  
  cat("\nNew subdirectories:\n")
  cat(sprintf("  SPC Charts:      %s\n", dirs$spc_charts))
  cat(sprintf("  Quality Data:    %s\n", dirs$quality_data))
  cat(sprintf("  Quality Metrics: %s\n", dirs$quality_metrics))
  cat(sprintf("  Lib:             %s\n", dirs$lib))
  cat(sprintf("  Analysis:        %s\n", dirs$analysis))
  cat("\n")
  
  invisible(NULL)
}
