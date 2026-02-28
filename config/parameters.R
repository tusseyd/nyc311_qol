################################################################################
# NYC 311 DATA QUALITY ASSESSMENT - CONFIGURATION
# All parameters, paths, and constants
################################################################################

# =============================================================================
# FILE PATHS
# =============================================================================

# CHANGE TO (add the subdirectory path):
MAIN_DATA_FILE <- file.path("quality_data", 
                "4-year_311SR_01-01-2022_thru_12-31-2025_AS_OF_02-02-2026.rds")

USPS_ZIPCODE_FILE <- file.path("quality_data", "USPS_zipcodes.rds")

# =============================================================================
# WORKING DIRECTORY
# =============================================================================

WD_PATH <- file.path(
  "C:", "Users", "David", "OneDrive", "Documents",
  "datacleaningproject", "journal_of_data_science", "nyc_311_data_quality"
)

# =============================================================================
# ANALYSIS PARAMETERS
# =============================================================================

# Date range for quality assessment
QUALITY_START_DATE <- as.Date("2022-01-01")
QUALITY_END_DATE   <- as.Date("2025-12-31")

# SPC parameters
SPC_PERIOD <- "month"                # "month" or "day" or "week"
RECALC_LIMITS_EVERY <- 16            # Recalculate control limits every N periods
MIN_SAMPLE_SIZE <- 100               # Minimum sample size per period for reliable p-chart

# Minimum periods required for P-chart generation
MIN_PERIODS_FOR_CHART <- 20

# =============================================================================
# OUTPUT CONTROL
# =============================================================================

ENABLE_SINK <- FALSE
CONSOLE_FILENAME <- "revised_JDS_data_quality_console_output.txt"

# =============================================================================
# VISUALIZATION
# =============================================================================

# Okabe-Ito colorblind-safe palette
COLOR_PALETTE <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                   "#0072B2", "#D55E00", "#CC79A7", "#999999")

# Chart dimensions (inches)
CHART_WIDTH <- 11
CHART_HEIGHT <- 8.5

# =============================================================================
# DATA PROCESSING
# =============================================================================

# Timezone for temporal calculations
ANALYSIS_TIMEZONE <- "America/New_York"

# Columns to retain in analysis (memory optimization)
COLUMNS_TO_KEEP <- c(
  "created_date",
  "closed_date",
  "agency", 
  "problem_formerly_complaint_type",
  "incident_zip",
  "street_name", 
  "cross_street_1",
  "cross_street_2", 
  "intersection_street_1", 
  "intersection_street_2",
  "landmark",
  "status", 
  "due_date",
  "resolution_action_updated_date", 
  "community_board",
  "police_precinct",
  "borough"
)

# =============================================================================
# DIRECTORY STRUCTURE (ADAPTED TO YOUR SETUP)
# =============================================================================
# NOTE: These use your directory structure
# Paths are relative to WD_PATH

DIR_ANALYTICS <- "analytics"
DIR_CHARTS <- "charts"
DIR_SPC_CHARTS <- file.path("charts", "spc")          # NEW: subfolder for SPC charts
DIR_CODE <- "code"
DIR_CONSOLE <- "console_output"
DIR_DATA <- "data"
DIR_QUALITY_DATA <- file.path("data", "quality_data")  # NEW: subfolder for quality data files
DIR_QUALITY_METRICS <- file.path("data", "quality_metrics")  # NEW: calculated metrics
DIR_FUNCTIONS <- file.path("code", "functions")       # YOUR EXISTING
DIR_LIB <- file.path("code", "lib")                   # NEW: reusable functions
DIR_ANALYSIS <- file.path("code", "analysis")         # NEW: analysis scripts

# =============================================================================
# QUALITY THRESHOLDS
# =============================================================================

# Threshold for flagging high violation rates in summary
HIGH_VIOLATION_THRESHOLD <- 10  # percent

# Correlation threshold for trend classification
TREND_CORRELATION_THRESHOLD <- 0.1
