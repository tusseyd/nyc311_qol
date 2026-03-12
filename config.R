# =============================================================================
# config.R
# NYC 311 Quality of Life Index
#
# Central configuration file — all user-defined parameters live here.
# Every script in the pipeline sources this file at startup, so changes
# made here propagate automatically across the entire project.
#
# SECTIONS:
#   1. Project Paths    — all input/output file locations
#   2. Study Periods    — Phase I baseline and Phase II monitoring date ranges
#   3. Geographic Scope — citywide analysis (borough-level reserved for future)
#   4. Complaint Bundles — nine QoL bundles and their display labels
#   5. SPC Parameters   — control chart settings (sigma, subgroup size)
#   6. Output Settings  — chart format, dimensions, and table format
#   7. Validation       — confirms project root exists before pipeline runs
#
# TO ADD A NEW YEAR OF MONITOR DATA:
#   Update raw_monitor path in PATHS and extend PHASE2_YEARS and PHASE2_END.
#
# TO CHANGE CHART OUTPUT FORMAT OR DIMENSIONS:
#   Edit OUTPUT$plot_format, OUTPUT$plot_width, OUTPUT$plot_height.
#   All ggsave() calls reference these values — no other changes needed.
#
# TO ADD A NEW BUNDLE:
#   Add an entry to BUNDLES, then update complaint_hierarchy.csv and
#   re-run 00_build_classification_master.R.
# =============================================================================
# -----------------------------------------------------------------------------
# 1. PROJECT PATHS
# -----------------------------------------------------------------------------

PROJECT_ROOT <- file.path(
  "C:", "Users", "David", "OneDrive", "Documents",
  "datacleaningproject", "nyc311_qol"
)

PATHS <- list(
  # Raw input data
  raw_baseline  = file.path(PROJECT_ROOT, "data", "raw",
                            "1-year_311SR_01-01-2021_thru_12-31-2021_AS_OF_03-08-2026.csv"),
  raw_monitor   = file.path(PROJECT_ROOT, "data", "raw",
                            "4-year_311SR_01-01-2022_thru_12-31-2025_AS_OF_02-02-2026.csv"),
  raw_2026      = file.path(PROJECT_ROOT, "data", "raw",
                            "2026_ytd_311SR.csv"),
  # Processed / intermediate data
  processed_baseline  = file.path(PROJECT_ROOT, "output", "data", "prepared_baseline.RDS"),
  processed_monitor   = file.path(PROJECT_ROOT, "output", "data", "prepared_monitor.RDS"),
  processed_2026      = file.path(PROJECT_ROOT, "output", "data", "prepared_2026.RDS"),
  baseline_stats      = file.path(PROJECT_ROOT, "output", "data", "baseline_stats.RDS"),
  analysis_results    = file.path(PROJECT_ROOT, "output", "data", "analysis_results.RDS"),
  data_out            = file.path(PROJECT_ROOT, "output", "data"),
  complaint_hierarchy = file.path(PROJECT_ROOT, "data", "reference", "complaint_hierarchy.csv"),
  ref_classification  = file.path(PROJECT_ROOT, "output", "reference",
                                  "complaint_classification.xlsx"),
  # Outputs
  plots          = file.path(PROJECT_ROOT, "output", "plots"),
  plots_family   = file.path(PROJECT_ROOT, "output", "plots", "family"),
  plots_detail   = file.path(PROJECT_ROOT, "output", "plots", "detail"),
  plots_heatmap  = file.path(PROJECT_ROOT, "output", "plots", "heatmap"),
  plots_emerging = file.path(PROJECT_ROOT, "output", "plots", "emerging"),
  tables         = file.path(PROJECT_ROOT, "output", "tables"),
  # Function libraries
  r_dir  = file.path(PROJECT_ROOT, "R")
)

# -----------------------------------------------------------------------------
# 2. STUDY PERIODS
# -----------------------------------------------------------------------------

PHASE1_YEARS <- 2021L         # Baseline period (single year)
PHASE2_YEARS <- 2022:2025   # Monitoring period

# Date boundaries (used for validation in data prep)
PHASE1_START <- as.Date("2021-01-01")
PHASE1_END   <- as.Date("2021-12-31")
PHASE2_START <- as.Date("2022-01-01")
PHASE2_END   <- as.Date("2025-12-31")

# Temporal aggregation unit for SPC charts
# Options: "month" | "quarter"  (week not recommended at citywide level)
AGGREGATION_UNIT <- "month"

# -----------------------------------------------------------------------------
# 3. GEOGRAPHIC SCOPE
# Phase 1 of project: citywide only
# Future phases: "borough" | "community_board"
# -----------------------------------------------------------------------------

GEO_LEVEL <- "citywide"

VALID_BOROUGHS <- c(
  "BRONX",
  "BROOKLYN",
  "MANHATTAN",
  "QUEENS",
  "STATEN ISLAND",
  "UNSPECIFIED"
)

# -----------------------------------------------------------------------------
# 4. COMPLAINT BUNDLES
# Nine quality-of-life bundles — keys must match bundle column in complaint_hierarchy.csv
# -----------------------------------------------------------------------------

BUNDLES <- list(
  housing_quality    = list(label = "Housing Quality"),
  sanitation         = list(label = "Sanitation"),
  blight_nuisance    = list(label = "Blight & Nuisance"),
  streets_signals    = list(label = "Streets & Signals"),
  water_sewers_trees = list(label = "Water, Sewers & Trees"),
  street_safety      = list(label = "Street Safety"),
  social_distress    = list(label = "Social Distress"),
  public_health      = list(label = "Public Health"),
  transportation     = list(label = "Transportation")
)

# Flat named vector: bundle key -> label
# Used by 02_ and 03_ to attach human-readable labels to baseline/index data.
BUNDLE_LABELS <- setNames(
  lapply(names(BUNDLES), function(k) list(bundle = k, label = BUNDLES[[k]]$label)),
  names(BUNDLES)
)
# Convert to data.frame-friendly format for as.data.table()
BUNDLE_LABELS <- do.call(rbind, lapply(names(BUNDLES), function(k) {
  data.frame(bundle = k, label = BUNDLES[[k]]$label, stringsAsFactors = FALSE)
}))

# -----------------------------------------------------------------------------
# 5. SPC PARAMETERS
# -----------------------------------------------------------------------------

SPC <- list(
  sigma          = 3,       # Control limit multiplier (standard: 3)
  min_subgroup   = 20,      # Minimum observations per time period
  chart_type     = "xbar"   # "xbar" for counts | "p" for proportions
)

# -----------------------------------------------------------------------------
# 6. OUTPUT SETTINGS
# -----------------------------------------------------------------------------

OUTPUT <- list(
  plot_format  = "pdf",    # "png" | "pdf" | "svg"
  plot_width   = 13,       # inches (landscape letter)
  plot_height  = 8.5,      # inches (landscape letter)
  plot_dpi     = 300,      # resolution for publication (ignored by PDF device)
  table_format = "csv"     # "csv" | "xlsx"
)

# -----------------------------------------------------------------------------
# 7. VALIDATION - confirm project root exists on load
# -----------------------------------------------------------------------------

stopifnot(
  "PROJECT_ROOT does not exist - check config.R" = dir.exists(PROJECT_ROOT)
)

message("config.R loaded successfully.")
message("  Bundles defined : ", length(BUNDLES))
message("  Phase 1         : ", PHASE1_START, " to ", PHASE1_END)
message("  Phase 2         : ", PHASE2_START, " to ", PHASE2_END)
message("  Aggregation     : ", AGGREGATION_UNIT)
message("  Geography       : ", GEO_LEVEL)
