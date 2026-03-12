# =============================================================================
# run_all.R
# NYC 311 Quality of Life Index
#
# Master orchestration script — runs the full pipeline in sequence.
# Edit the RUN_* flags below to skip individual steps as needed.
#
# Typical use cases:
#   Full pipeline     : all flags TRUE
#   New monitor data  : skip 00_ and 01_data_prep (if hierarchy unchanged)
#   Rerun charts only : set only RUN_VISUALIZATIONS and RUN_HEATMAP to TRUE
#
# Run time estimates (approximate):
#   00_build_classification_master : ~2 sec
#   01_data_prep                   : varies (large file read)
#   02_phase1_baseline             : ~2 sec
#   03_analysis                    : ~5 sec
#   04_visualizations              : varies (chart rendering)
#   05_heatmap                     : ~5 sec
# =============================================================================

pipeline_start <- proc.time()

# -----------------------------------------------------------------------------
# RUN FLAGS — set to FALSE to skip individual steps
# -----------------------------------------------------------------------------
RUN_CLASSIFICATION  <- TRUE   # 00_ rebuild complaint classification master
RUN_DATA_PREP       <- TRUE   # 01_ data preparation
RUN_BASELINE        <- TRUE   # 02_ Phase I baseline
RUN_ANALYSIS        <- TRUE   # 03_ index computation
RUN_VISUALIZATIONS  <- TRUE   # 04_ time series charts
RUN_HEATMAP         <- TRUE   # 05_ bundle scorecards

# -----------------------------------------------------------------------------
# PIPELINE
# -----------------------------------------------------------------------------
source("C:/Users/David/OneDrive/Documents/datacleaningproject/nyc311_qol/config.R")
setwd(PROJECT_ROOT)

if (RUN_CLASSIFICATION)  source("00_build_classification_master.R")
if (RUN_DATA_PREP)       source("01_data_prep.R")
if (RUN_BASELINE)        source("02_phase1_baseline.R")
if (RUN_ANALYSIS)        source("03_analysis.R")
if (RUN_VISUALIZATIONS)  source("04_visualizations.R")
if (RUN_HEATMAP)         source("05_heatmap.R")

# -----------------------------------------------------------------------------
# WRAP UP
# -----------------------------------------------------------------------------
elapsed <- proc.time() - pipeline_start
elapsed_sec <- elapsed["elapsed"]

elapsed_fmt <- if (elapsed_sec >= 60) {
  sprintf("%d min %d sec", as.integer(elapsed_sec %/% 60),
          as.integer(elapsed_sec %%  60))
} else {
  sprintf("%.1f seconds", elapsed_sec)
}

message("\n", strrep("=", 80))
message("PIPELINE COMPLETE")
message(strrep("=", 80))
message(sprintf("  Total elapsed : %s", elapsed_fmt))
message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")