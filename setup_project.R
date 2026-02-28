# =============================================================================
# setup_project.R
# NYC 311 Quality of Life Index - One-time project scaffolding script
# Run once from RStudio, then delete or archive this script
# =============================================================================

project_root <- file.path(
  "C:", 
  "Users", 
  "David", 
  "OneDrive", 
  "Documents",
  "datacleaningproject", 
  "nyc311_qol"
)

# --- Directory structure ------------------------------------------------------

dirs <- c(
  file.path(project_root, "R"),
  file.path(project_root, "data", "raw"),
  file.path(project_root, "data", "processed"),
  file.path(project_root, "output", "data"),
  file.path(project_root, "output", "plots"),
  file.path(project_root, "output", "tables"),
  file.path(project_root, "docs")
)

lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
cat("Directories created.\n")

# Placeholder files so git tracks empty folders
lapply(file.path(dirs, ".gitkeep"), file.create)

# --- Empty R scripts ----------------------------------------------------------

scripts <- c(
  "config.R",
  "01_data_prep.R",
  "02_phase1_baseline.R",
  "03_analysis.R",
  "04_visualizations.R",
  "R/prep_functions.R",
  "R/spc_functions.R",
  "R/plot_functions.R"
)

lapply(file.path(project_root, scripts), file.create)
cat("Script files created.\n")

# --- .gitignore ---------------------------------------------------------------

gitignore_content <- "
# Large raw data files - do not commit to GitHub
data/raw/
data/processed/

# Intermediate RDS files - reproducible from scripts
output/data/

# R environment and history
.Rhistory
.RData
.Rproj.user/
*.Rproj

# OS files
.DS_Store
Thumbs.db

# Rendered outputs (optional - remove these lines if you want plots tracked)
output/plots/
output/tables/
"

writeLines(trimws(gitignore_content), file.path(project_root, ".gitignore"))
cat(".gitignore created.\n")

# --- README.md ----------------------------------------------------------------

readme_content <- "# NYC 311 Quality of Life Index

## Overview
This project uses NYC 311 Service Request data as a surrogate measure of 
quality of life across New York City. Statistical Process Control (SPC) 
methodology is applied to detect meaningful changes in complaint patterns 
over time, using 2020-2021 as the Phase I baseline period.

## Project Structure

```
nyc311_qol/
├── config.R                  # All user-defined parameters and complaint bundles
├── 01_data_prep.R            # Ingest, clean, and standardize raw 311 data
├── 02_phase1_baseline.R      # Compute SPC baselines from 2020-2021 data
├── 03_analysis.R             # Apply baselines to 2022-2025, detect signals
├── 04_visualizations.R       # Generate SPC charts and summary tables
├── R/
│   ├── prep_functions.R      # Data cleaning and preparation functions
│   ├── spc_functions.R       # SPC calculation functions
│   └── plot_functions.R      # Chart generation functions
├── data/
│   ├── raw/                  # Raw CSV files (not tracked by git)
│   └── processed/            # Intermediate cleaned data (not tracked by git)
└── output/
    ├── data/                 # Analysis results as RDS files
    ├── plots/                # Generated charts
    └── tables/               # Summary tables
```

## Data
Source: NYC Open Data - 311 Service Requests
- Phase I (Baseline): 2020-2021
- Phase II (Monitoring): 2022-2025

Raw data files are excluded from this repository due to size.

## Complaint Bundles
Quality of life is measured across five thematic domains:
1. **Shelter Quality** - Residential habitability complaints
2. **Neighborhood Cleanliness** - Sanitation and waste management
3. **Public Infrastructure** - Streets, sidewalks, and utilities
4. **Street Safety** - Traffic signals, lighting, and signage
5. **Social Distress** - Encampments and homeless assistance

## Methodology
Statistical Process Control (SPC) p-charts and x-bar charts are used to 
monitor complaint rates relative to Phase I baselines. Signals are defined 
as points exceeding 3-sigma control limits.

## Dependencies
- data.table
- ggplot2
- lubridate
- qcc
- scales
- knitr / kableExtra (for tables)
"

writeLines(readme_content, file.path(project_root, "README.md"))
cat("README.md created.\n")

# --- Summary ------------------------------------------------------------------

cat("\n=== Project scaffolding complete ===\n")
cat("Root:", project_root, "\n\n")
cat("Next steps:\n")
cat("  1. Open RStudio and create a new Project from existing directory\n")
cat("  2. Initialize git: Tools > Version Control > Project Setup\n")
cat("  3. Create GitHub repo and push\n")
cat("  4. Begin with config.R\n")
