################################################################################

# NYC 311 Quality of Life Index

## Overview
This project uses NYC 311 Service Request data as a surrogate measure of
quality of life across New York City. Complaint volumes are indexed against
a 2021 baseline to track how conditions have changed over time across nine
thematic quality-of-life bundles.

The analysis is conducted at two levels of granularity:
- **Family level** — complaint types grouped into meaningful families
  (e.g. HEAT & HOT WATER, GRAFFITI) for primary index tracking
- **Original type level** — individual 311 complaint types for drill-down
  analysis within each family

Results are published as part of a Journal of Data Science paper co-authored
with Jun Yan.

---

## Pipeline Overview

The pipeline runs sequentially. Scripts are numbered to reflect execution
order. A master orchestration script (`run_all.R`) runs the full pipeline
with configurable run flags for skipping individual steps.
```
run_all.R                         # Master orchestration script
config.R                          # Central configuration — all parameters live here
00_build_classification_master.R  # Build complaint classification reference Excel
01_data_prep.R                    # Ingest, clean, and filter raw 311 CSV data
02_phase1_baseline.R              # Compute Phase I baseline statistics (2021)
03_analysis.R                     # Compute QoL index values (2022-2025)
04_visualizations.R               # Generate time series PDF charts
05_heatmap.R                      # Generate bundle scorecard PDFs
```

---

## Project Structure
```
nyc311_qol/
├── run_all.R
├── config.R
├── 00_build_classification_master.R
├── 01_data_prep.R
├── 02_phase1_baseline.R
├── 03_analysis.R
├── 04_visualizations.R
├── 05_heatmap.R
├── R/
│   ├── prep_functions.R          # Data cleaning and preparation functions
│   └── apply_classification.R    # Complaint type -> family -> bundle mapping
├── data/
│   ├── raw/                      # Raw CSV files (not tracked by git)
│   └── reference/
│       └── complaint_hierarchy.csv  # Master complaint type -> family -> bundle mapping
└── output/
    ├── data/                     # Intermediate and final RDS files
    ├── plots/
    │   ├── family/               # One PDF per complaint family
    │   ├── detail/               # One PDF per original complaint type
    │   ├── emerging/             # One PDF per emerging complaint type
    │   └── heatmap/              # Bundle scorecard PDFs
    ├── reference/
    │   └── complaint_classification.xlsx  # Living document for include/exclude decisions
    └── tables/                   # CSV summary tables
```

---

## Data

**Source:** NYC Open Data — 311 Service Requests
([https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/ertm-nq55](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/ertm-nq55))

| Period | Role | File |
|--------|------|------|
| 2021 | Phase I Baseline | 1-year_311SR_01-01-2021_thru_12-31-2021_*.csv |
| 2022–2025 | Phase II Monitor | 4-year_311SR_01-01-2022_thru_12-31-2025_*.csv |
| 2026 YTD | Optional overlay | 2026_ytd_311SR.csv |

Raw data files are excluded from this repository due to file size (~9GB+).

---

## Complaint Bundles

Quality of life is measured across nine thematic bundles. Each bundle
contains one or more complaint families, which in turn aggregate one or
more original 311 complaint types.

| Bundle | Description |
|--------|-------------|
| Blight & Nuisance | Graffiti, illegal postings, rodents, unsanitary conditions |
| Housing Quality | HPD complaints — heat, elevators, plumbing, paint, public housing |
| Public Health | Lead, food establishments, animal abuse, consumer complaints |
| Sanitation | Street sweeping and waste-related complaints |
| Social Distress | Encampments and homeless person assistance |
| Street Safety | Drug activity, abandoned vehicles, parking meters |
| Streets & Signals | Traffic signals, sidewalks, street lights, street signs |
| Transportation | FHV and sanitation vehicle complaints |
| Water, Sewers & Trees | Water issues, meters, and tree complaints |

Families are included in the index if they meet a minimum volume threshold
of 10,000 complaints over the four-year monitor period, or if designated
as mayoral interest in `complaint_hierarchy.csv`.

---

## Methodology

### Index Formula
```
Index = monthly_count / baseline_mean
```
- **> 1.0** — more complaints than baseline (worse quality of life)
- **< 1.0** — fewer complaints than baseline (better quality of life)
- **= 1.0** — at baseline

### Two-Tier Baseline
Complaint families are classified into one of two tiers based on data
availability during the 2021 baseline year:

- **Tier 1 (seasonally adjusted)** — families with activity in ≥ 9 of 12
  baseline months receive a separate baseline mean for each calendar month.
  Index comparisons are month-to-month, removing seasonal bias.
- **Tier 2 (overall mean)** — families with < 9 qualifying months receive
  a single annual baseline mean. Index values are not seasonally adjusted
  and are flagged as having limited baseline reliability.

A qualifying month must have ≥ 100 complaints. Isolated gaps of 1–3
consecutive missing months in Tier 1 families are filled by linear
interpolation.

### Direction Labels
2025 annual average index values are classified into seven direction labels:

| Change from Baseline | Label |
|----------------------|-------|
| ≤ −0.50 | Greatly Improved |
| −0.50 to −0.25 | Improved |
| −0.25 to −0.10 | Slightly Improved |
| −0.10 to +0.10 | Little Changed |
| +0.10 to +0.25 | Slightly Worse |
| +0.25 to +0.50 | Worse |
| ≥ +0.50 | Much Worse |

### Emerging Complaint Types
Types with no meaningful 2021 baseline activity are tracked separately
via a growth index (2025 average / first active year average) and a
linear trend slope. Two sources feed the emerging pipeline:
- **Auto-detected** — zero baseline activity meeting minimum volume criteria
- **Manually flagged** — designated in `complaint_hierarchy.csv` as
  `anomalous` (invalid baseline) or `emerging` (documented zero baseline)

---

## Running the Pipeline

1. Confirm raw data files are placed in `data/raw/`
2. Review and update paths and parameters in `config.R` if needed
3. Open `run_all.R` and set run flags as needed
4. Source `run_all.R`

To run only selected steps, set the corresponding flags to `FALSE`:
```r
RUN_CLASSIFICATION <- FALSE  # skip if hierarchy unchanged
RUN_DATA_PREP      <- FALSE  # skip if raw data unchanged
RUN_BASELINE       <- FALSE  # skip — baseline is fixed at 2021
RUN_ANALYSIS       <- TRUE
RUN_VISUALIZATIONS <- TRUE
RUN_HEATMAP        <- TRUE
```

---

## Dependencies

| Package | Purpose |
|---------|---------|
| data.table | High-performance data manipulation |
| ggplot2 | Chart generation |
| lubridate | Date parsing and manipulation |
| openxlsx | Reading and writing Excel files |
| grid | Chart layout elements |

---

## Output Files

| File | Description |
|------|-------------|
| `output/data/prepared_baseline.RDS` | Cleaned 2021 baseline data |
| `output/data/prepared_monitor.RDS` | Cleaned 2022-2025 monitor data |
| `output/data/baseline_stats.RDS` | Phase I baseline means by family |
| `output/data/analysis_results.RDS` | Monthly index values by family |
| `output/data/index_emerging.RDS` | Emerging type monthly counts |
| `output/tables/index_monthly.csv` | Monthly index — all families |
| `output/tables/index_annual.csv` | Annual average index — all families |
| `output/tables/index_snapshot.csv` | 2025 status snapshot — all families |
| `output/tables/emerging_snapshot.csv` | 2025 emerging type summary |
| `output/reference/complaint_classification.xlsx` | Living classification document |