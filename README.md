# NYC 311 Quality of Life Index

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

