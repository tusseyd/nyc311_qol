# setup_project.R
# Run once to create project directory structure
# Delete this script after running

project_root <- file.path(
  "C:", 
  "Users", 
  "David", 
  "OneDrive", 
  "Documents",
  "datacleaningproject", 
  "nyc311_qol"
)

dirs <- c(
  file.path(project_root, "R"),
  file.path(project_root, "data", "raw"),
  file.path(project_root, "data", "processed"),
  file.path(project_root, "output", "data"),
  file.path(project_root, "output", "plots"),
  file.path(project_root, "output", "tables"),
  file.path(project_root, "docs")
)

# Create all directories
lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

# Create placeholder files so git tracks empty folders
placeholders <- file.path(dirs, ".gitkeep")
lapply(placeholders, file.create)

# Create empty script files
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

cat("Project structure created at:", project_root, "\n")
cat("Files created:\n")
lapply(file.path(project_root, scripts), function(f) cat(" ", f, "\n"))