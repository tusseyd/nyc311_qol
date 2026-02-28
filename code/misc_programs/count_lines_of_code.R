# ---- Line Count for R Files in code_dir and functions_dir (excluding comments/blanks) ----

library(data.table)

base_dir <- file.path(
  "C:",
  "Users",
  "David",
  "OneDrive",
  "Documents", 
  "datacleaningproject", 
  "journal_of_data_science"
)

code_dir      <- file.path(base_dir, "code")
functions_dir <- file.path(base_dir, "code", "functions")

# Function to count actual code lines (ignoring comments and blanks)
count_lines_in_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    warning(sprintf("Directory not found: %s", dir_path))
    return(data.table(file = character(), lines = integer()))
  }
  
  files <- list.files(dir_path, pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
  if (!length(files)) {
    return(data.table(file = character(), lines = integer()))
  }
  
  line_counts <- vapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    lines <- trimws(lines)
    # Keep only lines that are not blank and do not start with '#'
    code_lines <- lines[lines != "" & !grepl("^#", lines)]
    length(code_lines)
  }, integer(1L))
  
  data.table(file = basename(files), lines = line_counts)
}

# ---- Apply to both directories ----
code_summary      <- count_lines_in_dir(code_dir)
functions_summary <- count_lines_in_dir(functions_dir)

# ---- Combine and Summarize ----
combined <- rbindlist(list(
  code_summary[, .(directory = "code",      file, lines)],
  functions_summary[, .(directory = "functions", file, lines)]
), use.names = TRUE)

# Sort and summarize
setorder(combined, directory, file)

cat("\n=== Line Counts by File (excluding comments & blanks) ===\n")
print(combined)

cat("\n=== Summary ===\n")
combined[, .(
  files = .N,
  total_lines = sum(lines)
), by = directory] |> print()

cat(sprintf("\nTotal lines of actual R code across both directories: %s\n",
            format(sum(combined$lines), big.mark = ",")))
