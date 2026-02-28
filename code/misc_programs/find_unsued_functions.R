# ==========================================================
# FINAL: SEARCH ONLY top-level code/ + top-level code/functions/
# EXCLUDES ALL code/functions SUBDIRECTORIES (e.g. Archive)
# OUTPUTS SAVED TO code_dir
# ==========================================================
library(data.table)

# ----------------------------------------------------------
# STANDALONE MODE
# ----------------------------------------------------------
base_dir <- file.path(
  "C:",
  "Users",
  "David",
  "OneDrive",
  "Documents",
  "datacleaningproject",
  "Journal_of_Data_Science"
)
cat("Running in STANDALONE mode\n")
cat("Base directory:", base_dir, "\n")

# Define directories
code_dir      <- file.path(base_dir, "code")
functions_dir <- file.path(base_dir, "code", "functions")

# ----------------------------------------------------------
# STEP 1: Get function names from top-level of functions_dir
# ----------------------------------------------------------
fn_files <- list.files(
  functions_dir,
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = FALSE
)

# Safety filter: remove any that contain "Archive"
fn_files <- fn_files[!grepl("Archive", fn_files, ignore.case = TRUE)]

if (length(fn_files) == 0L) stop("No top-level .R files found in functions_dir.")

function_names <- sub("\\.R$", "", basename(fn_files))
cat(sprintf("Found %d top-level function files in %s\n", length(fn_files), functions_dir))
print(function_names)

# ----------------------------------------------------------
# STEP 2: Build search file list (exclude all under /functions/)
# ----------------------------------------------------------
# 1. All R files under code_dir recursively
all_code_files <- list.files(
  code_dir,
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = TRUE
)

# 2. Normalize paths for safe comparison
norm_all  <- normalizePath(all_code_files, winslash = "/", mustWork = FALSE)
norm_func <- normalizePath(functions_dir,  winslash = "/", mustWork = TRUE)

# 3. EXCLUDE everything inside functions_dir (and its subs)
keep_idx <- !startsWith(norm_all, paste0(norm_func, "/"))
code_files_code <- all_code_files[keep_idx]

# 4. Add back ONLY top-level files from functions_dir
code_files <- unique(c(code_files_code, fn_files))

cat(sprintf("\nFiles to search: %d\n", length(code_files)))
cat("  - code_dir: recursive, excluding everything under /functions/\n")
cat("  - functions_dir: top-level only\n\n")

# ----------------------------------------------------------
# STEP 3: Search each file for each function name
# ----------------------------------------------------------
search_function_usage <- function(search_term, files) {
  matches <- lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep(search_term, lines, fixed = TRUE)
    if (length(hits) > 0L) {
      data.table(
        function_name = search_term,
        file = f,
        line_number = hits,
        line_text = trimws(lines[hits])
      )
    } else NULL
  })
  rbindlist(matches, fill = TRUE)
}

cat("Searching for all function names...\n")
usage_results <- rbindlist(
  lapply(function_names, search_function_usage, files = code_files),
  fill = TRUE
)

# ----------------------------------------------------------
# STEP 4: Summarize and report
# ----------------------------------------------------------
if (nrow(usage_results) == 0L) {
  cat("\nNo references found for any function names.\n")
} else {
  cat(sprintf("\nFound %d total references across %d files.\n",
              nrow(usage_results), length(unique(usage_results$file))))
}

usage_summary <- usage_results[, .N, by = function_name]
setnames(usage_summary, "N", "usage_count")

unused_functions <- setdiff(function_names, usage_summary$function_name)

cat("\n==== SUMMARY ====\n")
cat(sprintf("Total function files checked: %d\n", length(function_names)))
cat(sprintf("Functions found somewhere: %d\n", nrow(usage_summary)))
cat(sprintf("Functions not found anywhere: %d\n\n", length(unused_functions)))

if (length(unused_functions) > 0L) {
  cat("Unused function files:\n")
  print(unused_functions)
} else {
  cat("✅ Every function file name was found at least once.\n")
}

# ----------------------------------------------------------
# STEP 5: Save results (to code_dir)
# ----------------------------------------------------------
fwrite(usage_results,  file.path(code_dir, "function_usage_details.csv"))
fwrite(usage_summary,  file.path(code_dir, "function_usage_summary.csv"))
fwrite(data.table(unused_function = unused_functions),
       file.path(code_dir, "unused_functions.csv"))

cat("\nSaved CSV outputs to code_dir:\n")
cat(" - function_usage_details.csv\n")
cat(" - function_usage_summary.csv\n")
cat(" - unused_functions.csv\n")
# ==========================================================
