# Install if needed
install.packages("styler")
library(styler)

# Style all R files in your functions directory
styler::style_dir(
  path = functions_dir,
  style = tidyverse_style,
  filetype = "R"
)

# Or style specific files
styler::style_file(file.path(functions_dir, "your_function.R"))


# Search for old column name references in all function files
search_old_column_names <- function(functions_dir) {
  
  # Old column names to search for
  old_names <- c("complaint_type", "descriptor")
  
  # Get all R files
  files <- list.files(functions_dir, pattern = "\\.R$", full.names = TRUE)
  
  # Track results
  results <- list()
  
  for (file in files) {
    content <- readLines(file, warn = FALSE)
    
    # Search for each old name
    for (old_name in old_names) {
      # Look for the column name (with quotes or in get/subset operations)
      patterns <- c(
        paste0('"', old_name, '"'),           # "complaint_type"
        paste0("'", old_name, "'"),           # 'complaint_type'
        paste0("\\$", old_name, "\\b"),       # $complaint_type
        paste0("\\[", old_name, "\\]"),       # [complaint_type]
        paste0("get\\(", old_name, "\\)"),    # get(complaint_type)
        paste0("\\b", old_name, "\\s*=")      # complaint_type =
      )
      
      for (pattern in patterns) {
        matches <- grep(pattern, content, value = FALSE)
        
        if (length(matches) > 0) {
          for (line_num in matches) {
            results[[length(results) + 1]] <- list(
              file = basename(file),
              line = line_num,
              column = old_name,
              code = trimws(content[line_num])
            )
          }
        }
      }
    }
  }
  
  # Display results
  if (length(results) == 0) {
    cat("✅ No references to old column names found!\n")
    cat("   Your functions should work with the new column names.\n")
    return(invisible(NULL))
  }
  
  cat(sprintf("⚠️  Found %d reference(s) to old column names:\n\n", 
              length(results)))
  
  # Group by file
  by_file <- split(results, sapply(results, function(x) x$file))
  
  for (file_name in names(by_file)) {
    cat(strrep("=", 70), "\n")
    cat("FILE:", file_name, "\n")
    cat(strrep("=", 70), "\n")
    
    for (item in by_file[[file_name]]) {
      cat(sprintf("Line %d | Column: %s\n", item$line, item$column))
      cat(sprintf("  %s\n\n", item$code))
    }
  }
  
  # Summary
  cat(strrep("=", 70), "\n")
  cat("SUMMARY\n")
  cat(strrep("=", 70), "\n")
  
  files_affected <- unique(sapply(results, function(x) x$file))
  cat(sprintf("Files affected: %d\n", length(files_affected)))
  cat(paste("  -", files_affected, collapse = "\n"), "\n\n")
  
  complaint_type_count <- sum(sapply(results, function(x) x$column == "complaint_type"))
  descriptor_count <- sum(sapply(results, function(x) x$column == "descriptor"))
  
  cat(sprintf("References to 'complaint_type': %d\n", complaint_type_count))
  cat(sprintf("References to 'descriptor': %d\n", descriptor_count))
  
  return(invisible(results))
}

# Run the search
results <- search_old_column_names(functions_dir)