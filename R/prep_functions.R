# =============================================================================
# R/prep_functions.R
# NYC 311 Quality of Life Index
# Data preparation helper functions
# Adapted from nyc_311_data_quality project functions
# =============================================================================

# -----------------------------------------------------------------------------
# modify_column_names()
# Standardizes raw NYC Open Data column names:
#   - Spaces replaced with underscores
#   - Parentheses removed
#   - Converted to lowercase
# After this runs, "Problem (formerly Complaint Type)" becomes
# "problem_formerly_complaint_type", etc.
# -----------------------------------------------------------------------------
modify_column_names <- function(dataset) {
  if (!is.data.frame(dataset))
    stop("modify_column_names: input must be a data.frame or data.table.")

  new_names <- names(dataset)
  new_names <- gsub("\\s+", "_", new_names)   # spaces -> underscores
  new_names <- gsub("[\\(\\)]", "", new_names, perl = TRUE)  # remove parentheses
  new_names <- tolower(new_names)              # lowercase

  names(dataset) <- new_names
  return(dataset)
}

# -----------------------------------------------------------------------------
# standardize_missing_chars()
# Converts blank strings and common missing-value tokens to NA
# Operates in-place on a data.table (by reference)
# Returns invisible summary list
# -----------------------------------------------------------------------------
standardize_missing_chars <- function(DT,
                                      tokens = c("na", "<na>", "n/a", "null"),
                                      report_zeroes = FALSE) {
  stopifnot(is.data.table(DT))

  labels       <- c("<blank>", tokens)
  token_counts <- setNames(integer(length(labels)), labels)
  total_to_na  <- 0L

  cat("\n[ Normalizing missing-like values -> NA ]\n")

  char_cols <- sort(names(DT)[vapply(DT, is.character, logical(1))])

  for (col in char_cols) {
    x       <- DT[[col]]
    x_trim  <- trimws(x)
    keys    <- ifelse(is.na(x_trim), NA_character_,
                      ifelse(x_trim == "", "<blank>", tolower(x_trim)))

    to_na   <- !is.na(keys) & (keys == "<blank>" | keys %in% tokens)
    to_trim <- !is.na(x) & (x != x_trim) & !to_na

    if (any(to_na)) {
      tab <- table(keys[to_na])
      for (k in names(tab))
        token_counts[k] <- token_counts[k] + as.integer(tab[[k]])
    }

    if (any(to_na))   set(DT, which(to_na),   col, NA_character_)
    if (any(to_trim)) set(DT, which(to_trim),  col, x_trim[to_trim])

    added       <- sum(to_na)
    total_to_na <- total_to_na + added

    if (added > 0L || any(to_trim)) {
      cat(sprintf("  %-35s: %6d -> NA (%.2f%%)%s\n",
                  col, added, 100 * added / nrow(DT),
                  if (any(to_trim)) " [whitespace trimmed]" else ""))
    }
  }

  # Summary
  idx <- if (report_zeroes) seq_along(token_counts) else which(token_counts > 0L)
  if (length(idx) > 0L) {
    cat("\nTokens converted to NA:\n")
    for (j in idx[order(token_counts[idx], decreasing = TRUE)])
      cat(sprintf("  %-12s: %d\n", names(token_counts)[j], token_counts[j]))
  }

  cat(sprintf("\nTotal values normalized to NA: %s (%.2f%% of all cells)\n",
              format(total_to_na, big.mark = ","),
              100 * total_to_na / (nrow(DT) * ncol(DT))))

  invisible(list(token_counts = token_counts, total_converted = total_to_na))
}

# -----------------------------------------------------------------------------
# parse_date_column()
# Converts character date columns to POSIXct (America/New_York)
# Handles DST spring-forward failures by shifting ambiguous times +1 hour
# Trimmed for QoL project: only processes created_date
# Returns list: parsed_data, summary, failures
# -----------------------------------------------------------------------------
parse_date_column <- function(temp_raw_data,
                              valid_date_columns,
                              fmt      = "%m/%d/%Y %I:%M:%S %p",
                              local_tz = "America/New_York") {

  temp_raw_data  <- data.table::copy(temp_raw_data)
  parse_failures <- list()
  time_summary   <- list()

  # --- 1. Parse each date column ---
  for (col in valid_date_columns) {
    cat(sprintf("\n  [%s] Parsing to POSIXct...\n", col))

    vals   <- trimws(temp_raw_data[[col]])
    vals[vals == ""] <- NA_character_

    parsed <- suppressWarnings(
      as.POSIXct(vals, format = fmt, tz = local_tz)
    )

    bad_idx <- which(!is.na(vals) & is.na(parsed))

    if (length(bad_idx)) {
      cat(sprintf("  WARNING: %d rows failed to parse in [%s]\n",
                  length(bad_idx), col))

      parse_failures[[col]] <- data.table(
        column         = col,
        original_value = vals[bad_idx]
      )
      cat("  Sample failed values:\n")
      print(head(parse_failures[[col]], 5))

      # DST spring-forward fix: shift ambiguous times by +1 hour
      cat("  Applying DST spring-forward correction...\n")
      fixed <- vals[bad_idx]
      fixed_times <- suppressWarnings(
        as.POSIXct(fixed, format = fmt, tz = "UTC")
      )
      fixed_times <- fixed_times + 3600  # shift +1 hour
      attr(fixed_times, "tzone") <- local_tz
      parsed[bad_idx] <- fixed_times
      still_bad <- sum(!is.na(vals[bad_idx]) & is.na(parsed[bad_idx]))
      cat(sprintf("  After DST fix: %d rows still NA\n", still_bad))

    } else {
      cat(sprintf("  Parsed successfully (no failures)\n"))
    }

    posix_col <- paste0(col, "_posix")
    temp_raw_data[, (posix_col) := parsed]
  }

  # --- 2. Replace original columns with parsed versions ---  (renumber from 3 to 2)
  for (col in valid_date_columns) {
    posix_col <- paste0(col, "_posix")
    if (posix_col %in% names(temp_raw_data)) {
      temp_raw_data[[col]] <- temp_raw_data[[posix_col]]
      temp_raw_data[, (posix_col) := NULL]
    }
  }
  
  invisible(list(
    parsed_data = temp_raw_data,
    failures    = if (length(parse_failures)) rbindlist(parse_failures, fill = TRUE) else NULL
  ))

  # --- 3. Replace original columns with parsed versions ---
  for (col in valid_date_columns) {
    posix_col <- paste0(col, "_posix")
    if (posix_col %in% names(temp_raw_data)) {
      temp_raw_data[[col]] <- temp_raw_data[[posix_col]]
      temp_raw_data[, (posix_col) := NULL]
    }
  }

  invisible(list(
    parsed_data = temp_raw_data,
    summary     = rbindlist(time_summary, fill = TRUE),
    failures    = if (length(parse_failures)) rbindlist(parse_failures, fill = TRUE) else NULL
  ))
}

# -----------------------------------------------------------------------------
# save_and_verify()
# Saves a data.table as RDS and verifies row/column integrity on read-back
# CSV output disabled by default for this project (large files)
# -----------------------------------------------------------------------------
save_and_verify <- function(dt, path, write_csv = FALSE) {

  if (!is.data.table(dt))
    stop("save_and_verify: input must be a data.table.")
  if (nrow(dt) == 0L) {
    warning("save_and_verify: empty data.table — nothing saved.")
    return(invisible(NULL))
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  # Save
  saveRDS(dt, path)

  # Read back and verify
  rb         <- readRDS(path)
  rows_ok    <- nrow(dt)   == nrow(rb)
  cols_ok    <- ncol(dt)   == ncol(rb)
  all_good   <- rows_ok && cols_ok
  file_mb    <- round(file.size(path) / 1024^2, 1)

  status <- if (all_good)          "OK"
            else if (!rows_ok)     "ROW MISMATCH"
            else if (!cols_ok)     "COLUMN MISMATCH"
            else                   "FULL MISMATCH"

  cat(sprintf("  %-55s | %s rows x %s cols | %s MB | %s\n",
              basename(path),
              format(nrow(dt), big.mark = ","),
              ncol(dt),
              file_mb,
              status))

  if (!all_good)
    warning(sprintf("save_and_verify: %s for %s", status, basename(path)))

  rm(rb)
  invisible(path)
}
