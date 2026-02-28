# Filter dates before 2003-01-01
dates_before_2003 <- d311$closed_date[d311$closed_date < as.POSIXct("2003-01-01")]

# Remove NA values if any
dates_before_2003 <- dates_before_2003[!is.na(dates_before_2003)]

# Get the count
count_before_2003 <- length(dates_before_2003)
print(paste("Count of dates before 2003:", count_before_2003))

# Display the actual dates
print("Dates before 2003:")
print(dates_before_2003)

# Get unique dates (if you want distinct dates only)
unique_dates_before_2003 <- unique(dates_before_2003)
print(paste("Count of unique dates before 2003:", length(unique_dates_before_2003)))

table(dates_before_2003)


# How many rows have resolution_action_update_date before created_date?
bad_rows <- d311[resolution_action_updated_date < created_date]

cat("Number of rows with resolution_action_update_date before created_date:",
    prettyNum(nrow(bad_rows), big.mark = ","), "\n")

  