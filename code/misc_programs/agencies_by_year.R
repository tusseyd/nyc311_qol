library(data.table)

agencies <- fread(file.path(data_dir, "agencies.csv"))
agencies[, year := year(as.POSIXct(`Created Date`, format = "%m/%d/%Y %I:%M:%S %p"))]

# Get unique combinations of year and agency
agencies_by_year <- unique(agencies[, .(Year = year, Agency)])[order(Year, Agency)]

# Print to console
print(agencies_by_year)

# Write to CSV
fwrite(agencies_by_year, file.path(data_dir, "agencies_by_year.csv"))