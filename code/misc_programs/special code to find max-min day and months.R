library(data.table)

# Read CSV with explicit comma separator
DT <- fread(file_path, sep = ",")

# Convert to POSIXct
DT[, created_date := as.POSIXct(`Created Date`,
                                format = "%m/%d/%Y %I:%M:%S %p",
                                tz = "UTC")]

# Extract calendar day
DT[, created_day := as.Date(created_date)]

# Count per day
daily_counts <- DT[, .N, by = created_day]

# --- Day with maximum count
max_day <- daily_counts[which.max(N)]

# --- Day with minimum count
min_day <- daily_counts[which.min(N)]

# --- Top 10 busiest days
top10_days <- daily_counts[order(-N)][1:10]

# --- Monthly counts
DT[, ym := format(created_day, "%Y-%m")]
monthly_counts <- DT[, .N, by = ym]

max_month <- monthly_counts[which.max(N)]
min_month <- monthly_counts[which.min(N)]

# Show results
cat("Day with maximum count:\n")
print(max_day)
  
cat("\nDay with minimum count:\n")
print(min_day)

cat("\nTop 10 busiest days:\n")
print(top10_days)

cat("\nMonth with maximum count:\n")
print(max_month)

cat("\nMonth with minimum count:\n")
print(min_month)
