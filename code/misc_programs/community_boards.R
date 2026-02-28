# Read raw 311 data
main_data_file <- "community_board.csv"

main_data_path <- file.path(data_dir, main_data_file)

# Read as simple text lines
community_boards <- data.table(
  `Community Board` = readLines(main_data_path)[-1]  # Skip header
)

# Remove the surrounding quotes
community_boards[, `Community Board` := gsub('^"|"$', '', `Community Board`)]
head(community_boards)
table(community_boards)
unique(community_boards)

# Extract borough
community_boards[, borough := sub('.*\\s+', '', `Community Board`)]

# See unique community boards per borough
community_boards[, unique(`Community Board`), by = borough][order(borough, V1)]