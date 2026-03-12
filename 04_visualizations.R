# =============================================================================
# 04_visualizations.R
# NYC 311 Quality of Life Index
#
# PURPOSE: Generates all PDF charts for the QoL Index. Produces three chart
# types across four output directories, plus one standalone seasonal summary
# chart. Chart dimensions and format are controlled by OUTPUT settings in
# config.R.
#
# CHART TYPES:
#   Family charts   — one per complaint family showing monthly QoL index
#                     (2022-2025). Solo families (only one original type) are
#                     skipped as their detail chart is identical.
#   Detail charts   — one per original complaint type showing monthly QoL
#                     index against the family baseline. Subtitle includes
#                     parent family name for context.
#   Emerging charts — one per emerging complaint type showing raw monthly
#                     counts with a linear trend line. No index shown —
#                     these types have no valid 2021 baseline.
#   Seasonal chart  — total complaints by calendar month across all years,
#                     arranged by season (Winter -> Spring -> Summer -> Fall).
#
# CHART ELEMENTS (family and detail charts):
#   - Warm season shading (March-August, light yellow)
#   - Solid black baseline reference line at 1.0, annotated with avg monthly count
#   - Monthly index points (solid) with connecting line
#   - Open circles for interpolated baseline months
#   - Upward triangles at cap line for clipped values (Y_AXIS_CAP = 5.8)
#   - Dashed annual average segments with year labels
#   - Colored trend arrow from Jan 2022 (1.0) to 2025 annual average
#
# CHART ELEMENTS (emerging charts):
#   - Warm season shading
#   - Raw monthly count line and points
#   - Linear trend line colored by growth direction
#   - Dashed annual average segments
#   - Earliest-year average reference line (analogous to baseline)
#
# STATUS COLOR PALETTE (7-level diverging, colorblind-considerate):
#   Greatly Improved  : #006837 (dark green)
#   Improved          : #31A354 (medium green)
#   Slightly Improved : #78C679 (light green)
#   Little Changed    : #969696 (grey)
#   Slightly Worse    : #FD8D3C (light orange)
#   Worse             : #E6550D (medium orange)
#   Much Worse        : #A50F15 (dark red)
#
# SUBTITLE CONTENT:
#   Family/detail : 2025 status label | percent change from baseline | n=
#   Detail only   : also includes parent family name
#   Emerging      : EMERGING label | growth direction | percent change | n=
#
# Y-AXIS CAP:
#   Index values above Y_AXIS_CAP (5.8) are clipped from view and marked
#   with an upward triangle. Exempt types (NO_CAP_TYPES) show full range.
#   Both parameters are defined near the top of the script.
#
# Inputs:  output/data/analysis_results.RDS
#          output/data/index_data_detail.RDS
#          output/data/index_emerging.RDS         (optional)
#          output/tables/index_snapshot.csv
#          output/tables/index_snapshot_detail.csv
#          output/tables/emerging_snapshot.csv    (optional)
# Outputs: output/plots/family/<FAMILY>.pdf
#          output/plots/detail/<COMPLAINT_TYPE>.pdf
#          output/plots/emerging/<TYPE>_EMERGING.pdf
#          output/plots/SEASONAL_COMPLAINTS_BY_MONTH.pdf
# =============================================================================

message("\n", strrep("=", 80))
message("NYC 311 QUALITY OF LIFE INDEX - VISUALIZATIONS")
message(strrep("=", 80))
message("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

prog_start <- proc.time()

source("config.R")

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(lubridate)
  library(grid)
})

# -----------------------------------------------------------------------------
# david_theme
# -----------------------------------------------------------------------------
david_theme <- function(
    text_size          = 12,
    plot_title_size    = text_size + 3,
    plot_subtitle_size = text_size - 1,
    axis_title_size    = text_size - 1,
    x_axis_text_size   = text_size - 1,
    y_axis_text_size   = text_size - 1,
    x_axis_angle       = 0,
    x_axis_hjust       = if (x_axis_angle > 0) 1 else 0.5,
    x_axis_vjust       = 1,
    x_axis_face        = "plain",
    y_axis_face        = "plain",
    grid_line_color    = "white",
    grid_line_type     = "solid",
    grid_line_width    = 0.8,
    base_family        = "sans",
    remove_x_title     = FALSE,
    remove_y_title     = FALSE,
    plot_margin        = NULL
) {
  t <- ggplot2::theme(

    # Panel
    panel.background    = ggplot2::element_rect(fill = "gray97", color = NA),
    panel.border        = ggplot2::element_rect(fill = NA, color = "gray80",
                                                linewidth = 0.5),
    panel.grid.major.y  = ggplot2::element_line(color     = grid_line_color,
                                                linetype  = grid_line_type,
                                                linewidth = grid_line_width),
    panel.grid.major.x  = ggplot2::element_line(color     = grid_line_color,
                                                linetype  = grid_line_type,
                                                linewidth = grid_line_width),
    panel.grid.minor    = ggplot2::element_blank(),

    # Plot background
    plot.background     = ggplot2::element_rect(fill = "white", color = NA),

    # Titles
    plot.title          = ggplot2::element_text(face   = "bold",
                                                size   = plot_title_size,
                                                hjust  = 0.5,
                                                family = base_family),
    plot.title.position = "plot",

    plot.subtitle       = ggplot2::element_text(size   = plot_subtitle_size,
                                                hjust  = 0,
                                                family = base_family),

    plot.caption        = ggplot2::element_text(size   = text_size - 4,
                                                color  = "gray55",
                                                hjust  = 0,
                                                family = base_family),
    plot.caption.position = "plot",

    # Legend
    legend.position     = "none",

    # Axes
    axis.text.x         = ggplot2::element_text(angle  = x_axis_angle,
                                                size   = x_axis_text_size,
                                                vjust  = x_axis_vjust,
                                                hjust  = x_axis_hjust,
                                                face   = x_axis_face,
                                                family = base_family),
    axis.text.y         = ggplot2::element_text(size   = y_axis_text_size,
                                                face   = y_axis_face,
                                                family = base_family),
    axis.title.x        = ggplot2::element_text(size   = axis_title_size,
                                                family = base_family),
    axis.title.y        = ggplot2::element_text(size   = axis_title_size,
                                                family = base_family),
    axis.ticks          = ggplot2::element_blank(),
    axis.ticks.length   = grid::unit(0.3, "cm")
  )

  if (remove_x_title) {
    t <- t + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  if (remove_y_title) {
    t <- t + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }
  if (!is.null(plot_margin)) {
    t <- t + ggplot2::theme(plot.margin = ggplot2::margin(
      plot_margin[1], plot_margin[2], plot_margin[3], plot_margin[4], unit = "cm"
    ))
  }

  t
}

# -----------------------------------------------------------------------------
# COLORS
# Trend line color reflects 2025 status (7-level diverging palette)
# Greens for improvement, grey for neutral, orange/red for worse
# Remaining colors for chart elements
# -----------------------------------------------------------------------------

# Status colors (diverging, colorblind-considerate)
STATUS_COLORS <- c(
  "Greatly Improved"  = "#006837",   # dark green
  "Improved"          = "#31A354",   # medium green
  "Slightly Improved" = "#78C679",   # light green
  "Little Changed"    = "#969696",   # grey
  "Slightly Worse"    = "#FD8D3C",   # light orange
  "Worse"             = "#E6550D",   # medium orange
  "Much Worse"        = "#A50F15"    # dark red
)

COL_POINTS   <- "#666666"   # dark grey      - monthly index points
COL_INTERP   <- "#AAAAAA"   # light grey     - interpolated baseline points
COL_BASELINE <- "#000000"   # black          - baseline reference line
COL_ANNUAL   <- "#CC79A7"   # reddish purple - annual avg dashes
COL_CAP      <- "#CC4400"   # burnt orange   - truncated (above cap) markers

# Y-axis cap: points above this value are clipped from view
# Increase to show more of the range, decrease to tighten the view
Y_AXIS_CAP <- 5.8

# Types exempt from Y_AXIS_CAP — full range shown regardless of peak values.
# Add original type names (uppercase) to remove the cap for that detail chart.
NO_CAP_TYPES <- c("DRUG ACTIVITY", "GRAFFITI")

# -----------------------------------------------------------------------------
# HELPERS
# -----------------------------------------------------------------------------

# Look up trend line color from 2025 status label
status_color <- function(direction) {
  col <- STATUS_COLORS[direction]
  if (is.na(col)) "#969696" else col   # default to grey if unknown
}

sanitize_filename <- function(name) {
  name <- gsub("[/\\\\:*?\"<>|]", "_", name)
  name <- gsub("\\s+", "_", name)
  name <- gsub("_{2,}", "_", name)
  name <- trimws(name, whitespace = "_")
  toupper(name)
}

# -----------------------------------------------------------------------------
# LOAD DATA
# -----------------------------------------------------------------------------
message("Loading data...")

if (!file.exists(PATHS$analysis_results))
  stop("Index data not found. Run 03_analysis.R first.")

detail_path <- file.path(PATHS$data_out, "index_data_detail.RDS")
if (!file.exists(detail_path))
  stop("Detail index data not found. Run 03_analysis.R first.")

snapshot_path        <- file.path(PATHS$tables, "index_snapshot.csv")
snapshot_detail_path <- file.path(PATHS$tables, "index_snapshot_detail.csv")
if (!file.exists(snapshot_path))
  stop("Snapshot CSV not found. Run 03_analysis.R first.")
if (!file.exists(snapshot_detail_path))
  stop("Detail snapshot CSV not found. Run 03_analysis.R first.")

index_data   <- readRDS(PATHS$analysis_results)
index_detail <- readRDS(detail_path)
snapshot     <- fread(snapshot_path)
snap_detail  <- fread(snapshot_detail_path)

cat(sprintf("  Family rows     : %s\n", format(nrow(index_data),   big.mark = ",")))
cat(sprintf("  Families        : %d\n", uniqueN(index_data$complaint_type)))
cat(sprintf("  Detail rows     : %s\n", format(nrow(index_detail), big.mark = ",")))
cat(sprintf("  Original types  : %d\n", uniqueN(index_detail$original_type)))

# Dynamic baseline label for chart captions — reflects PHASE1_YEARS from config
baseline_label <- if (length(PHASE1_YEARS) == 1L) {
  as.character(PHASE1_YEARS)
} else {
  paste0(min(PHASE1_YEARS), "-", max(PHASE1_YEARS))
}

# Emerging types data (optional)
path_emerging <- file.path(PATHS$data_out, "index_emerging.RDS")
has_emerging  <- file.exists(path_emerging)

if (has_emerging) {
  index_emerging    <- readRDS(path_emerging)
  emerging_snap_path <- file.path(PATHS$tables, "emerging_snapshot.csv")
  emerging_snapshot <- if (file.exists(emerging_snap_path))
                         fread(emerging_snap_path)
                       else NULL
  cat(sprintf("  Emerging types  : %d\n",
              uniqueN(index_emerging$original_type)))
} else {
  index_emerging    <- NULL
  emerging_snapshot <- NULL
  message("  No emerging types data found.")
}

# Create output subdirectories
for (d in c(PATHS$plots, PATHS$plots_family, PATHS$plots_detail, PATHS$plots_emerging))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# Annual averages
annual_avg <- index_data[, .(
  mean_index = round(mean(index, na.rm = TRUE), 4)
), by = .(complaint_type, year)]

# Total monitor period counts
total_counts <- index_data[, .(total_n = sum(monthly_count)),
                            by = complaint_type]

# Overall baseline monthly average per complaint type
# For Tier 1: mean of the 12 seasonal baseline means
# For Tier 2: the single overall baseline mean (same value repeated)
baseline_avgs <- index_data[, .(
  baseline_avg = round(mean(unique(baseline_mean), na.rm = TRUE), 0)
), by = complaint_type]

# Complaint order by bundle then name
complaint_order <- unique(
  index_data[order(complaint_type),
             .(complaint_type, bundle_label, tier)]
)

# Exclude solo families — families with only 1 original type get a detail
# chart only (identical to the family chart, so family chart is redundant).
types_per_family <- index_detail[, .(n_types = uniqueN(original_type)),
                                  by = family]
solo_families    <- types_per_family[n_types == 1L, family]

n_solo <- sum(complaint_order$complaint_type %in% solo_families)
if (n_solo > 0) {
  cat(sprintf("\n  Skipping %d solo-family chart(s) — detail chart is identical:\n",
              n_solo))
  cat(paste0("    ", sort(complaint_order$complaint_type[
                            complaint_order$complaint_type %in% solo_families]),
             collapse = "\n"), "\n")
}
complaint_order <- complaint_order[!complaint_type %in% solo_families]

# Diagnostic: show all points that will be clipped by Y_AXIS_CAP
clipped <- index_data[index > Y_AXIS_CAP,
                       .(complaint_type, year_month, index)][order(-index)]
if (nrow(clipped) > 0) {
  cat(sprintf("\n  Points exceeding Y_AXIS_CAP (%.1f) - will be clipped:\n",
              Y_AXIS_CAP))
  print(clipped)
} else {
  cat(sprintf("\n  No points exceed Y_AXIS_CAP (%.1f)\n", Y_AXIS_CAP))
}

# -----------------------------------------------------------------------------
# GENERATE CHARTS
# -----------------------------------------------------------------------------
message(sprintf("\nGenerating %d charts...\n", nrow(complaint_order)))

for (i in seq_len(nrow(complaint_order))) {

  ct         <- complaint_order$complaint_type[i]
  bundle_lbl <- complaint_order$bundle_label[i]
  ct_tier    <- complaint_order$tier[i]

  ct_data   <- index_data[complaint_type == ct]
  ct_annual <- annual_avg[complaint_type == ct]
  ct_n      <- total_counts[complaint_type == ct, total_n]
  ct_baseline_avg <- baseline_avgs[complaint_type == ct, baseline_avg]

  # 2025 status and index value from snapshot
  ct_direction  <- snapshot[complaint_type == ct, direction][1]
  ct_index_2025 <- snapshot[complaint_type == ct, index_2025][1]
  if (length(ct_direction) == 0 || is.na(ct_direction))
    ct_direction <- "Unknown"

  # Status color for trend arrow
  tc <- status_color(ct_direction)

  # Subtitle: tight, no date range
  ct_pct_change <- (ct_index_2025 - 1.0) * 100
  ct_pct_label  <- sprintf("%+.0f%%", ct_pct_change)
  subtitle_text <- sprintf(
    "2025 Trend: %s (%s)  |  n = %s",
    ct_direction,
    ct_pct_label,
    format(ct_n, big.mark = ",")
  )

  # Y-axis limits: always include 1.0, cap at Y_AXIS_CAP, note clipped points
  y_min       <- min(c(ct_data$index, 1.0), na.rm = TRUE)
  y_max       <- min(max(c(ct_data$index, 1.0), na.rm = TRUE), Y_AXIS_CAP)
  y_pad       <- (y_max - y_min) * 0.12
  y_lo        <- max(0, y_min - y_pad)
  y_hi        <- y_max + y_pad * 2.5
  n_clipped   <- ct_data[index > Y_AXIS_CAP, .N]
  clip_note   <- if (n_clipped > 0)
    sprintf("  |  %d truncated values", n_clipped)
  else ""

  # Interpolated baseline points
  has_interp_fam  <- "baseline_interpolated" %in% names(index_data) &&
                     any(ct_data$baseline_interpolated == TRUE, na.rm = TRUE)
  interp_note_fam <- if (has_interp_fam)
    "  |  (o) Open circles = interpolated baseline months"
  else ""

  # Caption
  caption_line1 <- if (ct_tier == 2L) {
    paste0("Bundle: ", bundle_lbl,
           "  |  * Tier 2: Limited baseline reliability - ",
           "complaint category not fully active during baseline period (",
           baseline_label, ")")
  } else {
    paste0("Bundle: ", bundle_lbl,
           "  |  Seasonally adjusted: monthly count / baseline mean (",
           baseline_label, ")")
  }
  caption_text <- if (ct_tier == 2L) {
    paste0("Bundle: ", bundle_lbl,
           "  |  * Tier 2: Limited baseline reliability - ",
           "complaint category not fully active during baseline period (",
           baseline_label, ")",
           interp_note_fam, clip_note)
  } else {
    paste0("Bundle: ", bundle_lbl,
           "  |  Seasonally adjusted: monthly count / baseline mean (",
           baseline_label, ")",
           interp_note_fam, clip_note)
  }

  # Annual avg segments spanning each calendar year
  year_ranges <- ct_data[, .(x_start = min(year_month),
                              x_end   = max(year_month)),
                          by = year]
  ct_annual   <- merge(ct_annual, year_ranges, by = "year")

  # Build seasonal shading - single warm season band (Mar-Aug)
  # Unshaded = cool season (Sep-Feb)
  chart_years  <- sort(unique(ct_data$year))
  season_rects_ct <- do.call(rbind, lapply(chart_years, function(yr) {
    data.frame(xmin   = as.Date(paste0(yr, "-03-01")),
               xmax   = as.Date(paste0(yr, "-09-01")),
               season = "Spring/Summer")
  }))
  season_rects_ct <- as.data.table(season_rects_ct)

  # Clip to actual data range
  data_start <- min(ct_data$year_month)
  data_end   <- max(ct_data$year_month)
  season_rects_ct <- season_rects_ct[xmax > data_start & xmin < data_end]
  season_rects_ct[xmin < data_start, xmin := data_start]
  season_rects_ct[xmax > data_end,   xmax := data_end]

  season_fill_colors <- c("Spring/Summer" = "#FFF3CC")

  # Numeric x for linear model
  ct_data[, x_num := as.numeric(year_month)]
  ct_data[, index_plot := fifelse(index > Y_AXIS_CAP, NA_real_, index)]
  ct_data[, index_cap  := fifelse(index > Y_AXIS_CAP, Y_AXIS_CAP, NA_real_)]

  # Split observed vs interpolated baseline months
  ct_obs_fam   <- ct_data[is.na(baseline_interpolated) | baseline_interpolated == FALSE]
  ct_interp_fam <- if (has_interp_fam) ct_data[baseline_interpolated == TRUE] else NULL

  p <- ggplot(ct_data, aes(x = year_month, y = index)) +

    # Seasonal background shading
    geom_rect(data        = season_rects_ct,
              aes(xmin    = xmin,
                  xmax    = xmax,
                  ymin    = -Inf,
                  ymax    = Inf,
                  fill    = season),
              inherit.aes = FALSE,
              alpha       = 0.83) +

    scale_fill_manual(values = season_fill_colors,
                      name   = "Season",
                      guide  = guide_legend(
                        nrow           = 1,
                        title.position = "left",
                        keywidth       = unit(0.8, "cm"),
                        keyheight      = unit(0.4, "cm")
                      )) +

    # Baseline reference line
    geom_hline(yintercept = 1.0,
               color      = COL_BASELINE,
               linewidth  = 0.8,
               linetype   = "solid") +

    # Annual average dashed lines
    geom_segment(data = ct_annual,
                 aes(x    = x_start,
                     xend = x_end,
                     y    = mean_index,
                     yend = mean_index),
                 color       = COL_ANNUAL,
                 linewidth   = 0.7,
                 linetype    = "dashed",
                 inherit.aes = FALSE) +

    # Annual average labels
    geom_text(data = ct_annual,
              aes(x     = x_end,
                  y     = mean_index,
                  label = sprintf("%d avg: %.2f", year, mean_index)),
              hjust       = 1.0,
              vjust       = -0.6,
              size        = 2.8,
              fontface    = "plain",
              color       = COL_ANNUAL,
              inherit.aes = FALSE) +

    # Connect monthly points with line - gaps where values exceed Y_AXIS_CAP
    geom_line(aes(y = index_plot),
              color     = COL_POINTS,
              linewidth = 0.5,
              alpha     = 0.6) +

    # Monthly index points - observed (solid)
    geom_point(data  = ct_obs_fam,
               aes(y = index_plot),
               color = COL_POINTS,
               size  = 1.8,
               alpha = 0.75) +

    # Interpolated baseline months - hollow circles
    { if (!is.null(ct_interp_fam) && nrow(ct_interp_fam) > 0)
        geom_point(data        = ct_interp_fam,
                   aes(y       = index_plot),
                   shape       = 21,
                   color       = COL_INTERP,
                   fill        = "white",
                   size        = 1.8,
                   stroke      = 0.8,
                   inherit.aes = FALSE)
      else
        NULL } +

    # Truncated points: upward triangle at cap line
    geom_point(aes(y = index_cap),
               shape = 24,
               color = COL_CAP,
               fill  = COL_CAP,
               size  = 2.2,
               na.rm = TRUE) +

    # Trend arrow: straight line from baseline (Jan 2022, 1.0) to 2025 annual avg
    annotate("segment",
             x      = as.Date("2022-01-01"),
             xend   = as.Date("2025-12-01"),
             y      = 1.0,
             yend   = min(ct_annual[year == 2025, mean_index], Y_AXIS_CAP),
             color  = tc,
             linewidth = 1.0,
             arrow  = arrow(length = unit(0.25, "cm"), type = "closed")) +

    # Baseline label with average monthly count
    annotate("text",
             x     = min(ct_data$year_month),
             y     = 1.0,
             label = sprintf("Baseline (1.0 | avg %s/mo)",
                             format(ct_baseline_avg, big.mark = ",")),
             hjust = 0,
             vjust = -0.5,
             size  = 3.0,
             color = COL_BASELINE) +

    scale_x_date(date_breaks = "6 months",
                 date_labels = "%b %Y",
                 expand      = expansion(mult = c(0.02, 0.06))) +

    scale_y_continuous(labels = function(x) sprintf("%.2f", x),
                       limits = c(y_lo, y_hi + if (n_clipped > 0) y_pad else 0)) +

    labs(
      title    = paste0(ct, "  —  Family"),
      subtitle = subtitle_text,
      y        = "Quality of Life Index",
      caption  = caption_text
    ) +

    david_theme(
      x_axis_angle   = 45,
      remove_x_title = TRUE,
      plot_margin    = c(0.3, 0.6, 0.3, 0.3)
    ) +

    theme(legend.position = "none")

  filename <- paste0(sanitize_filename(ct), ".pdf")
  filepath <- file.path(PATHS$plots_family, filename)

  ggsave(filepath,
         plot   = p,
         width  = OUTPUT$plot_width,
         height = OUTPUT$plot_height,
         device = OUTPUT$plot_format)

  print(p)
  Sys.sleep(0.5)

  cat(sprintf("  [%02d/%02d] %-45s -> %s  [%s]\n",
              i, nrow(complaint_order),
              ct, filename, ct_direction))
}

# -----------------------------------------------------------------------------
# GENERATE DETAIL CHARTS (one per original complaint type)
# Each is indexed against its family baseline — scales are comparable.
# Title = original complaint type name
# Subtitle includes family name for context
# -----------------------------------------------------------------------------
message(sprintf("\nGenerating detail charts (original complaint types)...\n"))

detail_order <- unique(
  index_detail[order(original_type),
               .(original_type, family, bundle_label, tier)]
)

# Annual averages for detail
annual_avg_detail <- index_detail[, .(
  mean_index = round(mean(index, na.rm = TRUE), 4)
), by = .(original_type, year)]

# Total monitor period counts for detail
total_counts_detail <- index_detail[, .(total_n = sum(monthly_count)),
                                     by = original_type]

# Baseline avg for detail — same family baseline mean
baseline_avgs_detail <- index_detail[, .(
  baseline_avg = round(mean(unique(baseline_mean), na.rm = TRUE), 0)
), by = original_type]

# Diagnostic: clipped points in detail data
clipped_detail <- index_detail[index > Y_AXIS_CAP,
                                .(original_type, year_month, index)][order(-index)]
if (nrow(clipped_detail) > 0) {
  cat(sprintf("  Points exceeding Y_AXIS_CAP (%.1f) in detail data: %d\n",
              Y_AXIS_CAP, nrow(clipped_detail)))
}

for (i in seq_len(nrow(detail_order))) {

  ct         <- detail_order$original_type[i]
  fam        <- detail_order$family[i]
  bundle_lbl <- detail_order$bundle_label[i]
  ct_tier    <- detail_order$tier[i]

  ct_data   <- index_detail[original_type == ct]
  ct_annual <- annual_avg_detail[original_type == ct]
  ct_n      <- total_counts_detail[original_type == ct, total_n]
  ct_baseline_avg <- baseline_avgs_detail[original_type == ct, baseline_avg]

  # 2025 status from detail snapshot
  ct_direction  <- snap_detail[original_type == ct, direction][1]
  ct_index_2025 <- snap_detail[original_type == ct, index_2025][1]
  if (length(ct_direction) == 0 || is.na(ct_direction))
    ct_direction <- "Unknown"

  tc <- status_color(ct_direction)

  ct_pct_change <- (ct_index_2025 - 1.0) * 100
  ct_pct_label  <- sprintf("%+.0f%%", ct_pct_change)
  subtitle_text <- sprintf(
    "2025 Trend: %s (%s)  |  Family: %s  |  n = %s",
    ct_direction,
    ct_pct_label,
    fam,
    format(ct_n, big.mark = ",")
  )

  # Effective cap: NO_CAP_TYPES get full range, others capped at Y_AXIS_CAP
  eff_cap   <- if (ct %in% NO_CAP_TYPES) Inf else Y_AXIS_CAP

  y_min     <- min(c(ct_data$index, 1.0), na.rm = TRUE)
  y_max     <- min(max(c(ct_data$index, 1.0), na.rm = TRUE), eff_cap)
  y_pad     <- (y_max - y_min) * 0.12
  y_lo      <- max(0, y_min - y_pad)
  y_hi      <- y_max + y_pad * 2.5
  n_clipped <- ct_data[index > eff_cap, .N]
  clip_note <- if (n_clipped > 0)
    sprintf("  |  [^] %d point%s truncated at cap (%.1f) - true value higher",
            n_clipped, ifelse(n_clipped > 1, "s", ""), Y_AXIS_CAP)
  else ""

  year_ranges <- ct_data[, .(x_start = min(year_month),
                              x_end   = max(year_month)),
                          by = year]
  ct_annual   <- merge(ct_annual, year_ranges, by = "year")

  ct_data[, index_plot := fifelse(index > eff_cap, NA_real_, index)]
  ct_data[, index_cap  := fifelse(index > eff_cap, eff_cap,  NA_real_)]

  # Split observed vs interpolated for distinct point rendering
  has_interp  <- "baseline_interpolated" %in% names(ct_data) &&
                 any(ct_data$baseline_interpolated == TRUE, na.rm = TRUE)
  ct_observed <- ct_data[is.na(baseline_interpolated) | baseline_interpolated == FALSE]
  ct_interp   <- if (has_interp) ct_data[baseline_interpolated == TRUE] else NULL

  interp_note <- if (has_interp)
    "  |  (o) Open circles = interpolated baseline months"
  else ""

  clip_note_short <- if (n_clipped > 0)
    sprintf("  |  %d truncated values", n_clipped)
  else ""

  caption_text <- if (ct_tier == 2L) {
    paste0("Bundle: ", bundle_lbl,
           "  |  * Tier 2: Limited baseline reliability",
           "  |  Baseline: ", baseline_label,
           interp_note, clip_note_short)
  } else {
    paste0("Bundle: ", bundle_lbl,
           "  |  Seasonally adjusted: count / baseline mean",
           "  |  Baseline: ", baseline_label,
           interp_note, clip_note_short)
  }

  chart_years     <- sort(unique(ct_data$year))
  season_rects_ct <- do.call(rbind, lapply(chart_years, function(yr) {
    data.frame(xmin = as.Date(paste0(yr, "-03-01")),
               xmax = as.Date(paste0(yr, "-09-01")),
               season = "Spring/Summer")
  }))
  season_rects_ct <- as.data.table(season_rects_ct)

  data_start <- min(ct_data$year_month)
  data_end   <- max(ct_data$year_month)
  season_rects_ct <- season_rects_ct[xmax > data_start & xmin < data_end]
  season_rects_ct[xmin < data_start, xmin := data_start]
  season_rects_ct[xmax > data_end,   xmax := data_end]

  season_fill_colors <- c("Spring/Summer" = "#FFF3CC")

  ct_data[, x_num := as.numeric(year_month)]

  p <- ggplot(ct_data, aes(x = year_month, y = index)) +

    geom_rect(data        = season_rects_ct,
              aes(xmin    = xmin,
                  xmax    = xmax,
                  ymin    = -Inf,
                  ymax    = Inf,
                  fill    = season),
              inherit.aes = FALSE,
              alpha       = 0.83) +

    scale_fill_manual(values = season_fill_colors,
                      name   = "Season",
                      guide  = guide_legend(
                        nrow           = 1,
                        title.position = "left",
                        keywidth       = unit(0.8, "cm"),
                        keyheight      = unit(0.4, "cm")
                      )) +

    geom_hline(yintercept = 1.0,
               color      = COL_BASELINE,
               linewidth  = 0.8,
               linetype   = "solid") +

    geom_segment(data = ct_annual,
                 aes(x    = x_start,
                     xend = x_end,
                     y    = mean_index,
                     yend = mean_index),
                 color       = COL_ANNUAL,
                 linewidth   = 0.7,
                 linetype    = "dashed",
                 inherit.aes = FALSE) +

    geom_text(data = ct_annual,
              aes(x     = x_end,
                  y     = mean_index,
                  label = sprintf("%d avg: %.2f", year, mean_index)),
              hjust       = 1.0,
              vjust       = -0.6,
              size        = 2.8,
              fontface    = "plain",
              color       = COL_ANNUAL,
              inherit.aes = FALSE) +

    geom_line(aes(y = index_plot),
              color     = COL_POINTS,
              linewidth = 0.5,
              alpha     = 0.6) +

    # Observed months: standard filled points
    geom_point(data  = ct_observed,
               aes(y = index_plot),
               color = COL_POINTS,
               size  = 1.8,
               alpha = 0.75) +

    # Interpolated baseline months: hollow open circles in muted grey
    {if (has_interp && nrow(ct_interp) > 0)
      geom_point(data   = ct_interp,
                 aes(y  = index_plot),
                 shape  = 21,
                 color  = COL_INTERP,
                 fill   = "white",
                 size   = 2.2,
                 stroke = 0.8)
    else list()} +

    # Truncated points: upward triangle at cap line
    geom_point(aes(y = index_cap),
               shape = 24,
               color = COL_CAP,
               fill  = COL_CAP,
               size  = 2.2,
               na.rm = TRUE) +

    annotate("segment",
             x      = as.Date("2022-01-01"),
             xend   = as.Date("2025-12-01"),
             y      = 1.0,
             yend   = min(ct_annual[year == 2025, mean_index], eff_cap),
             color  = tc,
             linewidth = 1.0,
             arrow  = arrow(length = unit(0.25, "cm"), type = "closed")) +

    annotate("text",
             x     = min(ct_data$year_month),
             y     = 1.0,
             label = sprintf("Baseline (1.0 | avg %s/mo)",
                             format(ct_baseline_avg, big.mark = ",")),
             hjust = 0,
             vjust = -0.5,
             size  = 3.0,
             color = COL_BASELINE) +

    scale_x_date(date_breaks = "6 months",
                 date_labels = "%b %Y",
                 expand      = expansion(mult = c(0.02, 0.06))) +

    scale_y_continuous(labels = function(x) sprintf("%.2f", x),
                       limits = c(y_lo, y_hi)) +

    labs(
      title    = paste0(ct, "  —  Detail"),      subtitle = subtitle_text,
      y        = "Quality of Life Index",
      caption  = caption_text
    ) +

    david_theme(
      x_axis_angle   = 45,
      remove_x_title = TRUE,
      plot_margin    = c(0.3, 0.6, 0.3, 0.3)
    ) +

    theme(legend.position = "none")

  filename <- paste0(sanitize_filename(ct), ".pdf")
  filepath <- file.path(PATHS$plots_detail, filename)

  ggsave(filepath,
         plot   = p,
         width  = OUTPUT$plot_width,
         height = OUTPUT$plot_height,
         device = OUTPUT$plot_format)

  print(p)
  Sys.sleep(0.5)

  cat(sprintf("  [%02d/%02d] %-45s -> %s  [%s]\n",
              i, nrow(detail_order),
              ct, filename, ct_direction))
}

# -----------------------------------------------------------------------------
# GENERATE EMERGING CATEGORY CHARTS
# One chart per emerging complaint type: raw monthly counts + linear trend.
# Earliest-year average shown as a reference line (analogous to the baseline
# line on standard detail charts). Growth factor annotated in subtitle.
#
# Chart elements:
#   - Seasonal shading (same warm/cool bands as other charts)
#   - Raw monthly count bars (muted blue-grey)
#   - Linear trend line (colored by slope direction)
#   - Annual average horizontal segments
#   - Earliest-year average reference line (dashed grey, labelled)
#   - Subtitle: growth factor (latest year avg / earliest year avg)
# -----------------------------------------------------------------------------

if (has_emerging && !is.null(index_emerging)) {

  emerging_order <- sort(unique(index_emerging$original_type))

  message(sprintf("\nGenerating %d emerging category charts...\n",
                  length(emerging_order)))

  for (i in seq_along(emerging_order)) {

    ct         <- emerging_order[i]
    ct_data    <- index_emerging[original_type == ct]
    fam        <- ct_data[1, family]
    bundle_lbl <- ct_data[1, bundle_label]
    ct_total   <- ct_data[1, total_count]

    # Growth factor from emerging_snapshot
    ct_snap <- if (!is.null(emerging_snapshot) && nrow(emerging_snapshot) > 0)
                 emerging_snapshot[original_type == ct]
               else NULL

    ct_first_year   <- if (!is.null(ct_snap) && nrow(ct_snap) > 0 &&
                           "first_year"      %in% names(ct_snap))
                         ct_snap[1, first_year]      else NA_integer_
    ct_first_mean   <- if (!is.null(ct_snap) && nrow(ct_snap) > 0 &&
                           "first_year_mean" %in% names(ct_snap))
                         ct_snap[1, first_year_mean] else NA_real_
    ct_growth_index <- if (!is.null(ct_snap) && nrow(ct_snap) > 0 &&
                           "growth_index"    %in% names(ct_snap))
                         ct_snap[1, growth_index]    else NA_real_

    # Direction label and pct change — same logic as standard detail charts
    # growth_index plays the role of index_2025: 1.0 = same as first year
    ct_pct_change  <- if (!is.na(ct_growth_index))
                        round((ct_growth_index - 1.0) * 100, 0)
                      else NA_real_
    ct_direction   <- if (!is.na(ct_growth_index))
                        classify_direction(ct_growth_index - 1.0)
                      else "Unknown"
    ct_pct_label   <- if (!is.na(ct_pct_change))
                        sprintf("%+.0f%%", ct_pct_change)
                      else ""

    # Subtitle — mirrors standard detail format as closely as possible
    subtitle_text <- as.character(paste0(
      "EMERGING  |  ", ct_direction, " (", ct_pct_label, ")",
      "  |  Family: ", fam,
      "  |  n = ", format(ct_total, big.mark = ",")
    ))[1]

    # Caption
    ct_source <- if (!is.null(ct_snap) && nrow(ct_snap) > 0 &&
                     "emerging_source" %in% names(ct_snap))
                   as.character(ct_snap[1, emerging_source])
                 else "emerging"
    baseline_note <- if (ct_source == "anomalous") {
      paste0("Baseline period (", baseline_label, ") data unreliable -- treated as emerging")
    } else {
      paste0("No activity during baseline period (", baseline_label, ")")
    }
    caption_text <- as.character(paste0(
      "Bundle: ", bundle_lbl,
      "  |  ", baseline_note,
      "  |  Growth index = ", ct_first_year, " avg (", 
      format(round(ct_first_mean), big.mark = ","), "/mo)",
      " vs ", max(ct_data$year), " avg"
    ))[1]

    # Annual avg segments
    ct_annual <- ct_data[, .(
      mean_monthly = round(mean(monthly_count), 1),
      x_start      = min(year_month),
      x_end        = max(year_month)
    ), by = year]

    # Y axis
    y_max <- max(ct_data$monthly_count, na.rm = TRUE)
    y_hi  <- y_max * 1.18

    # Seasonal shading
    chart_years     <- sort(unique(ct_data$year))
    season_rects_ct <- as.data.table(do.call(rbind, lapply(chart_years, function(yr) {
      data.frame(xmin = as.Date(paste0(yr, "-03-01")),
                 xmax = as.Date(paste0(yr, "-09-01")),
                 season = "Spring/Summer")
    })))
    data_start <- min(ct_data$year_month)
    data_end   <- max(ct_data$year_month)
    season_rects_ct <- season_rects_ct[xmax > data_start & xmin < data_end]
    season_rects_ct[xmin < data_start, xmin := data_start]
    season_rects_ct[xmax > data_end,   xmax := data_end]

    p <- ggplot(ct_data, aes(x = year_month, y = monthly_count)) +

      # Seasonal shading
      geom_rect(data        = season_rects_ct,
                aes(xmin    = xmin,
                    xmax    = xmax,
                    ymin    = -Inf,
                    ymax    = Inf,
                    fill    = season),
                inherit.aes = FALSE,
                alpha       = 0.83) +

      scale_fill_manual(values = c("Spring/Summer" = "#FFF3CC"),
                        name   = "Season",
                        guide  = guide_legend(
                          nrow           = 1,
                          title.position = "left",
                          keywidth       = unit(0.8, "cm"),
                          keyheight      = unit(0.4, "cm"))) +

      # Line + dots — matches standard detail chart style
      geom_line(color     = COL_POINTS,
                linewidth = 0.5,
                alpha     = 0.6) +

      geom_point(color = COL_POINTS,
                 size  = 1.8,
                 alpha = 0.75) +

      # Linear trend line — colored by growth direction
      geom_smooth(method    = "lm",
                  formula   = y ~ x,
                  se        = FALSE,
                  color     = status_color(ct_direction),
                  linewidth = 1.2,
                  linetype  = "solid") +

      # Annual average segments
      geom_segment(data = ct_annual,
                   aes(x    = x_start,
                       xend = x_end,
                       y    = mean_monthly,
                       yend = mean_monthly),
                   color       = COL_ANNUAL,
                   linewidth   = 0.7,
                   linetype    = "dashed",
                   inherit.aes = FALSE) +

      geom_text(data = ct_annual,
                aes(x     = x_end,
                    y     = mean_monthly,
                    label = sprintf("%d avg: %s/mo", year,
                                    format(round(mean_monthly), big.mark = ","))),
                hjust       = 1.0,
                vjust       = -0.6,
                size        = 2.8,
                color       = COL_ANNUAL,
                inherit.aes = FALSE) +

      scale_x_date(date_breaks = "6 months",
                   date_labels = "%b %Y",
                   expand      = expansion(mult = c(0.02, 0.06))) +

      scale_y_continuous(labels = function(x) format(x, big.mark = ","),
                         limits = c(0, y_hi),
                         expand = expansion(mult = c(0, 0))) +

      labs(
        title    = paste0(ct, "  --  Emerging"),
        subtitle = subtitle_text,
        y        = "Monthly Complaint Count",
        x        = NULL,
        caption  = caption_text
      ) +

      david_theme(
        x_axis_angle   = 45,
        remove_x_title = TRUE,
        plot_margin    = c(0.3, 0.6, 0.3, 0.3)
      ) +

      theme(legend.position = "none")

    filename <- paste0(sanitize_filename(ct), "_EMERGING.pdf")
    filepath <- file.path(PATHS$plots_emerging, filename)

    ggsave(filepath,
           plot   = p,
           width  = OUTPUT$plot_width,
           height = OUTPUT$plot_height,
           device = OUTPUT$plot_format)

    print(p)
    Sys.sleep(0.5)

    cat(sprintf("  [%02d/%02d] %-45s -> %s  [%s (%s)]\n",
                i, length(emerging_order),
                ct, filename, ct_direction, ct_pct_label))
  }

} else {
  message("\nNo emerging types to chart.")
}

# -----------------------------------------------------------------------------
# SEASONAL CHART: Total complaints by month (all years, all complaint types)
# Months ordered by season: Dec, Jan, Feb (Winter) -> Mar, Apr, May (Spring)
#   -> Jun, Jul, Aug (Summer) -> Sep, Oct, Nov (Fall)
# -----------------------------------------------------------------------------
message("\nGenerating seasonal complaints chart...")

# Load both baseline and monitor raw data
baseline_raw <- readRDS(PATHS$processed_baseline)
monitor_raw  <- readRDS(PATHS$processed_monitor)

all_data <- rbindlist(list(baseline_raw, monitor_raw), use.names = TRUE)

# Aggregate by calendar month only (ignore year)
by_month <- all_data[, .(total = .N), by = .(month = month(created_date))]
setorder(by_month, month)

# Seasonal month order: Winter -> Spring -> Summer -> Fall
seasonal_order <- c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
season_labels  <- c(
  "Dec\nWinter", "Jan\nWinter", "Feb\nWinter",
  "Mar\nSpring", "Apr\nSpring", "May\nSpring",
  "Jun\nSummer", "Jul\nSummer", "Aug\nSummer",
  "Sep\nFall",   "Oct\nFall",   "Nov\nFall"
)

# Season color bands for background shading
season_colors <- c(
  "Winter" = "#DDEEFF",   # light blue
  "Spring" = "#EEFFDD",   # light green
  "Summer" = "#FFF8DD",   # light yellow
  "Fall"   = "#FFE8DD"    # light orange
)

by_month[, month_ordered := factor(month,
                                    levels   = seasonal_order,
                                    labels   = season_labels)]
by_month[, season := c("Winter","Winter","Winter",
                        "Spring","Spring","Spring",
                        "Summer","Summer","Summer",
                        "Fall","Fall","Fall")[match(month, seasonal_order)]]

setorder(by_month, month_ordered)

# Background season shading rectangles
season_rects <- data.frame(
  xmin   = c(0.5, 3.5, 6.5, 9.5),
  xmax   = c(3.5, 6.5, 9.5, 12.5),
  season = c("Winter", "Spring", "Summer", "Fall")
)

p_seasonal <- ggplot(by_month, aes(x = month_ordered, y = total)) +

  # Season background shading
  geom_rect(data        = season_rects,
            aes(xmin    = xmin,
                xmax    = xmax,
                ymin    = -Inf,
                ymax    = Inf,
                fill    = season),
            inherit.aes = FALSE,
            alpha       = 0.7) +

  scale_fill_manual(values = season_colors, name = "Season") +

  # Bars
  geom_col(fill = "#0072B2", width = 0.7) +

  # Count labels on top of bars
  geom_text(aes(label = format(round(total / 1000, 0),
                                big.mark = ","),
                y     = total),
            vjust  = -0.4,
            size   = 3.2,
            color  = "#333333") +

  scale_y_continuous(labels = function(x) paste0(x / 1000, "K"),
                     expand = expansion(mult = c(0, 0.08))) +

  labs(
    title    = "NYC 311 Complaints by Month — Seasonal Pattern",
    subtitle = "All complaint types combined, all years (2020-2025), counts in thousands",
    x        = NULL,
    y        = "Total Complaints",
    caption  = "Source: NYC 311 Service Requests. Months ordered by season."
  ) +

  david_theme(
    remove_x_title = TRUE,
    plot_margin    = c(0.3, 0.5, 0.3, 0.3)
  ) +

  theme(legend.position = "right")

seasonal_path <- file.path(PATHS$plots, "SEASONAL_COMPLAINTS_BY_MONTH.pdf")

ggsave(seasonal_path,
       plot   = p_seasonal,
       width  = OUTPUT$plot_width,
       height = OUTPUT$plot_height,
       device = OUTPUT$plot_format)

print(p_seasonal)
Sys.sleep(0.5)

cat(sprintf("  Saved: %s\n", basename(seasonal_path)))

# -----------------------------------------------------------------------------
# WRAP UP
# -----------------------------------------------------------------------------
elapsed <- proc.time() - prog_start

message("\n", strrep("=", 80))
message("VISUALIZATIONS COMPLETE")
message(strrep("=", 80))
message(sprintf("  Family charts   : %d  -> %s", nrow(complaint_order), PATHS$plots_family))
message(sprintf("  Detail charts   : %d  -> %s", nrow(detail_order),    PATHS$plots_detail))
message(sprintf("  Emerging charts : %d  -> %s",
                if (has_emerging) uniqueN(index_emerging$original_type) else 0L,
                PATHS$plots_emerging))
message(sprintf("  Elapsed time    : %.1f seconds", elapsed["elapsed"]))
message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
