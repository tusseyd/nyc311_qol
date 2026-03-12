# =============================================================================
# 05_heatmap.R
# NYC 311 Quality of Life Index
#
# PURPOSE: Generates bundle scorecard PDFs providing a high-level summary view
# of the entire QoL Index. Each scorecard shows all complaint families in a
# bundle as a color-coded tile grid, allowing at-a-glance assessment of 2025
# status across the full index. Chart dimensions and format are controlled by
# OUTPUT settings in config.R.
#
# STANDARD SCORECARDS (one per bundle):
#   Tiles are sorted worst to best (highest index at top) and colored by the
#   7-level status palette. Each tile displays two lines:
#     Line 1 — 2025 index value (bold); Tier 2 families marked with *
#     Line 2 — baseline monthly average -> 2025 monthly average (e.g. 1,240 -> 1,984/mo)
#   Families with index > 2.0 (more than double the baseline) are flagged
#   with a bold black border.
#
# EMERGING SCORECARDS (one per bundle containing emerging types):
#   Uses the growth index (2025 avg / first active year avg) in place of the
#   standard QoL index. Tile format mirrors the standard scorecard:
#     Line 1 — growth index value (bold)
#     Line 2 — first year avg (year) -> 2025 avg/mo
#   Caption explicitly flags that the emerging index is not directly
#   comparable to the standard QoL index.
#   Types with growth index > 2.0 are flagged with a bold black border.
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
# BUNDLE ORDER:
#   Scorecards are generated in the order defined by bundle_order (alphabetical
#   by bundle key). Edit bundle_order in the script to change sequence.
#
# Inputs:  output/tables/index_snapshot.csv
#          output/tables/emerging_snapshot.csv  (optional)
#          output/data/baseline_stats.RDS
# Outputs: output/plots/heatmap/SCORECARD_<BUNDLE>.pdf
#          output/plots/heatmap/SCORECARD_EMERGING_<BUNDLE>.pdf
# =============================================================================

message("\n", strrep("=", 80))
message("NYC 311 QUALITY OF LIFE INDEX - BUNDLE SCORECARDS")
message(strrep("=", 80))
message("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

source("config.R")

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(grid)
})

# -----------------------------------------------------------------------------
# david_theme (inline)
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
    panel.background      = ggplot2::element_rect(fill = "white", color = NA),
    panel.border          = ggplot2::element_blank(),
    panel.grid            = ggplot2::element_blank(),
    plot.background       = ggplot2::element_rect(fill = "white", color = NA),
    plot.title            = ggplot2::element_text(face   = "bold",
                                                  size   = plot_title_size,
                                                  hjust  = 0.5,
                                                  family = base_family),
    plot.title.position   = "plot",
    plot.subtitle         = ggplot2::element_text(size   = plot_subtitle_size,
                                                  hjust  = 0.5,
                                                  color  = "gray40",
                                                  family = base_family),
    plot.caption          = ggplot2::element_text(size   = text_size - 4,
                                                  color  = "gray55",
                                                  hjust  = 0,
                                                  family = base_family),
    plot.caption.position = "plot",
    legend.position       = "bottom",
    legend.title          = ggplot2::element_text(size = text_size - 2,
                                                  face = "bold"),
    legend.text           = ggplot2::element_text(size = text_size - 3),
    axis.text.x           = ggplot2::element_text(size   = x_axis_text_size,
                                                  face   = "bold",
                                                  family = base_family),
    axis.text.y           = ggplot2::element_text(size   = y_axis_text_size,
                                                  family = base_family),
    axis.ticks            = ggplot2::element_blank()
  )

  if (remove_x_title) t <- t + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  if (remove_y_title) t <- t + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  if (!is.null(plot_margin)) {
    t <- t + ggplot2::theme(plot.margin = ggplot2::margin(
      plot_margin[1], plot_margin[2], plot_margin[3], plot_margin[4], unit = "cm"
    ))
  }
  t
}

# -----------------------------------------------------------------------------
# STATUS COLORS AND LEVELS
# -----------------------------------------------------------------------------
STATUS_COLORS <- c(
  "Greatly Improved"  = "#006837",
  "Improved"          = "#31A354",
  "Slightly Improved" = "#78C679",
  "Little Changed"    = "#969696",
  "Slightly Worse"    = "#FD8D3C",
  "Worse"             = "#E6550D",
  "Much Worse"        = "#A50F15"
)

STATUS_LEVELS <- names(STATUS_COLORS)

# White text on dark tiles, black on light
tile_text_color <- function(status) {
  ifelse(status %in% c("Greatly Improved", "Improved",
                        "Worse", "Much Worse"),
         "white", "black")
}

# Sanitize bundle name for filename
sanitize_filename <- function(name) {
  name <- gsub("[/\\\\:*?\"<>|]", "_", name)
  name <- gsub("\\s+", "_", name)
  name <- gsub("_{2,}", "_", name)
  trimws(toupper(name), whitespace = "_")
}

# -----------------------------------------------------------------------------
# LOAD DATA
# -----------------------------------------------------------------------------
message("Loading snapshot data...")

snapshot_path <- file.path(PATHS$tables, "index_snapshot.csv")
if (!file.exists(snapshot_path))
  stop("Snapshot CSV not found. Run 03_analysis.R first.")

snap <- fread(snapshot_path)
snap[, direction := factor(direction, levels = STATUS_LEVELS)]
snap[, txt_color := tile_text_color(as.character(direction))]

# Load baseline stats to get average monthly baseline per complaint type
baseline_stats <- readRDS(PATHS$baseline_stats)
baseline_avgs  <- baseline_stats[, .(
  baseline_avg = round(mean(baseline_mean, na.rm = TRUE), 0)
), by = complaint_type]

snap <- merge(snap, baseline_avgs, by = "complaint_type", all.x = TRUE)
snap[, current_avg := round(index_2025 * baseline_avg, 0)]

# Two-line tile label: index value on top, baseline avg below
snap[, tile_label := sprintf("%.2f%s\navg %s/mo",
                              index_2025,
                              ifelse(tier == 2L, "*", ""),
                              format(baseline_avg, big.mark = ","))]

cat(sprintf("  Families        : %d\n", nrow(snap)))
cat(sprintf("  Bundles         : %d\n", uniqueN(snap$bundle_key)))

for (d in c(PATHS$plots, PATHS$plots_heatmap))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# Bundle order — all 9 bundles
# Edit this list to change which bundles appear and in what order
bundle_order <- c("blight_nuisance", "housing_quality", "public_health",
                  "sanitation", "social_distress", "street_safety",
                  "streets_signals", "transportation", "water_sewers_trees")

# Dynamic baseline label — reflects PHASE1_YEARS from config
baseline_label <- if (length(PHASE1_YEARS) == 1L) {
  as.character(PHASE1_YEARS)
} else {
  paste0(min(PHASE1_YEARS), "-", max(PHASE1_YEARS))
}

# -----------------------------------------------------------------------------
# GENERATE ONE HEATMAP PER BUNDLE
# -----------------------------------------------------------------------------
message(sprintf("\nGenerating %d bundle scorecards...\n",
                length(bundle_order)))

for (bk in bundle_order) {

  bdata <- snap[bundle_key == bk]

  if (nrow(bdata) == 0) {
    cat(sprintf("  Skipping %s — no data\n", bk))
    next
  }

  bundle_label_text <- bdata[1, bundle_label]

  # Sort complaint types worst to best (highest index at top)
  setorder(bdata, -index_2025)
  bdata[, complaint_ordered := factor(complaint_type,
                                       levels = rev(unique(complaint_type)))]

  # Single x-axis column label
  bdata[, x_col := "2025 Index vs Baseline"]

  n_types <- nrow(bdata)

  # Identify which status levels are actually present in this bundle
  present_levels <- intersect(STATUS_LEVELS, as.character(bdata$direction))

  # Identify outliers: index > 2.0 (more than double the baseline)
  bdata[, is_outlier := index_2025 > 2.0]
  
  p <- ggplot(bdata, aes(x    = x_col,
                          y    = complaint_ordered,
                          fill = direction)) +
    
    geom_tile(color = "white", linewidth = 1.2) +

    # Outlier border overlay
    geom_tile(data        = bdata[is_outlier == TRUE],
              color       = "gray15",
              fill        = NA,
              linewidth   = 1.5,
              inherit.aes = TRUE) +
    
    # Index value - bold and large
    geom_text(aes(label = sprintf("%.2f%s", index_2025,
                                  ifelse(tier == 2L, "*", "")),
                  color = .data[["txt_color"]]),
              size     = 5.5,
              fontface = "bold",
              vjust    = 0.0) +

    # Baseline -> current avg - smaller, below index value
    geom_text(aes(label = sprintf("%s --> %s/mo",
                                  format(baseline_avg, big.mark = ","),
                                  format(current_avg, big.mark = ",")),
                  color = .data[["txt_color"]]),
              size     = 3.8,
              fontface = "plain",
              vjust    = 2.2) +

    scale_fill_manual(
      values = STATUS_COLORS[present_levels],
      drop   = TRUE,
      name   = "2025 Status",
      guide  = guide_legend(
        nrow           = 1,
        title.position = "top",
        label.position = "bottom",
        keywidth       = unit(2.0, "cm"),
        keyheight      = unit(0.6, "cm")
      )
    ) +

    scale_color_identity() +

    scale_x_discrete(position = "top") +

    labs(
      title    = sprintf("NYC 311 Quality of Life Index  —  %s", bundle_label_text),
      subtitle = NULL,
      x        = NULL,
      y        = NULL,
      caption  = paste0(
        "Baseline: ", baseline_label, " monthly averages (seasonally adjusted).  ",
        if (any(bdata$tier == 2L))
          "* Tier 2: limited baseline reliability.  "
        else "",
        if (any(bdata$is_outlier == TRUE))
          "Bold border = statistical outlier (index > 2.0).  "
        else "",
        "Source: NYC 311 Service Requests."
      )
    ) +

    david_theme(
      text_size      = 13,
      remove_x_title = TRUE,
      remove_y_title = TRUE,
      plot_margin    = c(0.4, 2.0, 0.4, 2.0)
    ) +

    theme(legend.position = "none")

  print(p)
  Sys.sleep(0.5)

  filename <- paste0("SCORECARD_", sanitize_filename(bundle_label_text), ".pdf")
  filepath <- file.path(PATHS$plots_heatmap, filename)

  ggsave(filepath,
         plot   = p,
         width  = OUTPUT$plot_width,
         height = OUTPUT$plot_height,
         device = OUTPUT$plot_format)

  cat(sprintf("  %-35s -> %s\n", bundle_label_text, filename))
}

# -----------------------------------------------------------------------------
# EMERGING CATEGORY SCORECARDS
# One scorecard per bundle that contains emerging types.
# Tiles show growth index (mean_2025 / first_year_mean) using the same
# STATUS_COLORS and classify_direction thresholds as the standard scorecard.
# Caption clearly notes the endogenous baseline.
# -----------------------------------------------------------------------------

emerging_snap_path <- file.path(PATHS$tables, "emerging_snapshot.csv")
has_emerging <- file.exists(emerging_snap_path)

if (has_emerging) {

  emerging_snap <- fread(emerging_snap_path)
  emerging_snap[, direction := factor(direction, levels = STATUS_LEVELS)]
  emerging_snap[, txt_color := tile_text_color(as.character(direction))]

  emerging_bundles <- sort(unique(emerging_snap$bundle))
  message(sprintf("\nGenerating %d emerging scorecard(s)...\n",
                  length(emerging_bundles)))

  for (bk in emerging_bundles) {

    bdata <- emerging_snap[bundle == bk]
    if (nrow(bdata) == 0) next

    bundle_label_text <- bdata[1, bundle_label]

    # Sort worst to best — highest growth index at top
    setorder(bdata, -growth_index)
 
    # Identify outliers: growth index > 2.0 (more than double first-year avg)
    bdata[, is_outlier := growth_index > 2.0]
    bdata[, type_ordered := factor(original_type,
                                    levels = rev(unique(original_type)))]
    bdata[, x_col := "Growth vs First Year"]

    present_levels <- intersect(STATUS_LEVELS, as.character(bdata$direction))

    p <- ggplot(bdata, aes(x    = x_col,
                            y    = type_ordered,
                            fill = direction)) +

      geom_tile(color = "white", linewidth = 1.2) +

      # Growth index — bold, top line (matches standard scorecard format)
      geom_text(aes(label = sprintf("%.2f", growth_index),
                    color = .data[["txt_color"]]),
                size     = 5.5,
                fontface = "bold",
                vjust    = 0.0) +

      # First year avg --> 2025 avg (mirrors "baseline --> current" in standard)
      geom_text(aes(label = sprintf("%s (%s) --> %s/mo",
                                     format(round(first_year_mean), big.mark = ","),
                                     first_year,
                                     format(round(mean_2025), big.mark = ",")),
                    color = .data[["txt_color"]]),
                size     = 3.8,
                fontface = "plain",
                vjust    = 2.2) +

      scale_fill_manual(
        values = STATUS_COLORS[present_levels],
        drop   = TRUE,
        name   = "Growth Status",
        guide  = guide_legend(
          nrow           = 1,
          title.position = "top",
          label.position = "bottom",
          keywidth       = unit(2.0, "cm"),
          keyheight      = unit(0.6, "cm")
        )
      ) +

      scale_color_identity() +
      scale_x_discrete(position = "top") +

      labs(
        title    = sprintf("NYC 311  —  %s  —  Emerging Categories", bundle_label_text),
        subtitle = sprintf(
          "Index = 2025 avg / first active year avg  |  No %s baseline available",
          baseline_label),
        x        = NULL,
        y        = NULL,
        caption  = paste0(
          "Emerging: no activity in baseline period (", baseline_label, "); ",
          "index uses first active calendar year as self-baseline.  ",
          "Not directly comparable to standard QoL index.  ",
          if (any(bdata$is_outlier == TRUE))
            "Bold border = high growth (index > 2.0).  "
          else "",
          "Source: NYC 311 Service Requests."
        )
      ) +

      david_theme(
        text_size      = 13,
        remove_x_title = TRUE,
        remove_y_title = TRUE,
        plot_margin    = c(0.4, 2.0, 0.4, 2.0)
      ) +

      theme(legend.position = "none")

    print(p)
    Sys.sleep(0.5)

    filename <- paste0("SCORECARD_EMERGING_", sanitize_filename(bundle_label_text), ".pdf")
    filepath <- file.path(PATHS$plots_heatmap, filename)

    ggsave(filepath,
           plot   = p,
           width  = OUTPUT$plot_width,
           height = OUTPUT$plot_height,
           device = OUTPUT$plot_format)
    
    cat(sprintf("  %-35s -> %s\n", bundle_label_text, filename))
  }

} else {
  message("\nNo emerging snapshot found — skipping emerging scorecards.")
}

# -----------------------------------------------------------------------------
# WRAP UP
# -----------------------------------------------------------------------------
message("\n", strrep("=", 80))
message("BUNDLE SCORECARDS COMPLETE")
message(strrep("=", 80))
message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
