# Get chart type for a metric
get_chart_type <- function(metric_name, all_quality_metrics) {
  # Find the metric in the list
  for (m_name in names(all_quality_metrics)) {
    if (all_quality_metrics[[m_name]]$name == metric_name) {
      return(all_quality_metrics[[m_name]]$chart_type)
    }
  }
  # Default to p-chart if not found
  return("p")
}