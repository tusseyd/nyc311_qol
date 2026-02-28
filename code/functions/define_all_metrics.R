################################################################################
# DEFINE ALL QUALITY METRICS
# File: define_all_metrics.R
################################################################################

define_all_metrics <- function(valid_zipcodes) {
  
  # Valid police precincts
  valid_precincts <- c(
    "PRECINCT 1", "PRECINCT 5", "PRECINCT 6", "PRECINCT 7", "PRECINCT 9",
    "PRECINCT 10", "PRECINCT 13", "PRECINCT 14", "PRECINCT 17", "PRECINCT 18",
    "PRECINCT 19", "PRECINCT 20", "PRECINCT 22", "PRECINCT 23", "PRECINCT 24",
    "PRECINCT 25", "PRECINCT 26", "PRECINCT 28", "PRECINCT 30", "PRECINCT 32",
    "PRECINCT 33", "PRECINCT 34", "PRECINCT 40", "PRECINCT 41", "PRECINCT 42",
    "PRECINCT 43", "PRECINCT 44", "PRECINCT 45", "PRECINCT 46", "PRECINCT 47",
    "PRECINCT 48", "PRECINCT 49", "PRECINCT 50", "PRECINCT 52", "PRECINCT 60",
    "PRECINCT 61", "PRECINCT 62", "PRECINCT 63", "PRECINCT 66", "PRECINCT 67",
    "PRECINCT 68", "PRECINCT 69", "PRECINCT 70", "PRECINCT 71", "PRECINCT 72",
    "PRECINCT 73", "PRECINCT 75", "PRECINCT 76", "PRECINCT 77", "PRECINCT 78",
    "PRECINCT 79", "PRECINCT 81", "PRECINCT 83", "PRECINCT 84", "PRECINCT 88",
    "PRECINCT 90", "PRECINCT 94", "PRECINCT 100", "PRECINCT 101", "PRECINCT 102",
    "PRECINCT 103", "PRECINCT 104", "PRECINCT 105", "PRECINCT 106", "PRECINCT 107",
    "PRECINCT 108", "PRECINCT 109", "PRECINCT 110", "PRECINCT 111", "PRECINCT 112",
    "PRECINCT 113", "PRECINCT 114", "PRECINCT 115", "PRECINCT 120", "PRECINCT 121",
    "PRECINCT 122", "PRECINCT 123", "UNSPECIFIED"
  )
  
  # Valid boroughs
  valid_boroughs <- c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", 
                      "STATEN ISLAND", "UNSPECIFIED")
  
  # Valid NYC ZIP codes
  valid_zip_codes <- valid_zipcodes$zip
  
  # Valid community boards (format: "## BOROUGH")
  valid_community_boards <- c(
    "01 BRONX", "01 BROOKLYN", "01 MANHATTAN", "01 QUEENS", "01 STATEN ISLAND",
    "02 BRONX", "02 BROOKLYN", "02 MANHATTAN", "02 QUEENS", "02 STATEN ISLAND",
    "03 BRONX", "03 BROOKLYN", "03 MANHATTAN", "03 QUEENS", "03 STATEN ISLAND",
    "04 BRONX", "04 BROOKLYN", "04 MANHATTAN", "04 QUEENS",
    "05 BRONX", "05 BROOKLYN", "05 MANHATTAN", "05 QUEENS",
    "06 BRONX", "06 BROOKLYN", "06 MANHATTAN", "06 QUEENS",
    "07 BRONX", "07 BROOKLYN", "07 MANHATTAN", "07 QUEENS",
    "08 BRONX", "08 BROOKLYN", "08 MANHATTAN", "08 QUEENS",
    "09 BRONX", "09 BROOKLYN", "09 MANHATTAN", "09 QUEENS",
    "10 BRONX", "10 BROOKLYN", "10 MANHATTAN", "10 QUEENS",
    "11 BRONX", "11 BROOKLYN", "11 MANHATTAN", "11 QUEENS",
    "12 BRONX", "12 BROOKLYN", "12 MANHATTAN", "12 QUEENS",
    "13 BROOKLYN", "13 QUEENS",
    "14 BROOKLYN", "14 QUEENS",
    "15 BROOKLYN",
    "16 BROOKLYN",
    "17 BROOKLYN",
    "18 BROOKLYN",
    "UNSPECIFIED BRONX", "UNSPECIFIED BROOKLYN", "UNSPECIFIED MANHATTAN",
    "UNSPECIFIED QUEENS", "UNSPECIFIED STATEN ISLAND",
    "0 UNSPECIFIED"
  )
  
  
  # Define metrics list
  metrics <- list(
    
    # TEMPORAL ANOMALIES
    closed_before_created = list(
      name = "Negative or Zero duration: Closed Before Created",
      calc_type = "condition",
      chart_type = "p",
      condition = "!is.na(duration_days) & duration_days <= 0",
      sample_filter = NULL
    ),
    
    resolution_before_created = list(
      name = "Resolution Action Updated Before Created",
      calc_type = "condition",
      chart_type = "p",
      condition = paste(
        "!is.na(resolution_action_updated_date) &",
        "!is.na(created_date) &",
        "resolution_action_updated_date <= created_date"
      ),
      sample_filter = NULL
    ),
    
    resolution_late_update = list(
      name = "Resolution Action Updated >30 Days After Closure",
      calc_type = "condition",
      chart_type = "p",
      condition = paste(
        "!is.na(resolution_action_updated_date) &",
        "(status == 'CLOSED' | !is.na(closed_date)) &",
        "(!is.na(closed_date)) &",
        "as.numeric(difftime(resolution_action_updated_date, closed_date, units = 'days')) > 30"
      ),
      sample_filter = NULL
    ),
    
    status_closed_no_date = list(
      name = "Status is 'CLOSED', But No Closed Date",
      calc_type = "condition",
      chart_type = "p",
      condition = "status == 'CLOSED' & is.na(closed_date)",
      sample_filter = NULL
    ),
    
    closed_date_but_not_closed_status = list(
      name = "The closed_date is presence, But Status Not 'CLOSED'",
      calc_type = "condition",
      chart_type = "p",
      condition = "!is.na(closed_date) & status != 'CLOSED'",
      sample_filter = NULL
    ),
    
    closed_date_at_midnight = list(
      name = "Closed Date at Exactly Midnight",
      calc_type = "condition",
      chart_type = "p",
      condition = paste(
        "!is.na(closed_date) &",
        "hour(closed_date) == 0 &",
        "minute(closed_date) == 0 &",
        "second(closed_date) == 0"
      ),
      sample_filter = NULL
    ),
    
    closed_date_at_noon = list(
      name = "Closed Date at Exactly Noon",
      calc_type = "condition",
      chart_type = "p",
      condition = paste(
        "!is.na(closed_date) &",
        "hour(closed_date) == 12 &",
        "minute(closed_date) == 0 &",
        "second(closed_date) == 0"
      ),
      sample_filter = NULL
    ),
    
    created_date_at_midnight = list(
      name = "Created Date at Exactly Midnight",
      calc_type = "condition",
      chart_type = "p",
      condition = paste(
        "!is.na(created_date) &",
        "hour(created_date) == 0 &",
        "minute(created_date) == 0 &",
        "second(created_date) == 0"
      ),
      sample_filter = NULL
    ),
    
    extreme_duration = list(
      name = "Extreme Duration Greater Than 730 Days",
      calc_type = "condition",
      chart_type = "p",
      condition = "!is.na(.SD$duration_days) & .SD$duration_days > 730",
      sample_filter = NULL
    ),
    
    improbably_short_duration = list(
      name = "Improbably Short Duration Less Than 40 Seconds",
      calc_type = "condition",
      chart_type = "p",
      condition = "!is.na(.SD$duration_days) & .SD$duration_days > 0 & .SD$duration_days <= (40/86400)",
      sample_filter = NULL
    ),
    
    # GEOGRAPHIC MISMATCHES
    cross_street_1_mismatch = list(
      name = "Cross Street 1 vs Intersection Street 1 Mismatch",
      calc_type = "condition",
      chart_type = "p",
      condition = paste(
        "xor(is.na(cross_street_1), is.na(intersection_street_1)) |",
        "(!is.na(cross_street_1) & !is.na(intersection_street_1) &",
        "cross_street_1 != intersection_street_1)"
      ),
      sample_filter = NULL
    ),
    
    cross_street_2_mismatch = list(
      name = "Cross Street 2 vs Intersection Street 2 Mismatch",
      calc_type = "condition",
      chart_type = "p",
      condition = paste(
        "xor(is.na(cross_street_2), is.na(intersection_street_2)) |",
        "(!is.na(cross_street_2) & !is.na(intersection_street_2) &",
        "cross_street_2 != intersection_street_2)"
      ),
      sample_filter = NULL
    ),
    
    street_landmark_mismatch = list(
      name = "Street Name vs Landmark Mismatch",
      calc_type = "condition",
      chart_type = "p",
      condition = paste(
        "xor(is.na(street_name), is.na(landmark)) |",
        "(!is.na(street_name) & !is.na(landmark) &",
        "street_name != landmark)"
      ),
      sample_filter = NULL
    ),
    
    # DOMAIN VALIDATION
    invalid_zip = list(
      name = "Invalid ZIP Codes",
      calc_type = "domain_validation",
      chart_type = "p",
      field = "incident_zip",
      valid_values = valid_zip_codes,
      sample_filter = "!is.na(incident_zip)"
    ),
    
    invalid_community_board = list(
      name = "Invalid Community Board",
      calc_type = "domain_validation",
      chart_type = "p",
      field = "community_board",
      valid_values = valid_community_boards,
      sample_filter = "!is.na(community_board)"
    ),
    
    invalid_police_precinct = list(
      name = "Invalid Police Precinct",
      calc_type = "domain_validation",
      chart_type = "p",
      field = "police_precinct",
      valid_values = valid_precincts,
      sample_filter = "!is.na(police_precinct)"
    ),
    
    # SPECIAL METRICS
    overall_missing = list(
      name = "Overall Missing Data Percentage",
      calc_type = "special",
      chart_type = "xbar"
    ),
    
    mean_response_time = list(
      name = "Mean Response Time - Top 100",
      calc_type = "special",
      chart_type = "xbar"
    ),
    
    median_response_time = list(
      name = "Median Response Time - Top 100",
      calc_type = "special",
      chart_type = "xbar"
    )
  )
  
  return(metrics)
}