log_emissions <- function(data, filepath = "emissions_log.json") {
  jsonlite::write_json(data, filepath, auto_unbox = TRUE, pretty = TRUE)
}

log_emissions_csv <- function(data, filepath = "emissions_log.csv") {
  flat <- unlist(data)
  write.csv(as.data.frame(t(flat)), filepath, row.names = FALSE)
}

append_emissions_logs <- function(metadata,
                                  csv_path = "emissions_log.csv",
                                  json_path = "emissions_log.json") {
  # Flatten equivalents for tidy CSV
  flat_data <- metadata
  if (!is.null(metadata$equivalents)) {
    for (field in names(metadata$equivalents)) {
      flat_data[[paste0("equivalent_", field)]] <- metadata$equivalents[[field]]
    }
    flat_data$equivalents <- NULL
  }
  
  # Flatten metadata to single-row data frame
  new_row <- as.data.frame(t(unlist(flat_data)), stringsAsFactors = FALSE)
  
  # Append to CSV with column alignment
  if (file.exists(csv_path)) {
    existing <- read.csv(csv_path, stringsAsFactors = FALSE)
    
    # Match column names: add NAs for new columns, remove extras
    for (col in setdiff(names(existing), names(new_row))) {
      new_row[[col]] <- NA
    }
    for (col in setdiff(names(new_row), names(existing))) {
      existing[[col]] <- NA
    }
    
    # Reorder to match
    new_row <- new_row[names(existing)]
    combined <- rbind(existing, new_row)
    write.csv(combined, csv_path, row.names = FALSE)
  } else {
    write.csv(new_row, csv_path, row.names = FALSE)
  }
  
  # Append to JSON
  if (file.exists(json_path)) {
    existing_json <- fromJSON(json_path, simplifyDataFrame = FALSE)
    combined_json <- append(existing_json, list(metadata))
  } else {
    combined_json <- list(metadata)
  }
  write_json(combined_json, json_path, auto_unbox = TRUE, pretty = TRUE)
  
  message("Appended to CSV and JSON logs.")
}
