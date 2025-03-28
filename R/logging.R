#' Log Emissions to JSON
#' @encoding UTF-8
#' @title Log Emissions to JSON
#' @description Writes a metadata list of emissions results to a .json file.
#'
#' @param data A named list of metadata fields (e.g., from `collect_metadata()`).
#' @param filepath Path to the JSON file (default is `"emissions_log.json"`).
#'
#' @return None (writes file as side effect).
#' @export
#' @importFrom jsonlite write_json
#' @examples
#' log_emissions(list(timestamp = Sys.time(), emissions = 0.001))

log_emissions <- function(data, filepath = "emissions_log.json") {
  jsonlite::write_json(data, filepath, auto_unbox = TRUE, pretty = TRUE)
}


#' Log Emissions to a CSV File (Single Row)
#'
#' Flattens and writes a metadata list of emissions results to a `.csv` file.
#' Designed for one-time runs with a single record.
#'
#' @param data A named list of metadata fields (e.g., from `collect_metadata()`).
#' @param filepath Path to the CSV file (default is `"emissions_log.csv"`).
#'
#' @return None (writes file as side effect).
#' @export
#' @importFrom utils read.csv write.csv
#' @examples
#' log_emissions_csv(list(project_name = "demo", emissions = 0.002))

log_emissions_csv <- function(data, filepath = "emissions_log.csv") {
  flat <- unlist(data)
  write.csv(as.data.frame(t(flat)), filepath, row.names = FALSE)
}


#' Append Emissions Metadata to Log Files (CSV and JSON)
#'
#' Appends metadata from a single run to both a cumulative CSV and JSON file.
#' Automatically flattens nested fields (like `equivalents`) and aligns columns
#' with existing logs.
#'
#' @param metadata A metadata list from `collect_metadata()`.
#' @param csv_path Path to the CSV log file.
#' @param json_path Path to the JSON log file.
#'
#' @return None (writes files as side effect).
#' @export
#' @importFrom jsonlite write_json fromJSON
#' @importFrom utils read.csv write.csv
#' @examples
#' meta <- collect_metadata(duration = 3, emissions = 0.001)
#' append_emissions_logs(meta)

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
