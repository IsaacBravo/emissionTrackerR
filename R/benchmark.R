#' Benchmark Emissions Across Multiple Methods
#' 
#' Runs each method and logs emissions, duration, and custom metadata.
#'
#' @param methods A named list of functions. Each function should return a performance metric (e.g. accuracy).
#' @param setup_fn A function that returns a list of shared data to pass to each method.
#' @param csv_path Path to the CSV file to append logs to.
#' @param json_path Path to the JSON file to append logs to.
#'
#' @return A data.frame summarizing emissions and accuracy per method
#' @export
benchmark_emissions <- function(methods, setup_fn, 
                                csv_path = "emissions_log_comparative.csv", 
                                json_path = "emissions_log_comparative.json") {
  results <- lapply(names(methods), function(method) {
    # Setup shared data
    data_parts <- setup_fn()
    
    tracker <- EmissionsTracker$new()
    tracker$start()
    
    metric <- methods[[method]](data_parts$train, data_parts$test)
    
    emissions <- tracker$stop()
    metadata <- collect_metadata(duration = tracker$duration,
                                 emissions = emissions,
                                 project_name = paste0("benchmark_", method))
    metadata$method <- method
    metadata$metric <- metric  # e.g., accuracy
    
    # Append logs
    append_emissions_logs(metadata, csv_path, json_path)
    
    # Return summary row
    data.frame(
      method = method,
      metric = metric,
      emissions_kg = round(emissions, 6),
      duration_sec = round(tracker$duration, 3),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, results)
}
