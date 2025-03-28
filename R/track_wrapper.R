track_emissions_for <- function(task_name, expr) {
  tracker <- EmissionsTracker$new()
  tracker$start()
  result <- eval(substitute(expr), envir = parent.frame())
  emissions <- tracker$stop()
  metadata <- collect_metadata(tracker$duration, emissions, project_name = task_name)
  log_emissions(metadata)
  log_emissions_csv(metadata)
  return(result)
}