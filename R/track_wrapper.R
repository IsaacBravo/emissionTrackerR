#' Track Emissions for an Expression or Task
#' @encoding UTF-8
#' @title Track Emissions for an Expression or Task
#' @description Wraps a block of code (expression) to track execution time and estimate
#'
#' @param task_name A short string describing the task (e.g., `"train_model"`).
#' Used as the `project_name` in the metadata.
#' @param expr An R expression to evaluate (e.g., a block of code to measure).
#'
#' @return The result of evaluating `expr`, invisibly.
#' @export
#'
#' @examples
#' track_emissions_for("example_sleep", {
#'   Sys.sleep(2)
#' })
#'
#' if (requireNamespace("randomForest", quietly = TRUE)) {
#'   track_emissions_for("iris_rf", {
#'     model <- randomForest::randomForest(Species ~ ., data = iris)
#'   })
#' }

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