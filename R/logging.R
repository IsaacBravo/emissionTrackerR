log_emissions <- function(data, filepath = "emissions_log.json") {
  jsonlite::write_json(data, filepath, auto_unbox = TRUE, pretty = TRUE)
}

log_emissions_csv <- function(data, filepath = "emissions_log.csv") {
  flat <- unlist(data)
  write.csv(as.data.frame(t(flat)), filepath, row.names = FALSE)
}