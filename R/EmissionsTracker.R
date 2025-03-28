EmissionsTracker <- R6::R6Class(
  "EmissionsTracker",
  public = list(
    start_time = NULL,
    end_time = NULL,
    emissions = NULL,
    duration = NULL,
    energy_factor = NULL,

    initialize = function(energy_factor = 0.0002) {
      self$energy_factor <- energy_factor
    },

    start = function() {
      self$start_time <- Sys.time()
      message("Tracking started at ", self$start_time)
    },

    stop = function() {
      self$end_time <- Sys.time()
      self$duration <- as.numeric(difftime(self$end_time, self$start_time, units = "secs"))
      self$emissions <- self$duration * self$energy_factor
      message("Tracking stopped at ", self$end_time)
      message("Estimated emissions (kg CO2): ", round(self$emissions, 6))
      return(self$emissions)
    }
  )
)