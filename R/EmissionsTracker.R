#' EmissionsTracker R6 Class
#' 
#' A lightweight tracker for estimating carbon emissions (kg CO2) of a code block
#' by measuring the elapsed time and applying a fixed energy-to-emission factor.
#'
#' @description
#' The `EmissionsTracker` class allows you to estimate CO2 emissions
#' based on the duration of a computation and a fixed energy factor.
#'
#' @importFrom R6 R6Class
#' @field start_time POSIX time when tracking began.
#' @field end_time POSIX time when tracking stopped.
#' @field emissions Estimated emissions in kg CO2.
#' @field duration Duration of the computation (in seconds).
#' @field energy_factor Emissions per second (default is 0.0002 kg/sec).
#'
#' @param energy_factor A numeric value representing emissions per second (kg CO2/sec).
#'
#' @return An R6 object of class `EmissionsTracker`.
#' @export
#' @examples
#' tracker <- EmissionsTracker$new()
#' tracker$start()
#' Sys.sleep(1)
#' tracker$stop()

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