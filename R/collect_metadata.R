#' @encoding UTF-8
#' Null Coalesce Operator (`%||%`)
#'
#' Returns the first argument if it is not `NULL`, otherwise returns the second.
#' Used for safe fallback values in metadata fields.
#'
#' @param x First value.
#' @param y Fallback value if `x` is NULL.
#' @return `x` if not NULL, otherwise `y`.
#' @keywords internal
# Full version of collect_metadata with geo + equivalents

`%||%` <- function(x, y) if (!is.null(x)) x else y


#' Get Full Country Name from ISO Code
#'
#' Converts a two-letter ISO country code (e.g., "DE") to the full country name
#' (e.g., "Germany") using the `countrycode` package.
#'
#' @param iso_code A 2-letter country ISO code (e.g., "US", "DE").
#' @return A full country name as a string.
#' @importFrom countrycode countrycode
#' @keywords internal

get_country_full_name <- function(iso_code) {
  full_name <- countrycode::countrycode(iso_code, origin = "iso2c", destination = "country.name")
  return(full_name %||% iso_code)  # fallback to ISO code if lookup fails
}


#' Get Geolocation Metadata from IP
#'
#' Queries the `ipinfo.io` API to retrieve location-based metadata (country, region, city, coordinates).
#'
#' @return A named list with `country_iso_code`, `country_name`, `region`, `city`, `latitude`, and `longitude`.
#' @importFrom jsonlite fromJSON
#' @keywords internal

get_geo_info <- function() {
  tryCatch({
    response <- jsonlite::fromJSON("https://ipinfo.io/json")
    loc_parts <- strsplit(response$loc, ",")[[1]]
    list(
      country_iso_code = response$country,
      country_name = get_country_full_name(response$country),
      region = response$region,
      city = response$city,
      latitude = as.numeric(loc_parts[1]),
      longitude = as.numeric(loc_parts[2])
    )
  }, error = function(e) {
    list(
      country_iso_code = NA,
      country_name = NA,
      region = NA,
      city = NA,
      latitude = NA,
      longitude = NA
    )
  })
}


#' Calculate Emission Equivalents
#' @encoding UTF-8  
#' @title Calculate Emission Equivalents
#' @description Converts emissions (in kg CO2) into human-readable equivalents
#' @param emissions_kg Emissions in kilograms of CO2.
#' @return A list with `lightbulb_seconds`, `phone_charge_seconds`, and `car_km`.
#' @keywords internal

calculate_equivalents <- function(emissions_kg) {
  list(
    lightbulb_seconds = round(emissions_kg / 0.00006),
    phone_charge_seconds = round(emissions_kg / 0.00000822),
    car_km = round(emissions_kg / 0.192, 2)
  )
}


#' Collect Emissions Metadata
#'
#' Aggregates metadata about an experiment run, including timestamp, duration,
#' system and location info, energy usage, and equivalent emissions in real-world terms.
#'
#' @param duration Duration of the task (in seconds).
#' @param emissions Emissions produced (in kilograms of CO2).
#' @param project_name Optional project name (defaults to `"codecarbon"`).
#' @param ... Additional parameters (not currently used).
#'
#' @return A named list of metadata fields.
#' @export
#' @examples
#' collect_metadata(duration = 5, emissions = 0.001)

collect_metadata <- function(duration, emissions, project_name = "codecarbon", ...) {
  geo <- get_geo_info()
  equivalents <- calculate_equivalents(emissions)
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  run_id <- paste0(sample(c(0:9, letters), 20, replace = TRUE), collapse = "")
  emissions_rate <- emissions / duration

  cpu_power <- 45.0
  ram_power <- 4.0
  gpu_power <- 0.0

  cpu_energy <- (cpu_power * duration) / 3600
  ram_energy <- (ram_power * duration) / 3600
  gpu_energy <- (gpu_power * duration) / 3600

  list(
    timestamp = timestamp,
    project_name = project_name,
    experiment_id = run_id,
    duration = duration,
    emissions = emissions,
    emissions_rate = emissions_rate,
    equivalents = equivalents,
    cpu_power = cpu_power,
    gpu_power = gpu_power,
    ram_power = ram_power,
    cpu_energy = cpu_energy,
    gpu_energy = gpu_energy,
    ram_energy = ram_energy,
    energy_consumed = cpu_energy + ram_energy + gpu_energy,
    country_name = geo$country_name,
    country_iso_code = geo$country_iso_code,
    region = geo$region,
    latitude = geo$latitude,
    longitude = geo$longitude,
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    tracking_mode = "machine",
    log_level = "info",
    pue = 1.58
  )
}
