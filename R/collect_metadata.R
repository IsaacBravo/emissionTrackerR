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

## RAM INFO
#' Collect RAM Information on Windows
#'
#' Retrieves detailed RAM information specific to Windows OS.
get_windows_ram_info <- function() {
  if (.Platform$OS.type == "windows") {
    ram_info <- list()
    
    # Function to safely execute wmic command and get output
    run_wmic <- function(command) {
      result <- tryCatch({
        system(command, intern = TRUE, ignore.stderr = TRUE)
      }, error = function(e) {
        warning(paste("Error executing command:", command, "-", e$message))
        return(NULL)
      })
      if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
        warning(paste("Command failed:", command, "- Exit code:", attr(result, "status")))
        return(NULL)
      }
      return(result)
    }
    
    # Total Physical Memory
    total_mem_output <- run_wmic("wmic ComputerSystem get TotalPhysicalMemory")
    if (!is.null(total_mem_output)) {
      total_mem_line <- grep("[0-9]+", total_mem_output, value = TRUE)
      if (length(total_mem_line) > 0) {
        ram_info$total_ram_bytes <- as.numeric(trimws(total_mem_line))
        ram_info$total_ram_gb <- ram_info$total_ram_bytes / (1024^3)
      }
    }
    
    # Available Physical Memory
    available_mem_output <- run_wmic("wmic OS get FreePhysicalMemory")
    if (!is.null(available_mem_output)) {
      free_ram_line <- grep("[0-9]+", available_mem_output, value = TRUE)
      if (length(free_ram_line) > 0) {
        ram_info$free_ram_bytes <- as.numeric(trimws(free_ram_line)) * 1024
        ram_info$free_ram_gb <- ram_info$free_ram_bytes / (1024^3)
      }
    }
    
    # Total Memory Slots
    # memory_slot_output <- run_wmic("wmic ComputerSystem get NumberOfMemorySlots")
    # Total Memory Slots (fallback using PowerShell)
    memory_slot_output <- run_wmic(
      'powershell -command "(Get-CimInstance -ClassName Win32_PhysicalMemoryArray).MemoryDevices"'
    )
    if (!is.null(memory_slot_output)) {
      total_slots_line <- grep("[0-9]+", memory_slot_output, value = TRUE)
      if (length(total_slots_line) > 0) {
        ram_info$total_memory_slots <- as.numeric(trimws(total_slots_line))
      }
    }
    
    # Get details of each memory chip and its bank label (slot)
    memory_chip_details_output <- run_wmic("wmic memorychip get BankLabel, Capacity")
    ram_info$used_memory_slots_count <- NA # Initialize
    
    if (!is.null(memory_chip_details_output)) {
      used_slots <- character(0)
      for (line in memory_chip_details_output) {
        if (grepl("BANK", line)) {
          parts <- trimws(strsplit(line, "\\s+")[[1]])
          if (length(parts) >= 2 && parts[1] == "BANK") {
            bank_label <- parts[1]
            if (!bank_label %in% used_slots) {
              used_slots <- c(used_slots, bank_label)
            }
          }
        } else if (grepl("Channel", line) && grepl("DIMM", line)) {
          bank_label <- trimws(strsplit(line, "\\s+")[[1]][1])
          if (!bank_label %in% used_slots) {
            used_slots <- c(used_slots, bank_label)
          }
        }
      }
      ram_info$used_memory_slots_count <- length(used_slots)
    }
    
    return(ram_info)
  } else {
    cat("This function is designed for Windows.\n")
    return(NULL)
  }
}

## Hardcode RAM Solution
# Get RAM Info on Linux (Dev)

#' Get Essential RAM information on Ubuntu Linux (Simplified)
#'
#' Gathers total RAM, available RAM from /proc/meminfo.
#' Optionally attempts to get memory slot counts using dmidecode.
#' Note: dmidecode typically requires sudo privileges.
#'
#' @return A list containing RAM information (total_ram_gb, available_ram_gb,
#'         used_memory_slots_count, total_memory_slots), with slot info
#'         being NA if dmidecode fails or is unavailable.

get_linux_ram_info <- function() {
  if (.Platform$OS.type != "unix" || !file.exists("/proc/meminfo")) {
    # message("This function is for Linux systems with /proc/meminfo.") # Optional message
    return(NULL)
  }
  
  ram_info <- list(
    total_ram_bytes = NA_real_, total_ram_gb = NA_real_,
    available_ram_bytes = NA_real_, available_ram_gb = NA_real_,
    used_memory_slots_count = NA_integer_, total_memory_slots = NA_integer_
  )
  
  # --- 1. Get Total and Available Memory from /proc/meminfo ---
  tryCatch({
    meminfo_lines <- readLines("/proc/meminfo")
    
    total_mem_line <- grep("^MemTotal:", meminfo_lines, value = TRUE)
    if (length(total_mem_line) > 0) {
      total_kb <- as.numeric(gsub("[^0-9]", "", total_mem_line))
      if (!is.na(total_kb)) {
        ram_info$total_ram_bytes <- total_kb * 1024
        ram_info$total_ram_gb <- ram_info$total_ram_bytes / (1024^3)
      }
    }
    
    available_mem_line <- grep("^MemAvailable:", meminfo_lines, value = TRUE)
    if (length(available_mem_line) > 0) {
      available_kb <- as.numeric(gsub("[^0-9]", "", available_mem_line))
      if (!is.na(available_kb)) {
        ram_info$available_ram_bytes <- available_kb * 1024
        ram_info$available_ram_gb <- ram_info$available_ram_bytes / (1024^3)
      }
    } else { # Basic fallback if MemAvailable is missing
      free_mem_line <- grep("^MemFree:", meminfo_lines, value = TRUE)
      if (length(free_mem_line) > 0) {
        free_kb <- as.numeric(gsub("[^0-9]", "", free_mem_line))
        # This is a very rough estimate of "available"
        ram_info$available_ram_bytes <- free_kb * 1024 
        ram_info$available_ram_gb <- (free_kb * 1024) / (1024^3)
        # message("MemAvailable not found in /proc/meminfo; using MemFree as a rough proxy for available RAM.")
      }
    }
  }, error = function(e) {
    warning(paste("Could not read/parse /proc/meminfo:", e$message))
  })
  
  # --- 2. Attempt to get slot information using dmidecode (optional) ---
  run_dmi_command <- function(dmi_command_args) {
    # Check if dmidecode command exists
    if (Sys.which("dmidecode") == "") return(NULL) 
    
    # Try with sudo first, then without if that fails (common pattern for optional sudo)
    # However, for true simplification, we might just try without and let user know.
    # For this version, we'll just try one way and note that sudo might be needed.
    
    # We'll try "dmidecode" and if it fails (often due to permissions), it fails.
    # The user would need to run the R script with sudo for dmidecode to work.
    # We could add a `try_sudo = TRUE` argument to the main function if more control is needed.
    
    full_command <- paste("dmidecode", dmi_command_args)
    # More direct: if user doesn't have sudo for R, this will likely get permission denied for dmidecode.
    # This is simpler than trying sudo from within R, which is complex and risky.
    output <- try(system(full_command, intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    
    if (inherits(output, "try-error") || (!is.null(attr(output, "status")) && attr(output, "status") != 0) ) {
      # message(paste("dmidecode command '", full_command, "' failed or produced no output. Sudo might be required or dmidecode not fully functional.", sep=""))
      return(NULL)
    }
    if (length(output) < 2) return(NULL) # Unlikely to be useful output
    return(output)
  }
  
  # Get number of installed memory modules (used slots)
  dmi_mem_devices <- run_dmi_command("-t memory") # or -t 17
  if (!is.null(dmi_mem_devices)) {
    # Count actual modules, not just "Handle" lines for empty slots
    size_lines <- grep("^\\s*Size:", dmi_mem_devices, value = TRUE, ignore.case = TRUE)
    # Filter out "No Module Installed" or similar indications of empty slots
    populated_size_lines <- size_lines[!grepl("No Module Installed|Not Installed|Empty", size_lines, ignore.case = TRUE)]
    # Also filter out very small sizes that might indicate unpopulated but reported slots (e.g. "0 MB")
    # This regex looks for a number followed by GB or MB (or KB, though less likely for Size)
    meaningful_size_lines <- populated_size_lines[grepl("[1-9][0-9]*\\s*(MB|GB|KB)", populated_size_lines, ignore.case = TRUE)]
    
    if (length(meaningful_size_lines) > 0) {
      ram_info$used_memory_slots_count <- length(meaningful_size_lines)
    }
  }
  
  # Get total number of memory slots
  dmi_mem_array <- run_dmi_command("-t 16") # Physical Memory Array
  if (!is.null(dmi_mem_array)) {
    num_devices_lines <- grep("Number Of Devices:", dmi_mem_array, value = TRUE, ignore.case = TRUE)
    if (length(num_devices_lines) > 0) {
      total_slots <- sum(as.numeric(gsub("[^0-9]", "", num_devices_lines)), na.rm = TRUE)
      if (total_slots > 0) ram_info$total_memory_slots <- total_slots
    }
  }
  
  # Final check and message if dmidecode parts failed
  if (is.na(ram_info$used_memory_slots_count) && is.na(ram_info$total_memory_slots)) {
    # Only message if BOTH are NA, suggesting dmidecode didn't work at all.
    # message("Could not retrieve memory slot information. 'dmidecode' may not be installed, or R lacks permission (try running R with sudo).")
  }
  
  return(ram_info)
}


ram_linux <- get_linux_ram_info()


# RAPL Solution 
read_sysfs_val <- function(filepath) {
  if (!file.exists(filepath)) return(NA_real_)
  tryCatch(as.numeric(readLines(filepath, n = 1, warn = FALSE)), error = function(e) NA_real_)
}

#' Start a simplified RAPL energy trace.
#' Discovers domains by looking for 'subsystem' directories potentially nested
#' under RAPL controllers (e.g., .../intel-rapl:0/intel-rapl:0:1/subsystem/).
#' @return A list with 'start_time_POSIXct', 'initial_readings_uJ' (named list),
#'         and 'domain_details' (named list with 'name_from_file', 'max_energy_uj').
#'         Returns NULL on failure.
start_rapl_trace_simple <- function() {
  if (Sys.info()["sysname"] != "Linux") {
    return(NULL)
  }
  
  base_powercap_path <- "/sys/class/powercap/"
  if (!dir.exists(base_powercap_path)) return(NULL)
  
  all_domains_config <- list()
  
  # 1. Find all top-level RAPL controllers (e.g., intel-rapl:0, amd_power:0)
  top_level_controllers <- list.files(base_powercap_path,
                                      pattern = "^(intel-rapl|amd_power|amd-rapl|tdp-rapl):[0-9]+$",
                                      full.names = TRUE)
  
  paths_to_search_for_subsystem <- c()
  
  # Add top-level controllers themselves to the search list for a 'subsystem' dir
  paths_to_search_for_subsystem <- c(paths_to_search_for_subsystem, top_level_controllers)
  
  # 2. For each top-level controller, find its direct sub-controllers (e.g., intel-rapl:0:0)
  for (controller_path in top_level_controllers) {
    sub_controllers <- list.files(controller_path,
                                  pattern = paste0("^", basename(controller_path), ":[0-9]+$"),
                                  full.names = TRUE)
    paths_to_search_for_subsystem <- c(paths_to_search_for_subsystem, sub_controllers)
  }
  
  paths_to_search_for_subsystem <- unique(paths_to_search_for_subsystem)
  
  
  # 3. For each of these potential parent paths, check for a 'subsystem' directory
  for (parent_path in paths_to_search_for_subsystem) {
    subsystem_dir_path <- file.path(parent_path, "subsystem")
    
    if (dir.exists(subsystem_dir_path)) {
      # 4. List all directories under .../subsystem/
      # These are assumed to be the actual RAPL controller directories with energy_uj files
      rapl_endpoint_dirs <- list.dirs(subsystem_dir_path, full.names = TRUE, recursive = FALSE)
      
      for (zone_path in rapl_endpoint_dirs) {
        energy_file <- file.path(zone_path, "energy_uj")
        name_file <- file.path(zone_path, "name")
        
        if (file.exists(energy_file) && file.exists(name_file)) {
          raw_name <- tryCatch(trimws(readLines(name_file, n = 1, warn = FALSE)),
                               error = function(e) basename(zone_path))
          
          # Create a unique ID from the relative path under /sys/class/powercap/ and the raw_name
          relative_path_id <- gsub("/", "_", sub(paste0(base_powercap_path,"/?"), "", zone_path))
          domain_id_candidate <- paste0(relative_path_id, "_", gsub("[^a-zA-Z0-9_]", "", raw_name))
          
          current_keys <- names(all_domains_config)
          if(domain_id_candidate %in% current_keys) {
            domain_id <- make.unique(c(current_keys, domain_id_candidate))[length(current_keys) + 1]
          } else {
            domain_id <- domain_id_candidate
          }
          
          all_domains_config[[domain_id]] <- list(
            path_to_energy_uj = energy_file,
            name_from_file = raw_name,
            max_energy_uj = read_sysfs_val(file.path(zone_path, "max_energy_range_uj"))
          )
        }
      }
    }
  } # End loop over paths_to_search_for_subsystem
  
  if (length(all_domains_config) == 0) {
    message("No RAPL domains found under any identified '[controller]/[sub-controller (optional)]/subsystem/' paths.")
    return(NULL)
  }
  
  initial_readings <- sapply(all_domains_config, function(d) read_sysfs_val(d$path_to_energy_uj), USE.NAMES = TRUE)
  
  valid_domains_idx <- !is.na(initial_readings)
  if(sum(valid_domains_idx) == 0) {
    message("Could not read any RAPL energy counters from discovered subsystem paths.")
    return(NULL)
  }
  
  return(list(
    start_time_POSIXct = Sys.time(),
    initial_readings_uJ = initial_readings[valid_domains_idx],
    domain_details = all_domains_config[valid_domains_idx]
  ))
}

#' Stop a simplified RAPL energy trace and report.
#' (This function remains unchanged)
stop_rapl_trace_simple <- function(start_data) {
  if (is.null(start_data) || !all(c("start_time_POSIXct", "initial_readings_uJ", "domain_details") %in% names(start_data))) {
    return(NULL)
  }
  end_time <- Sys.time()
  final_readings_uJ <- sapply(start_data$domain_details, function(d) read_sysfs_val(d$path_to_energy_uj), USE.NAMES = TRUE)
  duration_s <- as.numeric(difftime(end_time, start_data$start_time_POSIXct, units = "secs"))
  report <- list()
  for (domain_id in names(start_data$initial_readings_uJ)) {
    start_uJ <- start_data$initial_readings_uJ[[domain_id]]
    end_uJ <- final_readings_uJ[[domain_id]]
    details <- start_data$domain_details[[domain_id]]
    energy_J <- NA_real_
    power_W <- NA_real_
    if (!is.na(start_uJ) && !is.na(end_uJ)) {
      delta_uJ <- end_uJ - start_uJ
      if (!is.na(details$max_energy_uj) && details$max_energy_uj > 0 && delta_uJ < 0) {
        delta_uJ <- delta_uJ + details$max_energy_uj
      }
      energy_J <- delta_uJ / 1e6
      power_W <- if (duration_s > 1e-9) energy_J / duration_s else NA_real_
    }
    report[[domain_id]] <- list(
      original_name = details$name_from_file,
      energy_J = energy_J,
      power_W = power_W
    )
  }
  return(list(
    start_time = start_data$start_time_POSIXct,
    end_time = end_time,
    duration_s = duration_s,
    report_per_domain = report
  ))
}

#' Wrap an R expression with simplified RAPL snapshot energy tracing.
#' (This function remains unchanged)
with_rapl_trace_simple <- function(expr) {
  initial_trace_state <- start_rapl_trace_simple()
  if (is.null(initial_trace_state)) {
    eval_result <- eval.parent(substitute(expr))
    return(list(result = eval_result, energy_summary = NULL))
  }
  cat(sprintf("RAPL tracing started for %d domain(s) from identified subsystem paths.\n", length(initial_trace_state$initial_readings_uJ)))
  eval_outcome <- tryCatch({
    list(result = eval.parent(substitute(expr)), error = NULL)
  }, error = function(e) {
    list(result = NULL, error = e)
  })
  final_energy_report <- stop_rapl_trace_simple(initial_trace_state)
  cat("RAPL tracing stopped.\n")
  if (!is.null(eval_outcome$error)) {
    message("Error during traced expression. Energy data collected up to the error.")
  }
  return(list(
    result = eval_outcome$result,
    error_details = eval_outcome$error,
    energy_summary = final_energy_report
  ))
}

# Test 
cpu_intensive_task <- function(iterations = 1e7, use_matrix_ops = TRUE) {
  cat(sprintf("Starting CPU-intensive task with %s iterations...\n", format(iterations, scientific = FALSE)))
  
  # Some arithmetic operations
  result <- 0
  for (i in 1:iterations) {
    result <- result + log(sqrt(as.numeric(i) + 0.5)) / (as.numeric(i) + 1.1)
    if (i %% (iterations/10) == 0) {
      cat(".") # Progress indicator
    }
  }
  cat("\n")
  
  if (use_matrix_ops) {
    cat("Performing some matrix operations...\n")
    # Create moderately sized matrices
    mat_size <- min(500, floor(sqrt(iterations/100))) 
    if(mat_size < 10) mat_size <- 10 # ensure a minimum size
    
    A <- matrix(rnorm(mat_size*mat_size), nrow = mat_size)
    B <- matrix(rnorm(mat_size*mat_size), nrow = mat_size)
    
    # Perform a few matrix multiplications
    for(j in 1:3) {
      C <- A %*% B
      A <- B # Swap to do something slightly different next time
      B <- C[,1:mat_size] # Ensure B remains square
    }
    # Add something from matrix ops to the result to ensure it's used
    result <- result + sum(diag(C), na.rm = TRUE) 
  }
  
  cat("CPU-intensive task finished.\n")
  return(result)
}

num_iterations <- 100 # Example: 8 million iterations

cat("\n--- Starting RAPL Trace for cpu_intensive_task ---\n")
trace_results <- with_rapl_trace_simple({
  # This is the code block that will be monitored
  task_output <- cpu_intensive_task(iterations = num_iterations, use_matrix_ops = TRUE)
  task_output # The wrapper will return this as 'result'
})



  
  




# RAM power estimation logic
estimate_used_ram_power <- function(total_ram_gb,
                                    free_ram_gb,
                                    used_memory_slots = NA,
                                    system_type = c("Desktop", "Laptop")) {
  # RAM Power Consumption = 5 Watts * Number of RAM slots used
  ram_power_table <- tibble::tibble(
    system_type = c(
      "Laptop", "Laptop", "Laptop",
      "Desktop", "Desktop", "Desktop", "Desktop",
      "Desktop", "Server", "Server", "Server",
      "Server", "Server", "Server"
    ),
    total_ram_gb = c(
      4, 8, 16,
      16, 32, 64, 128,
      128, 256, 512, 1024,
      1024, 2048, 2048
    ),
    estimated_dimms = c(
      1, 2, 2,
      2, 4, 4, 4,
      8, 8, 8, 8,
      16, 16, 24
    ),
    power_per_dimm_w = c(
      5.0, 5.0, 5.0,
      5.0, 5.0, 5.0, 5.0,
      4.5, 4.5, 4.5, 4.5,
      4.0, 4.0, 3.5
    ),
    scaling_factor = c(
      1.00, 1.00, 1.00,
      1.00, 1.00, 1.00, 1.00,
      0.90, 0.90, 0.90, 0.90,
      0.80, 0.80, 0.70
    ),
    estimated_ram_power_w = c(
      5, 10, 10,
      10, 20, 20, 20,
      36, 36, 36, 36,
      64, 64, 84
    )
  )
  
  
  # Match system type safely
  system_type <- match.arg(system_type)
  
  # Set max GB per DIMM based on system type
  max_gb_per_dimm <- if (system_type == "Laptop") 16 else 32
  
  # Helper: DIMM scaling logic (CodeCarbon)
  get_scaled_dimm_power <- function(index) {
    if (index <= 4) return(5.0)
    else if (index <= 8) return(5.0 * 0.9)
    else if (index <= 16) return(5.0 * 0.8)
    else return(5.0 * 0.7)
  }
  
  estimate_dimm_count <- function(total_ram_gb, max_gb_per_dimm) {
    if (is.na(total_ram_gb) || total_ram_gb <= 0) return(NA)
    ceiling(total_ram_gb / max_gb_per_dimm)
  }
  
  # Determine number of DIMMs
  if (!is.na(used_memory_slots) && used_memory_slots > 0) {
    n_dimms <- used_memory_slots
  } else {
    n_dimms <- estimate_dimm_count(total_ram_gb, max_gb_per_dimm)
  }
  
  if (is.na(n_dimms)) return(NA)
  
  # Estimate total RAM power
  total_power <- sum(sapply(1:n_dimms, get_scaled_dimm_power))
  total_power <- max(total_power, 10)  # Minimum power for x86 systems
  
  # Scale by RAM usage ratio
  used_ram_gb <- total_ram_gb - free_ram_gb
  usage_ratio <- max(used_ram_gb / total_ram_gb, 0)
  used_power <- round(total_power * usage_ratio, 2)
  
  return(used_power)
}



# GET CPU POWER USAGE

# 📘 CodeCarbon’s CPU Power Estimation Logic
# cpu_power = TDP × CPU utilization fraction × number of active cores
# energy = cpu_power × time (in hours)

# Step 1: Define CPU Power Estimation Function in R

estimate_cpu_power <- function(tdp_watts = 65, cpu_util_percent = 25, active_cores = 1) {
  utilization_fraction <- cpu_util_percent / 100
  power <- tdp_watts * utilization_fraction * active_cores
  return(power)  # in watts
}

# Step 2: Calculate Energy Consumption (Wh or kWh)
calculate_energy_kwh <- function(cpu_power_watts, duration_sec) {
  duration_hours <- duration_sec / 3600
  energy_kwh <- cpu_power_watts * duration_hours / 1000
  return(energy_kwh)
}

# Step 3: Estimate CO₂ Emissions
estimate_co2_emissions <- function(energy_kwh, carbon_intensity = 475) {
  # CO₂ in grams
  co2_grams <- energy_kwh * carbon_intensity
  return(co2_grams)
}

get_cpu_info <- function() {
  # Load required packages
  if (!requireNamespace("benchmarkme", quietly = TRUE)) {
    install.packages("benchmarkme")
  }
  library(benchmarkme)
  
  if (!requireNamespace("parallel", quietly = TRUE)) {
    install.packages("parallel")
  }
  library(parallel)
  
  # CPU model and number of cores
  cpu_info <- benchmarkme::get_cpu()
  n_cores <- parallel::detectCores(logical = TRUE)
  
  # Try to get current CPU usage (Linux/Mac only)
  cpu_usage <- tryCatch({
    if (.Platform$OS.type == "unix") {
      as.numeric(system("top -bn1 | grep 'Cpu(s)' | awk '{print $2 + $4}'", intern = TRUE))
    } else if (.Platform$OS.type == "windows") {
      "CPU usage not available on Windows via base R"
    } else {
      "Unsupported platform"
    }
  }, error = function(e) {
    "CPU usage check failed"
  })
  
  list(
    model_name = cpu_info$model_name,
    number_of_cores = n_cores,
    cpu_benchmark = cpu_info$cpu_benchmark,
    cpu_usage_percent = cpu_usage
  )
}

get_cpu_tdp <- function() {
  # Retrieve full CPU model name
  full_model <- get_cpu_info()$model_name
  
  # Extract short model identifier (e.g., "i7-10750H")
  short_model <- extract_cpu_model(full_model)
  
  # Match short model with TDP dataset
  tdp_value <- cpu_tdp_table |> tibble::as_tibble() |> dplyr::filter(grepl(short_model, Name))|>
    dplyr::mutate(TDP = as.numeric(TDP))
  tdp_value <- tdp_value[1,2] 
  
  if (is.na(tdp_value)) {
    warning("CPU model not found in the TDP dataset.")
    return(NA)
  }
  
  return(tdp_value)
}

extract_cpu_model <- function(full_name) {
  # Extract something like "i7-10750H" or "Ryzen 7 5800X"
  pattern <- "(i[3579]-\\d{4,5}[A-Z]*)|(Ryzen [579] \\d{4}X?)"
  match <- regmatches(full_name, regexpr(pattern, full_name, perl = TRUE))
  if (length(match) == 0) return(NA)
  return(match)
}

get_cpu_usage_windows <- function() {
  command <- 'powershell -command "Get-CimInstance Win32_Processor | Measure-Object -Property LoadPercentage -Average | Select -ExpandProperty Average"'
  usage <- tryCatch({
    cpu_usage <- as.numeric(system(command, intern = TRUE))
    return(cpu_usage)
  }, error = function(e) {
    return(NA)
  })
  
  return(cpu_usage)
}

cpu_tdp_table <- read.csv("./cpu_power.csv", stringsAsFactors = FALSE)



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

  # CPU Power Estimation
  cpu_info <- get_cpu_info()
  tdp <- get_cpu_tdp()
  cpu_util_percent <- ifelse(is.numeric(cpu_info$cpu_usage_percent), cpu_info$cpu_usage_percent, 25)
  active_cores <- 1  # adjust as needed
  cpu_power <- estimate_cpu_power(tdp_watts = tdp, cpu_util_percent = cpu_util_percent, active_cores = active_cores)
  
  # RAM Power Estimation
  ram_data <- get_windows_ram_info()
  ram_power_in_use <- estimate_used_ram_power(
    total_ram_gb = ram_data$total_ram_gb,
    free_ram_gb = ram_data$free_ram_gb,
    used_memory_slots = ram_data$used_memory_slots_count,
    system_type = "Laptop"
  )
  
  # ram_power_in_use <- 4.0 # Default fallback
  # 
  # # Safely attempt runtime RAM power retrieval
  # if (.Platform$OS.type == "windows") {
  #   ram_data <- tryCatch(get_windows_ram_info(), error = function(e) NULL)
  #   
  #   if (!is.null(ram_data)) {
  #     ram_power_estimate <- tryCatch(
  #       estimate_used_ram_power(
  #         total_ram_gb = ram_data$total_ram_gb,
  #         free_ram_gb = ram_data$free_ram_gb,
  #         used_memory_slots = ram_data$used_memory_slots_count,
  #         system_type = "Laptop"
  #       ),
  #       error = function(e) NULL
  #     )
  #     
  #     ram_power_in_use <- ram_power_estimate %||% ram_power_in_use
  #   }
  # }

  ram_power <- ram_power_in_use
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
