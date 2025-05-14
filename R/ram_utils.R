get_windows_ram_info <- function() {
  if (.Platform$OS.type != "windows") return(NULL)
  
  ram_info <- list()
  
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
  
  # Get total RAM
  total_mem_output <- run_wmic("wmic ComputerSystem get TotalPhysicalMemory")
  if (!is.null(total_mem_output)) {
    total_mem_line <- grep("[0-9]+", total_mem_output, value = TRUE)
    if (length(total_mem_line) > 0) {
      ram_info$total_ram_bytes <- as.numeric(trimws(total_mem_line))
      ram_info$total_ram_gb <- ram_info$total_ram_bytes / (1024^3)
    }
  }
  
  # Free RAM
  available_mem_output <- run_wmic("wmic OS get FreePhysicalMemory")
  if (!is.null(available_mem_output)) {
    free_ram_line <- grep("[0-9]+", available_mem_output, value = TRUE)
    if (length(free_ram_line) > 0) {
      ram_info$free_ram_bytes <- as.numeric(trimws(free_ram_line)) * 1024
      ram_info$free_ram_gb <- ram_info$free_ram_bytes / (1024^3)
    }
  }
  
  # Used memory slots (simplified)
  memory_chip_output <- run_wmic("wmic memorychip get BankLabel, Capacity")
  if (!is.null(memory_chip_output)) {
    ram_info$used_memory_slots_count <- length(grep("BANK|Channel.*DIMM", memory_chip_output, ignore.case = TRUE))
  }
  
  return(ram_info)
}


