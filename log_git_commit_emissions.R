
args <- commandArgs(trailingOnly = TRUE)
task <- ifelse(length(args) > 1 && args[1] == "--task", args[2], "Git Task")

# Run emission tracker on git commit
track_emissions_for(task, {
  Sys.sleep(1.5)  # simulate a commit-time action
})
