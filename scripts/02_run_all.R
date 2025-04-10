# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

# List of states
states <- c("ar", "ca", "ct", "ga", "ia", "il", "in", "ks", 
            "mi", "mo", "ne", "nj", "ny", "oh", "ok", "or", 
            "pa", "tx", "va", "wi")

# Run all import scripts
for (state in states) {
  script_path <- file.path("scripts", paste0("01_import_", state, "_data.R"))
  if (file.exists(script_path)) {
    message("Running: ", script_path)
    env <- new.env()
    # Quietly run the script in an isolated environment
    sys.source(script_path, envir = env)
  } else {
    warning("Script not found: ", script_path)
  }
}

# Load processed .Rda files and bind
supers_list <- list()

for (state in states) {
  rda_path <- file.path("data", "processed", paste0("all_supers_", state, ".Rda"))
  if (file.exists(rda_path)) {
    message("Loading: ", rda_path)
    load(rda_path)
    supers_list[[state]] <- get("all_supers")
  } else {
    warning("Processed file not found: ", rda_path)
  }
}

# Combine all into a single data frame
all_supers <- bind_rows(supers_list, .id = "state")

# Save combined data
save(all_supers, file = file.path("data", "processed", "combined_superintendents.Rda"))
write.csv(all_supers, file = file.path("data", "processed", "combined_superintendents.csv"), row.names = FALSE)
