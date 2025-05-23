# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

# Define Missouri data directory
mo_dir_path <- here("data", "raw", "mo")

mo_raw <- read_xlsx(file.path(mo_dir_path,"MO_Superintendents.xlsx"))

# Map district IDs to LEAIDs
# Initialize an empty data frame
mo_distids <- data.frame()
years <- 2009:2016

# Loop through years to load and process data
for(y in years){
  print(y)
  
  # Load Rda file
  load(file.path(dist_chars_path, paste0("chars_", y, ".Rda")))
  df <- get(paste0("chars_", y))
  
  # Process the data
  temp <- df %>% 
    filter(fips == "Missouri") %>% 
    select(year, leaid, state_leaid, nces_lea_name = lea_name, agency_charter_indicator, enrollment) %>% 
    mutate(leaid = parse_number(leaid))
  
  mo_distids <- bind_rows(mo_distids, temp)
  
  # Remove the loaded object
  rm(list = paste0("chars_", y))
}

mo_distids$state_leaid_clean <- str_remove_all(str_remove_all(mo_distids$state_leaid, "MO"),"-")
mo_distids$state_leaid_clean <- as.numeric(mo_distids$state_leaid_clean)

mo_lea <- left_join(mo_raw, mo_distids, by = c("dist_code_num"="state_leaid_clean","year"))

mo_clean <- mo_lea %>% select(dist_name, leaid, year, administrator, file)

mo_clean$name_raw <- mo_clean$administrator
mo_clean$name_interm <- mo_clean$administrator
suffixes_to_remove <- paste0(c("Mrs","Ms","Dr","Mr","Miss"),".")
for(s in suffixes_to_remove){
  print(s)
  mo_clean$name_interm <- ifelse(endsWith(mo_clean$name_interm, s), 
                                 str_sub(mo_clean$name_interm, 1, nchar(mo_clean$name_interm) - nchar(s) - 1), 
                                 mo_clean$name_interm)
}

mo_clean$name_clean <- clean_names(mo_clean$name_interm)

mo_clean$state <- "MO"
mo_clean$id <- paste0("mo",1:nrow(mo_clean))

#Create table with names, district IDs, and years
all_supers <- mo_clean %>% select(id, state, leaid, name_raw, name_clean, year, leaid)

# Save the processed data
save(all_supers, file = file.path(clean_path, "all_supers_mo.Rda"))
