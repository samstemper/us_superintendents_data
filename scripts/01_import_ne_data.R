# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

# Define Nebraska data directory
ne_dir_path <- here("data", "raw", "ne")

ne_raw <- data.frame()
for(y in 2008:2018){
  print(y)
  df <- read_excel(file.path(ne_dir_path,"NE_Superintendents.xlsx"), sheet = as.character(y))
  colnames(df) <- c("dist_no","administrator",
                    "dist_name", "add1","add2","city","state","zip",
                    "loc_type","lowgr","higr")
  
  #In some year, no name listed and just says "ADMINISTRATOR"
  df <- df %>% filter(loc_type=="PUBLIC DISTRICT", tolower(administrator)!="administrator") %>% 
    select(dist_no,dist_name,administrator) %>% 
    mutate(year = y)
  print(nrow(df))
  
  ne_raw <- bind_rows(ne_raw, df)

}

# Map district IDs to LEAIDs
# Initialize an empty data frame
ne_distids <- data.frame()
years <- 2008:2018

# Loop through years to load and process data
for(y in years){
  print(y)
  
  # Load Rda file
  load(file.path(dist_chars_path, paste0("chars_", y, ".Rda")))
  df <- get(paste0("chars_", y))
  
  # Process the data
  temp <- df %>% 
    filter(fips == "Nebraska") %>% 
    select(year, leaid, state_leaid, nces_lea_name = lea_name, agency_charter_indicator, enrollment) %>% 
    mutate(leaid = parse_number(leaid))
  
  ne_distids <- bind_rows(ne_distids, temp)
  
  # Remove the loaded object
  rm(list = paste0("chars_", y))
}

ne_distids$state_leaid_clean <- str_remove_all(str_remove_all(ne_distids$state_leaid, "NE"),"-")
ne_distids$state_leaid_clean <- as.numeric(ne_distids$state_leaid_clean)

ne_raw$dist_no_num <- str_remove_all(ne_raw$dist_no, "-")
ne_raw$dist_no_num <- as.numeric(ne_raw$dist_no_num)

ne_lea <- left_join(ne_raw, ne_distids, by = c("dist_no_num"="state_leaid_clean", "year"))

ne_lea$name_raw <- ne_lea$administrator
ne_lea$name_clean <- clean_names(ne_lea$name_raw)

ne_lea$state <- "NE"
ne_lea$id <- paste0("ne",1:nrow(ne_lea))

#Create table with names, district IDs, and years
all_supers <- ne_lea %>% select(id, state, leaid, name_raw, name_clean, year, leaid)

# Save the processed data
save(all_supers, file = file.path(clean_path, "all_supers_ne.Rda"))
