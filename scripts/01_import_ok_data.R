# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

# Define Oklahoma data directory
ok_dir_path <- here("data", "raw", "ok")

df_2012 <- read_excel(path = file.path(ok_dir_path,"Oklahoma_School_District_Directory_2013.xls"), skip = 1) %>% 
  mutate(year=2012)
df_2013 <- read_excel(path = file.path(ok_dir_path,"Oklahoma_Public_School_Districts_Dec2013.xls"), skip = 1) %>% 
  mutate(year=2013)
df_2014 <- read_excel(path = file.path(ok_dir_path,"Oklahoma_School_Districts_Directory_May2015.xls"), skip = 0) %>% 
  mutate(year=2014)
df_2015 <- read_excel(path = file.path(ok_dir_path,"2015_Oklahoma_Public_School_DISTRICT_Directory.xls"), skip = 0) %>% 
  mutate(year=2015)
df_2016 <- read_excel(path = file.path(ok_dir_path,"OnlineDirectoryDistrictList-111016.xls"), skip = 2) %>% 
  mutate(year=2016)
df_2017 <- read_excel(path = file.path(ok_dir_path,"OnlineDirectoryDistrictList-11022017.xls"), skip = 2) %>% 
  mutate(year=2017)
df_2018 <- read_excel(path = file.path(ok_dir_path,"OnlineDirectoryDistrictList-11302018.xls"), skip = 0) %>% 
  mutate(year=2018)

ok_raw <- bind_rows(mget(paste0("df_",2012:2018))) %>% 
  rename(dist_code = `District Code`, 
         code = Code,
         dist_name = `District Name`, 
         administrator = Superintendent) %>% 
  mutate(dist_raw = ifelse(is.na(dist_code), code, dist_code)) %>% 
  select(year, dist_raw, dist_name, administrator) %>% 
  filter(is.na(administrator)==0)
ok_raw$dist_raw <- str_remove_all(ok_raw$dist_raw,"-")

# A small number of districts are repeated twice - take unique values
ok_raw <- ok_raw %>% ungroup() %>% distinct()

# Map district IDs to LEAIDs
# Initialize an empty data frame
ok_distids <- data.frame()
years <- 2012:2018

# Loop through years to load and process data
for(y in years){
  print(y)
  
  # Load Rda file
  load(file.path(dist_chars_path, paste0("chars_", y, ".Rda")))
  df <- get(paste0("chars_", y))
  
  # Process the data
  temp <- df %>% 
    filter(fips == "Oklahoma") %>% 
    select(year, leaid, state_leaid, nces_lea_name = lea_name, agency_charter_indicator, enrollment) %>% 
    mutate(leaid = parse_number(leaid))
  
  ok_distids <- bind_rows(ok_distids, temp)
  
  # Remove the loaded object
  rm(list = paste0("chars_", y))
}

ok_distids$state_leaid_clean <- 
  str_remove_all(str_remove_all(ok_distids$state_leaid, "OK-"),"-")

all_ok_lea <- inner_join(ok_raw, ok_distids, by = c("dist_raw" = "state_leaid_clean", "year"))

# Inspect unmatched
unmatched <- anti_join(ok_raw, ok_distids, by = c("dist_raw" = "state_leaid_clean", "year"))
table(unmatched$year)

all_ok_lea$state <- "OK"
all_ok_lea$id <- paste0("ok",1:nrow(all_ok_lea))

all_ok_lea$name_raw <- all_ok_lea$administrator
all_ok_lea$name_clean <- clean_names(all_ok_lea$name_raw)

#Create table with names, district IDs, and years
all_supers <- all_ok_lea %>% select(id, state, leaid, name_raw, name_clean, year, leaid)

# Save the processed data
save(all_supers, file = file.path(clean_path, "all_supers_ok.Rda"))
