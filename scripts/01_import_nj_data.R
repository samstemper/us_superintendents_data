# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

# Define New Jersey data directory
nj_dir_path <- here("data", "raw", "nj")

df_2011 <- read_delim(file = file.path(nj_dir_path,"NJPubSchoolDistricts_2012.01.15.csv"), 
                      delim=",", skip = 2) %>% mutate(year=2011)
df_2012 <- read_delim(file = file.path(nj_dir_path,"NJPubSchoolDistricts_2012.11.06.csv"), 
                      delim=",", skip = 2) %>% mutate(year=2012)
df_2013 <- read_delim(file = file.path(nj_dir_path,"NJPubSchoolDistricts_2013.06.29.csv"), 
                      delim=",", skip = 2) %>% mutate(year=2013)
df_2014 <- read_delim(file = file.path(nj_dir_path,"NJPubSchoolDistricts_2014.12.25.csv"), 
                      delim=",", skip = 2) %>% mutate(year=2014)
df_2015 <- read_delim(file = file.path(nj_dir_path,"NJPubSchoolDistricts_2015.11.13.csv"), 
                      delim=",", skip = 2) %>% mutate(year=2015)
df_2016 <- read_delim(file = file.path(nj_dir_path,"NJPubSchoolDistricts_2016.11.16.csv"), 
                      delim=",", skip = 2) %>% mutate(year=2016)
df_2017 <- read_delim(file = file.path(nj_dir_path,"NJPubSchoolDistricts_2017.09.10.csv"), 
                      delim=",", skip = 2) %>% mutate(year=2017)

nj_raw <- bind_rows(mget(paste0("df_",2011:2017)))

nj_raw$dist_raw <- paste0(str_sub(nj_raw$`County Code`,3,4), 
                           str_sub(nj_raw$` District Code`,3,6))
nj_raw$administrator <- paste0(nj_raw$` Supt. First Name`, " ", nj_raw$` Supt. Last Name`)
nj_raw$administrator <- ifelse(nj_raw$administrator=="NA NA", NA, nj_raw$administrator)

nj_raw <- nj_raw %>% 
  rename(dist_name = ` District Name`, 
         administrator = administrator) %>% 
  select(year, dist_raw, dist_name, administrator) %>% 
  filter(is.na(administrator)==0)

# Map district IDs to LEAIDs
# Initialize an empty data frame
nj_distids <- data.frame()
years <- 2011:2017

# Loop through years to load and process data
for(y in years){
  print(y)
  
  # Load Rda file
  load(file.path(dist_chars_path, paste0("chars_", y, ".Rda")))
  df <- get(paste0("chars_", y))
  
  # Process the data
  temp <- df %>% 
    filter(fips == "New Jersey") %>% 
    select(year, leaid, state_leaid, nces_lea_name = lea_name, agency_charter_indicator, enrollment) %>% 
    mutate(leaid = parse_number(leaid))
  
  nj_distids <- bind_rows(nj_distids, temp)
  
  # Remove the loaded object
  rm(list = paste0("chars_", y))
}

nj_distids$state_leaid_clean <- str_remove_all(str_remove_all(nj_distids$state_leaid, "NJ-"),"-")

all_nj_lea <- inner_join(nj_raw, nj_distids, by = c("dist_raw" = "state_leaid_clean", "year"))

# Inspect unmatched districts
unmatched <- anti_join(nj_raw, nj_distids, by = c("dist_raw" = "state_leaid_clean", "year"))

all_nj_lea$state <- "NJ"
all_nj_lea$id <- paste0("nj",1:nrow(all_nj_lea))

all_nj_lea$name_raw <- all_nj_lea$administrator
all_nj_lea$name_clean <- clean_names(all_nj_lea$name_raw)

#Create table with names, district IDs, and years
all_supers <- all_nj_lea %>% select(id, state, leaid, name_raw, name_clean, year, leaid)

# Save the processed data
save(all_supers, file = file.path(clean_path, "all_supers_nj.Rda"))
