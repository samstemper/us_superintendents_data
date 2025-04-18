# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

# Define Indiana data directory
in_dir_path <- here("data", "raw", "in")

df_2010 <- read_excel(path = file.path(in_dir_path,"Superintendents_List_2010.xls"), skip = 1) %>% 
  mutate(year=2010) %>% 
  select(year, Name = CONAME, Corp=CORP,Superintendent=SUPNAME) %>% 
  mutate(Corp = as.numeric(Corp))
df_2012 <- read_excel(path = file.path(in_dir_path,"2012-13-indiana-school-directory-1102013.xlsx")) %>% 
  mutate(year=2012) %>% 
  select(year, Name = `Corp Name`, Corp,Superintendent) %>% 
  mutate(Corp = as.numeric(Corp))
df_2014 <- read_excel(path = file.path(in_dir_path,"2014-2015-school-directory-1.20.15.xlsx")) %>% 
  mutate(year=2014) %>% 
  mutate(Superintendent = paste0(`Superintendent First Name`," ",`Superintendent Last Name`)) %>% 
  select(year, Name = `Corporation Name`, Corp = `Corp #`,Superintendent) %>% 
  mutate(Corp = as.numeric(Corp))
df_2015 <- read_excel(path = file.path(in_dir_path,"2015-2016-school-directory-3316.xlsx")) %>% 
  mutate(year=2015) %>% 
  mutate(Superintendent = paste0(`Superintendent First Name`," ",`Superintendent Last Name`)) %>% 
  select(year, Name = `Corporation Name`, Corp = `Corp #`,Superintendent) %>% 
  mutate(Corp = as.numeric(Corp))
df_2016 <- read_excel(path = file.path(in_dir_path,"2016-2017-school-directory-2017-06-21.xlsx")) %>% 
  mutate(year=2016) %>%
  mutate(Superintendent = paste0(`SUPERINTENDENT_FIRST_NAME`," ",`SUPERINTENDENT_LAST_NAME`)) %>%  
  select(year, Name = CORPORATION_NAME, Corp = IDOE_CORPORATION_ID,Superintendent) %>% 
  mutate(Corp = as.numeric(Corp))
df_2017 <- read_excel(path = file.path(in_dir_path,"2017-2018-school-directory-2017-08-07.xlsx")) %>% 
  mutate(year=2017) %>%
  mutate(Superintendent = paste0(`SUPERINTENDENT FIRST NAME`," ",`SUPERINTENDENT LAST NAME`)) %>%  
  select(year, Name = `CORPORATION NAME`, Corp = `IDOE CORPORATION ID`,Superintendent) %>% 
  mutate(Corp = as.numeric(Corp))
df_2018 <- read_excel(path = file.path(in_dir_path,"2018-2019-school-directory-2019-09-11.xlsx")) %>% 
  mutate(year=2018) %>%
  mutate(Superintendent = paste0(`SUPERINTENDENT_FIRST_NAME`," ",`SUPERINTENDENT_LAST_NAME`)) %>%  
  select(year, Name = CORPORATION_NAME, Corp = IDOE_CORPORATION_ID,Superintendent) %>% 
  mutate(Corp = as.numeric(Corp))
df_2019 <- read_excel(path = file.path(in_dir_path,"2019-2020-school-directory-2020-01-15.xlsx")) %>% 
  mutate(year=2019) %>%
  mutate(Superintendent = paste0(`SUPERINTENDENT_FIRST_NAME`," ",`SUPERINTENDENT_LAST_NAME`)) %>%  
  select(year, Name = CORPORATION_NAME, Corp = IDOE_CORPORATION_ID, Superintendent) %>% 
  mutate(Corp = as.numeric(Corp))

in_raw <- bind_rows(mget(paste0("df_",c(2010, 2012, 2014:2019))))
in_raw$administrator <- ifelse(in_raw$Superintendent=="NA NA", NA, in_raw$Superintendent)

in_raw <- in_raw %>% filter(is.na(administrator)==0)

# Map district IDs to LEAIDs
# Initialize an empty data frame
in_distids <- data.frame()
years <- 2009:2019

# Loop through years to load and process data
for(y in years){
  print(y)
  
  # Load Rda file
  load(file.path(dist_chars_path, paste0("chars_", y, ".Rda")))
  df <- get(paste0("chars_", y))
  
  # Process the data
  temp <- df %>% filter(fips=="Indiana") %>% 
    select(year, leaid, state_leaid, nces_lea_name = lea_name, agency_charter_indicator, enrollment) %>% 
    mutate(leaid = parse_number(leaid))
  
  in_distids <- bind_rows(in_distids, temp)
  
  # Remove the loaded object
  rm(list = paste0("chars_", y))
}

in_distids$state_leaid_clean <- str_remove_all(str_remove_all(in_distids$state_leaid, "IN"),"-")
in_distids$state_leaid_clean <- as.numeric(in_distids$state_leaid_clean)

all_in_lea <- inner_join(in_raw, in_distids, by = c("Corp" = "state_leaid_clean", "year"))

# Check unmatched
unmatched <- anti_join(in_raw, in_distids, by = c("Corp" = "state_leaid_clean", "year"))
table(unmatched$year)

check <- unmatched %>% filter(!str_detect(tolower(Name), "diocese"))

all_in_lea$state <- "in"
all_in_lea$id <- paste0("in",1:nrow(all_in_lea))

all_in_lea$name_raw <- all_in_lea$administrator
all_in_lea$name_clean <- clean_names(all_in_lea$name_raw)

#Create table with names, district IDs, and years
all_supers <- all_in_lea %>% select(id, state, leaid, name_raw, name_clean, year, leaid)

# Save the processed data
save(all_supers, file = file.path(clean_path, "all_supers_in.Rda"))