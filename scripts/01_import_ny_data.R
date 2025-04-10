# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

library(pdftools)

# Define New York data directory
ny_dir_path <- here("data", "raw", "ny")

# This PDF extraction code relies on tabulizer, which is not available for newer versions of R
files <- list.files(ny_dir_path, pattern = "*.pdf")
# for(f in files){
#   print(f)
#   setwd(ny_dir_path)
#   pages <- pdf_info(f)$pages
#   for(p in 1:pages){
#     print(p)
#     setwd(ny_dir_path)
#     table_list <- extract_tables(f, pages = p, area = list(c(0,0,792,612)))
#     setwd(paste0(ny_dir_path,"/Tables"))
#     save(table_list, file = paste0(f,"_",p,"_tables.Rda"))
#     print(table_list[[1]][1,])
#     gc()
#     Sys.sleep(1)
#   }
# }

ny_out <- data.frame()
for(f in files){
  print(f)
  pages <- pdf_info(file.path(ny_dir_path, f))$pages
  for(p in 1:pages){
    load(file.path(paste0(ny_dir_path,"/Tables"), paste0(f,"_",p,"_tables.Rda")))
    for(i in 1:length(table_list)){
      df <- data.frame(table_list[[i]])
      df$file <- f
      df$page <- p
      ny_out <- bind_rows(ny_out, df)
    }
    
    if(length(table_list)==0){
      print(paste0("No table on page ", p, " of ", pages))
    }
  }
}

#Some files concatenate district and administrator names
#Fix those by locating title (Ms. Mr. Dr.)
titles <- c("Mr.","Ms.","Mrs.","Dr")
ny_out$loc <- str_locate(ny_out$X1,paste(titles, collapse = "|"))[,1]

ny_out$administrator <- ifelse(is.na(ny_out$loc), ny_out$X2, 
                               str_sub(ny_out$X1,ny_out$loc,999))
ny_out$dist_name <- ifelse(is.na(ny_out$loc), ny_out$X1, 
                           str_sub(ny_out$X1,1,ny_out$loc-1)) %>% 
  str_trim()

#Clean file
ny_out$emptyrow <- ifelse(rowSums(ny_out[c(paste0("X",2:5))]=="")==4,1,0)
ny_out$headerrow <- ifelse(ny_out$X1=="District/School Name" | 
                             ny_out$X1=="District/School Name Administrator" | 
                             ny_out$X2=="Administrator",1,0)

#Flag rows that follow emptyrows
ny_out$lag_emptyrow <- lag(ny_out$emptyrow)

ny_clean <- ny_out %>% filter(emptyrow==0, headerrow==0) %>% select(-emptyrow, -headerrow) %>% 
  select(dist_name, administrator, file, page)

#Add one row that the algorithm misses
addrows <- data.frame(dist_name = c("WYOMING CSD"), 
                      administrator = c("Mr. Mickey Edwards"), 
                      file = "PubSchDir1516_101117.pdf", 
                      page = 82)

ny_clean <- bind_rows(ny_clean, addrows)

# Map district IDs to SEDs based on names
dist_map <- read_xlsx(file.path(ny_dir_path, "NY_District_Map.xlsx"))
ny_seds <- inner_join(ny_clean, dist_map)
ny_seds$sed <- as.numeric(ny_seds$sed)

#Import one file that was reported 
ny_2016 <- read_xlsx(file.path(ny_dir_path, "2016_12_02_SchoolDirectory.xlsx"))
ny_2016$administrator <- paste0(str_trim(ny_2016$`CEO FIRST NAME`), " ", str_trim(ny_2016$`CEO LAST NAME`))
ny_2016 <- ny_2016 %>% select(sed = `SED CODE`, 
                              dist_name = `LEGAL NAME`, 
                              administrator)
ny_2016$sed <- as.numeric(ny_2016$sed)
ny_2016$file <- "2016_12_02_SchoolDirectory.xlsx"

#Stack all data
all_ny <- bind_rows(ny_seds, ny_2016)

# Map district IDs to LEAIDs
# Initialize an empty data frame
ny_distids <- data.frame()
years <- 2009:2018

# Loop through years to load and process data
for(y in years){
  print(y)
  
  # Load Rda file
  load(file.path(dist_chars_path, paste0("chars_", y, ".Rda")))
  df <- get(paste0("chars_", y))
  
  # Process the data
  temp <- df %>% 
    filter(fips == "New York") %>% 
    select(year, leaid, state_leaid, nces_lea_name = lea_name, agency_charter_indicator, enrollment) %>% 
    mutate(leaid = parse_number(leaid))
  
  ny_distids <- bind_rows(ny_distids, temp)
  
  # Remove the loaded object
  rm(list = paste0("chars_", y))
}

ny_distids$state_leaid_n <- as.numeric(str_remove_all(ny_distids$state_leaid, "NY-"))

ny_distids <- ny_distids %>% filter(is.na(state_leaid_n)==0)

#Add years
fileyears <- data.frame(year = 2010:2017, 
                        file = c("PubSchDir1011_101117.pdf", 
                                 "PubSchDir1112_101117.pdf", 
                                 "PubSchDir1213_101117.pdf", 
                                 "PubSchDir1314_101117.pdf", 
                                 "PubSchDir1415_101117.pdf", 
                                 "PubSchDir1516_101117.pdf", 
                                 "2016_12_02_SchoolDirectory.xlsx",
                                 "PubSchDir1718_101117.pdf"))

all_ny_years <- left_join(all_ny, fileyears, by = "file")

all_ny_lea <- inner_join(all_ny_years, ny_distids, by = c("sed" = "state_leaid_n", "year"))

# Check unmatched
unmatched <- anti_join(all_ny_years, ny_distids, by = c("sed" = "state_leaid_n", "year"))
table(unmatched$year)

all_ny_lea$name_raw <- all_ny_lea$administrator
all_ny_lea$name_clean <- clean_names(all_ny_lea$name_raw)

all_ny_lea$state <- "NY"
all_ny_lea$id <- paste0("ny",1:nrow(all_ny_lea))

#Create table with names, district IDs, and years
all_supers <- all_ny_lea %>% select(id, state, leaid, name_raw, name_clean, year, leaid)

# Save the processed data
save(all_supers, file = file.path(clean_path, "all_supers_ny.Rda"))
