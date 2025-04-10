# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

library(pdftools)

# Define Texas data directory
tx_dir_path <- here("data", "raw", "tx")

files <- paste0("tx_",2007:2020,".pdf")

# This PDF extraction code relies on tabulizer, which is not available for newer versions of R
# for(f in files){
#   print(f)
#   setwd(tx_dir_path)
#   pages <- pdf_info(f)$pages  
#   for(p in 1:pages){
#     setwd(tx_dir_path)
#     table_list <- extract_tables(f, pages = p, area = list(c(108.5445, 47.2005, 732.5517, 595.9330)))
#     setwd(paste0(tx_dir_path,"/Tables"))
#     save(table_list, file = paste0(f,"_",p,"_tables.Rda"))
#   }
# }

tx_out <- data.frame()
for(f in files){
  print(f)
  pages <- pdf_info(file.path(tx_dir_path, f))$pages
  for(p in 1:pages){
    load(file.path(paste0(tx_dir_path, "/Tables"), paste0(f,"_",p,"_tables.Rda")))
    if(length(table_list) > 0){
      df <- data.frame(table_list[[1]])
      df$file <- f
      df$page <- p
      tx_out <- bind_rows(tx_out, df)
    } else{
      print(paste0("No table on page ", p, " of ", pages))
    }
  }
}

#Clean file
tx_out$emptyrow <- ifelse(rowSums(tx_out=="")==6,1,0)
tx_out$headerrow <- ifelse(tx_out$X1=="School district" | 
                             tx_out$X2=="Phone",1,0)
tx_clean <- tx_out %>% filter(emptyrow==0, headerrow==0) %>% select(-emptyrow, -headerrow)

colnames(tx_clean) <- c("dist_name","dist_phone","cty_distno",
                        "address","zip","name_raw","file","page")

#Add years
#Convert spring years to fall by subtracting 1
tx_clean$year <- parse_number(str_sub(tx_clean$file, 4, 7)) - 1

# Map district IDs to LEAIDs
# Initialize an empty data frame
tx_distids <- data.frame()
years <- 2006:2019

# Loop through years to load and process data
for(y in years){
  print(y)
  
  # Load Rda file
  load(file.path(dist_chars_path, paste0("chars_", y, ".Rda")))
  df <- get(paste0("chars_", y))
  
  # Process the data
  temp <- df %>% 
    filter(fips == "Texas") %>% 
    select(year, leaid, state_leaid, nces_lea_name = lea_name, agency_charter_indicator, enrollment) %>% 
    mutate(leaid = parse_number(leaid))
  
  tx_distids <- bind_rows(tx_distids, temp)
  
  # Remove the loaded object
  rm(list = paste0("chars_", y))
}

# Clean state_ids
tx_distids$len <- nchar(tx_distids$state_leaid)
tx_distids$cty_distno <- paste0(str_sub(tx_distids$state_leaid,tx_distids$len-5,tx_distids$len-3), 
                                "-",
                                str_sub(tx_distids$state_leaid,tx_distids$len-2,tx_distids$len))

tx_clean_lea <- inner_join(tx_clean, tx_distids, by = c("cty_distno", "year"))

# Inspect unmatched
unmatched <- anti_join(tx_clean, tx_distids, by = c("cty_distno", "year")) %>% 
  # Remove non-named superintendents
  filter(name_raw!="")
sort(table(unmatched$dist_name))

# Clean names
tx_clean_lea$name_clean <- clean_names(tx_clean_lea$name_raw)


tx_clean_lea$state <- "TX"
tx_clean_lea$id <- paste0("tx",1:nrow(tx_clean_lea))

#Create table with names, district IDs, and years
all_supers <- tx_clean_lea %>% select(id, state, leaid, name_raw, name_clean, year, leaid)

# Save the processed data
save(all_supers, file = file.path(clean_path, "all_supers_tx.Rda"))