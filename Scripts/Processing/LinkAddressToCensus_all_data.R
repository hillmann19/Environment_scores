# Read in the data and packages
library(tidycensus)
library(tidygeocoder)
library(tidyverse)
library(tigris)
library(parallel)
#library(bettermc)
library(VIM)

# Changed addresses and demographics to reflect updated data 
perm_address <- read_csv("/Users/hillmann/Projects/Geocoding/Data/Dec2022/perm_address_all_protocols_12_15_2022.csv")
current_address <- read_csv("/Users/hillmann/Projects/Geocoding/Data/Dec2022/current_address_all_protocols_12_15_2022.csv")
vars_to_pull <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Dec2022/LocalAddress_with_employment_vars_Dec2022.csv') %>% 
  select(matches('_[0-9]{3}$')) %>% 
  colnames()

perm_address <- perm_address %>% 
  mutate(bblid_date = str_replace_all(bblid_date,pattern = "^.*_",replacement = "")) %>% 
  rename(date = bblid_date) %>% 
  mutate(date = as.Date(date,format = "%Y-%m-%d"))

current_address <- current_address %>% 
  mutate(bblid_date = str_replace_all(bblid_date,pattern = "^.*_",replacement = "")) %>% 
  rename(date = bblid_date) %>% 
  mutate(date = as.Date(date,format = "%Y-%m-%d"))


# call_geolocator_latlon comes from the tigris package, had to make changes to the url so it ran properly
call_geolocator_latlon <- function(lat, lon, benchmark, vintage) {
  if(missing(benchmark)) {
    benchmark<-"Public_AR_Current"
  } else {
    benchmark<-benchmark
  }
  if(missing(vintage)) {
    vintage<-"Current_Current"
  } else {
    vintage<-vintage
  }
  # Build url
  call_start <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates?"
  
  url <- paste0("x=", lon,"&y=", lat)
  
  benchmark0 <- paste0("&benchmark=", benchmark)
  vintage0 <- paste0("&vintage=", vintage, "&format=json")
  
  url_full <- paste0(call_start, url, benchmark0, vintage0)
  #print(url_full)
  # Check response
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  
  if (length(response$result$geographies$`Census Blocks`[[1]]$GEOID) == 0 && length(response$result$geographies$`2020 Census Blocks`[[1]]$GEOID) == 0) {
    message(paste0("Lat/lon (", lat, ", ", lon,
                   ") returned no geocodes. An NA was returned."))
    return(data.frame(GEOID = NA,Land_area = NA))
  } else {
    
    #regex search for block group geography in response
    response_block<-grep(response[["result"]][["geographies"]], pattern = ".Block.")
    
    #check If a block group result is found or return NA
    #If block group response is found check GEOID length and return either NA for missing data or the value
    if(length(response_block) == 0){
      return(data.frame(GEOID = NA,Land_area = NA))
    } else {
      if (length(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID) == 0) {
        message(paste0("Lat/lon (", lat, ", ", lon,
                       ") returned no geocodes. An NA was returned."))
        return(data.frame(GEOID = NA,Land_area = NA))
      } else {
        if (length(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID) > 1) {
          message(paste0("Lat/lon (", lat, ", ", lon,
                         ") returned more than geocode. The first match was returned."))
        }
        
        df <- data.frame(GEOID = response[["result"]][["geographies"]][[response_block]][[1]]$GEOID,Land_area = response[["result"]][["geographies"]][[response_block]][[1]]$AREALAND)
        return(df)
      }
    }
    
  }
}

# Function which takes an address data frame and outputs a data frame with the latitude and longitude coordinates from the given address column

runGeocodeArcGIS <- function(df,address_col){
  newdf <- geo(address = as.character(df[,address_col]),method = "arcgis")
  newdf$BBLID <- df$BBLID
  newdf$date <- df$date
  newdf$year <- df$year
  newdf <- newdf %>% 
    relocate(BBLID) %>% 
    relocate(date,.after = BBLID) %>% 
    relocate(year,.after = date)
  return(newdf)
}

# Takes a data frame with latitude/longitude coordinates and attaches information about the Census Block from the 2020 Census

getCensusBlocks <- function(df){
  vintage_census <- ifelse(as.numeric(df$year) >= 2018,"Census2020_Current","Census2010_Current")
  CensusBlock <- call_geolocator_latlon(lat = df$lat, lon = df$long,vintage = vintage_census)
  newdf <- data.frame("address" = df$address,lat = df$lat,long = df$long,CensusBlock = CensusBlock$GEOID,Land_area = CensusBlock$Land_area,date = df$date,BBLID = df$BBLID,year = df$year)
  
  return(newdf)
}

vars_df <- tibble()
for(year in 2013:2020){
  new_df <- load_variables(year = year,dataset = "acs5") %>% 
    filter(name %in% vars_to_pull)
  new_df$year <- year
  vars_df <- bind_rows(vars_df,new_df)
}

# Finds survey data for all census block groups collected within a given year
getACSdata <- function(acs_year,df){
  vars_avail <- vars_df %>% 
    filter(year == acs_year) %>% 
    pull(name)
  
  state_counties <- df %>% 
    mutate(acs_year = year + 2) %>% 
    mutate(acs_year = max(2013,year)) %>% 
    mutate(acs_year = min(2020,acs_year)) %>% 
    filter(acs_year == acs_year) %>% 
    mutate(state_county = paste(state,county,sep = '_')) %>% 
    distinct(state_county) %>% 
    separate(state_county,into = c('state','county'))
  
  geoIDs <- df %>% 
    mutate(acs_year = year + 2) %>% 
    mutate(acs_year = max(2013,year)) %>% 
    mutate(acs_year = min(2020,acs_year)) %>% 
    filter(acs_year == acs_year) %>% 
    mutate(geoID = paste0(state,county,tract,str_sub(block,start = 1,end = 1))) %>% 
    with(unique(geoID))
  
  output_df <- data.frame()
  for(i in 1:nrow(state_counties)){
    new_rows <- get_acs(geography = "block group",state = state_counties$state[i],county = state_counties$county[i],variables = vars_avail,year = acs_year)
    new_df <- new_rows %>% 
      filter(GEOID %in% geoIDs) %>% 
      select(GEOID,variable,estimate) %>% 
      pivot_wider(names_from = variable,values_from = estimate)
    
    output_df <- bind_rows(output_df,new_df)
  }  
  
  output_df$acs_year <- acs_year
  
  return(output_df)
}

perm_address_clean <- perm_address %>% 
  mutate(PERM_ADDRESS = str_replace_all(PERM_ADDRESS,pattern = "^N/A$",replacement = NA_character_)) %>% 
  mutate(PERM_ADDRESS = str_replace_all(PERM_ADDRESS,pattern = "^=$",replacement = NA_character_)) %>%
  filter(!is.na(PERM_ADDRESS)) %>% 
  filter(!is.na(PERM_CITY)|!is.na(PERM_ZIP)) %>% 
  mutate(PERM_ADDRESS_FULL = paste(PERM_ADDRESS,PERM_CITY,PERM_STATE,PERM_ZIP,sep = ", ")) %>% 
  mutate(year = str_extract(date,pattern = "^....")) %>%
  relocate(year,.after = date)

current_address_clean <- current_address %>% 
  mutate(CURRENT_ADDRESS = str_replace_all(CURRENT_ADDRESS,pattern = "^N/A$",replacement = NA_character_)) %>% 
  mutate(CURRENT_ADDRESS = str_replace_all(CURRENT_ADDRESS,pattern = "^=$",replacement = NA_character_)) %>%
  filter(!is.na(CURRENT_ADDRESS)) %>% 
  filter(!is.na(CURRENT_CITY)|!is.na(CURRENT_ZIP)) %>% 
  mutate(CURRENT_ADDRESS_FULL = paste(CURRENT_ADDRESS,CURRENT_CITY,CURRENT_STATE,CURRENT_ZIP,sep = ", ")) %>% 
  mutate(year = str_extract(date,pattern = "^....")) %>%
  relocate(year,.after = date)




# Split permanent address data frame into list 
address.perm.list <- perm_address_clean %>% 
  group_split(row_number()) 

numCores <- detectCores()

# Link addresses to lat/lon coordinates
address_arcGIS <- bettermc::mclapply(address.perm.list,FUN = runGeocodeArcGIS,address_col = "PERM_ADDRESS_FULL",mc.cores = (numCores-2))
address_arcGIS_df <- bind_rows(address_arcGIS)


# Remove addresses which couldn't be mapped to a lat/long coordinate
address_arcGIS_clean <- address_arcGIS_df %>% 
  filter(!is.na(lat)|!is.na(long))

# How many addresses can't be mapped?
nrow(address_arcGIS_df) - nrow(address_arcGIS_clean)

# Write lat/long data frame to file 
#write_csv(address_arcGIS_clean,file = "/Users/hillmann/Projects/Geocoding/Data/Address_lat_long_Perm_Dec2022_all_protocols.csv")

address_arcGIS_clean <- read_csv(file = "/Users/hillmann/Projects/Geocoding/Data/Address_lat_long_Perm_Dec2022_all_protocols.csv") 

#Split data frame into list
address_arcGIS_clean_l <- address_arcGIS_clean %>% 
  group_split(row_number())

# Attach Census block info for all rows in address_arcGIS_clean by applying getCensusBlocks to the list address_arcGIS_clean_l 
CensusBlock <- bettermc::mclapply(address_arcGIS_clean_l,FUN = getCensusBlocks,mc.cores = numCores - 2)
CensusBlock_df <- bind_rows(CensusBlock)

# Remove rows where a Census Block was not found
CensusBlock.trim <- CensusBlock_df %>% 
  filter(!is.na(CensusBlock))

# How many coordinates can't be mapped to Census Blocks?
nrow(CensusBlock_df) - nrow(CensusBlock.trim)

# Parse the CensusBlock variable to determine state, county, tract, and block of addresses
CensusBlock.trim <- CensusBlock.trim %>% 
  mutate(state = str_sub(CensusBlock,start = 1,end = 2)) %>% 
  mutate(county = str_sub(CensusBlock,start = 3,end = 5)) %>% 
  mutate(tract = str_sub(CensusBlock,start = 6,end = 11)) %>% 
  mutate(block = str_sub(CensusBlock,start = 12,end = 15))

# Attach necessary ACS survey data to the original addresses; write data set to file
ACS_data_list <- bettermc::mclapply(2013:2020,FUN = getACSdata,df = CensusBlock.trim,mc.cores = (numCores - 2))
ACS_data_df <- bind_rows(ACS_data_list)

CensusBlock.trim.final <- CensusBlock.trim %>% 
  mutate(acs_year = year + 2) %>% 
  mutate(acs_year = max(2013,year)) %>% 
  mutate(acs_year = min(2020,acs_year)) %>% 
  mutate(GEOID = paste0(state,county,tract,str_sub(block,start = 1,end = 1))) %>% 
  left_join(ACS_data_df %>% select(-estimate)) 

write_csv(address_Census_df,file = "/Users/hillmann/Projects/Geocoding/Data/Dec2022/PermAddress_all_protocols_CensusLinked_Dec2022.csv")

### Same thing as above, except using the current address instead of permanent address

address.current.list <- current_address_clean %>% 
  group_split(row_number())

address_arcGIS_current <-  bettermc::mclapply(address.current.list,FUN = runGeocodeArcGIS,address_col = "CURRENT_ADDRESS_FULL",mc.cores = (numCores-2))
address_arcGIS_current_df <- bind_rows(address_arcGIS_current)

address_arcGIS_clean_current <- address_arcGIS_current_df %>% 
  filter(!is.na(lat))

address_arcGIS_clean_l_current <- address_arcGIS_clean_current %>% 
  group_split(row_number())

CensusBlock_current <- bettermc::mclapply(address_arcGIS_clean_l_current,FUN = getCensusBlocks,mc.cores = (numCores - 2))
CensusBlock_current_df <- bind_rows(CensusBlock_current)

CensusBlock.trim.current <- CensusBlock_current_df %>% 
  filter(!is.na(CensusBlock))

CensusBlock.trim.current <- CensusBlock.trim.current %>% 
  mutate(state = str_sub(CensusBlock,start = 1,end = 2)) %>% 
  mutate(county = str_sub(CensusBlock,start = 3,end = 5)) %>% 
  mutate(tract = str_sub(CensusBlock,start = 6,end = 11)) %>% 
  mutate(block = str_sub(CensusBlock,start = 12,end = 15)) %>% 
  mutate(year = as.numeric(year))

ACS_data_current_list <- bettermc::mclapply(2013:2020,FUN = getACSdata,df = CensusBlock.trim.current,mc.cores = (numCores - 2))
ACS_data_current_df <- bind_rows(ACS_data_current_list)

CensusBlock.trim.current.final <- CensusBlock.trim.current %>% 
  rowwise() %>% 
  mutate(acs_year = year + 2) %>% 
  mutate(acs_year = max(2013,acs_year)) %>% 
  mutate(acs_year = min(2020,acs_year)) %>% 
  ungroup() %>% 
  mutate(GEOID = paste0(state,county,tract,str_sub(block,start = 1,end = 1))) %>% 
  left_join(ACS_data_current_df %>% select(-estimate)) 

write_csv(CensusBlock.trim.current.final,file = "/Users/hillmann/Projects/Geocoding/Data/Dec2022/LocalAddress_all_protocols_CensusLinked_Dec2022.csv")
