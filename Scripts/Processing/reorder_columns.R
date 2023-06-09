# Read in data and packages 
library(tidyverse)
old_local <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Dec2022/LocalAddress_with_employment_vars_Dec2022.csv')
new_local <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Dec2022/LocalAddress_all_protocols_CensusLinked_Dec2022.csv')
old_perm <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Dec2022/PermAddress_with_employment_vars_Dec2022.csv')
new_perm <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Dec2022/PermAddress_all_protocols_CensusLinked_Dec2022.csv')

new_local <- new_local %>% 
  select(-acs_year,-GEOID) %>% 
  mutate(Census_Block_GRP = str_remove_all(CensusBlock,pattern = '...$')) %>% 
  relocate(colnames(old_local)) 

new_perm <- new_perm %>% 
  select(-acs_year,-GEOID) %>% 
  mutate(Census_Block_GRP = str_remove_all(CensusBlock,pattern = '...$')) %>% 
  relocate(colnames(old_perm)) 

write_csv(new_local,file = '/Users/hillmann/Projects/Geocoding/Data/Jan2023/LocalAddress_with_unemployment_vars_ordered_1_4_2023.csv')
write_csv(new_perm,file = '/Users/hillmann/Projects/Geocoding/Data/Jan2023/PermAddress_with_unemployment_vars_ordered_1_4_2023.csv')

