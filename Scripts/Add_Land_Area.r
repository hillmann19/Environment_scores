library(tidyverse)
censusPDB <- read_csv("~/Downloads/pdb2021bgv3_us.csv")
PermAddress <- read_csv("~/Projects/Geocoding/Data/Mar2022/PermAddress/PermAddress_CensusLinked_OldVarNames_Mar2022.csv", 
                        col_types = cols(...1 = col_skip()))
LocalAddress <- read_csv("~/Projects/Geocoding/Data/Mar2022/LocalAddress/LocalAddress_CensusLinked_OldVarNames_Mar2022.csv",
                         col_types = cols(...1 = col_skip()))

censusTrim <- censusPDB %>% 
  select(State,County,Tract,Block_group,LAND_AREA) %>% 
  mutate(block_id = paste0(State,County,Tract,Block_group))

PermAddress <- PermAddress %>% 
  mutate(block_id = paste0(state,county,tract,block)) %>% 
  mutate(block_id = str_replace_all(block_id,pattern = "...$","")) %>% 
  left_join(censusTrim[,c("block_id","LAND_AREA")]) 

LocalAddress <- LocalAddress %>% 
  mutate(block_id = paste0(state,county,tract,block)) %>% 
  mutate(block_id = str_replace_all(block_id,pattern = "...$","")) %>% 
  left_join(censusTrim[,c("block_id","LAND_AREA")])

write.csv(LocalAddress,file = "~/Projects/Geocoding/Data/Mar2022/LocalAddress/LocalAddress_CensusLinked_OldVarNames_withLandArea_Mar2022.csv")
write.csv(PermAddress,file = "~/Projects/Geocoding/Data/Mar2022/PermAddress/PermAddress_CensusLinked_OldVarNames_withLandArea_Mar2022.csv")
  