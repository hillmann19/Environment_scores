# Load in necessary data and packages 
library(tidyverse)
CensusVariableMap <- read_csv("~/Projects/Geocoding/Data/Extra/CensusVariableMap.csv")
LocalAddress <- read_csv("Projects/Geocoding/Data/Apr2022/LocalAddress/LocalAddress_CensusLinked_Apr2022.csv", 
                         col_types = cols(...1 = col_skip()))
PermAddress <- read_csv("Projects/Geocoding/Data/Apr2022/PermAddress/PermAddress_CensusLinked_Apr2022.csv", 
                        col_types = cols(...1 = col_skip()))

CensusVariableMap <- CensusVariableMap %>% 
  distinct(Old.var,.keep_all = T) %>% 
  select(Old.var,New.Var) %>% 
  rename(New.var = New.Var)

LocalAddress_OldNames <- LocalAddress %>%
  distinct(BBLID,DOVISIT,.keep_all = T) %>%  # remove repeat rows 
  pivot_longer(cols = B01001_001:C24010_001,names_to = "Variable",values_to = "Value") %>% 
  left_join(CensusVariableMap,by = c("Variable" = "New.var")) %>% 
  select(-Variable) %>% 
  pivot_wider(names_from = Old.var,values_from = Value) 

PermAddress_OldNames <- PermAddress %>% 
  distinct(BBLID,DOVISIT,.keep_all = T) %>%  # remove repeat rows
  pivot_longer(cols = B01001_001:C24010_001,names_to = "Variable",values_to = "Value") %>% 
  left_join(CensusVariableMap,by = c("Variable" = "New.var")) %>% 
  select(-Variable) %>% 
  pivot_wider(names_from = Old.var,values_from = Value) 

write.csv(LocalAddress_OldNames,file = "~/Projects/Geocoding/Data/Apr2022/LocalAddress/LocalAddress_CensusLinked_OldVarNames_Apr2022.csv")
write.csv(PermAddress_OldNames,file = "~/Projects/Geocoding/Data/Apr2022/PermAddress/PermAddress_CensusLinked_OldVarNames_Apr2022.csv")

# Find individuals with two submissions on the same day 

Local_mult_address <- LocalAddress %>% 
  group_by(BBLID,DOVISIT) %>% 
  filter(n() != 1) %>% 
  distinct(Local_address_full,.keep_all = T) %>% 
  filter(n() != 1) %>% 
  ungroup() %>% 
  distinct(BBLID,DOVISIT)

Perm_mult_address <- PermAddress %>% 
  group_by(BBLID,DOVISIT) %>% 
  filter(n() != 1) %>% 
  distinct(Perm_address_full,.keep_all = T) %>% 
  filter(n() != 1) %>% 
  ungroup() %>% 
  distinct(BBLID,DOVISIT)

mult_address <- bind_rows(Local_mult_address,Perm_mult_address) %>% 
  distinct()

write.csv(mult_address,file = "~/Projects/Geocoding/Data/Apr2022/Extra/Multiple_addresses_same_day_Apr2022.csv")
