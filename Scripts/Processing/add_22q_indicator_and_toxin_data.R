# Read in packages and data 
library(tidyverse)
deleted_sample <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Dec2022/demo_22q.csv')
perm <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Census_linked/PermAddress_with_unemployment_vars_ordered_1_4_2023.csv')
local <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Census_linked/LocalAddress_with_unemployment_vars_ordered_1_4_2023.csv')
env_toxins_2016 <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/EJSCREEN_Full_V3_USPR_TSDFupdate_2016.csv')
env_toxins_2017 <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/EJSCREEN_2017_USPR_Public.csv')
env_toxins_2018 <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/EJSCREEN_Full_USPR_2018.csv')
env_toxins_2019 <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/EJSCREEN_2019_USPR.csv')
env_toxins_2020 <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/EJSCREEN_2020_USPR.csv')
env_toxins_2021 <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/EJSCREEN_2021_USPR.csv')
env_toxins_2022 <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/EJSCREEN_2022_Full_with_AS_CNMI_GU_VI.csv')

env_toxins_2016_trim <- env_toxins_2016 %>% 
  rename(Census_Block_GRP = ID) %>% 
  select(Census_Block_GRP,PRE1960,PRE1960PCT,DSLPM:PM25,P_LDPNT,P_DSLPM:P_PM25) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.character(.x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ case_when(.x == 'None' ~ NA_character_,
                                                              TRUE ~ .x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.numeric(.x))) %>% 
  mutate(year = 2016)

env_toxins_2017_trim <- env_toxins_2017 %>% 
  rename(Census_Block_GRP = ID) %>% 
  select(Census_Block_GRP,PRE1960,PRE1960PCT,DSLPM:PM25,P_LDPNT,P_DSLPM:P_PM25) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.character(.x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ case_when(.x == 'None' ~ NA_character_,
                                                              TRUE ~ .x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.numeric(.x))) %>% 
  mutate(year = 2017)

env_toxins_2018_trim <- env_toxins_2018 %>% 
  rename(Census_Block_GRP = ID) %>% 
  select(Census_Block_GRP,PRE1960,PRE1960PCT,DSLPM:PM25,P_LDPNT,P_DSLPM:P_PM25) %>%
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.character(.x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ case_when(.x == 'None' ~ NA_character_,
                                                              TRUE ~ .x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.numeric(.x))) %>% 
  mutate(year = 2018)

env_toxins_2019_trim <- env_toxins_2019 %>% 
  rename(Census_Block_GRP = ID) %>% 
  select(Census_Block_GRP,PRE1960,PRE1960PCT,DSLPM:PM25,P_LDPNT,P_DSLPM:P_PM25) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.character(.x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ case_when(.x == 'None' ~ NA_character_,
                                                              TRUE ~ .x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.numeric(.x))) %>% 
  mutate(year = 2019)

env_toxins_2020_trim <- env_toxins_2020 %>% 
  rename(Census_Block_GRP = ID) %>% 
  select(Census_Block_GRP,PRE1960,PRE1960PCT,DSLPM:PM25,P_LDPNT,P_DSLPM:P_PM25) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.character(.x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ case_when(.x == 'None' ~ NA_character_,
                                                              TRUE ~ .x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.numeric(.x))) %>% 
  mutate(year = 2020)

env_toxins_2021_trim <- env_toxins_2021 %>% 
  rename(Census_Block_GRP = ID) %>% 
  select(Census_Block_GRP,PRE1960,PRE1960PCT,DSLPM:UST,P_LDPNT,P_DSLPM:P_UST) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.character(.x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ case_when(.x == 'None' ~ NA_character_,
                                                              TRUE ~ .x))) %>% 
  mutate(across(.cols = PRE1960:last_col(),.fns = ~ as.numeric(.x))) %>% 
  mutate(year = 2021)

env_toxins_2022_trim <- env_toxins_2022 %>% 
  rename(Census_Block_GRP = ID) %>% 
  select(Census_Block_GRP,PM25:PWDIS,P_LDPNT,P_PM25:P_PWDIS) %>% 
  mutate(across(.cols = PM25:last_col(),.fns = ~ as.character(.x))) %>% 
  mutate(across(.cols = PM25:last_col(),.fns = ~ case_when(.x == 'None' ~ NA_character_,
                                                              TRUE ~ .x))) %>% 
  mutate(across(.cols = PM25:last_col(),.fns = ~ as.numeric(.x))) %>% 
  mutate(year = 2022) 

env_toxins_all <- bind_rows(env_toxins_2016_trim,env_toxins_2017_trim,env_toxins_2018_trim,
                            env_toxins_2019_trim,env_toxins_2020_trim,env_toxins_2021_trim,
                            env_toxins_2022_trim)

env_toxins_UST <- env_toxins_all %>% 
  select(Census_Block_GRP,year,UST,P_UST)

env_toxins_no_UST <- env_toxins_all %>% 
  select(-UST,-P_UST)

ids_22q <- deleted_sample %>% 
  with(unique(bblid))

# Merge in UST data first (it's only available in 2021 and 2022)
perm <- perm %>% 
  rowwise() %>% 
  mutate(year_for_merge = case_when(year <= 2021 ~ 2021,TRUE ~ 2022)) %>% 
  ungroup() %>% 
  left_join(env_toxins_UST,by = c('Census_Block_GRP','year_for_merge' = 'year')) %>% 
  select(-year_for_merge)
  
perm <- perm %>% 
  mutate(deletion_22q = case_when(BBLID %in% ids_22q ~ '22q',TRUE ~ 'Not 22q')) %>% 
  relocate(deletion_22q,.after = BBLID) %>% 
  rowwise() %>% 
  mutate(year_for_merge = max(year,2016)) %>% 
  ungroup() %>% 
  left_join(env_toxins_no_UST,by = c('Census_Block_GRP','year_for_merge' = 'year')) %>% 
  select(-year_for_merge)

local <- local %>% 
  rowwise() %>% 
  mutate(year_for_merge = case_when(year <= 2021 ~ 2021,TRUE ~ 2022)) %>% 
  ungroup() %>% 
  left_join(env_toxins_UST,by = c('Census_Block_GRP','year_for_merge' = 'year')) %>% 
  select(-year_for_merge)

local <- local %>% 
  mutate(deletion_22q = case_when(BBLID %in% ids_22q ~ '22q',TRUE ~ 'Not 22q')) %>% 
  relocate(deletion_22q,.after = BBLID) %>% 
  rowwise() %>% 
  mutate(year_for_merge = max(year,2016)) %>% 
  ungroup() %>% 
  left_join(env_toxins_no_UST,by = c('Census_Block_GRP','year_for_merge' = 'year')) %>% 
  select(-year_for_merge)

write_csv(perm,file = '/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/PermAddress_with_env_toxins_1_13_2023.csv')
write_csv(local,file = '/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/LocalAddress_with_env_toxins_1_13_2023.csv')
