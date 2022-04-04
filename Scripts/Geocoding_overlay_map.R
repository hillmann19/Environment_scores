# Read in necessary data and packages
library(tidyverse)
library(mapview)
library(sf)
library(viridis)
library(leafsync)
library(leaflet)
library(tigris)
library(latticeExtra)
library(mgcv)
env_fscores <- read_csv("~/Projects/Geocoding/Data/Oct2021/FactorScores/environment_scores_bifactor_ALL.csv")
# addresses_2022 <- read_csv("~/Projects/Geocoding/Data/Mar2022/OrigData/address_dump_oracle_Mar2022.csv")
# Census_2022 <- read_csv("Projects/Geocoding/Data/Mar2022/PermAddress/PermAddress_CensusLinked_Mar2022.csv")
Perm_2022 <- read_csv("Projects/Geocoding/Data/Mar2022/PermAddress/PermAddress_CensusLinked_OldVarNames_withLandArea_Mar2022.csv", 
                      col_types = cols(...1 = col_skip()))
Local_2022 <- read_csv("Projects/Geocoding/Data/Mar2022/LocalAddress/LocalAddress_CensusLinked_OldVarNames_withLandArea_Mar2022.csv", 
                       col_types = cols(...1 = col_skip()))

Perm_2022 <- Perm_2022 %>% 
  select(BBLID,DOVISIT,lat,long,Perm_address_full) %>% 
  mutate(AddressType = "Permanent") %>% 
  rename(address = "Perm_address_full")

Local_2022 <- Local_2022 %>% 
  select(BBLID,DOVISIT,lat,long,Local_address_full) %>% 
  mutate(AddressType = "Local") %>% 
  rename(address = "Local_address_full")

All_2022 <- bind_rows(Perm_2022,Local_2022) %>% 
  distinct(DOVISIT,address,.keep_all = T)

env_fscores_clean <- env_fscores %>% 
  mutate(DOVISIT = str_replace_all(DOVISIT,pattern = "^([[:digit:]])/",replacement = "0\\1/")) %>% 
  mutate(DOVISIT = str_replace_all(DOVISIT,pattern = "/([[:digit:]])/",replacement = "/0\\1/")) %>% 
  mutate(DOVISIT = str_replace_all(DOVISIT,pattern = "^([[:digit:]]*)/([[:digit:]]*)/([[:digit:]]*)",replacement = "\\3-\\1-\\2")) %>% 
  mutate(DOVISIT = as.Date(DOVISIT))

All_fscores <- All_2022 %>% 
  left_join(env_fscores_clean) %>% 
  filter(!is.na(SES),!is.na(University),!is.na(Immigrant),!is.na(EducatedPro))

SES <- mapview(All_fscores,xcol = "long",ycol = "lat",crs = 4269,grid = F,zcol = "SES",cex = 4,col.regions = turbo)
University <- mapview(All_fscores,xcol = "long",ycol = "lat",crs = 4269,grid = F,zcol = "University",cex = 4,col.regions = turbo)
Immigrant <- mapview(All_fscores,xcol = "long",ycol = "lat",crs = 4269,grid = F,zcol = "Immigrant",cex = 4,col.regions = turbo)
EducatedPro <- mapview(All_fscores,xcol = "long",ycol = "lat",crs = 4269,grid = F,zcol = "EducatedPro",cex = 4,col.regions = turbo)


#sync(SES,University,Immigrant,EducatedPro)
#levelplot(EducatedPro ~ SES * University,All_fscores,panel = panel.levelplot.points,col.regions = viridis,alpha = .5) + layer_(panel.2dsmoother(form = z ~ s(x,y),method = "gam",n = 200,col.regions = viridis))

#All_fscores %>% 
#   pivot_longer(cols = SES:EducatedPro,names_to = "Fscore",values_to = "Value") %>% 
#   group_by(Fscore) %>% 
#   arrange(Value) %>% 
#   slice(c(1,n())) %>% 
#   View()

# Map plot by Census Block 

# PA_block_groups <- block_groups(state = "PA",cb = T)
# NJ_block_groups <- block_groups(state = "NJ",cb = T)
# block_groups <- bind_rows(PA_block_groups,NJ_block_groups)
# 
# env_clean_census_last_obs <- All_fscores %>% 
#   group_by(CensusBlock) %>% 
#   arrange(DOVISIT) %>% 
#   slice(n()) %>% 
#   ungroup() %>% 
#   mutate(CensusBlock = str_replace_all(CensusBlock,pattern = "[[:digit:]][[:digit:]][[:digit:]]$",""))
# 
# blocks_merged <- block_groups %>% 
#   left_join(env_clean_census_last_obs,by = c("GEOID" = "CensusBlock")) %>% 
#   filter(str_detect(GEOID,pattern = "^42|^34")) %>% 
#   filter(!is.na(SES))
# 
# popup <- paste0("Census Block: ", blocks_merged$GEOID, "<br>", "Socioeconomic Status: ", round(blocks_merged$SES,2))
# pal <- colorNumeric(
#   palette = "YlGnBu",
#   domain = blocks_merged$SES
# )
# 
# mapSES <-leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(data = blocks_merged, 
#               fillColor = ~pal(SES), 
#               color = "#b2aeae", # you need to use hex colors
#               fillOpacity = 0.7, 
#               weight = 1, 
#               popup = popup) %>%
#   addLegend(pal = pal, 
#             values = blocks_merged$SES, 
#             position = "bottomright", 
#             title = "SES of Philadelphia Region",
#             ) 
# mapSES
  
