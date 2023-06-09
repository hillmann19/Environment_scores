library(tidyverse)
library(data.table)

##pull address

fs <- read.csv("/Users/hillmann/Projects/Geocoding/Data/July2022/BBL_repo/fullscreen.csv")


fs$DOSCREEN<-as.Date(as.character(fs$DOSCREEN),"%d-%b-%y")

visit <- read.csv("/Users/hillmann/Projects/Geocoding/Data/July2022/BBL_repo/subjectvisitsall.csv")
#visit <- read_csv('/Users/hillmann/Projects/Geocoding/Data/oracle/subjectvisitsall.csv')

visit$DOVISIT<-as.Date(as.character(visit$DOVISIT),"%d-%b-%y")
visit$bblid_date<-paste0(visit$BBLID,"_",visit$DOVISIT)

#limit
visit<- visit[which(year(visit$DOVISIT)>=2008),]
visit<-visit[which(visit$BBLID >1000),]

fs$CURRENT_ADDRESS<-fs$ADDRESS
fs$CURRENT_CITY<-fs$CITY
fs$CURRENT_STATE<-fs$STATE
fs$CURRENT_ZIP<-fs$ZIP
fs$bblid_date<-paste0(fs$BBLID,"_",fs$DOSCREEN)
fs<-fs[which(year(fs$DOSCREEN)>2008),]
fs<-fs[which(fs$BBLID >1000),]

visitc<-visit[,c("BBLID","bblid_date","PROTOCOL","CURRENT_ADDRESS","CURRENT_CITY","CURRENT_STATE","CURRENT_ZIP")]

visitp<-visit[,c("BBLID","bblid_date","PROTOCOL","PERM_ADDRESS","PERM_CITY","PERM_STATE","PERM_ZIP")]

#go batch came with one address- current=perm was assumed
#go1 perm address = current
go1 <- read.csv("/Users/hillmann/Projects/Geocoding/Data/July2022/BBL_repo/n9498_demographics_go1_20161212.csv")

visitp_go1 <- visitp[visitp$BBLID %in% go1$bblid,]
visitp_go1<-dplyr::arrange(visitp_go1,BBLID,bblid_date)

visitp_go1 <- visitp_go1 %>% 
  distinct(BBLID,.keep_all = T)

names(visitp_go1)<-c("BBLID","bblid_date","PROTOCOL","CURRENT_ADDRESS","CURRENT_CITY","CURRENT_STATE","CURRENT_ZIP")

visitc<-rbind(visitc,visitp_go1)

fs2<-fs[,c("BBLID","bblid_date","PROTOCOL","CURRENT_ADDRESS","CURRENT_CITY","CURRENT_STATE","CURRENT_ZIP")]

# Remove rows from the fs2 "screened" data set who had a visit within 6 months of their screening
fs2_list <- fs2 %>% 
  group_split(row_number())

remove_duplicates <- function(df){
  bblid <- df$BBLID
  protocol <- df$PROTOCOL
  date <- as.Date(str_replace_all(df$bblid_date,pattern = ".*_",replacement = ""),format = "%Y-%m-%d")
  matched_row <- visitc %>% 
    mutate(DOVISIT = as.Date(str_replace_all(bblid_date,pattern = ".*_",replacement = ""),format = "%Y-%m-%d")) %>% 
    filter(BBLID == bblid) %>% 
    filter(as.numeric(DOVISIT - date) < 365.25/2,as.numeric(DOVISIT - date) > -30)
  
  if(nrow(matched_row) == 0){
    return(df)
  }
}

fs_no_dup <- bettermc::mclapply(fs2_list,remove_duplicates,mc.cores = 10) %>% 
  bind_rows() %>% 
  select(-`row_number()`)

final_datac<-bind_rows(visitc,fs_no_dup)
final_datac<-final_datac[!(is.na(final_datac$CURRENT_ADDRESS) | final_datac$CURRENT_ADDRESS=="" | final_datac$CURRENT_ADDRESS=="N/A"), ] #remove blank rows

final_datap<-visitp 
final_datap<-final_datap[!(is.na(final_datap$PERM_ADDRESS) | final_datap$PERM_ADDRESS=="" | final_datap$PERM_ADDRESS=="N/A"), ] #remove blank rows

#remove duplicates
#final_data2<-distinct(final_data)

final_datac2 <- distinct(final_datac, bblid_date, CURRENT_ADDRESS, .keep_all= TRUE)
final_datap2 <- distinct(final_datap, bblid_date, PERM_ADDRESS, .keep_all= TRUE)

write_csv(final_datac2,file="/Users/hillmann/Projects/Geocoding/Data/Aug2022/current_address_all_protocols_082022.csv")
write_csv(final_datap2,file="/Users/hillmann/Projects/Geocoding/Data/Aug2022/perm_address_all_protocols_082022.csv")
