# Load in necessary data and packages
library(tidyverse)
cnb <- read_csv("/Users/hillmann/Projects/Geocoding/Data/oracle/cnb_merged_webcnp_surveys_smryscores_allbbl_longform.csv")
sips <- read_csv("/Users/hillmann/Projects/Geocoding/Data/oracle/sips.csv")
imgs <- read_csv("/Users/hillmann/Projects/Geocoding/Data/oracle/imglook.csv")
env <- read_csv("/Users/hillmann/Projects/Geocoding/Data/Feb2023/census_scores_16february2023_4-factor.csv")
diag <- read_csv("/Users/hillmann/Projects/Geocoding/Data/oracle/diagnosis_wsmryvars_20220920.csv")
ind_ses <- read_csv("/Users/hillmann/Projects/Geocoding/Data/Individual_SES/SES_scores.csv")
demo <- read_csv('/Users/hillmann/Projects/Geocoding/Data/oracle/subject.csv')
all_visits <- read_csv('/Users/hillmann/Projects/Geocoding/Data/oracle/subjectvisitsall_v.csv')

# Clean data to prepare for merges

ind_ses_clean <- ind_ses %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(bblid = as.character(bblid)) %>% 
  filter(!is.na(date)) %>% 
  group_by(bblid,date) %>% 
  arrange(interview_id) %>% 
  slice_tail(n = 1) %>% 
  ungroup()

sips_trim <- sips %>% 
  filter(if_any(.cols = P1:GAF_C,.fns = ~ !is.na(.x))) %>% 
  rename(bblid = BBLID) %>% 
  mutate(DOSIPS = as.Date(DOSIPS,format = "%d-%b-%y")) %>% 
  mutate(bblid = as.character(bblid)) %>% 
  rowwise() %>% 
  mutate(Vars_non_na = sum(!is.na(c_across(cols = P1:GAF_C)))) %>% 
  ungroup() %>% 
  group_by(bblid,DOSIPS) %>% 
  arrange(Vars_non_na) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  select(-Vars_non_na)

env_perm_trim <- env %>% 
  filter(Address_Type == "Permanent",!is.na(Wealth),BBLID != 12345) %>% 
  select(BBLID,date,Address_Type,Wealth,University_YUP,Polluted_Urban,Retired) %>% 
  mutate(date = as.Date(date,format = '%m/%d/%Y')) %>% 
  rename(bblid = BBLID,date_perm_address = date,Wealth_perm = Wealth,University_YUP_perm = University_YUP,Polluted_Urban_perm = Polluted_Urban,Retired_perm = Retired) %>% 
  mutate(bblid = as.character(bblid)) %>% 
  select(-Address_Type)

# env_local_trim <- env %>% 
#   filter(Address_Type == "Local") %>% 
#   select(BBLID,date,Address_Type,Wealth,University_YUP_Urban,Polluted_Urban,Retired) %>% 
#   mutate(date = as.Date(date,format = '%m/%d/%Y')) %>% 
#   rename(bblid = BBLID,date_local_address = date,Wealth_local = Wealth,University_YUP_Urban_local = University_YUP_Urban,Polluted_Urban_local = Polluted_Urban,Retired_local = Retired) %>% 
#   mutate(bblid = as.character(bblid)) %>% 
#   select(-Address_Type)

#imgs_trim <- imgs %>% 
#  select(BBLID,SCANID,DOSCAN) %>% 
#  rename(bblid = BBLID) %>% 
#  mutate(DOSCAN = as.Date(DOSCAN,format = "%d-%b-%y")) %>% 
#  mutate(bblid = as.character(bblid))

cnb_trim <- cnb %>% 
  select(test_sessions.bblid,test_sessions_v.dotest,test_sessions_v.dob,platform,mpraxis_rtcr,pcet_acc2,pcet_rtcr,cpt_ptp,cpt_tprt,lnb_mcr,lnb_mrtc,er40_cr,er40_rtcr,pvrt_cr,pvrt_rtcr,pmat_pc,pmat_rtcr,volt_cr,volt_rtcr,cpf_cr,cpf_rtcr,medf_pc,medf_rtcr,adt_pc,adt_rtcr,plot_pc,plot_rtcr,cpw_cr,cpw_rtcr,tap_tot,matches("_valid")) %>% 
  rename(bblid = test_sessions.bblid) %>% 
  filter(!is.na(bblid)) %>% 
  filter(if_any(.cols = mpraxis_rtcr:tap_tot,.fns = ~ !is.na(.x))) %>% 
  rowwise() %>% 
  mutate(Vars_non_na = sum(!is.na(c_across(cols = mpraxis_rtcr:tap_tot)))) %>% 
  ungroup() %>% 
  group_by(bblid,test_sessions_v.dotest) %>% 
  arrange(Vars_non_na) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  select(-Vars_non_na)
  
diag_trim <- diag %>% 
  rename(bblid = BBLID,PROTOCOL_diag = PROTOCOL,VISITNUM_diag = VISITNUM,TYPE_diag = TYPE) %>% 
  mutate(bblid = as.character(bblid)) %>% 
  mutate(DODIAGNOSIS = as.Date(DODIAGNOSIS,format = '%d-%b-%y')) 

# Match data to diagnosis data set -- each row in match_df is mapped to a max of 1 diagnosis

#input_df <- env_list[[1235]]
#match_df <- cnb_trim
#input_date_col <- 'date_perm_address'
#match_date_col <- 'test_sessions_v.dotest'
#date_diff_col <- 'CNB_env_date_diff'


match_data <- function(input_df,match_df,input_date_col,match_date_col,date_diff_col,allow_duplicates = FALSE){
  input_bblid <- unique(input_df$bblid)
  
  if(!(input_bblid %in% match_df$bblid)){
    empty_match <- match_df %>% 
      slice_head(n = 1) %>% 
      mutate(across(.cols = 1:ncol(.),.fns = ~ NA))
    
    all_matches <- input_df %>% 
      left_join(empty_match)
    
    return(all_matches)
  } else{
    
    match_df_trim <- match_df %>% 
      filter(bblid == input_bblid)
    cntr <- 1
    all_matches <- data.frame()
    match_avail <- match_df_trim
    input_df_avail <- input_df
    
    while(nrow(input_df_avail) > 0){
  
      if(nrow(match_avail) == 0){
        matched_df <- input_df_avail %>% 
          slice_head(n = 1)
        all_matches <- bind_rows(all_matches,matched_df)
        input_df_avail <- input_df_avail %>% 
          slice(-1)
      } else{
        
        diff_mat <- abs(sapply(input_df_avail[[input_date_col]],'-',match_avail[[match_date_col]]))
        if(nrow(match_avail) == 1){
          diff_mat <- t(as.matrix(diff_mat))
        }
        best_match_loc <- as.vector(which(diff_mat == min(diff_mat),arr.ind = TRUE)[1,])
        matched_df <- bind_cols(input_df_avail %>% slice(best_match_loc[2]),
                                match_avail %>% select(-bblid) %>% slice(best_match_loc[1])) 
        
        all_matches <- bind_rows(all_matches,matched_df)
        
        if(allow_duplicates == FALSE){
          match_avail <- match_avail %>% 
            slice(-best_match_loc[1])
        }
        input_df_avail <- input_df_avail %>% 
          slice(-best_match_loc[2])
      }
    }
    
    all_matches <- tibble(all_matches)  
    all_matches[[date_diff_col]] <- as.numeric(all_matches[[input_date_col]] - all_matches[[match_date_col]])
    
    return(all_matches)
  }
}

env_list <- env_perm_trim %>% 
  group_split(bblid)

cnb_env <- map_dfr(env_list,match_data,match_df = cnb_trim,input_date_col = 'date_perm_address',match_date_col = "test_sessions_v.dotest",date_diff_col = "CNB_env_date_diff")

# Remove any CNBs that weren't within a year of the clinical assessment

cnb_env_clean <- cnb_env %>% 
  mutate(across(.cols = platform:last_col(),.fns = ~ ifelse(abs(CNB_env_date_diff) < 365.25,.x,NA))) %>% 
  mutate(test_sessions_v.dotest = as.Date(ifelse(is.na(CNB_env_date_diff),NA,test_sessions_v.dotest),origin = "1970-01-01")) %>% 
  relocate(CNB_env_date_diff,.after = test_sessions_v.dotest) 

# Create summary variables for CNB performance 

cnb_env_clean <- cnb_env_clean %>% 
  mutate(abf_acc_z = as.numeric(scale(pcet_acc2))) %>% 
  mutate(att_acc_z = as.numeric(scale(cpt_ptp))) %>%
  mutate(wm_acc_z = as.numeric(scale(lnb_mcr))) %>% 
  mutate(vmem_acc_z = as.numeric(scale(cpw_cr))) %>%
  mutate(fmem_acc_z = as.numeric(scale(cpf_cr))) %>% 
  mutate(smem_acc_z = as.numeric(scale(volt_cr))) %>%
  mutate(lan_acc_z = as.numeric(scale(pvrt_cr))) %>% 
  mutate(nvr_acc_z = as.numeric(scale(pmat_pc))) %>%
  mutate(spa_acc_z = as.numeric(scale(plot_pc))) %>% 
  mutate(eid_acc_z = as.numeric(scale(er40_cr))) %>%
  mutate(edi_acc_z = as.numeric(scale(medf_pc))) %>% 
  mutate(adi_acc_z = as.numeric(scale(adt_pc))) %>%
  mutate(abf_speed_z = as.numeric(scale(pcet_rtcr))) %>% 
  mutate(att_speed_z = as.numeric(scale(cpt_tprt))) %>%
  mutate(wm_speed_z = as.numeric(scale(lnb_mrtc))) %>% 
  mutate(vmem_speed_z = as.numeric(scale(cpw_rtcr))) %>%
  mutate(fmem_speed_z = as.numeric(scale(cpf_rtcr))) %>% 
  mutate(smem_speed_z = as.numeric(scale(volt_rtcr))) %>%
  mutate(lan_speed_z = as.numeric(scale(pvrt_rtcr))) %>% 
  mutate(nvr_speed_z = as.numeric(scale(pmat_rtcr))) %>%
  mutate(spa_speed_z = as.numeric(scale(plot_rtcr))) %>% 
  mutate(eid_speed_z = as.numeric(scale(er40_rtcr))) %>%
  mutate(edi_speed_z = as.numeric(scale(medf_rtcr))) %>% 
  mutate(adi_speed_z = as.numeric(scale(adt_rtcr))) %>% 
  mutate(mot_speed_z = as.numeric(scale(tap_tot))) %>% 
  mutate(sm_speed_z = as.numeric(scale(mpraxis_rtcr))) %>% 
  mutate(across(.cols = matches('_speed_z$'),.fns = ~ -1*.x))

# Map sips data to individual SES and CNB 

cnb_env_clean_list <- cnb_env_clean %>% 
  group_split(bblid)

env_cnb_sips <- map_dfr(cnb_env_clean_list,match_data,match_df = sips_trim,input_date_col = 'date_perm_address',match_date_col = "DOSIPS",date_diff_col = "SIPS_env_date_diff")

env_cnb_sips_clean <- env_cnb_sips %>% 
  relocate(DOSIPS,.before = PROTOCOL) %>% 
  mutate(across(.cols = PROTOCOL:last_col(),.fns = ~ ifelse(abs(SIPS_env_date_diff) < 365.25,.x,NA))) %>% 
  mutate(DOSIPS = as.Date(ifelse(abs(SIPS_env_date_diff) < 365.25,DOSIPS,NA),origin = "1970-01-01")) %>% 
  relocate(SIPS_env_date_diff,.after = DOSIPS) 

# Map diagnosis data to individual SES, CNB, and SIPS

env_cnb_sips_list <- env_cnb_sips_clean %>% 
  group_split(bblid)

env_cnb_sips_diag <- map_dfr(env_cnb_sips_list,match_data,match_df = diag_trim,input_date_col = 'date_perm_address',match_date_col = "DODIAGNOSIS",date_diff_col = "Diag_env_date_diff")

env_cnb_sips_diag_clean <- env_cnb_sips_diag %>% 
  relocate(DODIAGNOSIS,.before = CONSENSUS_TYPE) %>% 
  mutate(across(.cols = CONSENSUS_TYPE:last_col(),.fns = ~ ifelse(abs(Diag_env_date_diff) < 365.25,.x,NA))) %>% 
  mutate(DODIAGNOSIS = as.Date(ifelse(abs(Diag_env_date_diff) < 365.25,DODIAGNOSIS,NA),origin = "1970-01-01")) %>% 
  relocate(Diag_env_date_diff,.after = DODIAGNOSIS) 

# Map neuroimaging data to sips, diagnosis, and CNB -- will have more neuroimaging data once I can pull all relevant scan IDs into PMACS

#env_cnb_sips_diag_list <- env_cnb_sips_diag_clean %>% 
#  group_split(bblid)
#
#env_cnb_sips_diag_imgs <- map_dfr(env_cnb_sips_diag_list,match_data,match_df = imgs_trim,input_date_col = 'date_perm_address',match_date_col = "DOSCAN",date_diff_col = "MRI_env_date_diff")
#
#env_cnb_sips_diag_imgs_clean <- env_cnb_sips_diag_imgs %>% 
#  relocate(DOSCAN,.before = SCANID) %>% 
#  mutate(across(.cols = SCANID:last_col(),.fns = ~ ifelse(MRI_env_date_diff > 365.25,NA,.x))) %>% 
#  mutate(DOSCAN = as.Date(ifelse(abs(MRI_env_date_diff) < 365.25,DOSCAN,NA),origin = "1970-01-01")) %>% 
#  relocate(MRI_env_date_diff,.before = SCANID)

# Same thing for environment -- but allow an environment score to be mapped to multiple rows if it is closest to the date of diagnosis

penultimate_list <- env_cnb_sips_diag_clean %>% 
  group_split(bblid)

final <- map_dfr(penultimate_list,match_data,match_df = ind_ses_clean,input_date_col = 'date_perm_address',match_date_col = "date",date_diff_col = "Ind_ses_env_date_diff",allow_duplicates = TRUE)

colnames(demo) <- str_to_lower(colnames(demo))
demo_trim <- demo %>% 
  select(bblid,sex,dobirth,race,ethnic) %>% 
  mutate(bblid = as.character(bblid))

final_clean <- final %>% 
  mutate(across(.cols = redcapid:last_col(),.fns = ~ ifelse(abs(Ind_ses_env_date_diff) < 365.25,.x,NA))) %>% 
  rename(date_ind_ses = date) %>% 
  mutate(date_ind_ses = as.Date(ifelse(abs(Ind_ses_env_date_diff) < 365.25,date_ind_ses,NA),origin = "1970-01-01")) %>% 
  relocate(Ind_ses_env_date_diff,.after = date_ind_ses) %>% 
  left_join(demo_trim) %>% 
  relocate(sex,.after = bblid) %>% 
  relocate(dobirth,.after = bblid) %>%
  relocate(race,.after = sex) %>%
  relocate(ethnic,.after = race)

write_csv(final_clean,file = '/Users/hillmann/Projects/Geocoding/Data/Feb2023/Neighborhood_env_all_matched_02_17_2023.csv')

final_clean2 <- final_clean %>% 
  filter(bblid != 99999) %>% 
  filter(!is.na(dobirth)) %>% 
  filter(if_any(.cols = c(mpraxis_rtcr:tap_tot,P1:GAF_C,dx_none:dx_pscat),.fns = ~ !is.na(.x)))

write_csv(final_clean2,file = '/Users/hillmann/Projects/Geocoding/Data/Feb2023/Neighborhood_env_all_matched_clean_02_17_2023.csv')


test %>% 
  filter(bblid %in% ids)
