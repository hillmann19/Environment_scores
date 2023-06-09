library(missForest)
library(sjPlot)
library(psych)
library(sandwich)
library(clubSandwich)
library(doParallel)
library(visreg)
library(table1)
library(tidyverse) 
library(ggpubr)
library(viridis)
library(caret)
library(lmerTest)
library(insight)
library(broom.mixed)
library(ggtext)
df <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Feb2023/census_matched_full.csv')
perm <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/PermAddress_with_env_toxins_1_13_2023.csv')
deleted_sample <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Dec2022/demo_22q.csv') 
demo <- read_csv('/Users/hillmann/Projects/Geocoding/Data/oracle/subjectvisitsall.csv')

set.seed(19104)

# Clean data set variables

demo <- demo %>% 
  mutate(year = str_extract(DOVISIT,pattern = '..$')) %>% 
  mutate(year = case_when(as.numeric(year) > 23 ~ paste0('19',year),
                          as.numeric(year) <= 23 ~ paste0('20',year),
                          TRUE ~ year)) %>% 
  mutate(DOVISIT = str_replace_all(DOVISIT,pattern = '..$',replacement = year)) %>% 
  mutate(DOVISIT = as.Date(DOVISIT,format = '%d-%b-%Y')) %>% 
  rename(PROTOCOL_visit = PROTOCOL)

df_clean <- df %>% 
  mutate(sex = case_when(sex == 1 ~ 'Male',sex == 2 ~ 'Female',TRUE ~ NA_character_)) %>% 
  mutate(race = case_when(race == 1 ~ 'White',race == 2 ~ 'Black/African American',race == 3 ~ 'Native American',
                          race == 4 ~ 'Asian',race == 5 ~ 'More than one race',race == 6 ~ 'Hawaiian/Pacific Islander',
                          race == 9 ~ NA_character_,TRUE ~ NA_character_)) %>% 
  mutate(ethnic = case_when(ethnic == 1 ~ 'Hispanic',ethnic == 2 ~ 'Not Hispanic',ethnic == 9 ~ 'Unknown',TRUE ~ NA_character_)) %>% 
  mutate(across(.cols = P1:G4,.fns = ~ case_when(.x == 9 ~ NA_real_,TRUE ~ .x))) %>% 
  select(-Year_of_birth) %>% 
  mutate(date_ind_ses = as.Date(date_ind_ses,format = '%m/%d/%Y')) %>% 
  mutate(dobirth = as.Date(dobirth,format = '%m/%d/%Y')) %>% 
  mutate(DOSIPS = as.Date(DOSIPS,format = '%m/%d/%Y')) %>% 
  mutate(test_sessions_v.dotest = as.Date(test_sessions_v.dotest,format = '%m/%d/%Y')) %>% 
  mutate(date_perm_address = as.Date(date_perm_address,format = '%m/%d/%Y')) %>% 
  mutate(DODIAGNOSIS = as.Date(DODIAGNOSIS,format = '%m/%d/%Y')) %>% 
  mutate(age_at_ind_ses = as.numeric(date_ind_ses - dobirth)/365.25) %>% 
  mutate(age_at_sips = as.numeric(DOSIPS - dobirth)/365.25) %>% 
  mutate(age_at_cnb = as.numeric(test_sessions_v.dotest - dobirth)/365.25) %>% 
  mutate(age_at_env = as.numeric(date_perm_address - dobirth)/365.25) %>% 
  mutate(age_at_diag = as.numeric(DODIAGNOSIS - dobirth)/365.25) %>% 
  mutate(across(.cols = P1:G4,.fns = ~ as.numeric(scale(.x)), .names = '{.col}_z')) %>% 
  filter(deletion_22q == '22q-') %>% 
  rowwise() %>% 
  mutate(GAF = case_when(!is.na(GAF_C) & !is.na(GASR_CURRENT) ~ mean(c_across(c(GAF_C,GASR_CURRENT))),
                         !is.na(GAF_C) & is.na(GASR_CURRENT) ~ GAF_C,
                         is.na(GAF_C) & !is.na(GASR_CURRENT) ~ GASR_CURRENT,
                         is.na(GAF_C) & is.na(GASR_CURRENT) ~ NA_real_)) %>% 
  ungroup() %>% 
  left_join(demo[,c('BBLID','DOVISIT','PROTOCOL_visit')],by = c('bblid' = 'BBLID','date_perm_address' = 'DOVISIT'))
  

df_clean <- df_clean %>% 
  rowwise() %>% 
  mutate(Positive = mean(c_across(cols = P1_z:P5_z),na.rm = T),
         Negative = mean(c_across(cols = N1_z:N6_z),na.rm = T),
         Disorganized = mean(c_across(cols = D1_z:D4_z),na.rm = T),
         General = mean(c_across(cols = G1_z:G4_z),na.rm = T)) %>% 
  ungroup() %>% 
  mutate(across(.cols = c(Positive,Negative,Disorganized,General),.fns = ~ as.numeric(scale(.x)))) 
 

# Demographic table
demo_table <- df_clean %>% 
  filter(!is.na(sex),!is.na(race)) %>% 
  mutate(dx_any_sub = case_when(if_any(.cols = matches('dx_sub_'),.fns = ~ .x == 1) ~ 1,
                                if_all(.cols = matches('dx_sub_'),.fns = ~ is.na(.x)) ~ NA_real_,
                                TRUE ~ 0)) %>%
  mutate(across(.cols = c(dx_psychosis,dx_mdd,dx_anx,dx_any_sub,dx_adhd),
                .fns = ~ as.numeric(.x))) %>% 
  group_by(bblid) %>% 
  arrange(age_at_env) %>% 
  mutate(`Time since baseline (years)` = age_at_env[n()] - age_at_env[1]) %>% 
  mutate(`Time since baseline (years)` = ifelse(`Time since baseline (years)` == 0,NA,`Time since baseline (years)`)) %>% 
  mutate(Psychosis = case_when(sum(dx_psychosis,na.rm = T) > 0 ~ 'Yes',
                                   sum(is.na(dx_psychosis)) == n() ~ NA_character_,
                                   sum(dx_psychosis,na.rm = T) == 0 ~ 'No',
                                   ),
         MDD = case_when(sum(dx_mdd,na.rm = T) > 0 ~ 'Yes',
                                   sum(is.na(dx_mdd)) == n() ~ NA_character_,
                                   sum(dx_mdd,na.rm = T) == 0 ~ 'No'),
         Anxiety = case_when(sum(dx_anx,na.rm = T) > 0 ~ 'Yes',
                                   sum(is.na(dx_anx)) == n() ~ NA_character_,
                                   sum(dx_anx,na.rm = T) == 0 ~ 'No'),
         `Substance Abuse/Dependence` = case_when(sum(dx_any_sub,na.rm = T) > 0 ~ 'Yes',
                                   sum(is.na(dx_any_sub)) == n() ~ NA_character_,
                                   sum(dx_any_sub,na.rm = T) == 0 ~ 'No'),
         ADHD = case_when(sum(dx_adhd,na.rm = T) > 0 ~ 'Yes',
                             sum(is.na(dx_adhd)) == n() ~ NA_character_,
                             sum(dx_adhd,na.rm = T) == 0 ~ 'No')) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  rename(`Age at baseline` = age_at_env,Sex = sex,Race = race) %>% 
  table1(~ `Age at baseline` + `Time since baseline (years)` + Race + 
           Psychosis + MDD + Anxiety + `Substance Abuse/Dependence` + ADHD|Sex,data = .)


# Impute values for CNB models

#registerDoParallel(cores=10)
#
#df_cnb_imp <- df_clean %>% 
#  rowwise() %>% 
#  mutate(cnb_missing = sum(is.na(c_across(cols = pcet_acc2:cpw_cr)),na.rm = T)) %>% 
#  ungroup() %>% 
#  filter(cnb_missing <= 6) %>% 
#  mutate(sex = factor(sex)) %>% 
#  select(bblid,age_at_cnb,sex,Wealth_perm,University_YUP_perm,Polluted_Urban_perm,Retired_perm,pcet_acc2:tap_tot) %>% 
#  as.data.frame() %>% 
#  missForest(parallelize = "forests") %>% 
#  .[['ximp']] %>% 
#  tibble() 
#
#df_cnb_imp$date_perm_address <- df_clean %>% 
#  rowwise() %>% 
#  mutate(cnb_missing = sum(is.na(c_across(cols = pcet_acc2:cpw_cr)),na.rm = T)) %>% 
#  ungroup() %>% 
#  filter(cnb_missing <= 6) %>% 
#  pull(date_perm_address)
#
#df_cnb_imp_summary <- df_cnb_imp %>% 
#  rowwise() %>% 
#  mutate(pcet_eff = mean(c_across(c(pcet_acc2,pcet_rtcr)))) %>% 
#  mutate(cpt_eff = mean(c_across(c(cpt_ptp,cpt_tprt)))) %>% 
#  mutate(lnb_eff = mean(c_across(c(lnb_mcr,lnb_mrtc)))) %>% 
#  mutate(er40_eff = mean(c_across(c(er40_cr,er40_rtcr)))) %>% 
#  mutate(pvrt_eff = mean(c_across(c(pvrt_cr,pvrt_rtcr)))) %>% 
#  mutate(pmat_eff = mean(c_across(c(pmat_pc,pmat_rtcr)))) %>% 
#  mutate(volt_eff = mean(c_across(c(volt_cr,volt_rtcr)))) %>% 
#  mutate(cpf_eff = mean(c_across(c(cpf_cr,cpf_rtcr)))) %>% 
#  mutate(medf_eff = mean(c_across(c(medf_pc,medf_rtcr)))) %>% 
#  mutate(adt_eff = mean(c_across(c(adt_pc,adt_rtcr)))) %>% 
#  mutate(plot_eff = mean(c_across(c(plot_pc,plot_rtcr)))) %>% 
#  mutate(cpw_eff = mean(c_across(c(cpw_cr,cpw_rtcr)))) %>% 
#  mutate(Overall_Accuracy = mean(c_across(cols = pcet_acc2:cpw_cr))) %>% 
#  mutate(Executive_Accuracy = mean(c_across(cols = c(pcet_acc2,cpt_ptp,lnb_mcr)))) %>% 
#  mutate(Memory_Accuracy = mean(c_across(cols = c(cpw_cr,cpf_cr,volt_cr)))) %>% 
#  mutate(Complex_Accuracy = mean(c_across(cols = c(pvrt_cr,pmat_pc,plot_pc)))) %>% 
#  mutate(Social_Accuracy = mean(c_across(cols = c(er40_cr,medf_pc,adt_pc)))) %>% 
#  mutate(Overall_Speed = mean(c_across(cols = pcet_rtcr:tap_tot))) %>% 
#  mutate(Executive_Speed = mean(c_across(cols = c(pcet_rtcr,cpt_tprt,lnb_mrtc)))) %>% 
#  mutate(Memory_Speed = mean(c_across(cols = c(cpw_rtcr,cpf_rtcr,volt_rtcr)))) %>% 
#  mutate(Complex_Speed = mean(c_across(cols = c(pvrt_rtcr,pmat_rtcr,plot_rtcr)))) %>% 
#  mutate(Social_Speed = mean(c_across(cols = c(er40_rtcr,medf_rtcr,adt_rtcr)))) %>% 
#  mutate(Sensorimotor_Speed = mean(c_across(cols = c(mpraxis_rtcr,tap_tot)))) %>% 
#  mutate(Overall_Efficiency = mean(c_across(cols = matches('_eff$')))) %>% 
#  mutate(Executive_Efficiency = mean(c_across(cols = c(pcet_eff,cpt_eff,lnb_eff)))) %>% 
#  mutate(Memory_Efficiency = mean(c_across(cols = c(cpw_eff,cpf_eff,volt_eff)))) %>% 
#  mutate(Complex_Efficiency = mean(c_across(cols = c(pvrt_eff,pmat_eff,plot_eff)))) %>% 
#  mutate(Social_Efficiency = mean(c_across(cols = c(er40_eff,medf_eff,adt_eff)))) %>% 
#  ungroup() %>% 
#  mutate(across(.cols = Overall_Accuracy:Social_Efficiency,.fns = ~ as.numeric(scale(.x)))) %>% 
#  mutate(across(.cols = Overall_Accuracy:Social_Efficiency,.fns = ~ winsor(.x,trim = .01))) %>%
#  left_join(df_clean[,c('bblid','age_at_cnb','race','date_perm_address','Positive','Negative','Disorganized','General','ses_score','Wealth_perm','University_YUP_perm','Polluted_Urban_perm','Retired_perm','dx_psychosis','dx_prodromal')]) %>% 
#  mutate(race = factor(race,levels = c('Asian','Black/African American','Hawaiian/Pacific Islander',
#                                       'More than one race','Native American','White'))) 
  
#write_csv(df_cnb_imp_summary,'/Users/hillmann/Projects/Geocoding/Data/Feb2023/Neighborhood_env_all_matched_cnb_imputed_02_20_2023.csv')
df_cnb_imp_summary <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Feb2023/Neighborhood_env_all_matched_cnb_imputed_02_20_2023.csv')


# Models for Neurocognition

env_block_grp_perm <- perm %>% 
  select(BBLID,date,Census_Block_GRP)

demo_cross <- demo %>% 
  mutate(MOM_EDUC = case_when(MOM_EDUC == 99 ~ NA_real_,TRUE ~ MOM_EDUC)) %>% 
  filter(!is.na(MOM_EDUC)) %>% 
  group_by(BBLID) %>% 
  filter(MOM_EDUC == max(MOM_EDUC,na.rm = T)) %>% 
  slice_head(n = 1) %>% 
  select(BBLID,MOM_EDUC) %>% 
  ungroup()

df_cnb_for_mods <- df_cnb_imp_summary %>% 
  left_join(env_block_grp_perm,by = c('bblid' = 'BBLID','date_perm_address' = 'date')) %>% 
  left_join(demo_cross,by = c('bblid' = 'BBLID')) %>% 
  rename(Census_Block_GRP_perm = Census_Block_GRP) %>%  
  mutate(race = fct_collapse(race,Other = c('Asian','Hawaiian/Pacific Islander','More than one race','Native American'))) %>% 
  rename(Age = age_at_cnb,Sex = sex,Race = race,Wealth = Wealth_perm,
         University_YUP = University_YUP_perm,Polluted_Urban = Polluted_Urban_perm,Retired = Retired_perm,Individual_SES = ses_score) %>% 
  mutate(Race = factor(Race,levels = c('White','Black/African American','Other'))) %>% 
  filter(!is.na(Census_Block_GRP_perm)) %>% 
  group_by(bblid) %>% 
  arrange(Age) %>% 
  mutate(Timepoint = row_number()) %>% 
  mutate(Prev_CNB = case_when(Timepoint == 1 ~ 0,
                              TRUE ~ 1)) %>% 
  mutate(Time_since_baseline = Age - Age[1]) %>% 
  mutate(Age_at_baseline = Age[1]) %>% 
  ungroup() %>% 
  mutate(Age_at_baseline_12 = Age_at_baseline - 12) %>% 
  mutate(Age_at_baseline2 = Age_at_baseline^2,Time_since_baseline2 = Time_since_baseline^2,
         Age_at_baseline_X_Time_since_baseline = Age_at_baseline*Time_since_baseline) %>% 
  filter(!is.na(Age),!is.na(Sex),!is.na(Race),!is.na(MOM_EDUC)) %>% 
  mutate(year = str_extract_all(date_perm_address,pattern = '^....')) %>% 
  mutate(year_block_grp = paste(year,Census_Block_GRP_perm,pattern = '_'))

pred_cols <- df_cnb_for_mods %>% 
  select(Age_at_baseline,Age_at_baseline2,
         Sex,Time_since_baseline,Time_since_baseline2,Age_at_baseline_X_Time_since_baseline,
         MOM_EDUC,Wealth:Retired) %>% 
  colnames()

for(var in pred_cols){
  tmp_df <- df_cnb_for_mods %>% 
    mutate(Sex = ifelse(Sex == 'Female',1,2))
    
  f <- as.formula(paste(var,'~ Race'))
  m <- lm(f,data = tmp_df) 
  new_col <- paste(var,'rr',sep = '_')
  df_cnb_for_mods[,new_col] <- as.numeric(residuals(m))
}

# Mods with race regressed out 

ovr_acc_rr_mod <- lmer(Overall_Accuracy ~ Age_at_baseline_rr + Age_at_baseline2_rr +  
                         Time_since_baseline_rr + Time_since_baseline2_rr + Age_at_baseline_X_Time_since_baseline_rr + 
                         Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                         Retired_rr + (1|ID) + (1|year_block_group),
                       data = df_cnb_for_mods %>% rename(ID = bblid,year_block_group = year_block_grp))

exec_acc_rr_mod <- lmer(Executive_Accuracy ~ Age_at_baseline_rr + Age_at_baseline2_rr +  
                          Time_since_baseline_rr + Time_since_baseline2_rr + Age_at_baseline_X_Time_since_baseline_rr + 
                          Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                          Retired_rr + (1|ID) + (1|year_block_group),data = df_cnb_for_mods %>% rename(ID = bblid,year_block_group = year_block_grp))

mem_acc_rr_mod <- lmer(Memory_Accuracy ~ Age_at_baseline_rr + Age_at_baseline2_rr +  
                         Time_since_baseline_rr + Time_since_baseline2_rr + Age_at_baseline_X_Time_since_baseline_rr + 
                         Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                         Retired_rr + (1|ID) + (1|year_block_group),
                       data = df_cnb_for_mods %>% rename(ID = bblid,year_block_group = year_block_grp))

com_acc_rr_mod <- lmer(Complex_Accuracy ~ Age_at_baseline_rr + Age_at_baseline2_rr +  
                         Time_since_baseline_rr + Time_since_baseline2_rr + Age_at_baseline_X_Time_since_baseline_rr + 
                         Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                         Retired_rr + (1|ID) + (1|year_block_group),
                       data = df_cnb_for_mods %>% rename(ID = bblid,year_block_group = year_block_grp))

soc_acc_rr_mod <- lmer(Social_Accuracy ~ Age_at_baseline_rr + Age_at_baseline2_rr +  
                         Time_since_baseline_rr + Time_since_baseline2_rr + Age_at_baseline_X_Time_since_baseline_rr + 
                         Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                         Retired_rr + (1|ID) + (1|year_block_group),
                       data = df_cnb_for_mods %>% rename(ID = bblid,year_block_group = year_block_grp))

tab_model(ovr_acc_rr_mod,exec_acc_rr_mod,mem_acc_rr_mod,com_acc_rr_mod,soc_acc_rr_mod,
          dv.labels = c('Overall Accuracy','Executive Accuracy','Memory Accuracy','Complex Accuracy','Social Accuracy'),
          pred.labels = c('Intercept','Age at baseline','Age at baseline<sup>2',
                                                  'Time since baseline','Time since baseline<sup>2','Age at baseline x Time since baseline',
                                        'Sex [Male]',"Mother's Education",'Wealth','University/YUP','Polluted Urban','Retired'),
          collapse.ci = T,digits = 3,string.est = 'Estimate (95% CI)',p.style = 'stars',
          file = '/Users/hillmann/Projects/Geocoding/Results/cnb_mods_race_regressed.doc')

# Models for Psychopathology

df_dx_for_mods <- df_clean %>% 
  left_join(env_block_grp_perm,by = c('bblid' = 'BBLID','date_perm_address' = 'date')) %>% 
  rename(Census_Block_GRP_perm = Census_Block_GRP) %>% 
  filter(!is.na(DODIAGNOSIS)) %>% 
  filter(if_all(.cols = c(bblid,sex,race,age_at_diag,Wealth_perm),.fns = ~ !is.na(.x))) %>%
  left_join(demo_cross,by = c('bblid' = 'BBLID')) %>% 
  mutate(race = fct_collapse(race,Other = c('Asian','Hawaiian/Pacific Islander','More than one race','Native American'))) %>% 
  rowwise() %>% 
  mutate(dx_any_sub = case_when(if_any(.cols = matches('dx_sub_'),.fns = ~ .x == 1) ~ 1,
                                if_all(.cols = matches('dx_sub_'),.fns = ~ is.na(.x)) ~ NA_real_,
                                TRUE ~ 0)) %>% 
  ungroup() %>% 
  relocate(dx_any_sub,.after = dxsum) %>% 
  rename(Age = age_at_diag,Sex = sex,Race = race,Wealth = Wealth_perm,
         University_YUP = University_YUP_perm,Polluted_Urban = Polluted_Urban_perm,Retired = Retired_perm,Individual_SES = ses_score) %>% 
  mutate(Race = factor(Race,levels = c('White','Black/African American','Other'))) %>% 
  group_by(bblid) %>% 
  arrange(Age) %>% 
  mutate(Timepoint = row_number()) %>% 
  mutate(Time_since_baseline = Age - Age[1]) %>% 
  mutate(Age_at_baseline = Age[1]) %>% 
  ungroup() %>% 
  mutate(Age_at_baseline_12 = Age_at_baseline - 12) %>% 
  mutate(Age_at_baseline2 = Age_at_baseline^2,
         Time_since_baseline2 = Time_since_baseline^2,
         Age_at_baseline_X_Time_since_baseline = Age_at_baseline*Time_since_baseline) %>% 
  mutate(dx_any = ifelse(dx_none == 1,0,1)) %>% 
  mutate(year = str_extract_all(date_perm_address,pattern = '^....')) %>% 
  mutate(year_block_grp = paste(year,Census_Block_GRP_perm,pattern = '_')) %>% 
  filter(!is.na(Race),!is.na(Age),!is.na(Sex),!is.na(MOM_EDUC)) 

df_for_gaf <- df_clean %>% 
  left_join(env_block_grp_perm,by = c('bblid' = 'BBLID','date_perm_address' = 'date')) %>% 
  rename(Census_Block_GRP_perm = Census_Block_GRP) %>% 
  filter(!is.na(GAF)) %>% 
  filter(if_all(.cols = c(bblid,sex,race,age_at_env,Wealth_perm),.fns = ~ !is.na(.x))) %>%
  left_join(demo_cross,by = c('bblid' = 'BBLID')) %>% 
  mutate(race = fct_collapse(race,Other = c('Asian','Hawaiian/Pacific Islander','More than one race','Native American'))) %>% 
  rename(Age = age_at_env,Sex = sex,Race = race,Wealth = Wealth_perm,
         University_YUP = University_YUP_perm,Polluted_Urban = Polluted_Urban_perm,Retired = Retired_perm,Individual_SES = ses_score) %>% 
  mutate(Race = factor(Race,levels = c('White','Black/African American','Other'))) %>% 
  group_by(bblid) %>% 
  arrange(Age) %>% 
  mutate(Timepoint = row_number()) %>% 
  mutate(Time_since_baseline = Age - Age[1]) %>% 
  mutate(Age_at_baseline = Age[1]) %>% 
  ungroup() %>% 
  mutate(Age_at_baseline2 = Age_at_baseline^2,
         Time_since_baseline2 = Time_since_baseline^2,
         Age_at_baseline_X_Time_since_baseline = Age_at_baseline*Time_since_baseline) %>% 
  mutate(dx_any = ifelse(dx_none == 1,0,1)) %>% 
  mutate(year = str_extract_all(date_perm_address,pattern = '^....')) %>% 
  mutate(year_block_grp = paste(year,Census_Block_GRP_perm,pattern = '_')) %>% 
  filter(!is.na(Race),!is.na(Age),!is.na(Sex),!is.na(MOM_EDUC))

df_for_sips <- df_clean %>% 
  left_join(env_block_grp_perm,by = c('bblid' = 'BBLID','date_perm_address' = 'date')) %>% 
  rename(Census_Block_GRP_perm = Census_Block_GRP) %>% 
  filter(!is.na(DOSIPS)) %>% 
  filter(if_all(.cols = c(bblid,sex,race,age_at_sips,Wealth_perm),.fns = ~ !is.na(.x))) %>%
  left_join(demo_cross,by = c('bblid' = 'BBLID')) %>% 
  mutate(race = fct_collapse(race,Other = c('Asian','Hawaiian/Pacific Islander','More than one race'))) %>% 
  rename(Age = age_at_sips,Sex = sex,Race = race,Wealth = Wealth_perm,
         University_YUP = University_YUP_perm,Polluted_Urban = Polluted_Urban_perm,Retired = Retired_perm,Individual_SES = ses_score) %>% 
  mutate(Race = factor(Race,levels = c('White','Black/African American','Other'))) %>% 
  group_by(bblid) %>% 
  arrange(Age) %>% 
  mutate(Timepoint = row_number()) %>% 
  mutate(Time_since_baseline = Age - Age[1]) %>% 
  mutate(Age_at_baseline = Age[1]) %>% 
  ungroup() %>% 
  mutate(Age_at_baseline2 = Age_at_baseline^2,
         Time_since_baseline2 = Time_since_baseline^2,
         Age_at_baseline_X_Time_since_baseline = Age_at_baseline*Time_since_baseline) %>% 
  mutate(dx_any = ifelse(dx_none == 1,0,1)) %>% 
  mutate(year = str_extract_all(date_perm_address,pattern = '^....')) %>% 
  mutate(year_block_grp = paste(year,Census_Block_GRP_perm,pattern = '_')) %>% 
  filter(!is.na(Race),!is.na(Age),!is.na(Sex),!is.na(MOM_EDUC))

pred_cols <- df_dx_for_mods %>% 
  select(Age_at_baseline,Age_at_baseline2,
         Sex,Time_since_baseline,Time_since_baseline2,Age_at_baseline_X_Time_since_baseline,
         MOM_EDUC,Wealth:Retired) %>% 
  colnames()

clin_data_list <- list(df_dx_for_mods,df_for_gaf,df_for_sips)
for(i in 1:length(clin_data_list)){
  for(var in pred_cols){
    tmp_df <- clin_data_list[[i]] %>% 
      mutate(Sex = ifelse(Sex == 'Female',1,2))
    
    f <- as.formula(paste(var,'~ Race'))
    m <- lm(f,data = tmp_df) 
    new_col <- paste(var,'rr',sep = '_')
    clin_data_list[[i]][,new_col] <- as.numeric(residuals(m))
  }
}


mod_gaf <- lmer(GAF ~ Age_at_baseline_rr + Age_at_baseline2_rr + 
                             Time_since_baseline_rr + Time_since_baseline2_rr + 
                  Age_at_baseline_X_Time_since_baseline_rr + Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                             Retired_rr + (1|bblid) + (1|year_block_grp),
                           data = clin_data_list[[2]])

mod_pos <- lmer(Positive ~ Age_at_baseline_rr + Age_at_baseline2_rr + 
                  Time_since_baseline_rr + Time_since_baseline2_rr + 
                  Age_at_baseline_X_Time_since_baseline_rr + Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                  Retired_rr + (1|bblid) + (1|year_block_grp),
                data = clin_data_list[[3]])

mod_neg <- lmer(Negative ~ Age_at_baseline_rr + Age_at_baseline2_rr + 
                  Time_since_baseline_rr + Time_since_baseline2_rr + 
                  Age_at_baseline_X_Time_since_baseline_rr + Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                  Retired_rr + (1|bblid) + (1|year_block_grp),
                data = clin_data_list[[3]])

mod_gen <- lmer(General ~ Age_at_baseline_rr + Age_at_baseline2_rr + 
                  Time_since_baseline_rr + Time_since_baseline2_rr + 
                  Age_at_baseline_X_Time_since_baseline_rr + Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                  Retired_rr + (1|bblid) + (1|year_block_grp),
                data = clin_data_list[[3]])

mod_disorg <- lmer(Disorganized ~ Age_at_baseline_rr + Age_at_baseline2_rr + 
                     Time_since_baseline_rr + Time_since_baseline2_rr + 
                     Age_at_baseline_X_Time_since_baseline_rr + Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                     Retired_rr + (1|bblid) + (1|year_block_grp),
                   data = clin_data_list[[3]])

mod_test <- glmer(dx_psychosis ~ Age_at_baseline + Age_at_baseline2 + 
                             Time_since_baseline + Time_since_baseline2 + 
                             Age_at_baseline_X_Time_since_baseline + Race + Sex + MOM_EDUC + Wealth + University_YUP + Polluted_Urban + 
                             Retired + I(Polluted_Urban^2) + (1|bblid),family = 'binomial',nAGQ = 0,
                           data = clin_data_list[[1]])

mod_no_ind_ses_ps <- glmer(dx_psychosis ~ Age_at_baseline_rr + Age_at_baseline2_rr + 
                             Time_since_baseline_rr + Time_since_baseline2_rr + 
                             Age_at_baseline_X_Time_since_baseline_rr + Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                             Retired_rr + (1|bblid),family = 'binomial',nAGQ = 0,
                           data = clin_data_list[[1]])

mod_no_ind_ses_mdd <- glmer(dx_mdd ~ Age_at_baseline_rr + Age_at_baseline2_rr + 
                              Time_since_baseline_rr + Time_since_baseline2_rr + 
                              Age_at_baseline_X_Time_since_baseline_rr + Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                              Retired_rr + (1|bblid),family = 'binomial',nAGQ = 0,
                            data = clin_data_list[[1]])

mod_no_ind_ses_anx <-  glmer(dx_anx ~ Age_at_baseline_rr + Age_at_baseline2_rr + 
                               Time_since_baseline_rr + Time_since_baseline2_rr + 
                               Age_at_baseline_X_Time_since_baseline_rr + Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                               Retired_rr + (1|bblid),family = 'binomial',nAGQ = 0,
                             data = clin_data_list[[1]])


mod_no_ind_ses_adhd <- glmer(dx_adhd ~ Age_at_baseline_rr + Age_at_baseline2_rr + 
                               Time_since_baseline_rr + Time_since_baseline2_rr + 
                               Age_at_baseline_X_Time_since_baseline_rr + Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                               Retired_rr + (1|bblid),family = 'binomial',nAGQ = 0,
                             data = clin_data_list[[1]])


mod_no_ind_ses_sub <- glmer(dx_any_sub ~ Age_at_baseline_rr + Age_at_baseline2_rr + 
                              Time_since_baseline_rr + Time_since_baseline2_rr + 
                              Age_at_baseline_X_Time_since_baseline_rr + Sex_rr + MOM_EDUC_rr + Wealth_rr + University_YUP_rr + Polluted_Urban_rr + 
                              Retired_rr + (1|bblid),family = 'binomial',nAGQ = 0,
                            data = clin_data_list[[1]])

diag_mods <- c(mod_no_ind_ses_ps,mod_no_ind_ses_mdd,mod_no_ind_ses_anx,mod_no_ind_ses_adhd,mod_no_ind_ses_sub)
get_mod_output <- function(mod){
  diag <- str_remove_all(find_response(mod),pattern = '.*_')
  mod_output <- tidy(mod,conf.int = T,exponentiate = T) %>% 
    filter(effect == 'fixed',term != '(Intercept)') %>% 
    mutate(Diagnosis = diag)
  return(mod_output)
}

diag_mods_out <- map_dfr(diag_mods,get_mod_output)

theme_set(theme_minimal())
theme_update(plot.title = element_text(size = 16,face = 'bold'),
             axis.title.x = element_text(size = 14),
             axis.text.y = element_text(size = 12),
             axis.text.x = element_markdown(),
             legend.position = c(.75,.22))

diag_mods_out %>% 
  mutate(term = factor(term,levels = c('Age_at_baseline_rr','Age_at_baseline2_rr',
                                       'Time_since_baseline_rr','Time_since_baseline2_rr',
                                       'Age_at_baseline_X_Time_since_baseline_rr','Sex_rr','MOM_EDUC_rr',
                                       'Wealth_rr','University_YUP_rr','Polluted_Urban_rr','Retired_rr'))) %>% 
  mutate(term = fct_rev(term)) %>% 
  mutate(sig = ifelse(p.value < .05,1,0)) %>% 
  ggplot(aes(x = term,y = estimate,color = Diagnosis,group = Diagnosis)) + 
  geom_point(aes(size = sig),position = position_dodge(width = 1)) + 
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high,linewidth = sig),
                position = position_dodge(width = 1),width = .7) + 
  geom_hline(aes(yintercept = 1),linetype = 'dashed') + 
  scale_x_discrete(labels = c('Retired','Polluted Urban','University/YUP','Wealth',
                              "Mother's Education",'Sex [Male]','Age at baseline x Time since baseline',
                              bquote(Time~since~baseline^2),'Time since baseline',bquote(Age~at~baseline^2),'Age at baseline')) + 
  scale_color_brewer(palette = 'Dark2',labels = c('ADHD','Anxiety','MDD','Psychosis','Substance Abuse/Dependence')) + 
  scale_linewidth(range = c(.5,.9)) + 
  scale_size(range = c(.5,.9)) + 
  labs(x = '',y = 'Odds Ratio',color = '') + 
  guides(linewidth = 'none',size = 'none') + coord_flip() 

# Tabulate models into 1 table
tab_model(mod_no_ind_ses_ps,mod_no_ind_ses_mdd,mod,no_ind_ses_anx,mod_no_ind_ses_adhd,mod_no_ind_ses_sub,
          dv.labels = c('Psychosis','MDD','Anxiety','ADHD','Substance Dependence/Abuse'),
          pred.labels = c('Intercept','Age at baseline','Age at baseline<sup>2','Time since baseline',
                          'Time since baseline<sup>2','Age at baseline x Time since baseline','Sex [Male]',"Mother's Education",'Wealth','University/YUP',
                          'Polluted Urban','Retired'),
          collapse.ci = T,digits = 3,string.est = 'Estimate (95% CI)',p.style = 'stars',
          file = '/Users/hillmann/Projects/Geocoding/Results/clin_mods_race_regressed.doc')
tab_model(mod_gaf,mod_pos,mod_neg,mod_gen,mod_disorg,
          pred.labels = c('Intercept','Age at baseline','Age at baseline<sup>2','Time since baseline',
                          'Time since baseline<sup>2','Age at baseline x Time since baseline','Sex [Male]',"Mother's Education",'Wealth','University/YUP',
                          'Polluted Urban','Retired'),
          collapse.ci = T,digits = 3,string.est = 'Estimate (95% CI)',p.style = 'stars',
          file = '/Users/hillmann/Projects/Geocoding/Results/sips_mods_race_regressed.doc')
