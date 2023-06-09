# Load in packages and data
library(tidyverse)
library(psych)
library(ggpubr)
library(tigris)
library(sf)
library(mapdeck)
library(ggExtra)
library(moments)
slopes <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Mar2023/census_environment_slopes_all.csv')
df <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Feb2023/census_matched_full.csv')
deleted_sample <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Dec2022/demo_22q.csv') 
perm <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Jan2023/Env_toxins/PermAddress_with_env_toxins_1_13_2023.csv')
theme_set(theme_minimal())

df_clean <- df %>% 
  filter(!(bblid %in% deleted_sample$bblid)) %>% 
  left_join(perm[,c('BBLID',"date","Census_Block_GRP")],by = c('bblid' = 'BBLID','date_perm_address' = 'date')) 

slopes_df <- slopes %>% 
  mutate(across(.cols = c(Wealth_slopes_T1reg_cog,University_YUP_Urban_slopes_T1reg_cog,
                          Polluted_Urban_slopes_T1reg_cog,Retired_slopes_T1reg_cog,
                          overall_acc_slopes_T1reg,Wealth_slopes_T1reg_GAF,University_YUP_Urban_slopes_T1reg_GAF,
                          Polluted_Urban_slopes_T1reg_GAF,Retired_slopes_T1reg_GAF,
                          GAF_slope_T1reg),.fns = ~ winsor(.x,trim = .025))) %>% 
  mutate(across(.cols = c(dobirth,date_perm_address),.fns = ~ str_replace_all(.x,pattern = '^(.)/',replacement = '0\\1/'))) %>% 
  mutate(across(.cols = c(dobirth,date_perm_address),.fns = ~ str_replace_all(.x,pattern = '/(.)/',replacement = '/0\\1/'))) %>% 
  mutate(dobirth = as.Date(dobirth,format = '%m/%d/%Y')) %>% 
  mutate(date_perm_address = as.Date(date_perm_address,format = '%m/%d/%Y')) %>% 
  mutate(age_at_baseline = as.numeric((date_perm_address - dobirth)/365.25))

theme_set(theme_minimal())
theme_update(axis.text.x = element_text(size = 14),
             axis.text.y = element_text(size = 14))

c <- cor.test(slopes_df$Wealth_slopes_T1reg_cog,slopes_df$overall_acc_slopes_T1reg)

wealth_ovr_acc_slopes <- slopes_df %>% 
  filter(!is.na(Wealth_slopes_T1reg_cog),!is.na(overall_acc_slopes_T1reg)) %>% 
  ggplot(aes(x = Wealth_slopes_T1reg_cog,y = overall_acc_slopes_T1reg)) + 
  geom_point(size = 1.5,color = '#08519c',alpha = .5) + geom_smooth(method = 'lm',color = '#08519c') + 
  scale_x_continuous(breaks = -2:2,limits = c(-2,2)) + labs(x = 'Wealth Slope',y = 'Overall Accuracy Slope',caption = paste0('r = ',round(c$estimate,2),
                                                                        ', p-value = ',round(c$p.value,3))) 

wealth_ovr_acc_slopes <- ggMarginal(wealth_ovr_acc_slopes,type = 'density',size = 8,fill = 'grey')

c <- cor.test(slopes_df$University_YUP_Urban_slopes_T1reg_cog,slopes_df$overall_acc_slopes_T1reg)

uni_ovr_acc_slopes <- slopes_df %>% 
  filter(!is.na(University_YUP_Urban_slopes_T1reg_cog),!is.na(overall_acc_slopes_T1reg)) %>% 
  ggplot(aes(x = University_YUP_Urban_slopes_T1reg_cog,y = overall_acc_slopes_T1reg)) + 
  geom_point(size = 1.5,color = '#cb181d',alpha = .5) + geom_smooth(method = 'lm',color = '#cb181d') + 
  scale_x_continuous(breaks = -2:2,limits = c(-2,2)) + labs(x = 'University/YUP Slope',y = 'Overall Accuracy Slope',caption = paste0('r = ',round(c$estimate,2),
                                                                            ', p-value = ',round(c$p.value,3)))
uni_ovr_acc_slopes <- ggMarginal(uni_ovr_acc_slopes,type = 'density',size = 8,fill = 'grey')

c <- cor.test(slopes_df$Polluted_Urban_slopes_T1reg_cog,slopes_df$overall_acc_slopes_T1reg)

urban_ovr_acc_slopes <- slopes_df %>% 
  filter(!is.na(Polluted_Urban_slopes_T1reg_cog),!is.na(overall_acc_slopes_T1reg)) %>% 
  ggplot(aes(x = Polluted_Urban_slopes_T1reg_cog,y = overall_acc_slopes_T1reg)) + 
  geom_point(size = 1.5,color = '#756bb1',alpha = .5) + geom_smooth(method = 'lm',color = '#756bb1') + 
  scale_x_continuous(breaks = -2:2,limits = c(-2,2)) + labs(x = 'Polluted Urban Slope',y = 'Overall Accuracy Slope',caption = paste0('r = ',round(c$estimate,2),', p-value = ',round(c$p.value,3)))
                   
urban_ovr_acc_slopes <- ggMarginal(urban_ovr_acc_slopes,type = 'density',size = 8,fill = 'grey')                                                                              
                                                                                                                                             
c <- cor.test(slopes_df$Retired_slopes_T1reg_cog,slopes_df$overall_acc_slopes_T1reg)

retired_ovr_acc_slopes <- slopes_df %>% 
  filter(!is.na(Retired_slopes_T1reg_cog),!is.na(overall_acc_slopes_T1reg)) %>% 
  ggplot(aes(x = Retired_slopes_T1reg_cog,y = overall_acc_slopes_T1reg)) + 
  geom_point(size = 1.5,color = '#31a354',alpha = .5) + geom_smooth(method = 'lm',color = '#31a354') + 
  scale_x_continuous(breaks = -2:2,limits = c(-2,2)) + labs(x = 'Retired Slope',y = 'Overall Accuracy Slope',caption = paste0('r = ',round(c$estimate,2),', p-value = ',round(c$p.value,3)))

retired_ovr_acc_slopes <- ggMarginal(retired_ovr_acc_slopes,type = 'density',size = 8,fill = 'grey')

c <- cor.test(slopes_df$Wealth_slopes_T1reg_GAF,slopes_df$GAF_slope_T1reg) 

wealth_GAF_slopes <- slopes_df %>% 
  filter(!is.na(Wealth_slopes_T1reg_GAF),!is.na(GAF_slope_T1reg)) %>% 
  ggplot(aes(x = Wealth_slopes_T1reg_GAF,y = GAF_slope_T1reg)) + 
  geom_point(size = 1.5,color = '#08519c',alpha = .5) + geom_smooth(method = 'lm',color = '#08519c') + 
  scale_x_continuous(breaks = -2:2,limits = c(-2,2)) + labs(x = 'Wealth Slope',y = 'GAF Slope',caption = paste0('r = ',round(c$estimate,2),', p-value < ',max(.001,round(c$p.value,3))))

wealth_GAF_slopes <- ggMarginal(wealth_GAF_slopes,type = 'density',size = 8,fill = 'grey')

c <- cor.test(slopes_df$University_YUP_Urban_slopes_T1reg_GAF,slopes_df$GAF_slope_T1reg) 

uni_GAF_slopes <- slopes_df %>% 
  filter(!is.na(University_YUP_Urban_slopes_T1reg_GAF),!is.na(GAF_slope_T1reg)) %>% 
  ggplot(aes(x = University_YUP_Urban_slopes_T1reg_GAF,y = GAF_slope_T1reg)) + 
  geom_point(size = 1.5,color = '#cb181d',alpha = .5) + geom_smooth(method = 'lm',color = "#cb181d") + 
  scale_x_continuous(breaks = -2:2,limits = c(-2,2)) + labs(x = 'University/YUP Slope',y = 'GAF Slope',caption = paste0('r = ',round(c$estimate,2),
                                                               ', p-value = ',round(c$p.value,3)))
uni_GAF_slopes <- ggMarginal(uni_GAF_slopes,type = 'density',size = 8,fill = 'grey')

c <- cor.test(slopes_df$Polluted_Urban_slopes_T1reg_GAF,slopes_df$GAF_slope_T1reg)  

urban_GAF_slopes <- slopes_df %>% 
  ggplot(aes(x = Polluted_Urban_slopes_T1reg_GAF,y = GAF_slope_T1reg)) + 
  geom_point(size = 1.5,color = '#756bb1',alpha = .5) + geom_smooth(method = 'lm',color = "#756bb1") + 
  scale_x_continuous(breaks = -2:2,limits = c(-2,2)) + labs(x = 'Polluted Urban Slope',y = 'GAF Slope',caption = paste0('r = ',round(c$estimate,2),', p-value = ',round(c$p.value,3)))
     
urban_GAF_slopes <- ggMarginal(urban_GAF_slopes,type = 'density',size = 8,fill = 'grey')                                                                
                                                                                                                                 
c <- cor.test(slopes_df$Retired_slopes_T1reg_GAF,slopes_df$GAF_slope_T1reg)                                                                  
                                                                                                                   
retired_GAF_slopes <- slopes_df %>% 
  ggplot(aes(x = Retired_slopes_T1reg_GAF,y = GAF_slope_T1reg)) + 
  geom_point(size = 1.5,color = '#31a354',alpha = .5) + geom_smooth(method = 'lm',color = "#31a354") + 
  scale_x_continuous(breaks = -2:2,limits = c(-2,2)) + labs(x = 'Retired Slope',y = 'GAF Slope',caption = paste0('r = ',round(c$estimate,2),', p-value = ',round(c$p.value,3)))

retired_GAF_slopes <- ggMarginal(retired_GAF_slopes,type = 'density',size = 8,fill = 'grey')

theme_update(legend.position = c(.82,.85))
lepto_plot <- slopes %>%   
  pivot_longer(cols = c(Wealth_slope_GAF,University_YUP_Urban_slope_GAF,
                        Polluted_Urban_slope_GAF,Retired_slope_GAF),
               names_to = 'Env_var',values_to = 'Env_score') %>% 
  mutate(Env_var = str_remove_all(Env_var,pattern = '_slope.*')) %>% 
  mutate(Env_var = str_replace_all(Env_var,pattern = '_',replacement = ' ')) %>%  
  mutate(Env_var = ifelse(Env_var == 'University YUP Urban','University/YUP',Env_var)) %>% 
  mutate(Env_var = factor(Env_var,levels = c('Wealth','University/YUP','Polluted Urban','Retired'))) %>% 
  ggplot(aes(x = Env_score,fill = Env_var)) + geom_density(color = 'black',alpha = .5) + 
  coord_cartesian(xlim = c(-5,5)) + scale_fill_manual(values = c('#08519c','#cb181d','#756bb1','#31a354')) + 
  labs(x = 'Environment Slopes',y = 'density',fill = '') + stat_function(fun = dnorm, n = 1000,linewidth = 1, args = list(mean = 0, sd = 1))

CNB_plots <- ggarrange(wealth_ovr_acc_slopes,uni_ovr_acc_slopes,urban_ovr_acc_slopes,retired_ovr_acc_slopes)
GAF_plots <- ggarrange(wealth_GAF_slopes,uni_GAF_slopes,urban_GAF_slopes,retired_GAF_slopes)
ggarrange(annotate_figure(CNB_plots,fig.lab = 'A',fig.lab.face = 'bold',fig.lab.size = 18),
          annotate_figure(GAF_plots,fig.lab = 'B',fig.lab.face = 'bold',fig.lab.size = 18),
          annotate_figure(lepto_plot,fig.lab = 'C',fig.lab.face = 'bold',fig.lab.size = 18),nrow = 1) 

# Extra 

#c <- cor.test(slopes_df$Wealth_T1_cog,slopes_df$overall_acc_slopes_T1reg)

#wealth_t1_ovr_acc_slope <- slopes_df %>% 
#  filter(!is.na(Wealth_T1_cog),!is.na(overall_acc_slopes_T1reg)) %>%
#  ggplot(aes(x = Wealth_T1_cog,y = overall_acc_slopes_T1reg)) + 
#  geom_point(size = 1.5,color = '#08519c',alpha = .5) + geom_smooth(method = 'lm',color = '#08519c') + 
#  labs(x = 'Wealth at T1',y = 'Overall Accuracy Slope',caption = paste0('r = ',round(c$estimate,2),
#                                                                        ', p-value = ',round(c$p.value,3)))
#c <- cor.test(slopes_df$University_YUP_Urban_T1_cog,slopes_df$overall_acc_slopes_T1reg)
#
#uni_t1_ovr_acc_slopes <- slopes_df %>% 
#  filter(!is.na(University_YUP_Urban_T1_cog),!is.na(overall_acc_slopes_T1reg)) %>%
#  ggplot(aes(x = University_YUP_Urban_T1_cog,y = overall_acc_slopes_T1reg)) + 
#  geom_point(size = 1.5,color = '#cb181d',alpha = .5) + geom_smooth(method = 'lm',color = '#cb181d') + 
#  labs(x = 'University at T1',y = 'Overall Accuracy Slope',caption = paste0('r = ',round(c$estimate,2),
#                                                                            ', p-value = ',round(c$p.value,3)))
#
#c <- cor.test(slopes_df$Polluted_Urban_T1_cog,slopes_df$overall_acc_slopes_T1reg)
#
#urban_t1_ovr_acc_slopes <- slopes_df %>% 
#  filter(!is.na(Polluted_Urban_T1_cog),!is.na(overall_acc_slopes_T1reg)) %>%
#  ggplot(aes(x = Polluted_Urban_T1_cog,y = overall_acc_slopes_T1reg)) + 
#  geom_point(size = 1.5,color = '#756bb1',alpha = .5) + geom_smooth(method = 'lm',color = '#756bb1') + 
#  labs(x = 'Polluted Urban at T1',y = 'Overall Accuracy Slopes',caption = paste0('r = ',round(c$estimate,2),
#                                                                                 ', p-value = ',round(c$p.value,3)))
#
#c <- cor.test(slopes_df$Retired_T1_cog,slopes_df$overall_acc_slopes_T1reg)                                                                                                                                              
#
#retired_t1_ovr_acc_slopes <- slopes_df %>% 
#  filter(!is.na(Retired_T1_cog),!is.na(overall_acc_slopes_T1reg)) %>%
#  ggplot(aes(x = Retired_T1_cog,y = overall_acc_slopes_T1reg)) + 
#  geom_point(size = 1.5,color = '#31a354',alpha = .5) + geom_smooth(method = 'lm',color = '#31a354') + 
#  labs(x = 'Retired at T1',y = 'Overall Accuracy Slope',caption = paste0('r = ',round(c$estimate,2),
#                                                                         ', p-value = ',round(c$p.value,3)))
#
#ggarrange(labels = c('Wealth','University','Polluted Urban','Retired'),wealth_t1_ovr_acc_slope,
#          uni_t1_ovr_acc_slopes,urban_t1_ovr_acc_slopes,retired_t1_ovr_acc_slopes) 

#c <- cor.test(slopes_df$Wealth_T1_GAF,slopes_df$GAF_slope_T1reg)  
#
#wealth_t1_GAF_slopes <- slopes_df %>% 
#  ggplot(aes(x = Wealth_T1_GAF,y = GAF_slope_T1reg)) + 
#  geom_point(size = 1.5,color = '#08519c',alpha = .5) + geom_smooth(method = 'lm',color = '#08519c') + 
#  labs(x = 'Wealth at T1',y = 'GAF Slope',caption = paste0('r = ',round(c$estimate,2),
#                                                           ', p-value = ',round(c$p.value,3)))
#c <- cor.test(slopes_df$University_YUP_Urban_T1_GAF,slopes_df$GAF_slope_T1reg)  
#
#uni_t1_GAF_slopes <- slopes_df %>% 
#  ggplot(aes(x = University_YUP_Urban_T1_GAF,y = GAF_slope_T1reg)) + 
#  geom_point(size = 1.5,color = '#cb181d',alpha = .5) + geom_smooth(method = 'lm',color = '#cb181d') + 
#  labs(x = 'University at T1',y = 'GAF Slope',caption = paste0('r = ',round(c$estimate,2),
#                                                               ', p-value = ',round(c$p.value,3)))
#
#c <- cor.test(slopes_df$Polluted_Urban_T1_GAF,slopes_df$GAF_slope_T1reg)  
#
#urban_t1_GAF_slopes <- slopes_df %>% 
#  ggplot(aes(x = Polluted_Urban_T1_GAF,y = GAF_slope_T1reg)) + 
#  geom_point(size = 1.5,color = '#756bb1',alpha = .5) + geom_smooth(method = 'lm',color = '#756bb1') +
#  labs(x = 'Polluted Urban at T1',y = 'GAF Slope',caption = paste0('r = ',round(c$estimate,2),
#                                                                  ', p-value = ',round(c$p.value,3)))
#
#c <- cor.test(slopes_df$Retired_T1_GAF,slopes_df$GAF_slope_T1reg) 
#
#retired_t1_GAF_slopes <- slopes_df %>% 
#  ggplot(aes(x = Retired_T1_GAF,y = GAF_slope_T1reg)) + 
#  geom_point(size = 1.5,color = '#31a354',alpha = .5) + geom_smooth(method = 'lm',color = '#31a354') + 
#  labs(x = 'Retired at T1',y = 'GAF Slope',caption = paste0('r = ',round(c$estimate,2),', p-value = ',round(c$p.value,3)))
#
#       
#ggarrange(labels = c('Wealth','University','Polluted Urban','Retired'),wealth_t1_GAF_slopes,
#          uni_t1_GAF_slopes,urban_t1_GAF_slopes,retired_t1_GAF_slopes) 
