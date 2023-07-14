# Environment_scores
Create environment scores for Penn BBL subjects based on U.S Census data 

Overview of Project

1. Generate_address_datasets.R creates separate files for current and permanent addresses for all protocols in the Penn BBL. Since Go1 data only has one address, the permanent and current addresses are treated as the same. The files current_address_all_protocols_12_15_2022.csv and perm_address_all_protocols_12_15_2022.csv are created from this script
2. Next, the script LinkAddressToCensus_all_data.R merges Census data into the data sets from step 1 and outputs the files LocalAddress_all_protocols_CensusLinked_Dec2022.csv and PermAddress_all_protocols_CensusLinked_Dec2022.csv
3. Reorder_columns.R orders the columns into Tyler's preferred order for generating the factor scores. This script outputs the file LocalAddress_with_unemployment_vars_ordered_1_4_2023.csv and PermAddress_with_unemployment_vars_ordered_1_4_2023.csv
4. The script add_22q_indicator_and_toxin-data.R adds environmental toxin variables as well as an indicator variable for whether a subject has a 22q deletion syndrome. The data sets PermAddress_with_env_toxins_1_13_2023.csv and LocalAddress_with_env_toxins_1_13_2023.csv are outputted by this script and sent to Tyler for environment scores
5. Tyler sends back environment scores in the file census_scores_16february2023_4-factor.csv, which are matched to CNB, diagnosis, and SIPS data with the script 4_factor_with_toxins_match_neighborhood_env.R. In this script, the # of observations from permanent addresses drops from 17191 to 14571 since individuals with NA environment scores or no CNB/diag/SIPS data to accompany their environment scores are removed. The data set outputted from this step is named Neighborhood_env_all_matched_clean_02_17_2023.csv (at this point the decision was made to only use permanent addresses so only one file is generated in the script)
6. The file Neighborhood_env_all_matched_clean_02_17_2023.csv was sent to Tyler who generated age-regressed cognitive scores, removed invalid CNB data, and returned the file census_matched_full.csv which was used for all models
7. 4_factor_with_toxins_all_env_models.R contains mixed-effect models which use the census_matched_full.csv to evaluate the association between neighborhood environment and cognitive performance/diagnosis
8. Tyler used the census_matched_full.csv to create the file census_environment_slopes_all.csv which contains slopes that represent changes in CNB/environment/SIPS over time.
9. cog_env_clin_slopes_analysis.R contains analyses which examine how changes in environment are associated with changes in clinical symptoms and cognitive scores
