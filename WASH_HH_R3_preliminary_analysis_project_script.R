#ZACK ARNO 
#WASH HH ROUND 3 - 2019
library(survey)
library(rlang)
library(dplyr)
library(tidyr)
library(lubridate)
library(hypegrammaR)
library(koboquest)
source("functions/wash_container_volume_bgd_2019.R")
source("functions/wash_hh_r3_2019_recoding.R")
source("functions/mean_prop_survey_july2019.R")

hh<- read.csv(file = "inputs/Header_07july2019.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
ind <- read.csv("inputs/Individual_19June2019.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
cont<- read.csv("inputs/Container_19June2019.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
survey<- read.csv("inputs/Survey.csv")
choices<-read.csv("inputs/Choices.csv")



#FOR SOME REASON THERE WERE DUPLICATES- GET RID
hh<-hh[!duplicated(hh$instance_name),]
ind<-ind[!duplicated(ind$repeat_instance_name),]

#ADD COMPOSITE/AGGREGATE VARIABLES TO INDIVIDUAL AND HH DATA SET
individual_and_hh_data<-wash_hh_r3_2019_recoding(container_data = cont, individual_data = ind, hh_data = hh)

#SEPARATE DATASETS INTO TWO OBJECTS
household_data<-individual_and_hh_data$HH_dataset
individual_data<-individual_and_hh_data$individual_dataset

#USE KOBOQUEST TO GET QUESTIONNAIRES- WILL USE LATER
hh_survey<-koboquest::load_questionnaire(data = household_data,questions = survey, choices = choices,choices.label.column.to.use = "label")
ind_survey<-koboquest::load_questionnaire(data = individual_data,questions = survey, choices = choices,choices.label.column.to.use = "label")


#LOAD POPULATION DATA AND EXTRACT RELEVANT COLUMNS
pop_dir<-"WASH_unhcr_population_and_assessment_sample_size_data.csv"
pop<-read.csv(pop_dir, stringsAsFactors = FALSE, na.strings=c("NA", "", " "))
pop<-pop %>% select(camp_id, total_HH)

#GET HH WEIGHTS
wash_weights<-map_to_weighting(sampling.frame = pop, data.stratum.column = "camp_id",
                               data = household_data,
                               sampling.frame.population.column ="total_HH",
                               sampling.frame.stratum.column = "camp_id"
)

#SETUP HH SURVEY DESIGN OBJECTS
hh_svy_ob<-map_to_design(data=household_data,weighting_function = wash_weights)

#STEAL HH WEIGHTS FOR INDIVIDUAL WEIGHTS
individual_data_with_weights<-household_data %>% 
  mutate(weights= weights(hh_svy_ob)) %>% 
  select(instance_name,camp_id, Upazila, resp_gen, weights) %>% 
  right_join(individual_data, by= c("instance_name"="parent_instance_name"))

#MAKE INDIVIDUAL/REPEAT SURVEY OBJECT
ind_svy_ob<-svydesign(ids = ~ 1,
                strata =  ~ camp_id,
                weights= ~weights,
                data = individual_data_with_weights)

#CONVERT CHARACTER VARIABLES TO FACTOR FOR BOTH INDIVIDUAL AND HH DATA
ind_svy_ob$variables<-ind_svy_ob$variables %>% 
  mutate_if(sapply(ind_svy_ob$variables, is.character),as.factor)


hh_svy_ob$variables<-hh_svy_ob$variables %>% 
  mutate_if(sapply(hh_svy_ob$variables, is.character),as.factor)

#PRELIMINARY ANALYSIS
################################################################################################################
#1. HH LEVEL

#USE KOBOQUEST FUNCTIONALITY TO HELP REMOVE AGGREGATED SELECT MULTIPLE RESPONSES-- SHOULD WRITE A FUNCTION FOR THIS
sel_multiple_logical_list<-list()
for(col in colnames(hh_svy_ob$variables)){
  sel_multiple_logical_list[[col]]<-hh_survey$question_is_select_multiple(col)
}
sel_multiple_logical<-do.call("rbind", sel_multiple_logical_list)
not_sel_multiple_index<-sel_multiple_logical==FALSE
hh_sel_multiple_removed<-hh_svy_ob$variables[,not_sel_multiple_index]

#THIS IS JUST TO INSEPCT WEIRD COLUMNS TO REMOVE
lapply(hh_sel_multiple_removed, function(x)length(unique(x))) %>% 
  unlist() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% arrange(desc(.)) #%>% dplyr::select(rowname) %>% dput()

#REMOVE THESE SINCE THEY ADD NO VALUE AND CAN CAUSE ISSUES
remove_from_analysis<-c("instance_name", 
                        "survey_start",
                        "end_survey", 
                        "X_id", 
                        "X_uuid", 
                        "X_index", 
                        "X_submission_time", 
                        "arriv_bgd",
                        "arriv_shelter",
                        "arriv_shelter.dte",
                        "enu_id",
                        "camp_id",
                        "survey_date",
                        "deviceid"
                        )
cols_to_analyze<-hh_sel_multiple_removed[,!names(hh_sel_multiple_removed) %in%remove_from_analysis] %>% colnames()

#RUN ANALYSIS BOTH WITH NAS REPLACED AND THEN SUBSET


hh_all_cols_preliminary_na_replaced<-mean_prop_svy_july_2019(x=hh_svy, cols_to_analyze,z=TRUE,bv1="resp_gen")
hh_all_cols_preliminary_na_subset<-mean_prop_svy_july_2019(x=hh_svy, cols_to_analyze,z=FALSE,bv1="resp_gen")

#WRITOUTPUT
# write.csv(hh_all_cols_preliminary_na_replaced[[1]], "output/HH_WASH_R3_BasicStats_NA_Replaced_07july2019.csv")
# write.csv(hh_all_cols_preliminary_na_subset[[1]], "HH_WASH_R3_BasicStats_NA_Subset_07july2019.csv")

#2. DO THE SAME EXACT THING BUT THIS TIME FOR THE INDIVIDUAL/REPEAT DATA
#REMOVE AGGREGATED SELECT MULTIPLE RESPONSES-- SHOULD WRITE A FUNCTION FOR THIS
sel_multiple_logical_list<-list()
for(col in colnames(ind_svy_ob$variables)){
  sel_multiple_logical_list[[col]]<-ind_survey$question_is_select_multiple(col)
}
sel_multiple_logical<-do.call("rbind", sel_multiple_logical_list)
not_sel_multiple_index<-sel_multiple_logical==FALSE


ind_sel_multiple_removed<-ind_svy_ob$variables[,not_sel_multiple_index]

lapply(ind_sel_multiple_removed, function(x)length(unique(x))) %>% 
  unlist() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% arrange(desc(.)) #%>% dplyr::select(rowname) %>% dput()

remove_from_analysis<-c("instance_name", 
                        "survey_start",
                        "end_survey", 
                        "X_id", 
                        "X_uuid", 
                        "X_index", 
                        "X_submission_time", 
                        "arriv_bgd",
                        "arriv_shelter",
                        "arriv_shelter.dte",
                        "enu_id",
                        "camp_id",
                        "survey_date",
                        "deviceid", 
                        "repeat_instance_name",
                        "X_parent_index",
                        "weights",
                        "theloopcount",
                        "Upazila",
                        "X_parent_table_name"
                        
)

cols_to_analyze<-ind_sel_multiple_removed[,!names(ind_sel_multiple_removed) %in%remove_from_analysis] %>% colnames()


ind_all_cols_preliminary_na_replaced<-mean_prop_svy_july_2019(x=ind_svy_ob, cols_to_analyze,z=TRUE,bv1="indi_gen")
ind_all_cols_preliminary_na_subset<-mean_prop_svy_july_2019(x=ind_svy_ob, cols_to_analyze,z=TRUE,bv1="indi_gen")

write.csv(ind_all_cols_preliminary_na_replaced[[1]],"Indiv_WASH_R3_BasicStats_NA_Replaced_08082019.csv")
write.csv(ind_all_cols_preliminary_na_subset[[1]],"Indiv_WASH_R3_BasicStats_NA_subset_08082019.csv")


# write.csv(all_cols_basic[[1]], "HH_WASH_R3_BasicStats_NA_Replaced_07july2019.csv")
# write.csv(all_cols_basic2[[1]], "HH_WASH_R3_BasicStats_NA_Subset_07july2019.csv")





#ADDITIONAL ANALYSIS FOR REPORT
#####################################################################
#1.) INDIVIDUAL LEVEL ANALYSIS
# SELECT COLUMNS
library(dplyr)


individual_level_cols_for_more_complex_analysis<-c("indi_def_where","indi_def_prob",
                         individual_data %>% select(starts_with("indi_dia_measures.")) %>% colnames(),
                         individual_data %>%  select(starts_with("indi_def_typ.")) %>% colnames(),
                         "indi_def_unsafe",
                         "indi_diff_toilet",
                         "indi_dia_per",
                         "indi_bathe_normally",
                         individual_data %>% select(starts_with("indi_wat_prb.")) %>% colnames(),
                         individual_data %>% select(starts_with("indi_bath_prob_ty.")) %>% colnames() ,
                         "indi_collect_wat", "indi_prob_coll", "indi_bath_prob", 
                         "marital_status", "indi_carer", "indi_work_inc", "indi_disab_see", 
                         "indi_disab_hear", "indi_disab_climb", "indi_disab_remem", "indi_disab_wash", 
                         "indi_disab_comm", "indi_disab_trt")


#ANALYZE BY AGE GROUP 1 & GENDER AND THEN BY AGE GROUP 2 & GENDER
by_age_group1_and_gender_NA_subset<-mean_prop_svy_july_2019(x =ind_svy_ob,
                        y = individual_level_cols_for_more_complex_analysis,
                        z = FALSE, bv1=c("O.age_group1","indi_gen"))

by_age_group1_and_gender_NA_replaced<-mean_prop_svy_july_2019(x =ind_svy_ob,
                                 y = individual_level_cols_for_more_complex_analysis,
                                 z = TRUE, bv1=c("O.age_group1","indi_gen"))



by_age_group2_and_gender_NA_subset<-mean_prop_svy_july_2019(x =ind_svy_ob,
                                 y = individual_level_cols_for_more_complex_analysis,
                                 z = FALSE, bv1=c("O.age_group2","indi_gen"))

by_age_group2_and_gender_NA_subset<-mean_prop_svy_july_2019(x =ind_svy_ob,
                                 y = individual_level_cols_for_more_complex_analysis,
                                 z = TRUE, bv1=c("O.age_group2","indi_gen"))

# write.csv(by_age_group1_and_gender_NA_subset[[1]],'output/2019_08_20_NA_Subset_INDIV_additional_break_down.csv')
# write.csv(by_age_group1_and_gender_NA_replaced[[1]],'output/2019_08_20_NA_replaced_INDIV_additional_break_down.csv')
# write.csv(by_age_group2_and_gender_NA_subset[[1]],'output/2019_08_20_NA_Subset_INDIV_additional_break_down_agegroup2.csv')
# write.csv(by_age_group2_and_gender_NA_subset[[1]],'output/2019_08_20_NA_replaced_INDIV_additional_break_down_age_group2.csv')

#1.) HH LEVEL FOR ADDITIONAL ANALYSIS (REPORT)

hh_level_cols_for_more_complex_analysis<-c("sat_wat", "lat_sat", "bath_sat", "soap_yest",hh_svy$variables %>% select(starts_with("wash_hands.")) %>% colnames(),hh_svy$variables %>% select(starts_with("train_type.")) %>% colnames() ,hh_svy$variables %>% select(starts_with("more_train.")) %>% colnames() ,"hyg_train", "more_info", "menst_sat", 
                                           "camp_sat", "consult_three_months", "consult_opinion")



hh_svy$variables %>% select(starts_with("soap_yest.")) %>% colnames() %>% dput()
"wash_hands", 
hh_by_age_group1_and_gender_NA_subset<-mean_prop_svy_july_2019(x =hh_svy,
                                                               y = hh_level_cols_for_more_complex_analysis,
                                                               z = FALSE, bv1=c("resp.age_group2","resp_gen"))
hh_by_age_group1_and_gender_NA_replaced<-mean_prop_svy_july_2019(x =hh_svy,
                                                                 y = hh_level_cols_for_more_complex_analysis,
                                                                 z = TRUE, bv1=c("resp.age_group2","resp_gen"))

hh_by_age_group2_and_gender_NA_subset<-mean_prop_svy_july_2019(x =hh_svy,
                                                               y = hh_level_cols_for_more_complex_analysis,
                                                               z = FALSE, bv1=c("resp.age_group2","resp_gen"))

hh_by_age_group2_and_gender_NA_replaced<-mean_prop_svy_july_2019(x =hh_svy,
                                                                 y = hh_level_cols_for_more_complex_analysis,
                                                                 z = TRUE, bv1=c("resp.age_group2","resp_gen"))
# write.csv(hh_by_age_group1_and_gender_NA_subset[[1]],'output/2019_08_08_NA_Subset_HH_additional_break_down.csv')
# write.csv(hh_by_age_group1_and_gender_NA_replaced[[1]],'output/2019_08_08_NA_replaced_HH_additional_break_down.csv')

# write.csv(hh_by_age_group2_and_gender_NA_subset[[1]],'output/2019_08_08_NA_Subset_HH_additional_break_down_agegroup2.csv')
# write.csv(hh_by_age_group2_and_gender_NA_replaced[[1]],'output/2019_08_08_NA_replaced_HH_additional_break_down_age_group2.csv')





