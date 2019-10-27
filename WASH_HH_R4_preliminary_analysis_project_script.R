#ZACK ARNO 
#WASH HH ROUND 4 - 2019
library(survey)
library(rlang)
library(dplyr)
library(tidyr)
library(lubridate)
library(hypegrammaR)
library(koboquest)
source("functions/wash_container_volume_bgd_2019.R")
source("functions/wash_hh_r4_2019_recoding.R")
source("functions/mean_prop_survey_july2019.R")


hh<- read.csv(file = "inputs/WASH_HH_R4/intermediate/HH_data24102019.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
ind <- read.csv("inputs/WASH_HH_R4/intermediate/Indiv_data_24102019.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
cont<- read.csv("inputs/WASH_HH_R4/intermediate/Container_data24102019.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
survey<- read.csv("inputs/questionnaire/Survey.csv")
choices<-read.csv("inputs/questionnaire/Choices.csv")



#FOR SOME REASON THERE WERE DUPLICATES- GET RID


hh<-hh[!duplicated(hh$instance_name),]
ind<-ind[!duplicated(ind$repeat_instance_name),]

#ADD COMPOSITE/AGGREGATE VARIABLES TO INDIVIDUAL AND HH DATA SET
individual_and_hh_data<-wash_hh_r4_2019_recoding(container_data = cont, individual_data = ind, hh_data = hh)

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
# wash_weights<-map_to_weighting(sampling.frame = pop, data.stratum.column = "camp_id",
                               # data = household_data,
                               # sampling.frame.population.column ="total_HH",
                               # sampling.frame.stratum.column = "camp_id"
# )

#SETUP HH SURVEY DESIGN OBJECTS
# hh_svy_ob<-map_to_design(data=household_data)

hh_svy_ob<-svydesign(ids=~1,
          data=household_data)
#STEAL HH WEIGHTS FOR INDIVIDUAL WEIGHTS
individual_data_added_extra_data<-household_data %>% 
  select(instance_name,camp_id, Upazila, resp_gen) %>% 
  right_join(individual_data, by= c("instance_name"="parent_instance_name"))

#MAKE INDIVIDUAL/REPEAT SURVEY OBJECT
ind_svy_ob<-svydesign(ids = ~ 1,
                data = individual_data_added_extra_data)

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
question_distinct_answer_frequency_count<-lapply(hh_sel_multiple_removed, function(x)length(unique(x))) %>% 
  unlist() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% arrange(desc(.)) #%>% dplyr::select(rowname) %>% dput()
question_distinct_answer_frequency_count$rowname[1:11] %>% dput()
hh_sel_multiple_removed %>% 
  filter(n_distinct())
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
                        "deviceid",
                        "end_note",
                        "survey_start", "instance_name", "end_survey", "gps_reading", 
                        "X_gps_reading_longitude", "X_id", "X_uuid", "X_index", "X_gps_reading_latitude", 
                        "X_submission_time", "X_gps_reading_altitude"
                        )


cols_to_analyze<-hh_sel_multiple_removed[,!names(hh_sel_multiple_removed) %in%remove_from_analysis] %>% colnames()

#RUN ANALYSIS BOTH WITH NAS REPLACED AND THEN SUBSET
hh_all_cols_preliminary_na_replaced<-mean_prop_svy_july_2019(x=hh_svy_ob, cols_to_analyze,z=TRUE,bv1="resp_gen")
#only need response level

#flag drnk_nondrnk_both outliers >2 - should log-transform before calculating outliers. - write it out for cleaning
scaled_drnk_nondrnk_both<-hh_svy_ob$variables$ic.jrp.drnk_nondrnk_both_lpppd %>% log10 %>% scale() %>% as.vector()
hh_svy_ob$variables$ic.jrp.drnk_nondrnk_both_lpppd %>% log10()
which(scaled_drnk_nondrnk_both<2) %>% length()
mad(hh_svy_ob$variables$ic.jrp.drnk_nondrnk_both_lpppd,
    center = median(hh_svy_ob$variables$ic.jrp.drnk_nondrnk_both_lpppd), constant = 1.4826,
    na.rm = FALSE, low = FALSE, high = FALSE)
mad2<-mad(hh_svy_ob$variables$ic.jrp.drnk_nondrnk_both_lpppd)*2
median_lppd<-median(hh_svy_ob$variables$ic.jrp.drnk_nondrnk_both_lpppd)

hh_svy_ob$variables %>% 
  filter(ic.jrp.drnk_nondrnk_both_lpppd> median_lppd+mad2) %>% nrow()

sd2_outliers_lpppd<-hh_svy_ob$variables %>% 
  filter(ic.jrp.drnk_nondrnk_both_lpppd>
           mean(hh_svy_ob$variables$ic.jrp.drnk_nondrnk_both_lpppd)+
           2*sd(hh_svy_ob$variables$ic.jrp.drnk_nondrnk_both_lpppd)) %>% 
  select(X_uuid, ic.jrp.drnk_nondrnk_both_lpppd)
# write.csv(sd2_outliers_lpppd,"24102019_WASH_HH_R4_lpppd_outliers_2SD.csv")
ggplot(data = hh_svy_ob$variables)+geom_histogram(aes(x=ic.jrp.drnk_nondrnk_both_lpppd), bins=100)+
  scale_x_log10(breaks=c(10,25,50,60,70, 100))+
  geom_vline(xintercept =  median_lppd+mad2)+
  geom_vline(xintercept = median_lppd-mad2)+
  geom_vline(xintercept = median_lppd)+
  geom_vline(xintercept = 65, col= "red")
  geom_vline(xintercept = mean(hh_svy_ob$variables$ic.jrp.drnk_nondrnk_both_lpppd))

m
hh_all_cols_preliminary_na_replaced[[4]]$ic.jrp.both_water_only_lpppd
hh_all_cols_preliminary_na_replaced[[4]]$I.handwashing_three_critical
hh_all_cols_preliminary_na_replaced[[4]]


#should figure out why butter doesnt work here
# asdf<-butteR::mean_proportion_table(design = hh_svy_ob,list_of_variables = c("I.indi_def_prob_yes_male","I.indi_def_prob_yes_female"),aggregation_level = NULL,round_to = 2,return_confidence = TRUE,na_replace = TRUE,questionnaire = NULL)




hh_all_cols_preliminary_na_subset<-mean_prop_svy_july_2019(x=hh_svy_ob, cols_to_analyze,z=FALSE,bv1="resp_gen")
hh_all_cols_preliminary_na_subset[[4]]
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

age_group_1_by_ind_gender<-svymean(x = ~O.age_group1_gender,ind_svy_ob) %>% data.frame()

ind_all_cols_preliminary_na_subset<-mean_prop_svy_july_2019(x=ind_svy_ob, "O.age_group1_gender",z=TRUE)

write.csv(ind_all_cols_preliminary_na_replaced[[1]],"Indiv_WASH_R4_BasicStats_NA_Replaced_23102019.csv")
write.csv(ind_all_cols_preliminary_na_subset[[1]],"Indiv_WASH_R4_BasicStats_NA_subset_23102019.csv")


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
                         "indi_disab_comm", "indi_disab_trt", "indi_bath_unsafe")


# svyby(~indi_gen, ~O.age_group1, ind_svy_ob,na.rm.all = FALSE, na.rm.by = FALSE, svymean)
svyby( ~O.age_group1, ~indi_gen, ind_svy_ob,na.rm.all = FALSE, na.rm.by = FALSE, svymean) %>% t()

individual_gender_by_age_group<-mean_prop_svy_july_2019(x =ind_svy_ob,
                                                            y = "indi_gen",
                                                            z = FALSE, bv1="O.age_group1")
#ANALYZE BY AGE GROUP 1 & GENDER AND THEN BY AGE GROUP 2 & GENDER
by_age_group1_and_gender_NA_subset<-mean_prop_svy_july_2019(x =ind_svy_ob,
                        y = individual_level_cols_for_more_complex_analysis,
                        z = FALSE, bv1=c("O.age_group1","indi_gen"))

ind_svy_ob$variable
by_age_group1_and_gender_NA_subset[[5]]$indi_bath_unsafeyes
cbind(by_age_group1_and_gender_NA_subset[[5]]$Level_id, by_age_group1_and_gender_NA_subset[[5]]$indi_bath_unsafeyes)

cbind(by_age_group1_and_gender_NA_subset[[5]]$indi_def_unsafeyes,
by_age_group1_and_gender_NA_subset[[5]]$Level_id)
by_age_group1_and_gender_NA_replaced<-mean_prop_svy_july_2019(x =ind_svy_ob,
                                 y = individual_level_cols_for_more_complex_analysis,
                                 z = TRUE, bv1=c("O.age_group1","indi_gen"))

cbind(by_age_group1_and_gender_NA_replaced[[5]]$Level_id, by_age_group1_and_gender_NA_replaced[[5]]$indi_bath_unsafeyes)


cbind(by_age_group1_and_gender_NA_replaced[[5]]$Level_id, by_age_group1_and_gender_NA_replaced[[5]]$indi_bathe_normallycomm_bathing)

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





