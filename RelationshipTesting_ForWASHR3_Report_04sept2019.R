
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
sig_output_path<- "D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQP - UNICEF WASH 2019\\04_Household_assessment_3/Products/Report/Significance_tests/significantResults/"
not_sig_output_path<- "D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQP - UNICEF WASH 2019\\04_Household_assessment_3/Products/Report/Significance_tests/notSignficantResults/"

#COLORS FOR GRAPHS
####################################################################
GWC_colors<-list(WashGreen=rgb(0/256,153/256,153/256),
                  DarkBlue= rgb(0/256, 102/256, 102/256),
                  LightGrey= rgb(204/256,204/256,204/256))
######################################################################

#FOR SOME REASON THERE WERE DUPLICATES- GET RID
hh<-hh[!duplicated(hh$instance_name),]
ind<-ind[!duplicated(ind$repeat_instance_name),]

#ADD COMPOSITE/AGGREGATE VARIABLES TO INDIVIDUAL AND HH DATA SET
individual_and_hh_data<-wash_hh_r3_2019_recoding(container_data = cont, individual_data = ind, hh_data = hh)


#SEPARATE DATASETS INTO TWO OBJECTS
household_data<-individual_and_hh_data$HH_dataset
individual_data<-individual_and_hh_data$individual_dataset
colnames(individual_data)

individual_data<- individual_data %>% 
  left_join(household_data %>% select(X_uuid,camp_id ), by=c("parent_instance_name"  ="X_uuid"))
# individual_data<- individual_data %>% 
#   left_join(household_data %>% select(X_uuid,camp_id ), by=c("X_submission_uuid"="X_uuid"))

household_data$camp_id
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


ind_svy_ob$variables<-ind_svy_ob$variables %>% 
  mutate(
    rt.ind_def_prob= if_else(indi_def_prob=="yes", "yes", "other"),
    rt.indi_def_unsafe= if_else(indi_def_unsafe=="yes","yes", "other"),
    rt.I.indi_def_where_location= if_else(I.indi_def_where_location== "communal_location",
                                          "communal_location.yes", "other"),
    rt.I.indi_def_where_type2= if_else(I.indi_def_where_type2=="bucket_pot_cloth_bag",
                                       "bucket_pot_cloth_bag.yes","other"),
    rt.indi_dia_per=if_else(indi_dia_per=="yes", "yes", "other"),
    rt.indi_diff_toilet=if_else(indi_diff_toilet=="yes", "yes", "other"),
    rt.indi_bath_prob=if_else(indi_bath_prob=="yes", "yes", "other"),
    rt.indi_bath_unsafe=if_else(indi_bath_unsafe=="yes", "yes", "other"),
    rt.indi_bathe_normally= if_else(indi_bathe_normally=="makeshift_space","makeshift_space.yes", "other")
    
    
  )

ind_svy_ob$variables$I.prob_coll_water_INDI %>% unique()

individual_subset_collect_water<-subset(ind_svy_ob, indi_collect_wat%in% c("sometimes", "always","often")) 
individual_subset_collect_water$variables$indi_collect_wat %>% unique()
individual_subset_collect_water$variables$I.prob_coll_water_INDI

# prob_coll_Water_vs_disabled<-svyby(~IS.disabA, ~I.prob_coll_water_INDI,individual_subset_collect_water, svymean, na.rm=TRUE, vartype = "ci")
prob_coll_Water_vs_disabled<-svyby(~I.prob_coll_water_INDI,~IS.disabA,individual_subset_collect_water, svymean, na.rm=TRUE, vartype = "ci")
# write.csv(prob_coll_Water_vs_disabled, paste0(sig_output_path,"IND_prob_coll_water_vs_disabled.csv"))

test_these_against_disability<-ind_svy_ob$variables %>% select(starts_with("rt.")) %>% colnames()
plot_titles<- c("% Individuals Reporting Problems With Sanitatin Facilities",
           "% Individuals Reporting Feeling Unsafe going to Sanitaton Facilities",
           "% Individuals Reporting Using a Communal Latrine",
           "% Individuals Reporting Using a Bucket, Pot, Cloth, or Bag",
           "% Individuals Reporting Indi_dia_per.yes",
           "% Individuals Reporting indi_diff_toilet.yes",
           "% Individuals Reporting indi_bath_prob.yes",
           "% Individuals Reporting indi_bath_unsafe.yes",
           "% Individuals Reporting makeshift_space.yes")
#AGAINST DISABA          
##########################################################################################

plot_list<-list()
svyby_table_list<-list()
significant_table_list<-list()
not_significant_table_list<-list()
for ( i in 1:length(test_these_against_disability)){
  print(test_these_against_disability[i])
  independent_string<-"IS.disabA"
  dependent_string<-test_these_against_disability[i]
  
  dependent_string.yes<-paste0(dependent_string,"yes")
  se_dependent_string<-paste0("se.", dependent_string)
  formula_dependent<-paste0("~", dependent_string)
  formula_independent<-paste0("~",independent_string)
  table_formula<-(paste0(formula_independent, "+", dependent_string))
  result_chisq<-svychisq(formula(table_formula),ind_svy_ob)
  pval<-result_chisq$p.value
  pval_label<-ifelse(pval<0.05,"<.05", ifelse(pval<.005,"<.005",">.05"))
  plot_title<-paste0(plot_titles[i]," (p",pval_label,")")
  svyby_table<-svyby(formula(formula_dependent), formula(formula_independent),ind_svy_ob, svymean, na.rm=T) %>% data.frame() 
  # dependent_string.yes<-svyby_table %>%
    # select(matches(paste0("^",dependent_string,".yes$")))
  dependent_string.yes<-svyby_table %>%
    select(intersect(starts_with(dependent_string), 
           ends_with("yes"))) %>% colnames()
  se_dependent_string.yes<-svyby_table %>% 
    select(intersect(starts_with(se_dependent_string), 
           ends_with("yes"))) %>% colnames()
  
  svyby_table<-svyby_table %>% 
    mutate(
      err_ymin=!!sym(dependent_string.yes)-(1.96*!!sym(se_dependent_string.yes)),
      err_ymax=!!sym(dependent_string.yes)+(1.96*!!sym(se_dependent_string.yes)),
    )
  
  er<-geom_errorbar(data = svyby_table,aes(ymin=err_ymin,
                        ymax=err_ymax), color="white", size=2,width=0.2)
  plot_list[[dependent_string]]<-
    ggplot(svyby_table,aes(factor(!!sym(independent_string)), !!sym(dependent_string.yes)))+
    geom_bar(stat= "identity",fill= GWC_colors$WashGreen, colour="white")+
    er+
    xlab("With Disability")+ 
    ggtitle(plot_title)+ 
    scale_y_continuous(labels = scales::percent)+
    theme(
      panel.background = element_rect(colour= GWC_colors$LightGrey),
      panel.grid = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(angle=45,family = "arial",size = 10),
      # text = element_text(size=10)
      )
  
  svyby_table_list[[dependent_string]]<-svyby_table
  if(result_chisq$p.value<.05){
    significant_table_list[[dependent_string]]<-svyby_table
  }
  if(result_chisq$p.value>=0.05){
      not_significant_table_list[[dependent_string]]<-svyby_table
  }
}
  
windows();plot_list$rt.ind_def_prob
plot_list$rt.indi_def_unsafe
plot_list$rt.I.indi_def_where_location
plot_list$rt.I.indi_def_where_type2
plot_list$rt.indi_dia_per
plot_list$rt.indi_diff_toilet
plot_list$rt.indi_bath_prob
plot_list$rt.indi_bath_unsafe
#potentially dissagregate this one by gender
plot_list$rt.indi_bathe_normally
# dir.create("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQP - UNICEF WASH 2019\\04_Household_assessment_3/Products/Report/Significance_tests/significantResults")

for(i in 1: length(significant_table_list)){
  # write.csv(significant_table_list[[i]], paste0(sig_output_path,names(significant_table_list)[[i]],".csv"))
}

for(i in 1: length(not_significant_table_list)){
  # write.csv(not_significant_table_list[[i]], paste0(not_sig_output_path,names(not_significant_table_list)[[i]],".csv"))
}

hh_svy_ob$variables$trt_methods.aquatabs
hh_svy_ob$variables$drink_wat
hh_svy_ob$variables <- hh_svy_ob$variables %>% 
  mutate(
    aqua_tabs_yn= ifelse(is.na(trt_methods.aquatabs),0,trt_methods.aquatabs),
    piped_water=ifelse(drink_wat=="pip",1,0),
    tube_wells=ifelse(drink_wat=="tube",1,0)
  )


# ONE OFF LATER REQUEST FROM BEN 21 10 OCT 2019
################################################
svyby(~aqua_tabs_yn,~piped_water, hh_svy_ob, svymean, na.rm.all = TRUE, na.rm=T, vartype = "ci")
svyby(~aqua_tabs_yn,~tube_wells, hh_svy_ob, svymean, na.rm.all = TRUE, na.rm=T, vartype = "ci")

piped_water_vs_chlorination_chisq<-svychisq(~piped_water+ aqua_tabs_yn,hh_svy_ob)
piped_water_vs_chlorination_chisq$p.value
tube_wells_vs_chlorination_chisq<-svychisq(~tube_wells+ aqua_tabs_yn,hh_svy_ob)
tube_wells_vs_chlorination_chisq$p.value
#################################################



dia_disab<-svyby(~rt.indi_dia_per,~IS.disabA,ind_svy_ob,svymean,na.rm=T , vartype = "ci") %>% data.frame()
dia_disab<-dia_disab[,c(1,3,5,7)]
# write.csv(dia_disab, paste0(sig_output_path,"dia_per_disab_flipped.csv"))
svychisq(~rt.indi_dia_per+IS.disabA, ind_svy_ob)

#######################################################################################
#AGAINST IS.NOADULTMALE
###############################################################################################

test_these_against_is.noadult<-c("I.prob_coll_water_HH","I.wat_30min")
#not significant
test1<-svyby(~I.prob_coll_water_HH,~IS.no_adult_18to59_coll_water ,hh_svy_ob, svymean, na.rm=T, vartype = "ci")
# write.csv(test1,paste0(not_sig_output_path,"I.prob_col_water_HH_vs_18to59HH.csv"))
test2<-svyby(~I.wat_30min,~IS.no_adult_18to59_coll_water ,hh_svy_ob, svymean, na.rm=T, vartype = "ci")
# write.csv(test2,paste0(not_sig_output_path,"I.wat_30min_vs_18to59HH.csv"))
###############################################################################################

##############################################
#VS. GENDER HOH
##############################################
#NOT REMOTELY  SIGNIFICANT
problems_with_water_vs_gender_hoh<-svyby(~I.prob_coll_water_HH,~IS.gender_hoh ,hh_svy_ob, svymean, na.rm=T, vartype = "ci")
# write.csv(problems_with_water_vs_gender_hoh,paste0(not_sig_output_path,"I.prob_coll_water_HH_vs_IS.gender_hoh.csv"))
#############################################

#tiny bit significant, but not worth reporting on
hh_disabled_vs_time_coll_water<-svyby(~IS.disab_coll_water,~I.wat_30min ,hh_svy_ob, svymean, na.rm=T, vartype = "ci")
hh_disabled_vs_time_coll_water<-svyby(~I.wat_30min,~IS.disab_coll_water ,hh_svy_ob, svymean, na.rm=T, vartype = "ci") %>% data.frame()
hh_disabled_vs_time_coll_water<-data.frame(hh_disabled_vs_time_coll_water[,c(1,3,5,7)])
svychisq(~IS.disab_coll_water+I.wat_30min,hh_svy_ob)
write.csv(hh_disabled_vs_time_coll_water,paste0(sig_output_path,"IS.disab_coll_water_vs_I.wat_30min.csv"))
#highest edu vs s
###############################################################################
hh_svy_ob$variables<-hh_svy_ob$variables %>% 
  mutate(trt_wat.yes=if_else(trt_wat=="yes", "yes","other"))
# highest_edu_vs_treat_water<-svyby(~Is.higest_edu, ~trt_wat.yes,hh_svy_ob,svymean, na.rm=TRUE, vartype = "ci")
highest_edu_vs_treat_water<-svyby(~trt_wat.yes, ~Is.higest_edu ,hh_svy_ob,svymean, na.rm=TRUE, vartype = "ci")
svychisq(~Is.higest_edu+trt_wat.yes,hh_svy_ob)

# write.csv(highest_edu_vs_treat_water, paste0(not_sig_output_path,"highest_edu_vs_treat_water.csv"))
###############################################################################
#

hh_svy_ob$variables$IS.disab
hh_svy_ob$variables<-hh_svy_ob$variables %>% 
  mutate(drink_water_problem_7days.yes= ifelse(drnk_wat_7days=="yes", "yes", "other"))
asdf<-svyby(~drink_water_problem_7days.yes, ~IS.disab,hh_svy_ob,svymean, na.rm=TRUE, vartype = "ci")
asdf<-svyby(~IS.disab,~drink_water_problem_7days.yes ,hh_svy_ob,svymean, na.rm=TRUE, vartype = "ci")
?svychisq
svychisq(~drnk_wat_7days+IS.disab,hh_svy_ob)
svychisq(~IS.disab+drnk_wat_7days,hh_svy_ob)
#############################################################################
#AGAINST DIARRHEA      
##########################################################################################

household_data$I.indi_dia_per_INDIVHH
household_data$ic.con_lid_HH %>% unique()
household_data %>% select(starts_with("wash_hands.")) %>% colnames() %>% dput()
critical_handwashing_times<-c("wash_hands.bfr_eat",
                              "wash_hands.bfr_prep_food",
                              "wash_hands.cleaning_chld_bottom",
                              "wash_hands.bfr_feed_child",
                              "wash_hands.aftr_defecation",
                              "wash_hands.bfr_brst_feed")


hh_svy_ob$variables<-hh_svy_ob$variables %>% 
  mutate(
    rt.unsafe_water_sources= if_else(I.safe_water_sources==0, "yes", "no"),
    rt.fae_vic= if_else(fae_vic %in% c("always", "often"),"yes","other"),
    rt.wst_vic= if_else(fae_vic %in% c("always", "often"),"yes","other"),
    rt.no_hnd_sp= if_else(hnd_sp=="no", "yes","other"),
    rt.critical_handwashing= if_else(rowSums(.[critical_handwashing_times],na.rm=TRUE)>2,"yes","no"),
    rt.cont_lid_HH= if_else(ic.con_lid_HH==1, "yes","no"),
    rt.cont_often_no=if_else(cont_clean_often %in% c("once_week","les_once_week"), "yes",  "no"),
    rt.cont_clean_no=if_else(cont_clean=="no","yes","no"),
    # rt.cont_cleanno=if_else(cont_clean=="no", "yes", "no")
    rt.soap_yest_no=if_else(soap_yest=="no", "yes","no")
    
    
    
  
    
  )
hh_svy_ob$variables %>% select(starts_with("rt.")) %>% colnames() 

hh_svy_ob$variables$rt.cont_notoften %>% unique()

test_these_against_dia<-hh_svy_ob$variables %>% select(starts_with("rt.")) %>% colnames() 


plot_list<-list()
svyby_table_list<-list()
significant_table_list<-list()
for ( i in 1:length(test_these_against_dia)){
  print(test_these_against_dia[i])
  independent_string<-"I.indi_dia_per_INDIVHH"
  dependent_string<-test_these_against_dia[i]
  
  dependent_string.yesans<-paste0(dependent_string,"yes")
  se_dependent_string<-paste0("se.", dependent_string)
  formula_dependent<-paste0("~", dependent_string)
  formula_independent<-paste0("~",independent_string)
  table_formula<-(paste0(formula_independent, "+", dependent_string))
  print(table_formula)
  result_chisq<-svychisq(formula(table_formula),hh_svy_ob)
  print("chisq stat calculated")
  pval<-result_chisq$p.value
  pval_label<-ifelse(pval<0.05,"<.05", ifelse(pval<.005,"<.005",">.05"))
  print("pval label made")
  # plot_title<-paste0(plot_titles[i]," (p",pval_label,")")
  plot_title<-paste0(dependent_string," (p",pval_label,")")
  svyby_table<-svyby(formula(formula_dependent), formula(formula_independent),hh_svy_ob, svymean, na.rm=T) %>% data.frame() 
  print("svytable made")
  

  dependent_string.yes<-svyby_table %>%
    select(intersect(starts_with(dependent_string),
                     ends_with("yes"))) %>% colnames()
  se_dependent_string.yes<-svyby_table %>% 
    select(intersect(starts_with(se_dependent_string), 
                     ends_with("yes"))) %>% colnames()
  
  svyby_table<-svyby_table %>% 
    mutate(
      err_ymin=!!sym(dependent_string.yes)-(1.96*!!sym(se_dependent_string.yes)),
      err_ymax=!!sym(dependent_string.yes)+(1.96*!!sym(se_dependent_string.yes)),
    )
  
  er<-geom_errorbar(data = svyby_table,aes(ymin=err_ymin,
                                           ymax=err_ymax), color="white", size=2,width=0.2)
  plot_list[[dependent_string]]<-
    ggplot(svyby_table,aes(factor(!!sym(independent_string)), !!sym(dependent_string.yes)))+
    geom_bar(stat= "identity",fill= GWC_colors$WashGreen, colour="white")+
    er+
    xlab("I.indi_dia_per_INDIVHH")+ 
    ggtitle(plot_title)+
    scale_y_continuous(labels = scales::percent)+
    theme(
      panel.background = element_rect(colour= GWC_colors$LightGrey),
      panel.grid = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(angle=45,size = 10),
      # text = element_text(size=10)
    )
  
  svyby_table_list[[dependent_string]]<-svyby_table

  if(result_chisq$p.value<.05){
    significant_table_list[[dependent_string]]<-svyby_table
  }
  if(result_chisq$p.value>=0.05){
    not_significant_table_list[[dependent_string]]<-svyby_table
  }
}


plot_list$rt.unsafe_water_sources
plot_list$rt.fae_vic

for(i in 1: length(significant_table_list)){
  write.csv(significant_table_list[[i]], paste0(sig_output_path,names(significant_table_list)[[i]],".csv"))
}

for(i in 1: length(not_significant_table_list)){
  write.csv(not_significant_table_list[[i]], paste0(not_sig_output_path,names(not_significant_table_list)[[i]],".csv"))
}


# redo diarrhea with independent and dependent flipped --------------------
critical_handwashing_times<-c("wash_hands.bfr_eat",
                              "wash_hands.bfr_prep_food",
                              "wash_hands.cleaning_chld_bottom",
                              "wash_hands.bfr_feed_child",
                              "wash_hands.aftr_defecation",
                              "wash_hands.bfr_brst_feed")


hh_svy_ob$variables<-hh_svy_ob$variables %>% 
  mutate(
    rt.unsafe_water_sources= if_else(I.safe_water_sources==0, "yes", "no"),
    rt.fae_vic= if_else(fae_vic %in% c("always", "often"),"yes","other"),
    rt.wst_vic= if_else(fae_vic %in% c("always", "often"),"yes","other"),
    rt.no_hnd_sp= if_else(hnd_sp=="no", "yes","other"),
    rt.critical_handwashing= if_else(rowSums(.[critical_handwashing_times],na.rm=TRUE)>2,"yes","no"),
    rt.cont_lid_HH= if_else(ic.con_lid_HH==1, "yes","no"),
    rt.cont_often_no=if_else(cont_clean_often %in% c("once_week","les_once_week"), "yes",  "no"),
    rt.cont_clean_no=if_else(cont_clean=="no","yes","no"),
    # rt.cont_cleanno=if_else(cont_clean=="no", "yes", "no")
    rt.soap_yest_no=if_else(soap_yest=="no", "yes","no")
    rt.indi_dia_per
    
    
    
    
  )
hh_svy_ob$variables %>% select(starts_with("rt.")) %>% colnames() 

hh_svy_ob$variables$rt.cont_notoften %>% unique()

test_these_against_dia<-c(hh_svy_ob$variables %>% select(starts_with("rt.")) %>% colnames() ,("rt.indi_dia_per"))

plot_list<-list()
svyby_table_list<-list()
significant_table_list<-list()
for ( i in 1:length(test_these_against_dia)){
  print(test_these_against_dia[i])
  independent_string<-test_these_against_dia[i]
  dependent_string<-"I.indi_dia_per_INDIVHH"
  
  dependent_string.yesans<-paste0(dependent_string,"yes")
  se_dependent_string<-paste0("se.", dependent_string)
  formula_dependent<-paste0("~", dependent_string)
  formula_independent<-paste0("~",independent_string)
  table_formula<-(paste0(formula_independent, "+", dependent_string))
  print(table_formula)
  result_chisq<-svychisq(formula(table_formula),hh_svy_ob)
  print("chisq stat calculated")
  pval<-result_chisq$p.value
  pval_label<-ifelse(pval<0.05,"<.05", ifelse(pval<.005,"<.005",">.05"))
  print("pval label made")
  # plot_title<-paste0(plot_titles[i]," (p",pval_label,")")
  plot_title<-paste0(dependent_string," (p",pval_label,")")
  svyby_table<-svyby(formula(formula_dependent), formula(formula_independent),hh_svy_ob, svymean, na.rm=T) %>% data.frame() 
  print("svytable made")
  
  
  dependent_string.yes<-svyby_table %>%
    select(starts_with(dependent_string)) %>% colnames()
  se_dependent_string.yes<-svyby_table %>%
    select(starts_with("se")) %>% colnames()
  
  svyby_table<-svyby_table %>% 
    mutate(
      err_ymin=!!sym(dependent_string.yes)-(1.96*!!sym(se_dependent_string.yes)),
      err_ymax=!!sym(dependent_string.yes)+(1.96*!!sym(se_dependent_string.yes)),
    )
  
  er<-geom_errorbar(data = svyby_table,aes(ymin=err_ymin,
                                           ymax=err_ymax), color="white", size=2,width=0.2)
  plot_list[[independent_string]]<-
    ggplot(svyby_table,aes(factor(!!sym(independent_string)), !!sym(dependent_string.yes)))+
    geom_bar(stat= "identity",fill= GWC_colors$WashGreen, colour="white")+
    er+
    xlab("I.indi_dia_per_INDIVHH")+ 
    ggtitle(plot_title)+
    scale_y_continuous(labels = scales::percent)+
    theme(
      panel.background = element_rect(colour= GWC_colors$LightGrey),
      panel.grid = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(angle=45,size = 10),
      # text = element_text(size=10)
    )
  
  svyby_table_list[[independent_string]]<-svyby_table
  
  if(result_chisq$p.value<.05){
    significant_table_list[[independent_string]]<-svyby_table
  }
  if(result_chisq$p.value>=0.05){
    not_significant_table_list[[independent_string]]<-svyby_table
  }
}


for(i in 1: length(significant_table_list)){
  write.csv(significant_table_list[[i]], paste0(sig_output_path,names(significant_table_list)[[i]],"_dia_flipped.csv"))
}

for(i in 1: length(not_significant_table_list)){
  write.csv(not_significant_table_list[[i]], paste0(not_sig_output_path,names(not_significant_table_list)[[i]],"_dia_flipped.csv"))
}


hh_svy_ob$variables$I.indi_dia_per_INDIVHH %>% class()
hh_svy_ob$variables$rt.no_hnd_sp<-hh_svy_ob$variables$rt.no_hnd_sp %>% as.factor()

asdf<-svychisq(~rt.no_hnd_sp+I.indi_dia_per_INDIVHH, hh_svy_ob, na.rm=FALSE,statistic = c("F",  "Chisq","Wald","adjWald","lincom","saddlepoint"))
svyby(~I.indi_dia_per_INDIVHH,~rt.no_hnd_sp, hh_svy_ob, svymean, vartype= "ci") 
svyby(~I.indi_dia_per_INDIVHH,~rt.no_hnd_sp, hh_svy_ob, svyciprop, vartype= "ci") 


hh_svy_ob$variables$I.indi_dia_per_INDIVHH %>% table()
table(hh_svy_ob$variables$I.indi_dia_per_INDIVHH,hh_svy_ob$variables$rt.no_hnd_sp)
plot_list$rt.no_hnd_sp
#DONE
##################################################################











# ggsave(file="test.png", plot_list$rt.unsafe_water_sources, width=2.2, height=3.6 , units = "in")
plot_list$rt.fae_vic
plot_list$rt.cont_clean_no
plot_list$rt.cont_lid_HH
plot_list$rt.wst_vic
plot_list$rt.no_hnd_sp
plot_list$rt.cont_often_no
plot_list$rt.critical_handwashing


significant_table_list

svyby_table_list$rt.critical_handwashing
svyby_table_list$rt.cont_lid_HH 


svyby_table %>% select(matches("yes$"))

plot_list$rt.cont_lid_HH
hh_svy_ob$variables$rt.cont_cleanno











#no subset
test_these_against_disability<-c("indi_def_prob",
                                 "indi_def_unsafe",
                                 "I.indi_def_where_location",
                                 "I.indi_def_where_type2",
                                 "indi_dia_per",
                                 "indi_diff_toilet", 
                                 "indi_bath_prob",
                                 "indi_bath_unsafe")#,
                                 # "makeshift_space" )

svyby_table<-list()
for (test_these in test_these_against_disability){
  print(test_these)
  independent_string<-"IS.disabA"
  dependent_string<-test_these
  formula_dependent<-paste0("~", dependent_string)
  formula_independent<-paste0("~",independent_string)
  table_formula<-(paste0(formula_independent, "+", dependent_string))
  result_chisq<-svychisq(formula(table_formula),_svy_ob)
  svyby_table<-svyby(formula(formula_dependent), formula(formula_independent),ind_svy_ob, svymean, na.rm=T,vartype = "ci")
  svy_mean<- svyby_table %>% select(-starts_with("ci_u."), - starts_with("ci_l."),-independent_string) %>% tidyr::gather(key="answer",value="mean")
  high_ci<-svyby_table %>% select(IS.disabA,starts_with("ci_u.")) %>% tidyr::gather(key="answer",value="high_ci")
  low_ci<-svyby_table %>% select(starts_with("ci_l.")) %>% tidyr::gather(key="answer",value="low_ci")
  stats_table<-data.frame(svy_mean, high_ci=high_ci$high_ci,low_ci=low_ci$low_ci)
  
}

svyby_table %>% select(IS.disabA, starts_with("ci_u.")) 
high_ci
svyby_table

ggplot(stats_table)+geom_point(aes(x=answer, y=mean))
stats_table
svy_mean
svyby_table
low_ci
tidyr::gather(low_ci, key="answer", value="low_ci")
svytable
svyby_table
svy_table
ind_svy_ob$variables$indi_bath_prob %>% class()
svyby_table

svyby(formula(formula_dependent), formula(formula_independent),ind_svy_ob, svyciprop, na.rm=T,vartype = "ci")
formula_dependent
svyby_table
table_formula

svyby_table
dependent_string

formula_dependent
