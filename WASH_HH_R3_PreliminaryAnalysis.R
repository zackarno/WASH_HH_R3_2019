#ZACK ARNO 
#WASH HH ROUND 3 - 2019


library(survey)
library(rlang)
library(dplyr)
library(tidyr)


#CUSTOM FUNCTIONS
######################################################################################################################

# THIS FUNCTION DOES THE HEAVY LIFTING WHEN IT IS ANALYSIS TIME
# X= SURVEY OBJECT, Y= INDICATOR LIST (COLNAMES), Z= DO YOU WANT NAS TO REPLACED (TRUE) OR REAL NAS (FALSE)
# BV1= WHAT YOU WANT TO ANALYZE BY I.E (RESP GEN, AGE GROUP, ETC.)
# ** ALL CHARACTERS MUST BE CONVERTED TO FACTOR PRIOR TO RUNNING
# RIGHT NOW I RUN IT ON NUMERICAL  CHARACTERS AND FACTOR CATEGORICAL FACTORS SEPARATELY AND THEN MERGE THE ANALYSES
# NOT SURE IF IT IS NECESSARY OR JUST A RELIC OF BEFORE I HAD IMPROVED THE FUNCTIONS

mean_prop_svy<-function(x,y,z,bv1){
  #CREATE BY VAR FORMULA
  bvar_fa<-paste0(bv1,collapse="+")
  bvar_f<-paste("~",bvar_fa,collapse="")
  #PRINT FORMULA SO YOU SEE THAT IT IS CORRECT
  print(bvar_f)
  # MAKE A BUNCH OF LISTS TO FILL
  cl<-list()
  ul<-list()
  rl<-list()
  gl<-list()
  
  #LOOP THROUGH ALL OF THE INDICATORS IN THE LIST
  for(i in 1:length(y)){
    print(paste0((i/length(y))*100,"%"))
    print(y[i])
    ioi<-y[i]
    ioi_v<-x$variables[,ioi]
    #CREATE FORMULA FOR INDICATOR
    ioi_f<-paste0("~",ioi)
    #IF TRUE REPLACE NAS WITH SOMETHING
    if (z==TRUE){
      x2<-x
      x2$variables[,ioi]<-ifelse(is.na(ioi_v), "not_applicable", as.character(x2$variables[,ioi]))
      x2$variables[,ioi]<-factor(x2$variables[,ioi])
      x2<-x2
    }
    #IF FALSE LEAVE NAS
    if(z==FALSE){
      x2<-x
    }
    #DO ANALYSES WITH SURVEY PACKAGE FUNCTIONALITY
    xm<-svyby(formula(ioi_f),by = ~camp_id, x2, na.rm=TRUE, na.rm.by = FALSE ,FUN = svymean,keep.var = FALSE) %>% data.frame()
    ym<-svyby(formula(ioi_f),by = ~upa_strat, x2,na.rm = TRUE,na.rm.by = FALSE , FUN = svymean, keep.var=FALSE) %>% data.frame()
    zm <- svymean(formula(ioi_f), x2,na.rm=TRUE, na.rm.by = FALSE, keep.var=FALSE )
    gm<-svyby(formula(ioi_f),by = formula(bvar_f),FUN = svymean,na.rm=TRUE, na.rm.by=FALSE,design = x2, keep.var=FALSE) %>% data.frame()
    
    #CREATE LEV OBJECT TO BE USED TO CONCATENATE LABELS WHEN IT IS A COMPLEX CROSS OF VARIABLES
    lev <- do.call(paste, c(gm[bv1], sep="_"))
    #THIS EXTRACT NAMES FROM THE 
    name_cols<- zm  %>% data.frame() %>% t() %>% colnames()
    #REMOVE FIRST COUPLE OF COLUMNS FROM DATA SO THAT I CAN CBIND THEM TOGETHER
    xm2<-xm %>% select(-camp_id)
    colnames(xm2)<-name_cols
    ym2<-ym %>% select(-upa_strat)
    colnames(ym2)<-name_cols

    gm2<- gm %>% select(-bv1)
    # SET COL NAMES
    colnames(gm2)<-name_cols
    
    cl[[i]]<-xm2
    ul[[i]] <- ym2 
    rl[[i]] <- zm  %>% data.frame() %>% t()
    gl[[i]]<-gm2 %>% data.frame()
    
  }
  #BIND TOGETHER AND ADD RELEVANT COLUMNS
  cldf <- do.call("cbind", cl) %>% data.frame
  
  cldf <- cldf %>% mutate(Level = "Camp", Level_id = xm$camp_id) %>%
    select(Level, Level_id, everything())
  print("camp level complete")
  uldf <-do.call("cbind", ul) %>% data.frame
  
  uldf <-uldf %>% mutate(Level = "Upazilla", Level_id = ym$upa_strat) %>%
    select(Level, Level_id, everything())
  print("Upazila level complete")
  rldf <-do.call("cbind", rl) %>% data.frame
  
  rldf <-rldf %>% mutate(Level = "Response", Level_id = "Full Response") %>%
    select(Level, Level_id, everything())
  print("Response level complete")
  gldf <-do.call("cbind", gl) %>% data.frame
  #THIS ONE SHOULD BE MORE DYNAMIC, BUT WAS MADE EARLIER BEFORE I ADDED THE ABILITY FOR THE CODE TO
  #HANDLE MORE BY STATEMENTS, THEREFORE THE 
  gldf <-gldf %>% mutate(Level = "Response_Gender", Level_id = lev) %>%
    select(Level, Level_id, everything())
  print("Response level cross gender complete")
  all_strata <- do.call("rbind", list(cldf, uldf, rldf,gldf))
  
  return(list(all_strata,cldf, uldf, rldf,gldf))
}


#FUCNTION TO RETURN UNIQUE VALUES NOT INCLUDING NA-- CAN'T REMEMBER WHERE OR IF I USED IT
unique_nona<-function(x){
  unique(x[!is.na(x)])}



#############################################################

# SET REL PATHS
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#LOAD DATA
data_dir<-"../data analysis/01_Final_Data_19june2019/01_csvs/"
dir(data_dir)
hz <- read.csv(paste0(data_dir, "Header_07july2019.csv" ), stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
iz <- read.csv(paste0(data_dir,"Individual_19June2019.csv"), stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
cz <- read.csv(paste0(data_dir,"Container_19June2019.csv" ), stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))

#FOR SOME REASON THERE WERE DUPLICATES- GET RID
hz<-hz[!duplicated(hz$instance_name),]

iz2<-iz[!duplicated(iz$repeat_instance_name),]

######################
########CONTAINER LEVEL

#CREATE COMPOSITE CONTAINER VALUES AGGREGATED AT THE HH LEVEL
vol<-function(x,y,z,w){
  if(is.na(x) & is.na(y) & is.na(w) & !is.na(z)){
    ((4*pi*(((z*2.54)/(2*pi))^3)/3)*(-0.0078*z+1.1217))/1000
  } else if(!is.na(x) & !is.na(y) & is.na(w) & is.na(z)){
    ((pi*(x*2.54)*((y*2.54)/2)^2)*0.85)/1000
    
  } else if(!is.na(x) & !is.na(y) & !is.na(w) & is.na(z)){
    ((x*2.54*y*2.54*w*2.54)*0.95)/1000
  } else{
    0
  }
}

cz$wat_con_use %>% unique() %>% dput()

CZHH<-cz %>% 
  mutate(
    container_vol<-mapply(vol,cz$con_height, cz$con_width, cz$con_circum, cz$con_length),
    drinking_water_and_both=ifelse((wat_con_use %in% c("drnk_water", "both") &wat_con_collection== "yes"),container_vol*wat_col_tm,0),
    drink_wat_only= ifelse((wat_con_use=="drnk_water" & wat_con_collection== "yes"),container_vol*wat_col_tm,0),
    not_drink_only= ifelse((wat_con_use=="nonedrnk_wat" &wat_con_collection== "yes"),container_vol*wat_col_tm,0),
    both_only= ifelse((wat_con_use=="both" &wat_con_collection== "yes"),container_vol*wat_col_tm,0),
    # dom_drnk_all=ifelse((wat_con_use %in% c("nonedrnk_wat", "drnk_water", "both")& wat_con_collection== "yes"),container_vol*wat_col_tm,0)

         ) %>% 
  group_by(X_parent_index) %>% 
  summarise(ic.has_almm=ifelse(sum(con_type=="almn",na.rm=TRUE)>0,1,0),
            ic.has_plastic_container_lge=ifelse(sum(con_type=="plastic_container_lge",na.rm=TRUE)>0,1,0),
            ic.chas_bucket=ifelse(sum(con_type=="bucket" ,na.rm=TRUE)>0,1,0),
            ic.has_plastic_jug=ifelse(sum(con_type== "plastic_jug",na.rm=TRUE)>0,1,0),
            ic.has_plastic_jerry=ifelse(sum(con_type== "plastic_jerry" ,na.rm=TRUE)>0,1,0),
            ic.has_plastic_container_sm=ifelse(sum(con_type== "plastic_container_sm",na.rm=TRUE)>0,1,0),
            ic.has_bottle=ifelse(sum(con_type== "bottle" ,na.rm=TRUE)>0,1,0),
            ic.has_other=ifelse(sum(con_type== "Other"  ,na.rm=TRUE)>0,1,0),
            ic.con_lid_HH= ifelse(sum(con_lid=="yes", na.rm=TRUE)>0,1,0),
            ic.cont_cracked_HH= ifelse(sum(cont_cracked=="yes", na.rm=TRUE)>0,1,0),
            ic.drink_water_and_both_vol=sum(drinking_water_and_both,na.rm = TRUE),
            ic.drink_Wat_only_vol=sum(drink_wat_only,na.rm=TRUE),
            ic.not_drink_wat_vol= sum(not_drink_only, na.rm=TRUE),
            ic.wat_both_only=sum(both_only,na.rm=TRUE),
            ic.wat_drink_plus_both_none_drnk= sum(both_only, not_drink_only, drink_wat_only, na.rm = TRUE)
            
            ) 

czHH$ic.wat_drink_plus_both_none_drnk
czHH %>% View()
#HH DATA
########################################

#CHECK THAT THE NUMBER 
# REVA CATEGORIES
# DEPENDENCY RATIOS
# ECONOMIC STATUS
# GEOGRAPHIC LOCATION

#CONVERT TO DATE OBJECTS
hz$arriv_bgd.dte<-as.Date(hz$arriv_bgd)
hz$arriv_shelter.dte<-as.Date(hz$arriv_shelter)

#ONLY LOOK AT CONSENT= YES
hz<-hz %>% filter(consent=="yes")


#BREAK CAMPS INTO THREE CATEGORIES (KUTAPALONG, SATELLITE/ISOLATED CAMPS, TEKNAF)
kbc<-c("camp 13", "camp 14", "camp 10",  "camp 6", "camp 18", 
       "camp 20", "camp 1e",  "camp 17", "camp 9", 
       "camp 8w", "camp 1w", "camp 15", "camp 5" , "camp 3", 
       "camp 16", "camp 2w", "camp 20 extension", "camp 11", "camp 4", 
       "camp 19",  "camp 7", "camp 4 extension", 
       "camp 8e", "camp 2e", "camp 12")

iso<-c("camp 21", "camp 22","camp 23")
st<-c("camp 24","camp 25","camp 26","camp 27", "nayapara rc")


hz$upa_strat<-ifelse(hz$camp_id %in% kbc, "Kutapalong",
                     ifelse(hz$camp_id %in% iso, "Isolated camp_ids",
                            ifelse(hz$camp_id%in% st, "Southern Teknaf", "MISSED SOMETHING")))



#SOME RECODING OF EDUCATION VALUES
edu1<-c("no_education", "no_answer","dnt_know","other")
edu2<-c("kindergartern" ,"elementary_1","elementary_2", "elementary_3", "elementary_4")
edu3<-c("middleschool_5","middleschool_6", "middleschool_7", "middleschool_8")
edu4<-c( "highschool_9",  "highschool_10" )
edu5<-"tertiary_education"

#DID A LITTLE FANCY EXTRACT NUMERIC ADAPTATION TO MAKE EXISTING CHOICES FALL IN TEH CORRECT ORDER
hz$hi_edu_gradeZ<-ifelse(hz$hi_edu_grade %in% edu1,0,ifelse(hz$hi_edu_grade=="kindergartern",1,
                                                            ifelse(hz$hi_edu_grade %in% c(edu2,edu3, edu4), as.character(extract_numeric(hz$hi_edu_grade)+1),
                                                                   ifelse(hz$hi_edu_grade %in% edu5,12,"YOU MISSED SOMETHING"
                                                                   ))))

hz$hi_edu_gradeZ<-as.numeric(hz$hi_edu_gradeZ)






#NAS DONT MEAN NA
######################################################

treat_methods<-c("trt_methods.stone", "trt_methods.boiling", "trt_methods.pur_sach", 
  "trt_methods.liquid_chlorine", "trt_methods.clth_fil", "trt_methods.hh_fil", 
  "trt_methods.other", "trt_methods.aquatabs")
water_cook_secondary<- c("wat_cook.tube", "wat_cook.unpsprng", "wat_cook.psprng", "wat_cook.rcoll", 
"wat_cook.other", "wat_cook.unpdw", "wat_cook.pip", "wat_cook.wat_tnk", 
"wat_cook.pwell", "wat_cook.tnktr")

#######################
#make NA didnt clean



#LEAVE NAS BE
###################
reasons_no_aqua<- c("aqua_tab_trt.dnt_know", "aqua_tab_trt.dntknw_use_aquatab", 
                "aqua_tab_trt.no_aquatab_spply", "aqua_tab_trt.taste_bad", "aqua_tab_trt.watr_chlorinated", 
                "aqua_tab_trt.other", "aqua_tab_trt.dntknw_aquatab", "aqua_tab_trt.smell_bad", 
                "aqua_tab_trt.nev_recv_aquatab","cont_clean_often","cont_clean")



# COULD RUN THESE TOGETHER IN TEH CUSTOM MEAN_PROP_SURVEY FUNCTION
easy_wat<-c("time_wat","trt_wat", "sec_sourc_wat",
            "time_coll_wat","wat_store","drnk_wat_7days","sat_wat", "drink_wat","cont_clean")
# JUST LOOKING AT SOME VARIABLES
hz$time_wat# break down by option
hz$trt_wat # break down prop
hzsec_sourc_wat #breakdown prop
hz$time_coll_wat# break down by option
hz$wat_store
hz$drnk_wat_7days
hz$sat_wat
hz$drink_wat


hz %>% select(starts_with("soap_yest_when.")) %>% colnames() %>% dput()
hz %>% select(starts_with("wash_hands.")) %>% colnames() %>% dput()
###########################################
#REPLACE NA WITH 0
hz %>% select(starts_with("challengeS.")) %>% colnames() %>% dput()
hz %>% select(starts_with("train_type.")) %>% colnames() %>% dput()
hz %>% select(starts_with("more_train.")) %>% colnames() %>% dput()
hz %>% select(starts_with("more_train.")) %>% colnames() %>% dput()
hz %>% select(starts_with("consult_opinon.")) %>% colnames() %>% dput()

safe_water_sources<-c("pip", "tube", "wat_tnk", "pwell",  "tnktr", 
                      "psprng",  "bwat")
#MAKE SOME HH LEVEL COMPOSITE INDICATORS
hz2<-hz %>% 
  mutate(
    IS.REVA_arrival=ifelse(arriv_bgd.dte<as.Date("2016-01-01"),"< 2016", 
                           ifelse(arriv_bgd.dte>= as.Date("2016-01-01")& arriv_bgd.dte<as.Date("2017-08-01"),"2016 - Aug 2017",
                                  ifelse(arriv_bgd.dte>= as.Date("2017-08-01"),"post Aug 2018",0))),
    IS.REVA_arrival_shelt= ifelse(arriv_shelter.dte<as.Date("2016-01-01"),"< 2016", 
                                  ifelse(arriv_shelter.dte>= as.Date("2016-01-01")& arriv_shelter.dte<as.Date("2017-08-01"),"2016 - Aug 2017",
                                         ifelse(arriv_shelter.dte>= as.Date("2017-08-01"),"post Aug 2018",0))),
    
    Is.higest_edu= ifelse(hi_edu_gradeZ==0,"No Education",
                          ifelse(hi_edu_gradeZ>0& hi_edu_gradeZ<4, "Some Primary",
                                 "Completed Primary or More")),
    twpc= extract_numeric(time_wat)+extract_numeric(time_coll_wat),
    I.total_wat_time= ifelse(twpc <=10,"Less or Equal to 10",
                             ifelse(twpc<=20, "11 to 20",
                                    ifelse(twpc<=30, "21-30",
                                           ifelse(twpc>30, "More than 30", "You Missed Something")))),
    I.wat_30min= ifelse(twpc<=30, "Less Than 30", "More than 30"),
    I.cont_clean_how=ifelse(is.na(cont_clean), "Don't Clean", cont_clean_how),
    I.safe_child_fae=ifelse(rowSums(.[c("child_fae.coll_rinsed_disposed_lat", "child_fae.chldrn_san_fac")])>0,1,0),
    I.consulted_account_latrine= ifelse(is.na(consult_opinion), 0,
                                         ifelse(consult_three_months.yes_latrines==1&consult_opinion=="yes",1,0)),
    I.consulted_account_bathing= ifelse(is.na(consult_opinion), 0,
                                        ifelse(consult_three_months.yes_bathing==1&consult_opinion=="yes",1,0)),
    I.consulted_account_other= ifelse(is.na(consult_opinion), 0,
                                        ifelse(consult_three_months.yes_other==1&consult_opinion=="yes",1,0)),
    I.safe_water_sources= ifelse(drink_wat %in% safe_water_sources,1,0)
  )




disab_cols<-c("indi_disab_see", "indi_disab_hear", "indi_disab_climb", "indi_disab_remem", 
              "indi_disab_wash", "indi_disab_comm")

disab_recoded<-sapply(iz2[,disab_cols], function(x) ifelse(x %in% c("yes_alot", "cannot_do"),1,0)) %>% data.frame()


colnames(disab_recoded)<-paste0(colnames(disab_recoded),".recoded")


disab_recoded_cols<-iz3 %>% select(ends_with(".recoded")) %>% colnames() %>% dput()
disab_recoded_cols<-c("indi_disab_see.recoded", "indi_disab_hear.recoded", "indi_disab_climb.recoded", 
                      "indi_disab_remem.recoded", "indi_disab_wash.recoded", "indi_disab_comm.recoded")
iz3<-data.frame(iz2,disab_recoded)

hz2$drink_wat %>% unique() %>% dput()

#DO SOME COMPOSITE INDICATORS AT THE INDIVIDUAL LEVEL

iz4<-iz3 %>% 
  mutate(
    IS.disabA = ifelse(rowSums(.[disab_recoded_cols])>0,1,0),
    IS.dsabA_collWater= ifelse(IS.disabA==1 & indi_collect_wat %in% c("sometimes", "always","often"),1,0),
    I.disab_tr=ifelse(is.na(indi_disab_trt),NA,ifelse(IS.disabA==1& indi_disab_trt=="yes",1,0)),
    I.prob_coll_water_INDI= ifelse(indi_prob_coll=="yes",1,0),
    nomales_18_59_collwater= ifelse(indi_gen=="male" & 
                                      indi_collect_wat %in% c("sometimes", "always","often")&
                                      indi_age%in% seq(18,59,1),1,0),
    
    noadults_18_59_collwater= ifelse(indi_collect_wat %in% c("sometimes", "always","often")&
                                       indi_age%in% seq(18,59,1),1,0),
    IS.mhohA= ifelse(indi_hoh=="yes", marital_status,NA),
    Is.gender_hohA=ifelse(indi_hoh=="yes", indi_gen,NA),
    IS.men_wkA=ifelse(indi_work_inc=="yes"& indi_gen=="male",1,0),
    IS.women_wkA=ifelse(indi_work_inc=="yes"& indi_gen=="female",1,0),
    IS.total_wkA=ifelse(indi_work_inc=="yes",1,0),
    IS.work_age= ifelse(indi_age>=15 & indi_age<=64,1,0),
    IS.work_notage=ifelse(indi_age<15 & indi_age>64,1,0),
    #NEW AGE GROUPS
    O.age_group1= ifelse(indi_age %in% seq(0,4,1), "0-4", 
                        ifelse(indi_age %in% seq(5,17,1), "5-17",
                               ifelse(indi_age %in% seq(18,59,1),"18-59",
                                             ifelse(indi_age >= 60, "> 60", "YOU MISSED SOMETHING")))),
    
    O.age_group2= ifelse(indi_age %in% seq(0,4,1), "0-4", 
                        ifelse(indi_age %in% seq(5,11,1), "5-11",
                               ifelse(indi_age %in% seq(12,17,1),"12-17",
                                      ifelse(indi_age %in% seq(18, 59,1),"18-59",
                                             ifelse(indi_age >= 60, "> 60", "YOU MISSED SOMETHING"))))),
    child_vs_adult= ifelse( indi_age %in% seq (0,17,1), "Child",
                            ifelse(indi_age %in% seq(18,59,1),"Adult", "Elderly")),
    under5_indiv= ifelse(indi_age<5,1,0),
    I.coll_water_always_often= ifelse(indi_collect_wat %in% c("always","often"),1,0), # break down by age (both group) and gender
  ) 


#FOR THESE SELECT MULTIPLE WE WANT % BREAK DOWN  BY GENDER &  O.age_group2 O.age_group1 AT THE INDIVIDUAL LEVEL
collection_problems<-iz4 %>% select(starts_with("indi_wat_prb.")) %>% colnames() %>% dput()

nh$drnk_wat_7days
iz4$indi_wat_prb.source_dangerous
iz4$I.disab_tr
iz4$Is.gender_hohA
iz4$I.prob_coll_water_INDI
hs$variables$drnk_wat_7days

######################INDIVIDUAL LEVEL##########################
###### NA MEANS NOT APPLICABLE
#BREAK DOWN BY 2 AGE GROUPS AND GENDER
#JUST LOOKING AT AND GROUPING SOME INDICATORS
"indi_def_where" # indi_def_where by both age groups & Gender

"indi_def_prob"# NA IS NA --- broke down by both age groups and gender

iz4 %>%  select(starts_with("indi_def_typ.")) %>% colnames()
c("indi_def_typ.lat_insuffient_wat", "indi_def_typ.lat_opn_defecation_lat", 
  "indi_def_typ.lat_not_clean", "indi_def_typ.lat_prb_other", "indi_def_typ.lat_bad_smell", 
  "indi_def_typ.lat_no_lighting", "indi_def_typ.lat_full", "indi_def_typ.lat_many_people", 
  "indi_def_typ.lat_path_latrine_unsafe", "indi_def_typ.lat_far", 
  "indi_def_typ.lat_no_separation", "indi_def_typ.lat_not_private", 
  "indi_def_typ.lat_fac_unsafe")

"indi_def_unsafe"
"indi_diff_toilet"
"indi_dia_per"
"indi_bathe_normally"
iz4 %>% select(starts_with("indi_dia_measures.")) %>% colnames() %>% dput()
"indi_bath_prob"

iz4 %>% select(starts_with("indi_bath_prob_ty.")) %>% colnames() %>% dput()
"indi_bath_unsafe"



cols_for_big_brk_down<-c("indi_def_where","indi_def_prob",
                         iz4 %>% select(starts_with("indi_dia_measures.")) %>% colnames(),
                         iz4 %>%  select(starts_with("indi_def_typ.")) %>% colnames(),
                         "indi_def_unsafe",
                         "indi_diff_toilet",
                         "indi_dia_per",
                         "indi_bathe_normally",
                         iz4 %>% select(starts_with("indi_bath_prob_ty.")) %>% colnames() 
                         )
                         

######### END INDIVIDUAL SECTIOn WHERE NA MEANS NOT APPLICABLE


#AGGREGATE INDIVIDUAL TO HH LEVELS

iz_to_HH<-iz4 %>% group_by(parent_instance_name) %>% 
  summarise(IS.mhoh=unique_nona(IS.mhohA),
            IS.gender_hoh=unique_nona(Is.gender_hohA),
            Is.men_wk=sum(IS.men_wkA,na.rm=TRUE),
            IS.women_wk=sum(IS.women_wkA,na.rm=TRUE),
            IS.total_wk= sum(IS.total_wkA, na.rm=TRUE),
            IS.dependency_ratio= sum(IS.work_notage,na.rm=TRUE)/sum(IS.work_age, na.rm=TRUE),
            IS.disab= ifelse(sum(IS.disabA,na.rm=TRUE)>0,1,0),
            IS.disab_coll_water= ifelse(sum(IS.dsabA_collWater,na.rm=TRUE)>0,1,0),
            IS.no_male_18to59_coll_water= ifelse(sum(nomales_18_59_collwater,na.rm=TRUE)>0,0,1),
            IS.no_adult_18to59_coll_water= ifelse(sum( noadults_18_59_collwater,na.rm=TRUE)>0,0,1),
            I.under5= ifelse( sum(under5_indiv,na.rm=TRUE)>0,1,0),
            I.disab_trt_HH= ifelse(sum(I.disab_tr,na.rm=TRUE)>0,1,0),
            I.prob_coll_water_HH= ifelse( sum( I.prob_coll_water_INDI, na.rm = TRUE)>0,1,0),
            I.indi_wat_prb.wat_not_clean_HH=ifelse(sum(indi_wat_prb.wat_not_clean,na.rm=TRUE)>0,1,0), 
            I.indi_wat_prb.long_wait_HH=ifelse(sum(indi_wat_prb.long_wait,na.rm=TRUE)>0,1,0), 
            I.indi_wat_prb.wat_taste_bad_HH=ifelse(sum(indi_wat_prb.wat_taste_bad,na.rm=TRUE)>0,1,0), 
            I.indi_wat_prb.pump_difficult_HH=ifelse(sum(indi_wat_prb.pump_difficult,na.rm=TRUE)>0,1,0), 
            I.indi_wat_prb.wat_smell_bad_HH=ifelse(sum(indi_wat_prb.wat_smell_bad,na.rm=TRUE)>0,1,0), 
            I.indi_wat_prb.other_HH=ifelse(sum(indi_wat_prb.other,na.rm=TRUE)>0,1,0), 
            I.indi_wat_prb.source_not_avail_HH=ifelse(sum(indi_wat_prb.source_not_avail,na.rm=TRUE)>0,1,0), 
            I.indi_wat_prb.source_far_HH=ifelse(sum(indi_wat_prb.source_far,na.rm=TRUE)>0,1,0), 
            I.indi_wat_prb.source_dangerous_HH=ifelse(sum(indi_wat_prb.source_dangerous,na.rm=TRUE)>0,1,0), 
            I.indi_wat_prb.path_steep_HH=ifelse(sum(indi_wat_prb.path_steep,na.rm=TRUE)>0,1,0), 
            I.indi_wat_prb.available_sometimes_HH=ifelse(sum(indi_wat_prb.available_sometimes,na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_comm_pub_lat=ifelse(sum(indi_def_where=="def_comm_pub_lat",na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_opn_defecation=ifelse(sum(indi_def_where=="def_opn_defecation",na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_def_cloth=ifelse(sum(indi_def_where=="def_cloth",na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_def_potty=ifelse(sum(indi_def_where=="def_potty",na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_def_single_hh_lat_sm=ifelse(sum(indi_def_where=="def_single_hh_lat_sm",na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_shared_hh_lat_sm=ifelse(sum(indi_def_where=="def_shared_hh_lat_sm",na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_shared_hh_lat_nonsm=ifelse(sum(indi_def_where=="def_shared_hh_lat_nonsm",na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_shared_hh_bucket_toilet=ifelse(sum(indi_def_where=="def_bucket_toilet",na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_shared_hh_plastic_bag=ifelse(sum(indi_def_where=="def_plastic_bag",na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_shared_single_hh_lat_nonsm=ifelse(sum(indi_def_where=="def_single_hh_lat_nonsm",na.rm=TRUE)>0,1,0),
            I.indi_def_where.def_def_other=ifelse(sum(indi_def_where=="def_other",na.rm=TRUE)>0,1,0),
            I.indi_def_where.SHARED_SINGLE_LATSM=ifelse(sum(indi_def_where %in% c("def_single_hh_lat_sm","def_shared_hh_lat_sm"),na.rm=TRUE)>0,1,0),
            I.indi_def_where.SHARED_SINGLE_LAT_NONSM=ifelse(sum(indi_def_where %in% c("def_single_hh_lat_sm","def_single_hh_lat_nonsm"),na.rm=TRUE)>0,1,0),
            I.indi_def_prob_yes_female= ifelse(sum(indi_def_prob=="yes" & indi_gen=="female", na.rm=TRUE)>0,1,0),
            I.indi_def_prob_yes_male= ifelse(sum(indi_def_prob=="yes" & indi_gen=="male", na.rm=TRUE)>0,1,0),
            I.indi_def_unsafe_female_INDIVHH= ifelse(sum(indi_def_unsafe=="yes"& indi_gen=="female", na.rm=TRUE)>0,1,0),
            I.indi_def_unsafe_male_INDIVHH= ifelse(sum(indi_def_unsafe=="yes"& indi_gen=="male", na.rm=TRUE)>0,1,0),
            I.indi_diff_toilet_INDIVHH= ifelse( sum(indi_diff_toilet=="yes", na.rm = TRUE)>0,1,0),
            I.indi_dia_per_INDIVHH= ifelse( sum(indi_dia_per=="yes", na.rm = TRUE)>0,1,0),
            I.indi_bath_prob_female_INDIVHH= ifelse(sum(indi_bath_prob=="yes"& indi_gen=="female", na.rm=TRUE)>0,1,0),
            I.indi_bath_prob_male_INDIVHH= ifelse(sum(indi_bath_prob=="yes"& indi_gen=="male", na.rm=TRUE)>0,1,0),
            I.indi_bath_unsafe_female_INDIVHH= ifelse(sum(indi_bath_unsafe=="yes"& indi_gen=="female", na.rm=TRUE)>0,1,0),
            I.indi_bath_unsafe_male_INDIVHH= ifelse(sum(indi_bath_unsafe=="yes"& indi_gen=="male", na.rm=TRUE)>0,1,0)
          
            ) 


#SOME ADDITIONAL INDIVIDUAL TO HH AGGREGATIONS-- FIGURD OUT A FASTER WAY TO DO IT

def_cols<-iz4 %>% select(starts_with("indi_def_typ.")) %>% colnames()
I.def_type<-iz4 %>% select(parent_instance_name,indi_gen,X_index, starts_with("indi_def_typ.")) %>% 
  mutate_at(def_cols, function(x) ifelse((x==1 & .$indi_gen=="female"),1,0)) %>% 
  rename_at(def_cols, funs(paste0("female.",.,"_INDHH"))) %>% 
  right_join(iz4 %>% select(parent_instance_name,X_index,def_cols), by= "X_index") %>%
    arrange(indi_gen,desc(indi_def_typ.lat_fac_unsafe)) %>% 
  mutate_at(def_cols, function(x) ifelse(x==1 & .$indi_gen=="male",1,0)) %>%
  rename_at(def_cols, funs(paste0("male.",.,"_INDHH"))) %>% 
  right_join(iz4 %>% select(parent_instance_name,X_index,indi_gen, def_cols), by= "X_index") %>% 
  rename_at(def_cols, funs(paste0(.,"_INDHH"))) %>%
  group_by(parent_instance_name) %>% 
  summarise_at(c(paste0("female.",def_cols,"_INDHH"),paste0("male.",def_cols,"_INDHH"),paste0(def_cols,"_INDHH")), function(x)ifelse(sum(x,na.rm=TRUE)>0,1,0))

select(iz4, starts_with("indi_dia_measures.")) %>% colnames() %>% dput()
deftype_dia_measures_INDIVHH<-iz4 %>% select(parent_instance_name, starts_with("indi_def_typ."), starts_with("indi_dia_measures.")) %>% 
  group_by(parent_instance_name) %>% 
  summarise_all(function(x) ifelse(sum(x,na.rm=TRUE)>0,1,0)) 
colnames(deftype_dia_measures_INDIVHH)[2:ncol(deftype_dia_measures_INDIVHH)]<-paste0("I.",colnames(deftype_dia_measures_INDIVHH),"_INDHH")[2:ncol(deftype_dia_measures_INDIVHH)]



bath_prob_cols<-iz4 %>% select(starts_with("indi_bath_prob_ty.")) %>% colnames

I.bath_prob_type<-iz4 %>% select(parent_instance_name,indi_gen,X_index, bath_prob_cols) %>% 
  mutate_at(bath_prob_cols, function(x) ifelse((x==1 & .$indi_gen=="female"),1,0)) %>% 
  rename_at(bath_prob_cols, funs(paste0("female.",.,"_INDHH"))) %>% 
  right_join(iz4 %>% select(parent_instance_name,X_index,bath_prob_cols), by= "X_index") %>%
  mutate_at(bath_prob_cols, function(x) ifelse(x==1 & .$indi_gen=="male",1,0)) %>%
  rename_at(bath_prob_cols, funs(paste0("male.",.,"_INDHH"))) %>% 
  right_join(iz4 %>% select(parent_instance_name,X_index,indi_gen, bath_prob_cols), by= "X_index") %>% 
  rename_at(bath_prob_cols, funs(paste0(.,"_INDHH"))) %>%
  group_by(parent_instance_name) %>% 
  summarise_at(c(paste0("female.",bath_prob_cols,"_INDHH"),paste0("male.",bath_prob_cols,"_INDHH"),paste0(bath_prob_cols,"_INDHH")), function(x)ifelse(sum(x,na.rm=TRUE)>0,1,0))


#MERGE DATA SETS TOGETHER- MAKE A LIST OF SEVERAL 
dl<-list(iz_to_HH,I.def_type, deftype_dia_measures_INDIVHH,I.bath_prob_type)


#QUESTION FOR MARTIN
#WHY DOES THIS FUNCTION NOT WORK/REQUIRE SO MUCH MEMORY WHEN I INCLUDE THE FULLL HH DATA
start_time<-Sys.time()
new_indis<-Reduce(function(x, y) merge(x, y, all=TRUE), dl)
end_time<-Sys.time()
end_time-start_time
new_indis
#COMBNIE WITH HH DATA
nh<-left_join(hz2, new_indis, by= c("instance_name"="parent_instance_name"))
nh<-left_join(nh,czHH,by= c("X_index"="X_parent_index"))
# czHH$ic.wat_drink_plus_both_none_drnk
nh<-nh %>% 
  mutate(
    ic.jrp.not_drink_wat_lpppd=ic.not_drink_wat_vol/num_mem,
    ic.jrp.drink_Wat_only_lpppd=ic.drink_Wat_only_vol/num_mem,
    ic.jrp.drink_water_and_both_lpppd=ic.drink_water_and_both_vol/num_mem,
    ic.jrp.both_water_only_lpppd=ic.wat_both_only/num_mem,
    ic.jrp.drnk_nondrnk_both_lpppd=ic.wat_drink_plus_both_none_drnk/num_mem,
    ic.jrp.not_drink_wat_gr_threshold_3L=(ic.jrp.not_drink_wat_lpppd>=3) %>% as.numeric(),
    ic.jrp.drink_wat_only_gr_threshold_3L=(ic.jrp.drink_Wat_only_lpppd>=3) %>%  as.numeric(),
    ic.jrp.jrp.drnk_wat_both_gr_threshold_3L=(ic.jrp.drink_water_and_both_lpppd>=3) %>% as.numeric(),
    ic.jrp.not_drink_wat_gr_threshold_15L=(ic.jrp.drink_water_and_both_lpppd>=15 )%>% as.numeric(),
    ic.jrp.drink_wat_only_gr_threshold_15L=(ic.jrp.drink_Wat_only_lpppd>=15) %>% as.numeric(),
    ic.jrp.drnk_wat_both_gr_threshold_15L=(ic.jrp.drink_water_and_both_lpppd>=15) %>% as.numeric(),
    ic.jrp.not_drink_wat_gr_threshold_20L=as.numeric(ic.jrp.drink_water_and_both_lpppd>=20) %>% as.numeric(),
    ic.jrp.drink_wat_only_gr_threshold_20L=(ic.jrp.drink_Wat_only_lpppd>=20) %>% as.numeric(),
    ic.jrp.drnk_wat_both_gr_threshold_20L=(ic.jrp.drink_water_and_both_lpppd>=20) %>% as.numeric(),
    ic.jrp.wat_bothonly_gr_threshold_20L=(ic.jrp.both_water_only_lpppd>=20) %>% as.numeric(),
    ic.jrp.wat_bothonly_gr_threshold_15L=(ic.jrp.both_water_only_lpppd>=15) %>% as.numeric(),
    ic.jrp.wat_bothonly_gr_threshold_3L=(ic.jrp.both_water_only_lpppd>=3) %>% as.numeric(),
    ic.jrp.drnk_nondrnk_both_gr_threshold_20L=(ic.jrp.drnk_nondrnk_both_lpppd>=20) %>% as.numeric(),
    ic.jrp.drnk_nondrnk_both_gr_threshold_15L=(ic.jrp.drnk_nondrnk_both_lpppd>=15) %>% as.numeric(),
    ic.jrp.drnk_nondrnk_both_gr_threshold_3L=(ic.jrp.drnk_nondrnk_both_lpppd>=3) %>% as.numeric()
  )  



###########END COMPOSITE INDICATOR SECTIONS
#SETUP DESIGN OBJECTS AND WEIGHTS
#######################################################
#ANALYSIS
getwd()

pop_dir <- "..//data analysis//WASH_unhcr_population_and_assessment_sample_size_data.csv"
pop <-
  read.csv(
    pop_dir,
    stringsAsFactors = FALSE,
    na.strings = c(NA, "NA", " ", "", "<NA>")
  )
#REMOVE CAMPS NUOT ASSESSED
pop2 <- pop[!is.na(pop$hhs_assessed_correct), ]
#ADD PROBABILITIES
pop2$hhs_assessed_correct %>% sum()

pop3 <- pop2 %>%
  mutate(
    hh_prob = (total_HH / sum(total_HH)) * nrow(nh),
    indiv_prob = (total_indiv / sum(total_indiv)) * individuals_assessed
  ) %>%
  select(camp_id, hh_prob, indiv_prob) %>% data.frame()

#IMPORTANT- CONVERT CHARACTERS TO NUMERIC
nh2 <- nh %>%
  mutate_if(sapply(nh, is.character), as.factor)


svy_hh_unweighted <- svydesign(ids = ~ 1,
                               strata =  ~ camp_id,
                               data = nh2)
#JUST TAKE HH PROBS AND RAKE
pop_hh <- pop3 %>% select(camp_id, hh_prob)
svy_hh_rake <- rake(
  design = svy_hh_unweighted,
  sample.margins = list( ~ camp_id),
  population.margins = list(pop_hh)
)


hs<- svydesign(ids = ~ 1,
               strata =  ~ camp_id,
               weights = weights(svy_hh_rake),
               data = nh2)



#MAKE SURVYE OBJECT FOR INDIVIDUAL DATA
#STEAL WEIGHTS FROM HH DATA 

hh_w_weights<-data.frame(nh2, weights= weights(svy_hh_rake))

#ADD SOME RELEVANT COLUMNS FROM HH DATA TO INDIVUDAL DATA
iz_w_hh_weights<-iz4 %>% left_join(hh_w_weights %>% select(instance_name,camp_id,upa_strat,resp_gen, weights),c("parent_instance_name"="instance_name"))


iz_w_hh_weights2<- iz_w_hh_weights %>%
  mutate_if(sapply(iz_w_hh_weights, is.character), as.factor)

isvy<-svydesign(ids = ~ 1,
                strata =  ~ camp_id,
                weights= ~weights,
                data = iz_w_hh_weights2)




#########END SURVEY DESIGN OBJECT CREATION###############################


#ANALYZE
#########################################
#ANALYZE HH DATA 


# FOR FIRST QUICK AND DIRTY ANALYSIS: 

#GET THE LENGHT OF UNIQUE VALUES IN EACH COLUMN THEN SUBSET THEM SO THEY ARE GREATER THAN 1 AND < 12 ( A BIT ARBITRARY)
length_unique_cols<-sapply(nh, function(x)length(unique(x)))
cols_less_than_10_unique<-  which(length_unique_cols< 100& length_unique_cols>1)
gv<-hs$variables[,cols_less_than_10_unique] 
#NEXT SUBSET THEM SO THEY ARE LESS THAN 80 % NA
df <- gv[,colSums(is.na(gv))/nrow(gv)<0.95]

# cols_analyze1<-df %>% select_if(is.factor) %>% colnames()
# cols_analyze2<-cols_analyze1[cols_analyze1 %in% c("resp_gen","enu_gen")==FALSE]
# cols_numeric<-df %>% select_if(is.integer) %>% colnames()
# hs$variables$chld_under17
# nh$drink_wat %>% unique() %>% length()
new_cols_analyze<-df %>% colnames()
new_cols_analyze2<-new_cols_analyze[new_cols_analyze %in% c("survey_date", "enu_id")==FALSE]
new_cols_analyze2 %>% sort()
#RUN WITH NA REMOVE
all_cols_basic<-mean_prop_svy(x=hs, new_cols_analyze2,z=TRUE,bv1="resp_gen")
all_cols_basic2<-mean_prop_svy(x=hs, new_cols_analyze2,z=FALSE,bv1="resp_gen")
# write.csv(all_cols_basic[[1]], "HH_WASH_R3_BasicStats_NA_Replaced_07july2019.csv")
# write.csv(all_cols_basic2[[1]], "HH_WASH_R3_BasicStats_NA_Subset_07july2019.csv")
# 


jrp_columns<- nh2 %>% select(starts_with ("ic.jrp")) %>% colnames()

jrp_columns_Nas_left<-mean_prop_svy(x=hs, jrp_columns,z=FALSE,bv1="resp_gen")

jrp_columns_NAs_forced<-mean_prop_svy(x=hs, jrp_columns,z=TRUE,bv1="resp_gen")
# write.csv(jrp_columns_Nas_left[[1]],"JRP_Columns_Analyzed_NA_Subset_530pm_07july2019.csv")


#DO THE SAME FOR THE INDIVIDUAL DATA
################################################################
length_unique_cols<-sapply(iz_w_hh_weights2, function(x)length(unique(x)))
cols_less_than_10_unique<-  which(length_unique_cols< 100& length_unique_cols>1)
gv<-isvy$variables[,cols_less_than_10_unique] 
df <- gv[,colSums(is.na(gv))/nrow(gv)<0.95]

ind_cols_analyze<-df %>% colnames()

ind_basic_na_subset<- mean_prop_svy(x =isvy, y = ind_cols_analyze,z = FALSE, bv1= "resp_gen")
ind_basic_na_replaced<- mean_prop_svy(x =isvy, y = ind_cols_analyze,z = TRUE, bv1= "resp_gen")
# write.csv(ind_basic_na_subset[[1]], "INDIV_WASH_R3_BasicStats_NA_Subset_01july2019.csv")
# write.csv(ind_basic_na_replaced[[1]], "INDIV_WASH_R3_BasicStats_NA_Replaced_01july2019.csv")



#NOW MORE COMPLEX BREAKDOWNS FOR SPECIFIC INDICATORS REQUESTED

#AGEGROUP 1 AND GENDER-- REMOVE NAS
ag1_gNA<-mean_prop_svy(x =isvy, y = cols_for_big_brk_down,z = FALSE, bv1=c("O.age_group1","indi_gen"))
#AGEGROUP 1 AND GENDER-- REMOVE REPLACE NAS WITH 0
ag1_g0NA<-mean_prop_svy(x =isvy, y = cols_for_big_brk_down,z = TRUE, bv1=c("O.age_group1","indi_gen"))

#AGEGROUP 2 AND GENDER-- REMOVE NAS
ag2_gNA<-mean_prop_svy(x =isvy, y = cols_for_big_brk_down,z = FALSE, bv1=c("O.age_group2","indi_gen"))
#AGEGROUP 2 AND GENDER-- REMOVE REPLACE NAS WITH 0
ag2_g0NA<-mean_prop_svy(x =isvy, y = cols_for_big_brk_down,z = TRUE, bv1=c("O.age_group2","indi_gen"))

# write.csv(ag1_gNA[[5]], "IND_Analysis_AgeGrp1_Gen_NAs_Subset_29june2019.csv")
# write.csv(ag1_g0NA[[5]], "IND_Analysis_AgeGrp1_Gen_NAs_Replaced_29june2019.csv")
# write.csv(ag2_gNA[[5]], "IND_Analysis_AgeGrp2_Gen_NAs_Subset_29june2019.csv")
# write.csv(ag2_g0NA[[5]], "IND_Analysis_AgeGrp2_Gen_NAs_Replaced_29june2019.csv")


cmp_ag1_gNA<-mean_prop_svy(x =isvy, y = cols_for_big_brk_down,z = FALSE, bv1=c("camp_id", "O.age_group1","indi_gen"))
cmp_ag1_0NA<-mean_prop_svy(x =isvy, y = cols_for_big_brk_down,z = TRUE, bv1=c("camp_id", "O.age_group1","indi_gen"))
cmp_ag2_gNA<-mean_prop_svy(x =isvy, y = cols_for_big_brk_down,z = FALSE, bv1=c("camp_id", "O.age_group2","indi_gen"))
cmp_ag2_0NA<-mean_prop_svy(x =isvy, y = cols_for_big_brk_down,z = TRUE, bv1=c("camp_id", "O.age_group2","indi_gen"))

# write.csv(cmp_ag1_gNA[[5]], "IND_CmpLevel_AgeGrp1_Gen_NAs_Subset_30june2019.csv")
# write.csv(cmp_ag1_0NA[[5]], "IND_CmpLevel_AgeGrp1_Gen_NAs_Replaced_30june2019.csv")
# write.csv(cmp_ag2_gNA[[5]], "IND_CmpLevel_AgeGrp2_Gen_NAs_Subset_30june2019.csv")
# write.csv(cmp_ag2_0NA[[5]], "IND_CmpLevel_AgeGrp2_Gen_NAs_Replaced_30june2019.csv")

cmp_ag1_gNA<-mean_prop_svy(x =isvy, y = cols_for_big_brk_down,z = FALSE, bv1=c("camp_id", "O.age_group1","indi_gen"))
# write.csv(cmp_ag1_gNA[[5]], "IND_Analysis_CampLevel_Age1_Gen29june2019.csv")


















































do.call("rbind", numer1)

numeric_hh_list<-list()
hh_data_list<-list()
analysisLevels<-c("CampLevel","UpazilaLevel","ResponseLevel", "RespXGender")
setwd("C:\\01_REACH_BGD\\02_GIS_DataUnit\\02_Themes\\01_WASH\\02_70_DNW_UNICEF_2019\\03_WASH_HH_R3\\data analysis\\output_data_r\\hh_data")
for(i in 1:length(test2)){
  te<-test2[[i]]
  dat1<-test1[[i]]
  colnames(te)[3:ncol(te)]<-cols_numeric
  # numeric_hh_list<-te
  numeric_hh_list[[i]]<-te
  write.csv(te,paste0("HH_",analysisLevels[i],"numeric_logical.csv"))
  write.csv(dat1,paste0("HH_",analysisLevels[i],"Categorical.csv"))
}


test1<-mean_prop_svy(x=hs, cols_analyze2,z=TRUE)
test2<-mean_prop_svy(x=hs, cols_numeric,z=TRUE)

test1[[3]]
numeric_hh_list<-list()
hh_data_list<-list()
analysisLevels<-c("CampLevel","UpazilaLevel","ResponseLevel", "RespXGender")
setwd("C:\\01_REACH_BGD\\02_GIS_DataUnit\\02_Themes\\01_WASH\\02_70_DNW_UNICEF_2019\\03_WASH_HH_R3\\data analysis\\output_data_r\\hh_data")

for(i in 1:length(test2)){
  te<-test2[[i]]
  dat1<-test1[[i]]
  colnames(te)[3:ncol(te)]<-cols_numeric
  # numeric_hh_list<-te
  numeric_hh_list[[i]]<-te
  # write.csv(te,paste0("HH_",analysisLevels[i],"numeric_logical.csv"))
  # write.csv(dat1,paste0("HH_",analysisLevels[i],"Categorical.csv"))
}


"time_coll_wat"

factor(hs$variables$drnk_wat_7days) %>% class()
hs$variables$drnk_wat_7days<-ifelse(is.character(hs$variables$drnk_wat_7days)==TRUE,factor(hs$variables$drnk_wat_7days),hs$variables$drnk_wat_7days)

class(asdf)
hs$variables$enu_gen %>% is.logical()
hs$variables$trt_methods.boiling

hs$variables$time_wat<-factor(hs$variables$time_wat)
svyby(~trt_methods.boiling, ~resp_gen, design = hs,FUN = svymean,keep.var = FALSE) 

hs$variables$time_wat 
svymean(~time_coll_wat, hs,na.rm.by = FALSE, keep.var=FALSE ) %>% data.frame() %>% t()
hs$variables$trt_methods.stone
svymean(~trt_methods.stone, hs,na.rm=TRUE, keep.var=FALSE ) %>% data.frame() 


cols_analyze2[3]
hs$variables$trt_wat <-factor(hs$variables$trt_wat )
hs$variables$wat_store <-factor(hs$variables$wat_store)
asdf<-svyby(~wat_store, ~upa_strat, design = hs,FUN = svymean) %>% data.frame()

asdf<-svyby(~indi_resp_defecation_place_count, ~camp_id, design = hs,FUN = svymean,keep.var=FALSE) %>% data.frame()
names(asdf)

colnames(test1[[1]])
asdf2<-svyby(~trt_wat, ~upa_strat, design = hs,FUN = svymean) %>% data.frame()
asdf<-svyby(~chld_under17, ~upa_strat, design = hs,FUN = svymean, keep.var=FALSE) 
asdf
hs$variables$chld_under17

names(asdf) 
?svyby
asdfl<-list(asdf,asdf2)
unlist(asdfl)
asdf2

asdf2<-asdf %>% data.frame()
colnames(asdf2)
asdf2 %>% select(-camp_id)
svyby(~wat_store, ~upa_strat, design = hs,FUN = svymean)
svyby(~wat_store, ~resp_gen, design = hs,FUN = svymean)













nh$camp_id %>% unique() %>% length()





















iz3$indi_def_where %>% unique() %>% dput()
hz$drink_wat
iz4$indi_def_where
INDIV_def_where<-c("def_comm_pub_lat", "def_opn_defecation", "def_cloth", "def_potty", 
    "def_single_hh_lat_sm", "def_shared_hh_lat_sm", "def_shared_hh_lat_nonsm", 
    "def_bucket_toilet", "def_plastic_bag", "def_single_hh_lat_nonsm", 
    "def_other")
asdf<-function(x,y){
  ifelse(sum(x==y,na.rm=TRUE)>0,1,0)
}
iz4 %>% 
  group_by(parent_instance_name) %>%
  summarise_at(
    INDIV_def_where,
    asdf)
  
  

iz_to_HH %>% colnames()

nrow(iz4)
nrow(hz2)
nrow(iz_to_HH)
#MAKE ANOTHER HH LEVEL INDICATOR BEFORE JOING INDIVIDUAL TO HH AGG TO HH DATA
coping_strat_cols<- c("cop_strat.untrtd_ck_wsh_wat", "cop_strat.untrtd_drnk_wat", 
                      "cop_strat.drink_less", "cop_strat.sourc_frthr","cop_strat.chld_ftch", "cop_strat.spend_money")

hy<-hy %>% 
  mutate(
    i.cop_strat=ifelse(rowSums(.[coping_strat_cols],na.rm = TRUE)>0,1,0)
  )
#hh individual zack
hiz<-left_join(hz, iz_to_HH, by=c("instance_name"="parent_instance_name"))
#GET A FEW PREMADE INDICATOR YANNICKS OUT OF THE 
hiz2<-left_join(hiz,
                hy %>% 
                  select(instance_name ,prob_coll_HH.yes, drink_wat_quality.unimproved,
                         time_coll_wat.more_30,
                         X15_litres_perday.no., X3_litres_perday.no., i.cop_strat))
hiz2<-left_join(hiz, hy, by="instance_name")






logi_vars<-hs$variables %>% select_if(is.logical) %>% colnames()
mean_prop_svy(hs, logi_vars,y=FALSE)

# BATH TIM WAS DONE IN BIG 4 LOOP ABOVE HOWEVER IT WAS SUBSET.
new_hh_indicators<-c("sec_sourc_wat","wat_cook.dnt_know", "wat_cook.surfwat", "wat_cook.crt", "wat_cook.bwat", 
                     "wat_cook.tube", "wat_cook.unpsprng", "wat_cook.psprng", "wat_cook.rcoll", 
                     "wat_cook.other", "wat_cook.unpdw", "wat_cook.pip", "wat_cook.wat_tnk", 
                     "wat_cook.pwell", "wat_cook.tnktr","cop_strat.dnt_know", "cop_strat.untrtd_ck_wsh_wat", "cop_strat.untrtd_drnk_wat", 
                     "cop_strat.drink_less", "cop_strat.sourc_frthr", "cop_strat.other", 
                     "cop_strat.chld_ftch", "cop_strat.spend_money", "wash_hands.before_prayer", "wash_hands.feels_dirty", "wash_hands.bfr_eat", 
                     "wash_hands.dnt_know", "wash_hands.hands_dirty", "wash_hands.bfr_prep_food", 
                     "wash_hands.cleaning_chld_bottom", "wash_hands.other", "wash_hands.bfr_feed_child", 
                     "wash_hands.aftr_defecation", "wash_hands.bfr_brst_feed")



#RELATIONSHIP TESTS PART A
A_indics<-c(
  "prob_coll_HH.yes",
  "drink_wat_quality.unimproved",
  "time_coll_wat.more_30",
  "X15_litres_perday.no.",
  "X3_litres_perday.no.","i.cop_strat"
)
hs$variables[,A_indics]

iscg_cols<-hiz2 %>% select(starts_with("IS.")) %>% colnames() %>% dput()
c("IS.mhoh", "IS.gender_hoh", "Is.men_wk", "IS.women_wk", "IS.total_wk", 
  "IS.dependency_ratio", "IS.disab", "IS.disab_coll_water", "IS.no_male_18to59_coll_water", 
  "IS.no_adult_18to59_coll_water")
p2<-list()
for ( i in 1: length(iscg_cols)){
  print(i)
  d<-svyby(formula = ~prob_coll_HH.yes,by = formula(paste0("~", iscg_cols[i])), design = hs, FUN = svymean,na.rm.all = TRUE, vartype = "ci")
  d<-d[,c(3,5,7)]
  d$answer<-paste0(iscg_cols[i],"_", rownames(d))
  colnames(d)<- c("mean", "ci_l","ci_high", "answer")
  p2[[i]]<-d
}
p2 
length(iscg_cols)
length(A_indics)
p2 <- list()
Y<- matrix(NA, nrow=100, ncol=100)

list1<- list()

for (i in 1:length(A_indics)) {
  indic <- A_indics[i]
  # print(paste0(i," ",indic))
  indic_f <- paste0("~", indic)
  for (j in 1:length(iscg_cols)) {
    iscg_indic <- iscg_cols[j]
    print(i*j)
    # iscg_f<-paste0("~", iscg_indic)
    print(paste0(i," ",iscg_indic))
    d<-svyby(formula = formula(indic_f),by = formula(iscg_f), design = hs, FUN = svymean,na.rm.all = TRUE, vartype = "ci")
    # list[[i]]<- d
    # list[[j]]<-d
    # d2<-d[,1:2]
    # print(i*j)

  }
  p2[[i]]<-d
}



p2
d3
p2
length(p2)

    iscg_f<paste0("~", iscg_indic)
  d<-svyby(formula = formula(indic_f),by = formula(iscg_f), design = hs, FUN = svymean,na.rm.all = TRUE, vartype = "ci")
  d<-d[,c(3,5,7)]
  d$answer<-paste0(iscg_cols[i],"_", rownames(d))
  colnames(d)<- c("mean", "ci_l","ci_high", "answer")
  p2[[i]]<-d
  }}
asdfasdf

p2 









sapply(hs$variables[,logi_vars], function(x) which(!is.na(unique(x)))) 
# logi_vars2<-logi_vars[7:length(logi_vars)]
logi_vars %>% length()
logidf<-hs$variables[,logi_vars]
logi_vars2<-logidf[,colSums(is.na(logidf))!=nrow(logidf)] %>% colnames
hs$variables$resp_ge
logi_vars2
bam<-mean_prop_svy(x=hs, y=logi_vars2, z=FALSE)
write.csv(bam[[1]],"logical_indicators_AllStrata_23june2019.csv")
hs$variables[, logi_vars2[537]]

mean_prop_svy















































########################################
#FANCY STUFF


library(randomForest)
set.seed(4543)
# Random Forest relative importance of variables as predictorsd
dat<-hs$variables
dat$prob_coll_HH.yes
dat$child_under_5_count
dat[,c("prob_coll_HH.yes","IS.disab" , "trt_methods","child_under_5_count","time_coll_wat","resp_gen")]<- sapply(dat[,c("prob_coll_HH.yes", "IS.disab", "trt_methods","child_under_5_count","time_coll_wat","resp_gen")], as.factor) %>% data.frame()
dat2<- dat[,c("prob_coll_HH.yes", "IS.disab","child_under_5_count","time_coll_wat","resp_gen")]
dat2
rf <-randomForest(prob_coll_HH.yes~.,data=dat2, ntree=500) 
rf %>% summary()
pred1=predict(rf,type = "prob")
pred1
plot(rf)
rffit <- randomForest(prob_coll_HH.yes ~ camp_id+ trt_methods + child_under_5_count +time_coll_wat + resp_gen,  
                      data=dat, ntree=2000, keep.forest=FALSE, importance=TRUE)
# Fit a logistic regression model
fit_glm = glm(prob_coll_HH.yes~.,dat2,family = "binomial")
fit_glm
library(caret)
varImp(fit_glm)

ggplot(dat)
?randomForest
importance(rffit) # relative importance of predictors (highest <-> most important)
varImpPlot(rffit) # plot results

library(polycor) 
pc <- hetcor(dat2, ML=TRUE) 
library(psych)
dat3<-dat2[complete.cases(dat2),]
pc <- hetcor(dat3, ML=TRUE) 
pc
faPC <- fa(r=pc$correlations, nfactors=2, n.obs=N, rotate="varimax")
faPC$loadings
faPCdirect <- fa.poly(dat3, nfactors=2, rotate="varimax")
# install.packages(c("FactoMineR", "factoextra"))

library("FactoMineR")
library("factoextra")
MCA(dat3, ncp = 5, graph = TRUE)
res.mca <- MCA(dat3, graph = FALSE)


windows();fviz_mca_biplot(res.mca, 
                # repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())
