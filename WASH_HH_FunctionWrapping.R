

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

Container_Aggregations<- function(container_data){
  cz<-container_data
  CZHH<-cz %>% 
  mutate(
    container_vol<-mapply(vol,cz$con_height, cz$con_width, cz$con_circum, cz$con_length),
    drinking_water_and_both=ifelse((wat_con_use %in% c("drnk_water", "both") &wat_con_collection== "yes"),container_vol*wat_col_tm,0),
    drink_wat_only= ifelse((wat_con_use=="drnk_water" & wat_con_collection== "yes"),container_vol*wat_col_tm,0),
    not_drink_only= ifelse((wat_con_use=="nonedrnk_wat" &wat_con_collection== "yes"),container_vol*wat_col_tm,0),
    both_only= ifelse((wat_con_use=="both" &wat_con_collection== "yes"),container_vol*wat_col_tm,0),
    
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
  return(CZHH)}


WASH_HH_Level_Recoding<- function(hh_data){
  hz$arriv_bgd.dte<-as.Date(hz$arriv_bgd)
  hz$arriv_shelter.dte<-as.Date(hz$arriv_shelter)

#ONLY LOOK AT CONSENT= YES



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
    )}


  
  


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

######################INDIVIDUAL LEVEL##########################
###### NA MEANS NOT APPLICABLE
#BREAK DOWN BY 2 AGE GROUPS AND GENDER
#JUST LOOKING AT AND GROUPING SOME INDICATORS



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