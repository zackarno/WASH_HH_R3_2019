
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
