

library(koboquest)
library(dplyr)

hh<-read.csv("inputs/01_HH_WASH_HH_R3_Data_With_Recoded_Indicators_19july2019.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
indiv<-read.csv("inputs/01_INDIV_WASH_HH_R3_Data_With_Recoded_Indicators_19july2019.csv",  stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
choices<- read.csv("inputs/Choices.csv",  stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
survey<- read.csv("inputs/Survey.csv",  stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
surveys<-load_questionnaire(data = hh,questions = survey,choices = choices,choices.label.column.to.use ="label" )
sf<- read.csv("WASH_unhcr_population_and_assessment_sample_size_data.csv",  stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))


sf<-sf %>% select(camp_id, total_HH)
sf

#calculate_weights
library(hypegrammaR)
#set up survey weights and hypothesis testing
weighting<-map_to_weighting(sampling.frame = sf, data.stratum.column = "camp_id",
                            data = hh,
                            sampling.frame.population.column ="total_HH",
                            sampling.frame.stratum.column = "camp_id"
                              )




design_object<-map_to_design(data = hh,weighting_function = weighting)
class(design)

#varaible to
independent.variable <- "IS.disab"
dependent.variable<- "I.prob_coll_water_HH"


#ths determines the type of hypothesis test
case<-map_to_case( "group_difference",
                   dependent.var.type= "categorical",
                   independent.var.type="categorical")

  

surveys$question_type("more_train")



independent.variable <- "IS.disab"
dependent.variable<- "more_train"

test1<-map_to_result(data = hh,dependent.var = dependent.variable, 
                     independent.var = independent.variable, case = case, 
                     weighting = weighting,questionnaire = surveys) #%>% 


design<-map_to_design(hh,weighting,cluster_variable_name = NULL)


map_to_summary_statistic(design,
                         dependent.variable,
                         independent.variable,case)






map_to_hypothesis_test(design = design,
                       dependent.variable,
                       independent.variable,case)



test1 %>% map_to_labeled(surveys) %>%  map_to_visualisation() %>% map_to_file('test.jpg')
test1 %>% map_to_labeled(surveys) %>%  map_to_table() %>% map_to_file('test.csv')
browseURL('./test.csv')
output1$ggsave_parameters

pvalue<-function(result){
  x$hypothesis.test$result$p.value
}




###################################333

#forloop through ISCG
#ISCG COLUMNS 

hh$time_coll_wat

ind_var_cols<-c( "prob_coll_HH.yes",
  "drink_wat_quality.unimproved",
  "time_coll_wat.more_30",
  "X15_litres_perday.no.",
  "X3_litres_perday.no.","i.cop_strat")



ind_var_cols<- c( "I.prob_coll_water_HH", "I.wat_30min","time_coll_wat","ic.jrp.drink_wat_only_gr_threshold_15L")%>% tolower


iscg_cols<-c("IS.REVA_arrival", "IS.REVA_arrival_shelt", "Is.higest_edu", 
  "IS.mhoh", "IS.gender_hoh", "Is.men_wk", "IS.women_wk", "IS.total_wk", 
  "IS.dependency_ratio", "IS.disab", "IS.disab_coll_water", "IS.no_male_18to59_coll_water", 
  "IS.no_adult_18to59_coll_water") %>% tolower

hh %<>%( koboquest:::to_alphanumeric_lowercase_colnames_df)
ap<-purrr::map(ind_var_cols,function(independent.var){
  data.frame(research.question= "bla",
             hypothesis.type= "group_difference",
             dependent.variable= iscg_cols,
             dependent.variable.type= "categorical",
             repeat.for.variable= "camp_id",
             independent.variable= independent.var,
             independent.variable.type= "categorical", stringsAsFactors = FALSE)   
}) %>% do.call(rbind,.)


data_analyzed<-from_analysisplan_map_to_output(data = hh,analysisplan = ap,weighting = weighting,
                                questionnaire = surveys)


data_analyzed$results[[1]]$summary.statistic
ap
# extract design object
hh$improved
hh[,ind_var_cols]



independent.variable <- "IS.disab"
dependent.variable<- "more_train"

result_list<-list()

for (var in iscg_cols){
 result_list[[var]]<- map_to_result(data = hh,dependent.var = var, 
                independent.var = ind_var_cols[1], case = case,
                weighting = weighting,questionnaire = surveys)} #%>% 

result_list %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.)


result_list %pull% "summary.statistic" %pull% "dependent.var"


`%pull%`<-function(x,y){
  
  lapply(x,function(x){x[[y]]})
  
}






# 
# result_list %>% hypegrammaR:::map_to_generic_hierarchical_html(render_result_with = hypegrammaR:::rmdrs_pretty_summary_table,
#                                                                by_analysisplan_columns = c('dependent.var'),
#                                                                level = 2,
#                                                                questionnaire = surveys,
#                                                                dir = ".",
#                                                                filename = 'test.html',by_prefix = "")
# 
# 

test1$summary.statistic

test1#

hh %>% select(starts_with("ic"), starts_with("i")) %>% colnames()

hh %>% select(starts_with("IS.")) %>% colnames() %>% dput()
hh %>% select(contains("lat")) %>% colnames()
hh
browseVignettes("hypegrammaR")

#################
#example of question_ functions
surveys$question_get_choices(variable.name = "dry_mat")
surveys$question_type("dry_mat")

surveys$question_is_skipped(data = hh, question.name = "dry_mat")
hh




surveys$question_get_choice_labels(variable.name = "dry_mat",responses = hh$dry_mat)

debugonce(surveys$question_get_choice_labels)

surveys$question_get_question_label(variable.names = colnames(hh))

lets_see<-purrr::map2(.x= hh, .y=colnames(hh),.f=surveys$question_get_choice_labels) %>% as.data.frame()

hh$I.prob_coll_water_HH




