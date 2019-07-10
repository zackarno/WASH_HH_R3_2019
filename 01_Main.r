

library(koboquest)
library(dplyr)

hh<-read.csv("inputs/01_HH_WASH_HH_R3_Data_With_Recoded_Indicators_19july2019.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
indiv<-read.csv("inputs/01_INDIV_WASH_HH_R3_Data_With_Recoded_Indicators_19july2019.csv",  stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
choices<- read.csv("inputs/Choices.csv",  stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
survey<- read.csv("inputs/Survey.csv",  stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))

surveys<-load_questionnaire(data = hh,questions = survey,choices = choices,choices.label.column.to.use ="label" )


surveys$question_get_choices(variable.name = "dry_mat")
surveys$question_type("dry_mat")

surveys$question_is_skipped(data = hh, question.name = "dry_mat")
hh




surveys$question_get_choice_labels(variable.name = "dry_mat",responses = hh$dry_mat)

debugonce(surveys$question_get_choice_labels)

surveys$question_get_question_label(variable.names = colnames(hh))
lets_see<-purrr::map2(.x= hh, .y=colnames(hh),.f=surveys$question_get_choice_labels) %>% as.data.frame()

hh %>% select(starts_with("ic"), starts_with("i")) %>% colnames()

hh %>% select(starts_with("IS.")) %>% colnames()
 
hh
browseVignettes("hypegrammaR")


