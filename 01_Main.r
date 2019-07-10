hh<-read.csv("inputs/01_HH_WASH_HH_R3_Data_With_Recoded_Indicators_19july2019.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
indiv<-read.csv("inputs/01_INDIV_WASH_HH_R3_Data_With_Recoded_Indicators_19july2019.csv",  stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))

nrow(hh)
nrow(indiv)
