library(dplyr)
hz <- read.csv(paste0(data_dir, "Header_07july2019.csv" ), stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
iz <- read.csv(paste0(data_dir,"Individual_19June2019.csv"), stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))
cz <- read.csv(paste0(data_dir,"Container_19June2019.csv" ), stringsAsFactors = FALSE, na.strings=c(""," ", "<NA>",NA))

container_data<-read.csv("inputs/Container_19June2019.csv")



hz<-hz[!duplicated(hz$instance_name),]

iz2<-iz[!duplicated(iz$repeat_instance_name),]



container_HH<-Container_Aggregations(container_data)
hz<-hz %>% filter(consent=="yes")