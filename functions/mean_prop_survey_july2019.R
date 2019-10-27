mean_prop_svy_july_2019<-function(x,y,z,bv1){
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
    print(y[i]); print(i)
    ioi<-y[i]
    ioi_v<-x$variables[,ioi]
    #CREATE FORMULA FOR INDICATOR
    ioi_f<-paste0("~",ioi)
    #IF TRUE REPLACE NAS WITH SOMETHING
    if (z==TRUE & (length(unique(x$variables[,ioi])) >1)==TRUE){
      if(class(x$variables[,ioi])=="factor"){
        x2<-x
        x2$variables[,ioi]<-ifelse(is.na( x2$variables[,ioi]), "not_applicable", as.character(x2$variables[,ioi]))
        x2$variables[,ioi]<-factor(x2$variables[,ioi])
        x2<-x2}
      if(is.numeric(x$variables[,ioi])==TRUE){
        x2<-x
        x2$variables[,ioi]<-ifelse(is.na( x2$variables[,ioi]), 0, as.character(x2$variables[,ioi]))
        x2$variables[,ioi]<-as.numeric(x2$variables[,ioi])
        x2<-x2
      }
    }
    
    #IF FALSE LEAVE NAS
    if(z==FALSE| (length(unique(x$variables[,ioi])) >1)==FALSE){
      x2<-x
    }
    if(length(levels(x2$variable[,ioi]))==1){
      xm<-svyby(formula(ioi_f),by = ~camp_id, x2, na.rm=TRUE, na.rm.by = FALSE ,FUN = svyciprop,"mean",keep.var = FALSE) %>% data.frame()
      ym<-svyby(formula(ioi_f),by = ~Upazila, x2,na.rm = TRUE,na.rm.by = FALSE , FUN = svyciprop, "mean", keep.var=FALSE) %>% data.frame()
      gm<-svyby(formula(ioi_f),by = formula(bvar_f),FUN = svyciprop, "mean",na.rm=TRUE, na.rm.by=FALSE,design = x2, keep.var=FALSE) %>% data.frame()
      zm <- svyciprop(formula(ioi_f), x2,na.rm=TRUE, na.rm.by = FALSE, keep.var=FALSE )
      zm<-zm[[1]] %>% data.frame()
      colnames(zm)<-ioi
      name_cols<-colnames(zm)
      zm2<-zm
    }
    if(length(levels(x2$variable[,ioi]))>1|length(levels(x2$variable[,ioi]))==0){
      #DO ANALYSES WITH SURVEY PACKAGE FUNCTIONALITY
      xm<-svyby(formula(ioi_f),by = ~camp_id, x2, na.rm=TRUE, na.rm.by = FALSE ,FUN = svymean,keep.var = FALSE) %>% data.frame()
      
      ym<-svyby(formula(ioi_f),by = ~Upazila, x2,na.rm = TRUE,na.rm.by = FALSE , FUN = svymean, keep.var=FALSE) %>% data.frame()
      
      gm<-svyby(formula(ioi_f),by = formula(bvar_f),FUN = svymean,na.rm=TRUE, na.rm.by=FALSE,design = x2, keep.var=FALSE) %>% data.frame()
      
      zm <- svymean(formula(ioi_f), x2,na.rm=TRUE, na.rm.by = FALSE, keep.var=FALSE )
      name_cols<- zm  %>% data.frame() %>% t() %>% colnames()
      zm2<- zm  %>% data.frame() %>% t() %>% data.frame()
      zm3<-zm2[-2,]
    }
    
    
    #CREATE LEV OBJECT TO BE USED TO CONCATENATE LABELS WHEN IT IS A COMPLEX CROSS OF VARIABLES
    lev <- do.call(paste, c(gm[bv1], sep="_"))
    #THIS EXTRACT NAMES FROM THE 
    
    #REMOVE FIRST COUPLE OF COLUMNS FROM DATA SO THAT I CAN CBIND THEM TOGETHER
    xm2<-xm %>% select(-camp_id)
    colnames(xm2)<-name_cols
    ym2<-ym %>% select(-Upazila)
    colnames(ym2)<-name_cols
    
    
    
    gm2<- gm %>% select(-bv1)
    # SET COL NAMES
    colnames(gm2)<-name_cols
    
    cl[[i]]<-xm2
    ul[[i]] <- ym2 
    
    rl[[i]] <- zm2
    gl[[i]]<-gm2 %>% data.frame()
    
  }
  
  #BIND TOGETHER AND ADD RELEVANT COLUMNS
  cldf <- do.call("cbind", cl) %>% data.frame
  
  cldf <- cldf %>% mutate(Level = "camp_id", Level_id = xm$camp_id) %>%
    select(Level, Level_id, everything())
  print("camp level complete")
  uldf <-do.call("cbind", ul) %>% data.frame
  
  uldf <-uldf %>% mutate(Level = "Upazilla", Level_id = ym$Upazila) %>%
    select(Level, Level_id, everything())
  print("Upazila level complete")
  rldf <-do.call("cbind", rl) %>% data.frame
  
  rldf <-rldf %>% mutate(Level = "Response", Level_id ="Response") %>%
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
