##nb
require(plyr)
require(dplyr)
require(car)
require(reshape2)
require(stringr)
require(survey)
require(ggplot2)
require(ggthemes)
require(extrafont)

options(survey.lonely.psu="remove")

###########################################################################################33
###########################################################################################33
###########################################################################################33


# create a simple analysis plan based on variables in the data  in case there is no analysis plan
# db = data
# db = disag - variable for disaggregation
# filter = useless
# CL = confidence level

create.analysisplan<-function(db,disag,filter=NULL,Y_cl=TRUE,CL=0.95){
  
	db<-class_assess(db)
	d<-sapply(db,class)
	
	log_analysis<-data.frame(class=d,xi=rownames(data.frame(d)))
	log_analysis$xi<-sanit_vector(log_analysis$xi)
	log_analysis$yi<-disag
	
	log_analysis$nomb[log_analysis$class!="numeric"]<-"no"
	log_analysis$nomb[log_analysis$class=="numeric"]<-"yes"

	log_analysis$clt	<-	'None'
	log_analysis$strata	<-	'None'
	log_analysis$weight	<-	'weight'
	log_analysis$fpop	<-	'None'
	log_analysis$sm_xi	<-	'NA'
	log_analysis$heatmap	<-	TRUE
	log_analysis$piechart	<-	TRUE
	log_analysis$barchart	<-	TRUE
	log_analysis$CL	<-	'0.95'
	log_analysis$Y_cl	<-	Y_cl
	log_analysis$smult	<-	'FALSE'
	log_analysis$sort	<-	'FALSE'
	log_analysis$D1filter	<-	'None'
	log_analysis$T1filter	<-	'None'
	log_analysis$D2filter	<-	'None'
	log_analysis$T2filter	<-	'None'
	log_analysis$tabxfilter	<-	'None'
	log_analysis$tabyfilter	<-	'None'
	log_analysis$labx	<-	'NA'
	log_analysis$laby	<-	'NA'
	log_analysis$fsize	<-	'5'
	log_analysis$ffont	<-	'Arial Narrow'
	log_analysis$hgt	<-	'4000'
	log_analysis$wdh	<-	'750'
	log_analysis$H_col	<-	'#A7A9AC'
	log_analysis$sdonut	<-	'0'
	log_analysis$flip	<-	'TRUE'
	log_analysis$col_ramp	<-	'REACH blue'
	log_analysis$col_revert	<-	'FALSE'
	return(log_analysis)
	
}


# create list of parameter based on the data analysis plan

# create list of parameter based on the data analysis plan
parametres<-function(dt,log_analysis,sampling_frame=NULL,questions=NULL,choices=NULL,lang="english"){
  
  
  dt$No_disagregation<-"all"
  dt<-dt %>% sanit_name %>% class_assess
  
  log_analysis$xi<-sanit_vec_all(log_analysis$xi)
  log_analysis$yi<-sanit_vec_all(log_analysis$yi)
  log_analysis$lang<-lang
  log_analysis$sm_xi<-NA
  log_analysis$smult<-FALSE
  
  if(!"sector" %in% names(log_analysis)){
    log_analysis$sector<-NA
  }
  
  cond<-log_analysis$odk_type=="select_multiple"
  grrrr<-function(x,y){
    paste(names(y)[grep(paste0(x,"_._"),names(y))],collapse=" -/- ")
  }
  if(all(is.na(log_analysis$sm_xi))){
    sm_xi<-lapply(log_analysis$xi[cond],grrrr,y=dt) %>% unlist
    
    log_analysis$sm_xi[cond]<-sm_xi
    log_analysis$smult[cond]<-TRUE
    log_analysis$xi[cond]<-"NA"
  } else{
    log_analysis$smult<-FALSE
  }
  
  params <- list(		
    db_up = dt,
    questions= questions,
    choices= choices,
    sampling_frame = sampling_frame,
    lang=as.character(log_analysis[["lang"]]),
    sector = as.character(log_analysis[["sector"]]),
    clt = as.character(log_analysis[["clt"]]),
    strata = strsplit(as.character(log_analysis[["strata"]])," -/- "),
    weight = as.character(log_analysis[["weight"]]),
    fpop = as.character(log_analysis[["fpop"]]),
    xi = as.character(log_analysis[["xi"]]),
    sm_xi =  strsplit(as.character(log_analysis[["sm_xi"]])," -/- "),
    T1filter = strsplit(as.character(log_analysis[["T1filter"]])," -/- "),
    T2filter = strsplit(as.character(log_analysis[["T2filter"]])," -/- "),
    yi = strsplit(as.character(log_analysis[["yi"]])," -/- "),
    D1filter =  as.character(log_analysis[["D1filter"]]),
    D2filter =  as.character(log_analysis[["D2filter"]]),
    nomb = as.character(log_analysis[["nomb"]]),
    heatmap =  as.logical(log_analysis[["heatmap"]]),
    piechart =  as.logical(log_analysis[["piechart"]]),
    barchart =  as.logical(log_analysis[["barchart"]]),
    CL = as.numeric(log_analysis[["CL"]]),
    Y_cl = as.logical(log_analysis[["Y_cl"]]),
    fsize = as.numeric(log_analysis[["fsize"]]),
    ffont = as.character(log_analysis[["ffont"]]),
    hgt = as.numeric(log_analysis[["hgt"]]),
    wdh = as.numeric(log_analysis[["wdh"]]),
    tabxfilter =  strsplit(as.character(log_analysis[["tabxfilter"]])," -/- "),
    tabyfilter = strsplit(as.character(log_analysis[["tabyfilter"]])," -/- "),
    labx = as.character(log_analysis[["labx"]]),
    laby = as.character(log_analysis[["laby"]]),
    H_col = as.character(log_analysis[["H_col"]]),
    sdonut = as.numeric(log_analysis[["sdonut"]]),
    flip = as.logical(log_analysis[["flip"]]),
    col_ramp = as.character(log_analysis[["col_ramp"]]),
    smult = as.logical(log_analysis[["smult"]]),
    col_revert = as.logical(log_analysis[["col_revert"]]),
    sort = as.logical(log_analysis[["sort"]])
  )	
  
  return(params)
}



check_param<-function(params){
  # run test
  
 thecheck<-function(params,weight) {
	params[[weight]][!params[[weight]]%in% names(params$db_up)]
    }
  
  # varaible check
  if (!all(params$xi[params$smult==FALSE]%in% names(params$db_up))){
    message(paste0("xi does not match data names: ",paste(unique(params$xi[ (!params$xi %in% names(params$db_up))& params$log$smult==FALSE]),collapse= ", ")))
  } else {print("xi OK")}
  
  # disaggregation varaible check
  if (!all(params$yi%in% names(params$db_up))){
    message(paste0("yi does not match data names: ",paste(unique(params$yi[ (!params$yi %in% names(params$db_up))& params$log$smult==FALSE]),collapse= ", ")))
  } else {print("yi OK")}
  
  # select multiple check
  test_sm<-params$sm_xi %>% unlist %>% na.omit
  if (!all(test_sm%in%names(params$db_up))){
    print("sm_xi does not match data names")
    test_sm[ (!test_sm %in% names(params$db_up))]
  } else {print("sm_xi OK")}
  
  
  # weight check
  cond_wgh<-(params$weight%in% names(params$db_up) | params$weight=="None")
  if(!all(cond_wgh)){
    print("weigth column does not match data names")
    params$weight[cond_wgh]
  } else {
    print("weight ok")
  }
    
  # strata check
  cond_strata<-(params$strata%in% names(params$db_up) | params$strata=="None")
  if(!all(cond_strata)){
    print("weigth column does not match data names")
    params$strata[cond_strata]
  } else {
    print("strata ok")
  }
  
  # fpop check
  cond_fpop<-(params$fpop%in% names(params$db_up) | params$fpop=="None")
  if(!all(cond_fpop)){
    print("weigth column does not match data names")
    params$fpop[cond_fpop]
  } else {
    print("fpop ok")
  }  
}

# small function to merge CI and data used into process numbers
merge_cfint<-function(a,dbgr,y){
	
	index<-do.call(paste, c(dbgr[,1:2], sep = ":"))
	dbgr<-cbind(dbgr,index) 
	
	rown<-data.frame(do.call('rbind', strsplit(as.character(row.names(a)),"\\:",fixed=F)))
	rown[,2]<-gsub(paste0("^",y),"",rown[,2])
	cfint<-data.frame(a,rown)
	
	index1<-do.call(paste, c(cfint[,4:3], sep = ":"))
	index2<-do.call(paste, c(cfint[,3:4], sep = ":"))
	if(length(which(index%in%index1))>length(which(index%in%index2))){
		index<-index1	
	} else {
		index<-index2
	}
	
	cfint<-cbind(cfint,index)[,c(1,2,5)]
	names(cfint)[1:2]<-c("CI_lw","CI_up")
	dbgr<-merge(dbgr,cfint,by="index")[,-1]
	return(dbgr)
}




create_weights<-function(sampling_frame,hh){
  statname<-names(sampling_frame)[1]
  table(hh[[statname]]) %>% 
    as.data.frame %>% 
    merge(sampling_frame,.,by.x=statname,by.y="Var1",all.x=T)->poptable
  
  poptable$weight<-(poptable$population/sum(poptable$population,na.rm=T))/(poptable$Freq/sum(poptable$Freq,na.rm=T))
  db<-merge(hh,poptable[c(statname,"weight")],by=statname)
  return(db)
}


prep_results<-function(df,group,rd=1){
  
  df<- df %>% class_assess
  df$group<-as.character(df[[group]])
  
  if (group == "rowname"){
    df$disa<-" "
  }
  
  df$count<-NA
  
  filt<-which(df$type_data=="no")
  df$count[filt]<-round(df$value[filt]*df$valid_n[filt],0)
  df$value<-ifelse(df$type_data=="no",round(df$value*100,rd),round(df$value,rd+1))
  
  if(all(c("CI_lw","CI_up") %in% names(df))){
    df$CI_lw<-ifelse(df$type_data=="no",round(df$CI_lw*100,rd),round(df$CI_lw,1))
    df$CI_lw[df$type_data=="no" & df$CI_lw<0]<-0
    df$CI_lw[df$type_data=="no" & df$CI_lw>100]<-100
    
    df$CI_up<-ifelse(df$type_data=="no",round(df$CI_up*100,rd),round(df$CI_up,1))
    df$CI_up[df$type_data=="no" & df$CI_up<0]<-0
    df$CI_up[df$type_data=="no" & df$CI_up>100]<-100
  }
  
  # here create some ranks for the answers
  df$key<-tolower(paste(df$xi,df$group,sep = "/"))
  
  # create unique key
  oldvalues <- unique(df$key)
  newvalues <- as.character(1:length(oldvalues))  # Make this a factor
  df$code <- newvalues[ match(df$key, oldvalues)]		
  
  # rank by key/code
  df<-base::transform(df,rank=ave(value,code,FUN=function(x) rank(-x,ties.method = "first")))	  
  return(df)
}




boom_rmd<-function(params,i,loop=""){
  # Create directory to store the results
  check_create_directory("graph")
  check_create_directory("results")
  
  # define the parameters
  if(!all(c("labx","laby") %in% names(params)) ){
    labx<-params$yi[[i]]
    laby<-params$xi[[i]]
  }else{
    labx<-params$labx[i]
    laby<-params$laby[i]
  }
  
  if(!is.na(params$Y_cl[i]) & (tolower(params$Y_cl[i])=="false")){
    CI_up<-NA
  } else {
    CI_up<-params$CL[i]
  }
  
  if(params$smult[i] & is.null(params$sampling_frame) ){
    index<-which(names(params$db_up) %in% c(c(params$sm_xi[[i]]),params$yi[[i]],params$strata[[i]],params$clt[i],params$weight[i],params$fpop[i],params$D1filter[i],params$D2filter[i]))
  }else if(params$smult[i] & !is.null(params$sampling_frame) ){
    index<-which(names(params$db_up) %in% c(c(params$sm_xi[[i]]),params$yi[[i]],params$strata[[i]],params$clt[i],params$fpop[i],params$D1filter[i],params$D2filter[i]))
  }else if(!params$smult[i] & is.null(params$sampling_frame)){
    index<-which(names(params$db_up) %in% c(params$xi[[i]],params$yi[[i]],params$strata[[i]],params$clt[i],params$weight[i],params$fpop[i],params$D1filter[i],params$D2filter[i]))
  } else {
    index<-which(names(params$db_up) %in% c(params$xi[[i]],params$yi[[i]],params$strata[[i]],params$clt[i],params$fpop[i],params$D1filter[i],params$D2filter[i]))
  }
  
  de<-params$db_up[,index]
  
  if(params$smult[i]){
    de<- lapply(de,function(x) {car::recode(x,"c('Yes','yes','TRUE','true','1',1)='yes'; c(0,'0','No','no','FALSE','false')='no'")}) %>% data.frame
  }
  
  if(!params$smult[i]){
    de<-na.omit(de)
  } else {
    de<-de[de[,params$sm_xi[[i]]] %>% apply(.,1,function(x){!all(is.na(x))}) %>% which,]
  }
  
  # omit the low count on some data
  try({
    if(!params$smult[i]){
      if(
        (params$nomb[[i]]=="no" & 
         all(apply(table(de[[params$xi[[i]]]],de[[params$yi[[i]]]]),2,sum)<2))|
        all(is.na(de[[params$xi[[i]]]]))
      ){
        df<-data.frame(
          rowname="sample size too small / no valids observation",
          variable=params$xi[[i]],
          value="NA",
          xi=params$xi[[i]],
          yi=params$yi[[i]]
        )
        return(df)
        stop("sample size too small")
      }
    }
  },silent=TRUE)
  
  # set up the filter and formula
  if(params$D1filter[i]!="None"){de<-de[de[[params$D1filter[i]]]%in%params$T1filter[[i]],]}
  if(params$D2filter[i]!="None"){de<-de[de[[params$D2filter[i]]]%in%params$T2filter[[i]],]}
  if(params$nomb[i]!="yes" & !params$smult[i]){de[[params$xi[i]]]<- de[[params$xi[i]]] %>% as.character}
  
  # remove the one strata
  de <- de %>% class_assess
  
  assess_psu<-table(de[[params$strata[[i]]]]) %>% as.data.frame() 
  cond<-which(!de[[params$strata[[i]]]] %in% assess_psu$Var1[assess_psu$Freq<2])
  if(length(cond)>0){de<-de[cond,]}
  
  if(!is.null(params$sampling_frame)){de<-create_weights(params$sampling_frame,de)}
  
  if(params$clt[i]=="None"){clust<-formula(paste0("~",1))}else{clust<-formula(paste0("~",params$clt[i]))}
  
  if(all(params$strata[[i]]=="None")){
    strat<-NULL
  }else if(length(params$strata[[i]]) >1){
    strat<-formula(paste0("~interaction(",paste(params$strata[[i]],collapse=","),")"))
  }else{
    strat<-formula(paste0("~",params$strata[[i]]))
  }
  
  if(params$weight[i]=="None"){
	wgh=NULL
  }else{
	wgh<-formula(paste0("~",params$weight[i]))
  }
  
  if(params$fpop[i]=="None"){
    fpop=NULL
  }else{
    fpop<-formula(paste0("~",params$fpop[i]))
  }

  # define the sample design
  dsamp<-svydesign(
    id=clust,
    strata=strat,
    weights=wgh,
    fpc=fpop, 
    data=de,
    nest=TRUE
  )	
  
  # do the aggregation
  if(params$smult[i]){
    dbgr<-process_smultiple(params$sm_xi[[i]],params$yi[[i]],dsamp,CI_up)
  }else  if(params$nomb[i]=="yes" & length(params$yi[[i]])==1 ){
    dbgr<-process_num(params$xi[i],params$yi[[i]],dsamp,CI_up)
  }else if(length(params$yi[[i]])>1 & params$nomb[i]=="yes"){
    dbgr<-process_mutl_disa(params$xi[i],params$yi[[i]],dsamp,CI_up,params$nomb[i])
  }else if(length(params$yi[[i]])>1 & params$nomb[i]!="yes"){
    dbgr<-process_mutl_disa(params$xi[[i]],params$yi[[i]],dsamp,CI_up,params$nomb[i])
  }else{
    dbgr<-process_categories(params$yi[[i]],params$xi[i],dsamp,CI_up)
  }
  
  # remove the tab filter choices from the results
  df<-dbgr[[1]][!dbgr[[1]][,1]%in%params$tabxfilter[[i]],]
  df<-df[!df[,2]%in%params$tabyfilter[[i]],]
  
  gh <- de[[params$yi[[i]]]] %>% table %>% data.frame
  
  names(gh)<-c("rowname","valid_n")
  df<-merge(df,gh,by="rowname",all.x=T)
  df$type_data<-params$nomb[i]
  df$aggregation_type<-if(params$nomb[i]=="yes"){"average"}else{"percentage"}
  
  # format the results
  # extract the last part of multiple choices variable name to get the option
  if(params$smult[i]){
    spl<-str_split(params$sm_xi[[i]][1],"_._")
    lab<-spl[[1]][length(spl[[1]])-1]
    df$xi<-rep(lab,nrow(df))
  } else {
    df$xi<-rep(params$xi[i],nrow(df))
  }
  
  df$yi<-rep(params$yi[i],nrow(df))			
  
  # order the results
  df<-df%>% class_assess	
  
  if(params$nomb[i]=="yes"){
    cdata <- df
    ordeR<-cdata[with(cdata, order(-value)), ][,1]	
  }else{	
    if(length(unique(df[,2]))==1){
      cdata <- df
      ordeR<-cdata[with(cdata, order(-value, variable)), ][,1]
    }else{
      cdata <- ddply(df, "variable", summarise, mean = mean(value,na.rm=TRUE))
      ordeR<-cdata[with(cdata, order(-mean, variable)), ][,1]
    }
  }
  
  if(loop!=""){loop<-paste0(loop," - ")}
  
  if(params$smult[[i]]){
    mystr<-strsplit(params$sm_xi[[i]][1],"_._")[[1]][1]
    titl<-paste0(paste(loop,mystr)," - ", paste(params$yi[[i]],collapse = "_"))
  } else {
    titl<-paste0(paste(loop,params$xi[[i]],collapse = "_")," - ", paste(params$yi[[i]],collapse = "_"))
  }
  
  titl<-substring(titl, 1,200) 
  message(paste0("done: ",titl))

  df<-prep_results(df,"rowname",rd=1)
  
  params$questions$type %>% ch %>% strsplit(.," ") %>% do.call(rbind,.)-> tosplit
  params$questions$choices <- ifelse(tosplit[,1]==tosplit[,2],NA,tosplit[,2])
  names(params$choices)<-paste0("ch_",names(params$choices))
  questionnaires<-merge(params$questions,params$choices,by.x="choices",by.y="ch_list_name",all=T)
  
  myfiltxi<-which(questionnaires$name %in% df$xi)
  myfiltyi<-which(questionnaires$name %in% df$yi)
  df$variable_label<-r3c(df$variable,questionnaires$ch_name[myfiltxi] %>% ch ,questionnaires[[sprintf("ch_label::%s",params$lang[i])]][myfiltxi])
  df$variable_label<-factor(df$variable_label,levels=questionnaires[[sprintf("ch_label::%s",params$lang[i])]][myfiltxi])
  df$rowname_label<-r3c(df$rowname,questionnaires$ch_name[myfiltyi] %>% ch ,questionnaires[[sprintf("ch_label::%s",params$lang[i])]][myfiltyi])
  df$rowname_label<-factor(df$rowname_label,levels=questionnaires[[sprintf("ch_label::%s",params$lang[i])]][myfiltyi])
  
  
  df$yi_lab<-r3c(df$yi,questionnaires$name %>% ch ,questionnaires[[sprintf("label::%s",params$lang[i])]])
  df$xi_lab<-r3c(df$xi,questionnaires$name %>% ch ,questionnaires[[sprintf("label::%s",params$lang[i])]])
  
  df<-df[,which(names(df)!="type_data")]
  # write.csv(df,paste0("results/",titl,".csv"))
  
  outparams<-params[which(!params %>%  names %in% c("db_up","questions","choices","sampling_frame"))]
  paramsout<-lapply(
    outparams,
    function(x,i){
      if(class(x)=="list"){ x[[i]] }else{ x[i] }
    },i=i
  )
  
  if(paramsout$smult){
    paramsout$xi<-strsplit(paramsout$sm_xi[1],"_._")[[1]][1]
  } 
  
  return(list(
    data=df,
    pvalue=dbgr$pvalue,
    test.name=dbgr$test.name,
    paramsout=paramsout
  ))
}


print_rmd<-function(results, stat=T){
  
  require(knitr)
  require(pander)

  df<-results$results$data
  
  cat(knit(text = knit_expand(text = 
    sprintf(
      "```{r %s, results='asis'  }\n	
      cat('\n\n\n\n')
      \n
      cat(paste0('\n\n\n## ', unique(results$data$xi_lab),' - by - ',unique(results$data$yi_lab),'\n\n\n'))
      \n
      cat('\n\n\n\n')
      \n
      cat(paste0('\n\n\n ', unique(results$data$xi),' - / - ',unique(results$data$yi),'\n\n\n'))
      \n
      cat('\n\n\n### Graph \n\n') 
      \n
      cat('\n![](%s)\n')
      \n\n\n
      cat('\n![](%s)\n')
      \n\n\n
      cat('\n\n\n### Table \n\n') 
      \n
      set.alignment('left')
      kable(results$data)
      \n\n\n
      \n\n\n\n
      ```",results$heatmapPATH,paste0('../',results$heatmapPATH),paste0('../',results$barchartPATH)
      )
  )))
  
  if (stat==TRUE){
    if (results$paramsout$nomb=='yes'){
      cat(paste('\n\n###  Statistics \n\n\n\n'))
      cat(paste0('\n\n\n ',results$test.name,'\n\n\n'))
      pandoc.table(results$data, use.hyphening = TRUE, keep.line.breaks = TRUE, split.table  = Inf, style='rmarkdown')
    } else {
      cat(paste('\n\n###  Statistics \n\n\n\n'))
      cat(paste0('\n\n\n ',results$test.name,'\n\n\n'))
      cat('\n\n\n\n')
    }
  }
  
}

plotresults<-function(results){
  
  check_create_directory("graph")
  
  dt<-results$data
  flip<-results$paramsout$flip
  disaname<-results$paramsout$yi
  variablename<-results$paramsout$xi
  makeheatmap<-results$paramsout$heatmap
  makebarchart<-results$paramsout$barchart
  makepiechart<-results$paramsout$piechart
  lang<-results$paramsout$lang
  
  setsize<-all(!is.na(results$paramsout$wdh) & !is.na(results$paramsout$hgt))
  
  if(setsize){
    lg<-results$paramsout$wdh
    ht<-results$paramsout$hgt
  }
  
  
  if(length(unique(dt[["variable_label"]]))>12){
    makebarchart<-F
    makepiechart<-F
  }
  
  if(makebarchart){
    chart<-bar_chart(
      dt,
      disa="rowname_label",
      variable="variable_label",
      value="value",
      nb=results$paramsout$nomb, 
      flip=flip
    )
    
    # adjust the size here
    if(!setsize){
      if(flip){
        lg<-12
        ht<-nrow(dt)*0.8
      } else  {
        lg<-nrow(dt)+2
        ht<-5
      }
    }
    chartname<-sprintf("graph/barchart_%s_%s_%s.png",disaname,variablename,lang)
    ggsave(chartname,chart, width = lg, height = ht, units="in",limitsize = FALSE)
    message("saved: ",chartname)
    results$barchartPATH<-chartname
    
  } 
  
  if(makepiechart){
    p<-pie(dt,
           sdonut=results$paramsout$sdonut,
           fsize=results$paramsout$fsize,
           font_family="Arial Narrow",
           variable="variable_label",
           value="value",
           rowname="rowname_label"
    )
    
    if(!setsize){
      ht<-(length(unique(dt$rowname))/2)*5
      lg<-(2.5)*5  
    }
    
    chartname<-sprintf("graph/piechart_%s_%s_%s.png",disaname,variablename,lang)
    ggsave(chartname,p, width = lg, height = ht, units="in",limitsize = FALSE)
    message("saved: ",chartname)
    results$piechartPATH<-chartname
  }
  
  if(makeheatmap){
    
    if(!setsize){
      ht<-(length(unique(dt$variable))+1.5)*0.6
      lg<-(length(unique(dt$rowname))*2+3)*1
    }
    
    chartname<-sprintf("graph/heatmap_%s_%s_%s.png",disaname,variablename,lang)
    png(filename=chartname,width = lg, height = ht, units="in", res=150)
    chart<-heat_map(
      datag=dt,
      disa="rowname_label",
      variable="variable_label",
      value="value",
      type="count_perc", 
      H_col=results$paramsout$H_col,
      order=results$paramsout$sort
      # save and hg / lg inclde inside ?
    )
    dev.off()
    message("saved: ",chartname)
    results$heatmapPATH<-chartname
  }
  return(results)
}


# wrap the boom version
extract_results<-function(dt,log_analysis,level,group="quartier",write=FALSE){
  ct<-0
  
  for(l in level){
    ct<-ct+1
    if(l=="all"){
      datas<-dt
    } else {
      datas<-dt[dt$group==l,]
    }
    
    params<-parametres(datas,log_analysis)
    
    for(i in 1:length(params$xi)){
      print(paste0(l,"  by  ",params$xi[i]))
      df<-as.data.frame(boom(params,i,l))
      names(df)<-make.names(names(df))
      if(i==1) {
        dg<-df
      }else{
        dg<-rbind.fill(dg,df)	
      }
    }
    dg$group<-l
    dg<-as.data.frame(sapply(dg,as.character))
    
    if(ct==1){
      out<-dg
    } else{
      out<-rbind.fill(out,dg)
    }
  }
  out$key<-paste0(
    out$xi,"-",
    out$rowname,"-",
    out$variable,"-",
    out$group
  )
  names(out)<-r3c(names(out),"group",group)
  
  if(write){
    write.csv(out,"results_all.csv")
  }
  return(out)
  
  
}


## type of aggregation? svymean svytotal svyquantile

# create mean aggregatation and t-test
process_num<-function(x,y,dstrat,CL){

	for (dis in 1:length(x)) 
	{
		f<-formula(paste0("~",y,"+",x[dis]))
		ft<-formula(paste0(x[dis],"~",y))
		fx<-formula(paste0("~",x[dis]))
		fy<-formula(paste0("~",y))

		mytable<- mytable<-svyby(fx,fy,dstrat,svymean,na.rm.all=T,na.rm=T)
		mytable<-mytable[,-3]
		
		
		a<-NA
		try(a<-confint(svyby(fx,fy, dstrat,svymean,na.rm=T,na.rm.by=T,drop.empty.groups=T),level=CL) %>% as.data.frame() ,silent=TRUE)
		
		if(!all(is.na(a))){
		  names(a)<-c("CI_lw","CI_up")
			dbgr<-cbind(mytable,a)
		} else {
			dbgr<-mytable
		}
		names(dbgr)[2]<-"value"
		
		dbgr<-data.frame(append(dbgr,"variable", after =  1),check.names=FALSE,stringsAsFactors=F)
		
		names(dbgr)[2]<-"variable"
		dbgr$variable<-rep(x[dis],nrow(dbgr))
		
		stest<-NA
		s_dstrat<-NA
		
		lev<-unique(dstrat$variable[[y]])
		sq<-length(lev)
		cr<-as.data.frame(matrix(,nrow=sq,ncol=sq))
		row.names(cr)<-lev
		names(cr)<-lev
		
		
		# create a matrix with p-values
		if(length(x)==1){	
			for (i in lev)
			{
				for(j in lev)
				{
					if(i!=j){
						s_dstrat<-subset(dstrat,(!is.na(dstrat$variable[[x[dis]]])&!is.na(dstrat$variable[[y]]))&(dstrat$variable[[y]]==i|dstrat$variable[[y]]==j))
						try(stest<-svyttest(ft, design=s_dstrat),silent=TRUE)
						if(!is.na(stest)){
							cr[which(row.names(cr)==i),which(names(cr)==j)]<-round(as.numeric(stest$p.value),3)
						}	
					}
				}
			}
		}  else {
			cr<-NA
		}
		
		if (dis==1){
			out<-dbgr
		}else{
			out<-rbind.fill(out,dbgr)
		}
	}
	names(out)[1]<-"rowname"
	dbgr<-out
		
	if(length(x)==1){	
		tname<-paste0("Pairwise design based t-test; Valid n=", nrow(dstrat$variable)," p-values = ")
	}else{
			tname<-paste0("Valid n=", nrow(dstrat$variable))
	}
	
	return(list(data=dbgr, pvalue = cr , test.name = tname))	
}

process_categories <- function (x,y,dstrat,CL){
	f<-formula(paste0("~",x,"+",y))
	fx<-formula(paste0("~",x))
	fy<-formula(paste0("~",y))
	tname<-NA;stat<-NA;stdf<-NA;pval<-NA;test<-NA
	
	if(length(na.omit(unique(dstrat$variable[[y]])))==1)
	{
	  
	  dbgr<-data.frame(
	    rowname=unique(dstrat$variable[[x]]),
	    variable=na.omit(unique(dstrat$variable[[y]])),
	    value=1
	  )
	  
	}else{
	  
  	mytable<-svytable(f, dstrat, Ntotal = NULL, round = F)
  	
  	dbgr<-
  		mytable %>% 
  		apply(.,1,function(x){round(x/sum(x),3)}) %>% 
  		melt
  		
  	if(ncol(dbgr)==1){
  		dbgr$variable<-unique(dstrat$variable[[y]])
  		dbgr$rowname<-rownames(dbgr)
  	}else{
  		names(dbgr)[names(dbgr)%in%y]<-c("variable")
  		names(dbgr)[names(dbgr)%in%x]<-c("rowname")
  	}
  	
  	# try chi square
  	try({
  		test<-svychisq(f,dstrat,statistic = "Chisq")
  		tname<-test$method
  		stat<-round(test$statistic,2)
  		stdf<-test$parameter
  		pval<-round(test$p.value,3)
  	},silent=TRUE)
  	
  	a<-NA
  	try(a<-confint(svyby(fy,fx, dstrat,svymean,na.rm.all=T,na.rm.by=T,drop.empty.groups=F),level=CL),silent=TRUE)
  	
  	if (ncol(dbgr)==1){
  		dbgr<-cbind(dbgr,a)
  		dbgr$rownname<-rownames(dbgr)
  	} else if(!all(is.na(a))){
  		dbgr<-merge_cfint(a,dbgr,y)
  	}
	
	}
	
			
	dbgr<-dbgr[!is.na(dbgr$value),]
	dbgr[,1]<-factor(dbgr[,1], levels=sort(unique(dbgr[,1]), decreasing=TRUE))
	dbgr[,2]<-factor(dbgr[,2], levels=sort(unique(dbgr[,2]), decreasing=TRUE))
 
	
	tname<-paste0(tname,", p-value=",pval,"; Valid n: ",nrow(dstrat$allprob),"\n\n\n")
	statis<-pval
	
	return(list(data=dbgr, pvalue = statis , test.name = tname))	
}

# aggregation select multiple
process_smultiple<-function (x,y,dstrat,CL){

	for (dis in 1:length(x)) 
	{
		level<-tail(strsplit(x[dis],"_._")[[1]],1)
		
		factlev<-'yes'
		output<-process_categories(y,x[dis],dstrat,CL)
		dbgr<-output$data
		dbgr<-dbgr[dbgr$variable==factlev,]
		dbgr$variable<-rep(level,nrow(dbgr))
		
		if (dis==1){
			out<-dbgr
		}else{
			out<-rbind.fill(out,dbgr)
		}
	}
  test="None"
  pval="None"
	dbgr<-out
	return(list(data=dbgr, pvalue = pval , test.name = test))	
}

# aggregation with multiple disaggregation
process_mutl_disa<-function(x,y,dstrat,CL,nb){

	f<-formula(paste0("~",x,"+",paste0("interaction(",paste(y,collapse=","),")")))
	ft<-formula(paste0(x,"~",paste0("interaction(",paste(y,collapse=","),")")))
	fx<-formula(paste0("~",x))
	fy<-formula(paste0("~",paste0("interaction(",paste(y,collapse=","),")")))

	if(nb=="no"){
		process_categories(y,x,dstrat,CL)
	} else if(nb=="yes"){
		process_num(x,y,dstrat,CL)	
	}
}	


