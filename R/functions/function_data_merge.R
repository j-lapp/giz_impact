

# function to add the variables
transform_variable_to_rank<-function(df, torank, group, number_of_rank="all",variable="variable"){
  
  df<-df[df$xi%in%torank,]
  if(number_of_rank!="all"){
    df<-df[df$rank<=number_of_rank,]
  }
  
  # create index
  df$index<-paste(df$xi,df$rank,sep="-")
  # rank categories
  dh<-dcast(df, group ~ index, drop=TRUE, fill=NA,value.var=variable)
  names(dh)[2:ncol(dh)]<-paste0(names(dh)[2:ncol(dh)],"_category")
  # pull matching percentages  
  dg<-dcast(df, group ~ index, drop=TRUE, fill=0, fun=function(x){round(x,0)},value.var=c("value"))
  names(dg)[2:ncol(dg)]<-paste0(names(dg)[2:ncol(dg)],"_perc")
  di<-join_all(list(dg,dh),by="group")
  
  di[[group]]<-di$group
  di<-di[,-which(names(di)=="group")]
  
  return(di)
}



# recode in chartwell friendly format
rec_charwell<-function(x){ifelse(x==0,"",paste0("'+",round(x,0)))}


# create the column for the charwell graphs
format_chartwell<-function(dtm, group="disaggregation", variables=NULL) {
  if(is.null(variables)){
    data<-dtm[,-which(names(dtm)%in%group)]
    } else {
      data<-dtm[grep_mutl(names(dtm),variables)]
    }
    
    chartwell<-lapply(data,rec_charwell) %>% as.data.frame(check.names=F)
    names(chartwell)<-paste0(names(chartwell),"_forgraphs")
    chartwell[[group]]<-dtm[[group]]
    return(chartwell)
}



pivot_table<-function(results,disaggregation,label="variable",value="value"){
  
  	results$disaggregation<-as.character(results[[disaggregation]])
  	results$value<-results[[value]]
  	
  	if (disaggregation == "rowname"){
  	 results$rowname<-" "
  	}
  	
  	# create a unique key by result
  	results$key<-paste(results$xi,results$rowname,results[[label]],sep="-")
  
  	# select the variables to keep
  	results<-results[c("value","disaggregation","key")]
  	
  	results$value<-round(results$value,1)
  # check for duplicated entries in the log
  # duplicated(results) %>% which

	# convert to short format
	# dtm<-reshape2::dcast(results, disaggregation ~ key, drop=TRUE, fun.aggregate=first, value.var=c("value"), fill=0)
	dtm<-reshape2::dcast(results, disaggregation~factor(key, levels=unique(key)), drop=TRUE, fun.aggregate=first, value.var=c("value"), fill=0)
	dtm<-class_assess(dtm)
	return(dtm)
}


create_the_data_merge<-function(results,vartorank=NULL,label,cnt=F,valid_n=F){
 
  results %>% 
    pivot_table(.,"rowname",label=label) -> datamerge 
  names(datamerge)<-gsub("- -","- value -",names(datamerge))

  # datamerge %>% format_chartwell -> datamerge_graph 
  # datamerge<-merge(datamerge,datamerge_graph, by="disaggregation",all.x=T)

  mycount<-by(results$valid_n,results$rowname,max,na.rm=T,simplify=F) %>% unlist %>% as.data.frame()
  names(mycount)<-"samplesize"
  mycount$disaggregation<-row.names(mycount)
  datamerge<-merge(datamerge,mycount, by="disaggregation",all.x=T)
  
  
   if(cnt==T){
    results %>% 
      pivot_table(.,disaggregation="rowname",label="label",value="count") -> datamerge_cnt
    names(datamerge_cnt)<-gsub("- -","- cnt -",names(datamerge_cnt))
    
    datamerge<-merge(datamerge,datamerge_cnt, by="disaggregation",all.x=T)
  }
  
  if(valid_n==T){
    results %>% 
      pivot_table(.,disaggregation="rowname",label="label",value="valid_n") -> datamerge_vn
    names(datamerge_vn)<-gsub("- -","- valid_n -",names(datamerge_vn))
    datamerge<-merge(datamerge,datamerge_vn, by="disaggregation",all.x=T)
  }

  
  if(!is.null(vartorank)){  
    group<-"rowname"
    
    # function to add the variables
    ranked<-transform_variable_to_rank(results, vartorank, group,5,variable="variable_label")
    
    datamerge <-merge(datamerge,ranked,by.x="disaggregation",by.y="rowname")
    #datamerge %>% write.csv(.,"../datamerge/datamerge.csv",row.names = FALSE)
  }

  return(datamerge)
  
}

