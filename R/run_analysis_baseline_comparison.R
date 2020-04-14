
source("R/functions/small_function.r")
source("R/functions/surveymodule.r")

session<-"GIZ_2020_comparison"
lang="english"


library(dplyr);library(scales);library(readxl);library(srvyr)

####### BASELINE #########
##########################################################################################
# get the data
data <- read.csv("input/data/recoded/baseline/giz_baseline_recoded_data.csv",check.names=F)
names(data) <- gsub(x = names(data), pattern = "\\.", replacement = "/")

# Load Analysis Plan
log <- read_excel("input/analysisplan/baseline/GIZ_IE_analysis plan_all indicators_R_ordered_baseline.xlsx")
log$analysis_key<-paste0(log$xi,"/",log$yi)

questions <- read_excel("input/questionnaire/endline/REACH_IRQ_GIZ_HH_survey_FINAL.xlsx", sheet = 'survey')
choices   <- read_excel("input/questionnaire/endline/REACH_IRQ_GIZ_HH_survey_FINAL.xlsx", sheet = 'choices')

sampling_frame <- read.csv("input/sampling_frame/GIZ_sampling_frame_comparison.csv")

data <- create_weights(sampling_frame, data)

# export to check weights
write.csv(data,"input/data/data_w_weight_baseline.csv")

# load the parameters
analysis.params<-parametres(data,log,questions=questions,choices=choices,lang=lang)
check_param(analysis.params)


##########################################################################################
# process the results
results <- lapply(seq_along(analysis.params$xi),boom_rmd,params=analysis.params)

check_create_directory("RDS")
saveRDS(results,sprintf("RDS/baseline/results_for_request_%s_baseline.RDS",today()))


##########################################################################################
# extract the data results
data_results <- lapply(results, function(x){x$data}) %>% do.call(rbind.fill,.)

baseline_results <- data_results

write.csv(baseline_results,sprintf("results/baseline/AnalysisResults_longformat_%s_baseline.csv",today()), row.names = FALSE)


##########################################################################################
# create data merge
source("R/functions/function_data_merge.R")
check_create_directory("datamerge")

mergelevel<-unique(log$yi)

lapply(mergelevel,function(mergelevel,data_results){
  torank <- NULL
  # extract results for chosen level
  res<-data_results[data_results$yi==mergelevel,]
  datamerge<-create_the_data_merge(results=res,label="variable_label",vartorank=torank)
  datamerge$level<-mergelevel
  return(datamerge)
},data_results=data_results) %>% bind_rows -> datamerge

datamerge_baseline <- datamerge

####### ENDLINE DATA ANALYZING ONLY FOR SAME INDICATORS #########
##########################################################################################
# get the data
data <- read.csv("input/data/recoded/endline/giz_endline_recoded_data.csv",check.names=F)
names(data) <- gsub(x = names(data), pattern = "\\.", replacement = "/")

data <- create_weights(sampling_frame, data)

# export to check weights
write.csv(data,"input/data/data_w_weight_endline_comp.csv")

# load the parameters
analysis.params<-parametres(data,log,questions=questions,choices=choices,lang=lang)
check_param(analysis.params)


##########################################################################################
# process the results
results<-lapply(seq_along(analysis.params$xi),boom_rmd,params=analysis.params)
check_create_directory("RDS")
saveRDS(results,sprintf("RDS/endline/results_for_request_%s_endline_comp.RDS",today()))


##########################################################################################
# extract the data results
data_results<- lapply(results, function(x){x$data}) %>% do.call(rbind.fill,.)

endline_results <- data_results

write.csv(endline_results,sprintf("results/endline/GIZ_AnalysisResults_longformat_%s_endline_comp.csv",today()), row.names = FALSE)


##########################################################################################
# create data merge
source("R/functions/function_data_merge.R")
check_create_directory("datamerge")

mergelevel<-unique(log$yi)

lapply(mergelevel,function(mergelevel,data_results){
  torank <- NULL
  # extract results for chosen level
  res<-data_results[data_results$yi==mergelevel,]
  datamerge<-create_the_data_merge(results=res,label="variable_label",vartorank=torank)
  datamerge$level<-mergelevel
  return(datamerge)
},data_results=data_results) %>% bind_rows -> datamerge

datamerge_endline <- datamerge

comparison_datamerge <- bind_rows(datamerge_baseline,datamerge_endline)

write.csv(comparison_datamerge,sprintf("datamerge/datamerge_format_%s_comparison.csv",today()), row.names = FALSE)


