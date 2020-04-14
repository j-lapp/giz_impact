#########################################
##### GIZ IMPACT ASSESSMENT  ############
#########################################

# Date: Mar/2020

# libraries
library(tidyverse)
library(lubridate)
library(parallel)
library(readxl)

##################################################################################################
############# ENDLINE ###########################################################################

############# load endline data
endline_data <- read.csv("input/data/endline/REACH_IRQ_GIZ_HH_Survey_February2020_analysis purpose.csv", 
                 stringsAsFactors = F, 
                 na.strings = c("", "na", "NA", "NaN", "#N/A"))

# column names 
colname_df <- read.csv("input/col_names/endline_colnames.csv")
names(endline_data) <- colname_df$names_without_groups

###### prepare new variables

# as documented in analysis guidance, these security variables are calculated from other variables
endline_data <- endline_data %>%
  mutate(security_1 = coexistence_1,
         security_2 = trust_1,
         security_3 = quality_4)

# complete binary for displacement status percent 
endline_data <- endline_data %>% 
  mutate(Displace_Status_idp      = ifelse(is.na(Displace_Status_idp) & consent == 1, 0, Displace_Status_idp),
         Displace_Status_returnee = ifelse(is.na(Displace_Status_returnee) & consent == 1, 0, Displace_Status_returnee))

# displacement status
endline_data <- endline_data %>% 
                mutate(final_displace_status = case_when(
                  calc_idp      == 1 & consent == 1 ~ "idp",
                  calc_returnee == 1 & consent == 1 ~ "returnee",
                  calc_remainee == 1 & consent == 1 ~ "remainee",
                  consent == 1 ~ 'moved_other_reason',
                  TRUE ~ NA_character_))


############## recode likert vars ####################

# recode functions
# reverse negatively worded questions
recode_neg <- function (x) {
  recode_factor(x, `1`=5, `2`=4, `3`=3, `4`=2, `5`=1, .default = NA_real_)
}

# categorize into disagree, neutral, agree
recode_binary <- function (x) {
  recode_factor(x, `1`="negative", `2`="negative", `4`="positive", `5`="positive", .default = NA_character_)
}

# import likert vars
likert <- read.csv("input/col_names/likert_var.csv") 

# positive variables
likert_var_pos <- likert %>% 
  filter(pos_neg == "pos")%>% 
  column_to_rownames(var = "likert_var") %>% 
  row.names()

# negative variables
likert_var_neg <- likert %>% 
  filter(pos_neg == "neg")%>% 
  column_to_rownames(var = "likert_var") %>% 
  row.names()

# new df for likert var, reversing negatively worded questions
likert_var_rev <- endline_data %>% 
  select(X_uuid, all_of(likert_var_neg))%>%  
  mutate_at(likert_var_neg, recode_neg)

colnames(likert_var_rev) <- paste0(colnames(likert_var_rev), "_reversed")
colnames(likert_var_rev)[1] <- "X_uuid"

# join to dataset
endline_data <- endline_data %>% 
  left_join(likert_var_rev, by = "X_uuid")

####### new df for likert var categorized into positive and negative sentiments
likert_var_cat <- endline_data %>% 
  select(X_uuid, all_of(likert_var_pos))%>%  
  mutate_at(likert_var_pos, recode_binary)

colnames(likert_var_cat) <- paste0(colnames(likert_var_cat), "_cat")
colnames(likert_var_cat)[1] <- "X_uuid"

# join to dataset
endline_data <- endline_data %>% 
  left_join(likert_var_cat, by = "X_uuid")

############################################
# write recoded dataset for use in analysis
write.csv(endline_data, "input/data/recoded/endline/giz_endline_recoded_data.csv", row.names = F)
