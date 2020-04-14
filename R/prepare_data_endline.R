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

# recode variables to character in order to retrieve Confidence Intervals
endline_data <- endline_data %>% 
  mutate(gender_respondent = case_when(
                  gender_respondent == 1 ~ "male",
                  gender_respondent == 2 ~ "female",
                  TRUE ~ NA_character_),
    community_helping = case_when(
                  community_helping == 1 ~ "never", 
                  community_helping == 2 ~ "few_times", 
                  community_helping == 3 ~ "not_often", 
                  community_helping == 4 ~ "usually", 
                  community_helping == 5 ~ "always", 
                  community_helping == 98 ~ "dont_know", 
                  community_helping == 99 ~ "refuse_answer", TRUE ~ NA_character_),
    common_problem = recode_factor(common_problem, 
                 `1` = "lack_livlihoods", 
                 `2` = "lack_schools", 
                 `3` = "lack_healthcare", 
                 `4` = "lack_safety", 
                 `5` = "lack_assistance",
                 `6` = "lack_legal",
                 `7` = "harassment_auth",
                 `8` = "harassment_local",
                 `9` = "phys_sec",
                 `10` = "other",
                 `11` = "dont_know",
                 `12` = "prefer_not_say",
                 `13` = "no_problems",
                 .default = NA_character_),
    assistance_received = recode_factor(assistance_received, 
                  `0` = "no_assistance", 
                  `1` = "cash", 
                  `2` = "agri_farm", 
                  `3` = "food", 
                  `4` = "water", 
                  `5` = "hh_items",
                  `6` = "shelt_mat",
                  `7` = "health",
                  `8` = "education",
                  `9` = "legal_assistance",
                  `10` = "trainings",
                  `11` = "return_package",
                  `12` = "other",
                  .default = NA_character_)
  )

# displacement status
endline_data <- endline_data %>% 
                mutate(final_displace_status = case_when(
                  calc_idp      == 1 & consent == 1 ~ "idp",
                  calc_returnee == 1 & consent == 1 ~ "returnee",
                  calc_remainee == 1 & consent == 1 ~ "remainee",
                  consent == 1 ~ 'moved_other_reason',
                  TRUE ~ NA_character_))

###### recode yn answers
recode_yn <- function (x) {
  recode_factor(x, `1`= "no", `2`="yes", `98` = "dont_know", `99` = "refuse_answer", .default = NA_character_)
}

# import likert vars
yn_endline <- read.csv("input/col_names/yn_var.csv") 

# col names as vector
yn_endline <- yn_endline %>% 
  column_to_rownames(var = "yn_var") %>% 
  row.names()

# recode likert var into characters
endline_data <- endline_data %>% 
  mutate_at(yn_endline, recode_yn)


############## recode likert vars ####################

# recode functions
# likert variables to char
recode_likert <- function (x) {
  recode_factor(x, `1`="strongly_disagree", `2`="disagree", `3`="neutral", `4`="agree", `5`="strongly_agree", `98` = "dont_know", `99` = "refuse_answer", .default = NA_character_)
}

# reverse negatively worded questions
recode_neg <- function (x) {
  recode_factor(x, `1`=5, `2`=4, `3`=3, `4`=2, `5`=1, .default = NA_real_)
}

# categorize into disagree, neutral, agree
recode_binary <- function (x) {
  recode_factor(x, `1`="negative", `2`="negative", `4`="positive", `5`="positive", .default = NA_character_)
}

#####################################################
# import likert vars
likert <- read.csv("input/col_names/likert_var.csv") 

# col names as vector
likert_var_endline <- likert %>% 
  column_to_rownames(var = "likert_var") %>% 
  row.names()

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

# join reversed variables to dataset
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

# recode likert var in characters
endline_data <- endline_data %>% 
  mutate_at(likert_var_endline, recode_likert)

############################################
# write recoded dataset for use in analysis
write.csv(endline_data, "input/data/recoded/endline/giz_endline_recoded_data.csv", row.names = F)
