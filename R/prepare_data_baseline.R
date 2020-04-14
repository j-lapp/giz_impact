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
############# BASELINE ###########################################################################

############# load baseline data
baseline_data <- read.csv("input/data/baseline/REACH_IRQ_GIZ_dataset_cleaned_011019_compiled_translated_clean.csv", 
                 stringsAsFactors = F, 
                 na.strings = c("", "na", "NA", "NaN", "#N/A"))

# column names 
colname_df <- read.csv("input/col_names/baseline_colnames.csv")
names(baseline_data) <- colname_df$names_without_groups

###### prepare new variables

# as documented in analysis guidance, these security variables are calculated from other variables
baseline_data <- baseline_data %>%
  mutate(security_1 = coexistence_1,
         security_2 = trust_1,
         security_3 = quality_4)

# complete binary for displacement status percent 
baseline_data <- baseline_data %>% 
  mutate(Displace_Status_idp      = ifelse(is.na(Displace_Status_idp) & consent == 1, 0, Displace_Status_idp),
         Displace_Status_returnee = ifelse(is.na(Displace_Status_returnee) & consent == 1, 0, Displace_Status_returnee))

# recode district name
baseline_data <- baseline_data %>% 
  mutate(District_GPS = case_when(
                                          District_GPS == 1 ~ "Al-Mosul",
                                          District_GPS == 2 ~ "Al-Hamdaniya",
                                          District_GPS == 3 ~ "Sinjar",
                                          District_GPS == 4 ~ 'Telafar',
                                          District_GPS == 5 ~ 'Tilkaef',
                                          TRUE ~ NA_character_))

# displacement status
baseline_data <- baseline_data %>% 
  mutate(final_displace_status = case_when(
                                          calc_idp      == 1 & consent == 1 ~ "idp",
                                          calc_returnee == 1 & consent == 1 ~ "returnee",
                                          calc_remainee == 1 & consent == 1 ~ "remainee",
                                          consent == 1 ~ 'moved_other_reason',
                                          TRUE ~ NA_character_))


########## recode likert vars ####################

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
likert_baseline <- read.csv("input/col_names/likert_var.csv") %>% 
  filter(baseline_endline == "both")

# positive likert_baseline
likert_var_pos <- likert_baseline %>% 
  filter(pos_neg == "pos")%>% 
  column_to_rownames(var = "likert_var") %>% 
  row.names()

# negative variables
likert_var_neg <- likert_baseline %>% 
  filter(pos_neg == "neg")%>% 
  column_to_rownames(var = "likert_var") %>% 
  row.names()

# new df for likert var, reversing negatively worded questions
likert_var_rev <- baseline_data %>% 
  select(X_uuid, all_of(likert_var_neg))%>%  
  mutate_at(likert_var_neg, recode_neg)

colnames(likert_var_rev) <- paste0(colnames(likert_var_rev), "_reversed")
colnames(likert_var_rev)[1] <- "X_uuid"

# join to dataset
baseline_data <- baseline_data %>% 
  left_join(likert_var_rev, by = "X_uuid")

####### new df for likert var categorized
likert_var_cat <- baseline_data %>% 
  select(X_uuid, all_of(likert_var_pos))%>%  
  mutate_at(likert_var_pos, recode_binary)

colnames(likert_var_cat) <- paste0(colnames(likert_var_cat), "_cat")
colnames(likert_var_cat)[1] <- "X_uuid"

######## join to dataset
baseline_data <- baseline_data %>% 
  left_join(likert_var_cat, by = "X_uuid")

############################################
# write recoded dataset for use in analysis
write_excel_csv(baseline_data, "input/data/recoded/baseline/giz_baseline_recoded_data.csv")
