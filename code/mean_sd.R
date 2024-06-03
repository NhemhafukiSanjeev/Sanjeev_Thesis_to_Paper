## clear plots
if(!is.null(dev.list()))dev.off()

## clean workspace
rm(list=ls())

## set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library("dplyr")
data_dir<-"../dataset/raw/"
output  <-"../../output/"
data <- read.csv(paste0(data_dir, "data_pen.csv"))

##----------------------------removing missing data-----------------------------
no_missing_data<-na.omit(data)

##---------------------------renaming variable names----------------------------
renamed_data<-no_missing_data |> 
  rename(hh_code          = household_code,
         vdc              = village_,
         district         = district_,
         hh_age           = Age_head_,
         hh_edu           = Head_educ_,
         hh_caste         = Head_belong_Biggest_caste_,
         max_hh_edu       = Max_HH_educ_,
         child_no         = num_of_children_,
         male_adult_no    = num_of_male_adults_,
         female_adult_no  = num_of_female_adults_,
         elders_no        = num_of_elders_,
         implements       = Total_imp_value_aeu_,
         livestock        = Tot_livstk_end_val_aeu_,
         land             = Total_land_owned_sqm_aeu_,
         bank_saving      = Bank_saving_aeu_,
         jewellery        = Jewellery_aeu_,
         debt             = debt_aeu_,
         env_income       = Tot_env_inc_aeu_,
         crop_income      = Tot_crop_inc_aeu_,
         livestock_income = Tot_live_inc_aeu_,
         remit_income     = Remit_inc_aeu_,
         support_income   = Tot_suppot_inc_aeu_,
         other_income     = Tot_other_inc_aeu_,
         biz_income       = Biz_inc_aeu_,
         wage_income      = Tot_wage_inc_aeu_,
         total_income     = Total_inc_aeu_) |> 
  select(hh_code:elders_no, implements, livestock, land, bank_saving, jewellery,
         debt, env_income, crop_income, livestock_income, remit_income, support_income, 
         other_income, biz_income, wage_income, total_income, everything()) |> 
  select(-c(contains("High_")   | contains("low_") | contains("other_imp")| 
            contains("livstk")  | contains("hives")| contains("sqm")| 
            contains("inc_aeu_")| contains("aeu_")))

##---------------Counting number of shocks and livelihoods----------------------
##------------------------------------Shocks------------------------------------
count_data<-renamed_data |> 
  mutate(shock_less_severe  = Serious_crop_fail_ + Serious_illness_+ Death_adult_
                              + land_loss_ + Livestock_loss_ + Oth_asset_loss_ 
                              + wage_employ_loss_ + Costly_social_events_,
         shock_more_severe  = Serious_crop_fail_severe_ + Serious_illness_severe_+
                              Death_adult_severe_ + Land_loss_severe_ + 
                              Livestock_loss_severe_ +  Oth_asset_loss_severe_ + 
                              wage_employ_loss_severe_ + Costly_social_events_severe_,
         shocks_no          = shock_less_severe + shock_more_severe)  |> 
  select(-c(contains("Serious") | contains("Death_")|
            contains("loss_")   | contains("_social")|contains("_severe")))

##------------------------------------Livelihoods-------------------------------
#############Counting the variables with non-zero values##########

##----------------------Selecting the variables of interest to count------------
selected_vars<-c("env_income",   "crop_income",    "livestock_income", 
                 "remit_income", "support_income", "other_income", 
                 "biz_income",   "wage_income")

##--------Counting the Livelihoods(variables with non-zero columns)-------------
livelihood_data <- count_data %>% 
  mutate(livelihood_no = rowSums(select(., selected_vars) != 0))

##-----------------------------grouping data by hh_code-------------------------
grouped_data<-livelihood_data[order(livelihood_data$hh_code),]

##------identification of panel data to remove the non-panel observations-------
panel_identifier<-grouped_data |>
  group_by(hh_code) |> 
  summarise(count_yr    = n_distinct(year)) |> 
  mutate(all_yr_present = case_when(count_yr == 3 ~ T,
                                            TRUE  ~F))
##---------------joining panel identifier data and grouped data-----------------
panel_id_data<-left_join(livelihood_data, panel_identifier, by = "hh_code")

##----------removing the non-panel data and keep full panel data only-----------
full_panel<-panel_id_data |> 
  filter(all_yr_present == TRUE) |> 
  group_by(hh_code) |> 
  select(-c(contains("yr") |contains("Head_")))



##-------------------------Applying Mini-max/Maxi-min scaling-------------------
##------------------------Setting the formulae for the scaling------------------
##------------------------------Mini-max scaling--------------------------------
  min_max_scale <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }

##-----------------------------Maxi-min scaling---------------------------------
  max_min_scale <- function(x) {
    (max(x) - x) / (max(x) - min(x))
  }

## Specifying the mini-max and maxi-min variables based on the upward and downward
## influence of the variables on the Household Vulnerability
----------Mini-max variabls----------
  min_max_vars <- c("HHH_age", "HHH_Female", "HHH_village_born", "HHH_Widow/er", "HHH_Divorced", "HHH_Spouse_working_away", "HHH_never_married", "HHH_other_marital_status", "No_of_Children", "No_of_female_adults", "No_of_elders",  "Debt")

------------Maxi-min variables--------
  max_min_vars <- c("HHH_Edu", "HHH_Biggest_Caste", "HHH_Married_Living_together", "HH_Max_Edu", "No_male_adults" , "Bank_saving", "Jewellery", "Total_land_owned_sqm", "Total_inc")

########## Creating Dataframe after scaling ###########
SOVI_scaled <- SOVI_Data %>% mutate(across(all_of(min_max_vars), min_max_scale), across(all_of(max_min_vars), max_min_scale))

#######Adding the variables#########
SOVI_scaled <- SOVI_scaled %>%
  mutate(SOV_index = HHH_age + HHH_Female + + HHH_Edu + HHH_village_born + HHH_Biggest_Caste + HHH_Married_Living_together + HHH_Widow_er + HHH_Divorced + HHH_Spouse_working_away + HHH_never_married + HHH_other_marital_status + HH_Max_Edu + No_of_Children + No_male_adults + No_of_female_adults + No_of_elders + Bank_saving + Jewellery + Debt + Total_land_owned_sqm + Total_inc)
























##---------getting mean and standard deviation by year, district and vdc--------
  library(dplyr)
HVI_stats_year_district_village <- full_panel %>%
  group_by(year, distreict, vdc) %>%
  summarise(
    mean_HHH_Age = mean(HHH_Age),
    sd_HHH_Age = sd(HHH_Age),
    mean_HHH_edu = mean(HHH_edu),
    sd_HHH_edu = sd(HHH_edu),
    mean_Max_HH_edu = mean(Max_HH_edu),  # Fix: Add closing parenthesis here
    sd_Max_HH_edu = sd(Max_HH_edu),
    mean_Male_adults = mean(Male_adults),
    sd_Male_adults = sd(Male_adults),
    mean_Female_adults = mean(Female_adults),
    sd_Female_adults = sd(Female_adults),
    mean_Child_No. = mean(Child_No.),
    sd_Child_No. = sd(Child_No.),
    mean_Elders_No. = mean(Elders_No.),
    sd_Elders_No. = sd(Elders_No.),
    mean_Total_imp_value_aeu_ = mean(Total_imp_value_aeu_),
    sd_Total_imp_value_aeu_ = sd(Total_imp_value_aeu_),
    mean_Tot_livstk_end_val_aeu_ = mean(Tot_livstk_end_val_aeu_),
    sd_Tot_livstk_end_val_aeu_ = sd(Tot_livstk_end_val_aeu_),
    mean_Land_owned_sqm = mean(Land_owned_sqm),
    sd_Land_owned_sqm = sd(Land_owned_sqm),
    mean_Total = mean(Total),
    sd_Total = sd(Total),
    mean_Bank_saving = mean(Bank_saving),
    sd_Bank_saving = sd(Bank_saving),
    mean_Jewellery = mean(Jewellery),
    sd_Jewellery = sd(Jewellery),
    mean_Livelihood_Count = mean(Livelihood_Count),
    sd_Livelihood_Count = sd(Livelihood_Count),
    mean_HVI = mean(HVI),
    sd_HVI = sd(HVI),
    mean_HH_Marital_Status = mean(HH_Marital_Status),
    sd_HH_Marital_Status = sd(HH_Marital_Status),
    mean_HHH_biggest_caste = mean(HHH_biggest_caste),
    sd_HHH_biggest_caste = sd(HHH_biggest_caste),
  ) %>%
  mutate(
    mean_sd_HHH_Age = paste(mean_HHH_Age, " (", sd_HHH_Age, ")"),
    mean_sd_HHH_edu = paste(mean_HHH_edu, " (", sd_HHH_edu, ")"),
    mean_sd_Max_HH_edu = paste(mean_Max_HH_edu, " (", sd_Max_HH_edu, ")"),
    mean_sd_Male_adults = paste(mean_Male_adults, " (", sd_Male_adults, ")"),
    mean_sd_Female_adults = paste(mean_Female_adults, " (", sd_Female_adults, ")"),
    mean_sd_Child_No. = paste(mean_Child_No., " (", sd_Child_No., ")"),
    mean_sd_Elders_No. = paste(mean_Elders_No., " (", sd_Elders_No., ")"),    
    mean_sd_Total_imp_value_aeu_ = paste(mean_Total_imp_value_aeu_, " (", sd_Total_imp_value_aeu_, ")"),
    mean_sd_Tot_livstk_end_val_aeu_ = paste(mean_Tot_livstk_end_val_aeu_, " (", sd_Tot_livstk_end_val_aeu_, ")"),
    mean_sd_Land_owned_sqm = paste(mean_Land_owned_sqm, " (", sd_Land_owned_sqm, ")"),
    mean_sd_Total = paste(mean_Total, " (", sd_Total, ")"),
    mean_sd_Bank_saving = paste(mean_Bank_saving, " (", sd_Bank_saving, ")"),
    mean_sd_Jewellery = paste(mean_Jewellery, " (", sd_Jewellery, ")"),
    mean_sd_Livelihood_Count = paste(mean_Livelihood_Count, " (", sd_Livelihood_Count, ")"),
    mean_sd_HVI = paste(mean_HVI, " (", sd_HVI, ")"),
    mean_sd_HH_Marital_Status = paste(mean_HH_Marital_Status, " (", sd_HH_Marital_Status, ")"),
    mean_sd_HHH_biggest_caste = paste(mean_HHH_biggest_caste, " (", sd_HHH_biggest_caste, ")")
  )
library(openxlsx)
write.xlsx(HVI_stats_year_district_village_, file ="D:\\Research\\A unique environmental augmented household-level livelihood dataset from Nepal\\Thesis\\Summary stats\\Not scaled HVI stats\\Not-Sclaed HVI stats.xlsx")
