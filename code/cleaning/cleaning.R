## clear plots
if(!is.null(dev.list()))dev.off()

## clean workspace
rm(list=ls())

## set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library("dplyr")
library("stargazer")
data_dir<-"../dataset/raw/"
output  <-"/../output/"
data <- read.csv(paste0(data_dir, "data_pen.csv"))

##----------------------------removing missing data-----------------------------
no_missing_data<-na.omit(data)
saveRDS(no_missing_data, "../dataset/intermediate/no_missing_data.rds")
##---------------------------renaming variable names----------------------------
renamed_data<-no_missing_data |> 
  rename(hh_code                 = household_code,
         vdc                     = village_,
         district                = district_,
         hh_age                  = Age_head_,
         hh_edu                  = Head_educ_,
         hh_caste                = Head_belong_Biggest_caste_,
         max_hh_edu              = Max_HH_educ_,
         child_no                = num_of_children_,
         male_adult_no           = num_of_male_adults_,
         female_adult_no         = num_of_female_adults_,
         elders_no               = num_of_elders_,
         implements              = Total_imp_value_aeu_,
         livestock               = Tot_livstk_end_val_aeu_,
         land                    = Total_land_owned_sqm_aeu_,
         bank_saving             = Bank_saving_aeu_,
         jewellery               = Jewellery_aeu_,
         debt                    = debt_aeu_,
         env_income              = Tot_env_inc_aeu_,
         crop_income             = Tot_crop_inc_aeu_,
         livestock_income        = Tot_live_inc_aeu_,
         remit_income            = Remit_inc_aeu_,
         support_income          = Tot_suppot_inc_aeu_,
         other_income            = Tot_other_inc_aeu_,
         biz_income              = Biz_inc_aeu_,
         wage_income             = Tot_wage_inc_aeu_,
         total_income            = Total_inc_aeu_) |> 
  select(hh_code:elders_no, implements, livestock, land, bank_saving, jewellery,
         debt, env_income, crop_income, livestock_income, remit_income, support_income, 
         other_income, biz_income, wage_income, total_income, everything()) |> 
  select(-c(contains("High_")   | contains("low_") | contains("other_imp")| 
              contains("livstk")  | contains("hives")| contains("sqm")| 
              contains("inc_aeu_")| contains("aeu_")))
saveRDS(renamed_data, "../dataset/intermediate/renamed_data.rds")
##---------------Counting number of shocks and livelihoods----------------------
##------------------------------------Shocks------------------------------------
count_data<-renamed_data |> 
  mutate(shock_less_severe       = Serious_crop_fail_ + Serious_illness_+ Death_adult_
         + land_loss_ + Livestock_loss_ + Oth_asset_loss_ 
         + wage_employ_loss_ + Costly_social_events_,
         shock_more_severe       = Serious_crop_fail_severe_ + Serious_illness_severe_+
           Death_adult_severe_ + Land_loss_severe_ + 
           Livestock_loss_severe_ +  Oth_asset_loss_severe_ + 
           wage_employ_loss_severe_ + Costly_social_events_severe_,
         shocks_no               = shock_less_severe + shock_more_severe)  |> 
  select(-c(contains("Serious") | contains("Death_") |
              contains("loss_")   | contains("_social")|contains("_severe")))
saveRDS(count_data, "../dataset/intermediate/shock_count_data.rds")
##------------------------------------Livelihoods-------------------------------

##----------------------Selecting the variables of interest to count------------
selected_vars<-c("env_income",   "crop_income",    "livestock_income", 
                 "remit_income", "support_income", "other_income", 
                 "biz_income",   "wage_income")

##--------Counting the Livelihoods(variables with non-zero columns)-------------
livelihood_data <- count_data %>% 
  mutate(livelihood_no      = rowSums(select(., selected_vars) != 0))
saveRDS(livelihood_data, "../dataset/intermediate/livelihood_count_data.rds")
##-----------------------------grouping data by hh_code-------------------------
grouped_data<-livelihood_data[order(livelihood_data$hh_code),]

##------identification of panel data to remove the non-panel observations-------
panel_identifier<-grouped_data |>
  group_by(hh_code) |> 
  summarise(count_yr             = n_distinct(year)) |> 
  mutate(all_yr_present          = case_when(count_yr == 3 ~ T,
                                             TRUE  ~ F))
##---------------joining panel identifier data and grouped data-----------------
panel_id_data<-left_join(livelihood_data, panel_identifier, by = "hh_code")

##----------removing the non-panel data and keep full panel data only-----------
full_panel<-panel_id_data |> 
  filter(all_yr_present == TRUE) |> 
  select(-c(contains("yr") |contains("Head_"), crop_income:wage_income)) |> 
  mutate(hh_id = paste(hh_code, year, sep = "-")) |> 
  select(hh_id, everything())

full_panel<-full_panel[order(full_panel$hh_code),]
saveRDS(full_panel, "../dataset/processed/full_panel.rds")

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
##-----------------------------Mini-max variables-------------------------------
min_max_vars <- c()

##---------------------------------Maxi-min variables---------------------------
max_min_vars <- c("hh_age",     "hh_edu",      "max_hh_edu", 
                  "implements", "livestock",   "land", 
                  "hh_caste",   "bank_saving", "jewellery", 
                  "livelihood_no")

########## Creating Dataframe after scaling ###########
prep_data <- full_panel %>% mutate(across(all_of(min_max_vars), min_max_scale), 
                                   across(all_of(max_min_vars), max_min_scale))
saveRDS(prep_data, "../dataset/processed/normalised_data.rds")
#######Adding the variables#########
hvi_data <- prep_data %>%
  mutate(hvi                     = (hh_age      + hh_edu      + max_hh_edu  + 
                                    implements  + livestock   + land        + 
                                    hh_caste    + bank_saving + jewellery   + 
                                    livelihood_no)/10,
         env_tot_income_ratio    = (env_income/total_income),
         env_tot_income_ratio    = case_when(env_tot_income_ratio == "NaN" ~ 0,
                                                                      TRUE ~ env_tot_income_ratio),
         env_tot_income_ratio_    = env_tot_income_ratio + 2.409811,  #2.409811 was added to make the negative values to 1 for log transformation
         ln_env_tot_income_ratio = log(env_tot_income_ratio_),
         dependency_ratio        = (child_no+elders_no)/(male_adult_no + female_adult_no),
         dependency_ratio        = case_when(dependency_ratio == "Inf" ~ 0,
                                                                  TRUE ~ dependency_ratio),
         ln_debt                 = log(debt),
         ln_debt                 = case_when(ln_debt == "-Inf" ~ 0,
                                                          TRUE ~ ln_debt),
         hh_id                   = paste(hh_code, year, sep = "-"))
join_data<-hvi_data |> 
  select(hh_id, hh_code, hvi, env_tot_income_ratio, ln_env_tot_income_ratio, dependency_ratio, ln_debt)


final_data<-left_join(full_panel, join_data, by = c("hh_id", "hh_code"))
saveRDS(final_data, "../dataset/processed/final_data.rds")
##============================================================================##