## clear plots
if(!is.null(dev.list()))dev.off()

## clean workspace
rm(list=ls())

## set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library("dplyr")
library("stargazer")
library("xtable")
data_dir<-"../dataset/"
output  <-"../output/"
data <- readRDS(paste0(data_dir, "processed/final_data.rds"))

##----------------mean and standard deviation by year, district---------------##
##------------------Variables used in construction of hvi---------------------##
  
  mean_sd_1 <- data %>%
  group_by(year, district) %>%
  summarise(
            mean_hh_age          = mean(hh_age),
            sd_hh_age            = sd(hh_age),
            mean_hh_edu          = mean(hh_edu),
            sd_hh_edu            = sd(hh_edu),
            mean_max_hh_edu      = mean(max_hh_edu),  # Fix: Add closing parenthesis here
            sd_max_hh_edu        = sd(max_hh_edu),
            mean_hh_caste        = mean(hh_caste),
            sd_hh_caste          = sd(hh_caste),
            mean_male_adult_no   = mean(male_adult_no),
            sd_male_adult_no     = sd(male_adult_no),
            mean_female_adult_no = mean(female_adult_no),
            sd_female_adult_no   = sd(female_adult_no),
            mean_child_no        = mean(child_no),
            sd_child_no          = sd(child_no),
            mean_elders_no       = mean(elders_no),
            sd_elders_no         = sd(elders_no),
            mean_implements      = mean(implements),
            sd_implements        = sd(implements),
            mean_livestock       = mean(livestock),
            sd_livestock         = sd(livestock),
            mean_land            = mean(land),
            sd_land              = sd(land),
            mean_total_income    = mean(total_income),
            sd_total_income      = sd(total_income),
            mean_bank_saving     = mean(bank_saving),
            sd_bank_saving       = sd(bank_saving),
            mean_jewellery       = mean(jewellery),
            sd_jewellery         = sd(jewellery),
            mean_livelihood_no   = mean(livelihood_no),
            sd_livelihood_no     = sd(livelihood_no),
            mean_hvi             = mean(hvi),
            sd_hvi               = sd(hvi)) %>%
  mutate(   mean_sd_hh_age          = paste(mean_hh_age, " (", sd_hh_age, ")"),
            mean_sd_hh_edu          = paste(mean_hh_edu, " (", sd_hh_edu, ")"),
            mean_sd_max_hh_edu      = paste(mean_max_hh_edu, " (", sd_max_hh_edu, ")"),
            mean_sd_hh_caste        = paste(mean_hh_caste, " (", sd_hh_caste, ")"),
            mean_sd_male_adult_no   = paste(mean_male_adult_no, " (", sd_male_adult_no, ")"),
            mean_sd_female_adult_no = paste(mean_female_adult_no, " (", sd_female_adult_no, ")"),
            mean_sd_child_no        = paste(mean_child_no, " (", sd_child_no, ")"),
            mean_sd_elders_no       = paste(mean_elders_no, " (", sd_elders_no, ")"),    
            mean_sd_implements      = paste(mean_implements, " (", sd_implements, ")"),
            mean_sd_livestock       = paste(mean_livestock, " (", sd_livestock, ")"),
            mean_sd_land            = paste(mean_land, " (", sd_land, ")"),
            mean_sd_total_income    = paste(mean_total_income, " (", sd_total_income, ")"),
            mean_sd_bank_saving     = paste(mean_bank_saving, " (", sd_bank_saving, ")"),
            mean_sd_jewellery       = paste(mean_jewellery, " (", sd_jewellery, ")"),
            mean_sd_livelihood_no   = paste(mean_livelihood_no, " (", sd_livelihood_no, ")"),
            mean_sd_hvi             = paste(mean_hvi, " (", sd_hvi, ")"))

  # mean_sd_hvi<- stargazer(
  #   mean_sd_1,
  #   type = 'latex', # Specify LaTeX output
  #   summary = FALSE, # Treat as data frame, not summary statistics
  #   title = "HVI construction variables",
  #   align = TRUE,
  #   out = paste0(output, "summary_statistics/mean_sd_hvi.htm") # Specify the output file
  # )

a<-xtable(mean_sd_1)
save(a, file = "D:/Research/Sanjeev_Thesis_to_Paper/output/summary_statistics/mean_sd_hvi.tex")
##============================================================================##

##------------------------Environmental dependence and hvi--------------------##

mean_sd_2 <- data %>%
  group_by(year, district, vdc) %>%
  summarise(
            mean_env_dependence      = mean(env_tot_income_ratio),
            sd_env_dependence        = sd(env_tot_income_ratio),
            mean_dependency_ratio    = mean(dependency_ratio),
            sd_dependency_ratio      = sd(dependency_ratio),
            mean_debt                = mean(debt),  # Fix: Add closing parenthesis here
            sd_debt                  = sd(debt),
            mean_shock               = mean(shocks_no),
            sd_shock                 = sd(shocks_no)) %>%
  mutate(   mean_sd_env_dependence   = paste(mean_env_dependence, " (", sd_env_dependence, ")"),
            mean_sd_dependency_ratio = paste(mean_dependency_ratio, " (", sd_dependency_ratio, ")"),
            mean_sd_debt             = paste(mean_debt, " (", sd_debt, ")"),
            mean_sd_shock            = paste(mean_shock, " (", sd_shock, ")"))

xtable(mean_sd_2, include.rownames = T)
print(xtable, file = "../output/summary_statistics/mean_sd_env_hvi.tex")

