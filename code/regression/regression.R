## clear plots
if(!is.null(dev.list()))dev.off()

## clean workspace
rm(list=ls())

## set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

data_dir<-"../../dataset/"
output  <-"../../output/"
data <- readRDS(paste0(data_dir, "/processed/final_data.rds"))


library(plm)
library(stargazer)

##-----------------Decalre data to be a Panel-----------------------------------
pdata<-pdata.frame(data, index=c("year", "hh_id"))

##--------------------Setting pooled ols regression formulae--------------------

formula <- hvi ~ ln_env_tot_income_ratio

adding_variables <- c("ln_debt",      "dependency_ratio", "shocks_no", 
                      "factor(year)", "factor(district)",    "factor(vdc)")

models <- list()

# Loop to create regression models
for (i in 0:length(adding_variables)) {
  if (i == 0) {
    formula <- formula
  } else {
    formula <- as.formula(paste(deparse(formula), paste(adding_variables[1:i], collapse = " + "), sep = " + "))
  }
  models[[i + 1]] <- plm(formula, data = pdata, model = "pooling")
}

pols.1 <- models[[1]]
pols.2 <- models[[2]]
pols.3 <- models[[3]]
pols.4 <- models[[4]]
pols.5 <- models[[5]]
pols.6 <- models[[6]]
pols.7 <- models[[7]]


##--------Getting the pooled ols regression results into the latex--------------
  pols_results<- stargazer(
    pols.1, pols.2, pols.3, pols.4, pols.5, pols.6, pols.7,
    type = 'text', # Specify LaTeX output
    omit.stat = c("f"), #remove F statistics
    omit = c("Year", "Dist", "VDC"), #omit the fixed effects
    omit.labels = c("Year-fixed effects", "District-fixed effects", "VDC-fixed effects"),
    omit.yes.no = c("Yes", "No"),
    title = "Pooled OLS Regression",
    align = TRUE,
    out = paste0(output, "regression_results/Pooled_OLS_Regression.tex") # Specify the output file
  )

##============================================================================##
##-----------------Run pooled ols regression------------------------------------

# pOLS.1<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1, data=pdata, model="pooling")
# pOLS.2<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt, data=pdata, model="pooling")
# pOLS.3<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio, data=pdata, model="pooling")
# pOLS.4<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock, data=pdata, model="pooling")
# pOLS.5<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock + factor(Year), data=pdata, model="pooling")
# pOLS.6<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock + factor(Year)+ factor(Dist), data=pdata, model="pooling")
# pOLS.7<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock + factor(Year)+ factor(Dist) + factor(VDC), data=pdata, model="pooling")
# pOLS.8<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock + factor(Year), data=pdata, model="pooling")

# ***********Getting the results into the latex***************
# latex_code <- stargazer(
#   POLS.1, POLS.2, POLS.3, POLS.4, POLS.5, POLS.6, POLS.7,
#   type = 'text', # Specify LaTeX output
#   omit.stat = c("f"), #remove F statistics
#   omit = c("Year", "Dist", "VDC"), #omit the fixed effects
#   omit.labels = c("Year-fixed effects", "District-fixed effects", "VDC-fixed effects"),
#   omit.yes.no = c("Yes", "No"),
#   title = "Pooled OLS Regression",
#   align = TRUE,
#   out = "D:\\Research\\A unique environmental augmented household-level livelihood dataset from Nepal\\Thesis\\WRITE-UP\\Pooled_OLS_Regression.tex" # Specify the output file
# )

##--------------------Setting random effects regression formulae----------------

formula <- hvi ~ ln_env_tot_income_ratio

adding_variables <- c("ln_debt",      "dependency_ratio", "shocks_no", 
                      "factor(year)", "factor(district)",    "factor(vdc)")

models <- list()

# Loop to create regression models
for (i in 0:length(adding_variables)) {
  if (i == 0) {
    formula <- formula
  } else {
    formula <- as.formula(paste(deparse(formula), paste(adding_variables[1:i], collapse = " + "), sep = " + "))
  }
  models[[i + 1]] <- plm(formula, data = pdata, model = "random")
}

re.1 <- models[[1]]
re.2 <- models[[2]]
re.3 <- models[[3]]
re.4 <- models[[4]]
re.5 <- models[[5]]
re.6 <- models[[6]]
re.7 <- models[[7]]


##--------Getting the random effects regression results into the latex----------
re_results<- stargazer(
  re.1, re.2, re.3, re.4, re.5, re.6, re.7,
  type = 'text', # Specify LaTeX output
  omit.stat = c("f"), #remove F statistics
  omit = c("Year", "Dist", "VDC"), #omit the fixed effects
  omit.labels = c("Year-fixed effects", "District-fixed effects", "VDC-fixed effects"),
  omit.yes.no = c("Yes", "No"),
  title = "Random Effects Regression",
  align = TRUE,
  out = paste0(output, "regression_results/Random_Effects_Regression.tex") # Specify the output file
)
##============================================================================##

##--------------------Setting fixed effects regression formulae-----------------

formula <- HVI ~ ln_Env_Tot_Ratio_Trsfm1

adding_variables <- c("ln_Debt",      "Depndency_ratio", "shock", 
                      "factor(Year)", "factor(Dist)",    "factor(VDC)")

models <- list()

# Loop to create regression models
for (i in 0:length(adding_variables)) {
  if (i == 0) {
    formula <- formula
  } else {
    formula <- as.formula(paste(deparse(formula), paste(adding_variables[1:i], collapse = " + "), sep = " + "))
  }
  models[[i + 1]] <- plm(formula, data = pdata, model = "within")
}

fe.1 <- models[[1]]
fe.2 <- models[[2]]
fe.3 <- models[[3]]
fe.4 <- models[[4]]
fe.5 <- models[[5]]
fe.6 <- models[[6]]
fe.7 <- models[[7]]


##--------Getting the random effects regression results into the latex----------
re_results<- stargazer(
  fe.1, fe.2, fe.3, fe.4, fe.5, fe.6, fe.7,
  type = 'text', # Specify LaTeX output
  omit.stat = c("f"), #remove F statistics
  omit = c("Year", "Dist", "VDC"), #omit the fixed effects
  omit.labels = c("Year-fixed effects", "District-fixed effects", "VDC-fixed effects"),
  omit.yes.no = c("Yes", "No"),
  title = "Fixed Effects Regression",
  align = TRUE,
  out = paste0(output, "regression_results/Fixed_Effects_Regression.tex") # Specify the output file
)
# ***********Random Effects Regression**************
# RE.1<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1, data=Pdata, model="random")
# RE.2<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt, data=Pdata, model="random")
# RE.3<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio, data=Pdata, model="random")
# RE.4<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock, data=Pdata, model="random")
# RE.5<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock + factor(Year), data=Pdata, model="random")
# RE.6<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock + factor(Year)+ factor(Dist), data=Pdata, model="random")
# RE.7<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock + factor(Year)+ factor(Dist) + factor(VDC), data=Pdata, model="random")
# 
# ***********Getting the results into the latex***************
#   latex_code <- stargazer(
#     RE.1, RE.2, RE.3, RE.4, RE.5, RE.6, RE.7,
#     type = 'text', # Specify LaTeX output
#     omit.stat = c("f"), #remove F statistics
#     omit = c("Year", "Dist", "VDC"), #omit the fixed effects
#     omit.labels = c("Year-fixed effects", "District-fixed effects", "VDC-fixed effects"),
#     omit.yes.no = c("Yes", "No"),
#     title = "Random Effects Regression",
#     align = TRUE,
#     out = "D:\\Research\\A unique environmental augmented household-level livelihood dataset from Nepal\\Thesis\\WRITE-UP\\Random Effects Regression.tex" # Specify the output file
#   )
# 
# ***********Fixed Effects Regression**************
# FE.1<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1, data=Pdata,  model="within")
# FE.2<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt, index=c("HH_Code", "Year"), data=Pdata, model="within")
# FE.3<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio, data=Pdata, index=c("HH_Code", "Year"), model="within")
# FE.4<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock, data=Pdata, index=c("HH_Code", "Year"), model="within")
# FE.5<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock + factor(Year), data=Pdata, index=c("HH_Code", "Year"), model="within")
# FE.6<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock + factor(Year)+ factor(Dist), data=Pdata, index=c("HH_Code", "Year"), model="within")
# FE.7<-plm(HVI~ln_Env_Tot_Ratio_Trsfm1 + ln_Debt + Depndency_ratio + shock + factor(Year)+ factor(Dist) + factor(VDC), data=Pdata, index=c("HH_Code", "Year"), model="within")
# 
# ***********Getting the results into the latex***************
#   latex_code <- stargazer(
#    POLS.1, POLS.7, RE.7, FE.7,
#     type = 'text', # Specify LaTeX output
#     omit.stat = c("f"), #remove F statistics
#     omit = c("Year", "Dist", "VDC"), #omit the fixed effects
#     omit.labels = c("Year-fixed effects", "District-fixed effects", "VDC-fixed effects"),
#     omit.yes.no = c("Yes", "No"),
#     title = "Fixed Effects Regression",
#     align = TRUE,
#     out = "D:\\Research\\A unique environmental augmented household-level livelihood dataset from Nepal\\Thesis\\WRITE-UP\\Panel Data Regression.tex" # Specify the output file
#   )

# *************Remove rows***********
#   Pdata <- Pdata %>% filter(HH_Code != 1023180)
#   Pdata <- Pdata %>% filter(HH_Code != 1023093)

##------------------------Test the need for time fixed effects----------------##
  time_fe<-plmtest(pols.7, test="time")
  
  stargazer(time_fe, type = "latex", style = "default")
  time_fe <- stargazer(
    time_fe,
    type = "latex", # Specify LaTeX output
    title = "Lagrange Multiplier Test - (Honda) Time Effects Test",
    style = "default",
    out = paste0(output, "diagnostic_tests/time_effects_honda.tex") # Specify the output file
 )

##--------------F test the need for need for individual fixed effects-----------
  indv_fe<-plmtest(fe.7, effect = "individual")
  indv_fe <- stargazer(
    time_fe,
    type = "latex", # Specify LaTeX output
    title = "Lagrange Multiplier Test - (Honda) Time Effects Test",
    style = "default",
    out = paste0(output, "diagnostic_tests/time_effects_honda.tex") # Specify the output file
  )
##----------------------------BP-LM test for RE vs OLS------------------------##
  bp_lm<-plmtest(pols.7, type=c("bp"))

##---------------------------Hausman Test for FE vs RE------------------------##
  hausman<-phtest(re.7, fe.7)

Sankey<-readxl::read_excel("C:\\Users\\DELL\\Desktop\\Sankey.xlsx", sheet = "Sheet3")

install.packages("tidyverse")
library(tidyverse)
install.packages("networkD3")
library(networkD3)

Sankey_long <- df %>%
  pivot_longer(cols = starts_with("HVI_Cat_1"), names_to = "Year", values_to = "HVI_Cat_1") %>%
  filter(HVI_Cat1 != "Low")  # Filter out rows with "Low" HVI_Cat

latex_code <- stargazer(
 pFtest,
  type = 'text', # Specify LaTeX output
  title = "Breusch Pagan Lagrange Multiplier test for Random effects",
  out = "D:\\Research\\A unique environmental augmented household-level livelihood dataset from Nepal\\Thesis\\WRITE-UP\\BPLM.tex" # Specify the output file
)

pFtest <- plmtest((pols.4), type = "bp")

