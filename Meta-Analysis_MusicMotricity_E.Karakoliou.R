### Meta-Analysis: Music Listening Effects on Motricity in Parkinson's Disease

## Install packages & load libraries
install.packages(c("dplyr", "meta", "metafor", "metasens", "readxl")
library(meta)
library(metafor)
library(dplyr)
library(readxl)
library(metasens)
            
## Import data
data0 <- read_xlsx('/Users/evangelia_karakoliou/Documents/Thesis.xlsx')
View(data0)

# check data for issues (whether it is characters or numeric values)
lapply(data0, class)

# replace missing values with 0s
data0[is.na(data0)] = "0"
View(data0)

# convert characters into numeric values for all DV columns
library(dplyr)
df %>% 
  mutate_at(vars(mean_Ex_UPDRS, sd_Ex_UPDRS, mean_Co_UPDRS, sd_Co_UPDRS, 
                 mean_Ex_WS, sd_Ex_WS, mean_Co_WS, sd_Co_WS,
                 mean_Ex_Cadence, sd_Ex_Cadence, mean_Co_Cadence, sd_Co_Cadence), as.numeric)

num.cols <- c('mean_Ex_UPDRS', 'sd_Ex_UPDRS', 'mean_Co_UPDRS', 'sd_Co_UPDRS', 
              'mean_Ex_WS', 'sd_Ex_WS', 'mean_Co_WS', 'sd_Co_WS',
              'mean_Ex_Cadence', 'sd_Ex_Cadence', 'mean_Co_Cadence', 'sd_Co_Cadence')
data0[num.cols] <- sapply(data0[num.cols], as.numeric)
lapply(data0, class)       # great, now all DV columns are numeric 

data1 <- write.csv(data0, file = "data1.csv")    # save as csv-file for emergency cases

## Create dataframes per DV to run separate meta-analyses
# dataset[ID Year etc. columns, DV columns]
UPDRS <- data0[c(1:9)]           # select columns for dataframe of UPDRS scale
View(UPDRS)
WalkTime <- data0[c(1:5, 10:13)]    # select columns for dataframe of dataframe Walked Speed (TUG+Speed)
View(WalkTime)
Cadence <- data0[c(1:5, 14:17)]     # select columns for dataframe of dataframe Cadence
View(Cadence)

### compute columns with standardized mean difference & variance:
    # yi = observed effect sizes or outcomes - all negative values
    # vi = corresponding sampling variances
library(metafor)
UPDRS_df <- escalc(n1i = n_Ex, n2i = n_Co, m1i = mean_Ex_UPDRS, m2i = mean_Co_UPDRS, 
                sd1i = sd_Ex_UPDRS, sd2i = sd_Co_UPDRS, data = UPDRS, measure = "SMD", 
                append = TRUE)
View(UPDRS_df)

# replace missing values with 0s
UPDRS_df[is.na(UPDRS_df)] = "0"
View(UPDRS_df)


### RUN META-ANALYSES
# Random Effects Model for Continuous Data Meta-Analyses

# UPDRS

meta_U <- metacont(n_Ex, mean_Ex_UPDRS, sd_Ex_UPDRS, n_Co, mean_Co_UPDRS, sd_Co_UPDRS,
                   data = UPDRS_df, subset = (1:4), 
                   digits.sd=2,
                   sm = "SMD", 
                   method.smd = "Hedges",
                   leftcols = c("studlab", "TE", "seTE"),
                   leftlabs = c("Study", "g", "SE"),
                   title = "UPDRS",
                   xlab = "Standardized Mean Difference",
                   studlab = Study,
                   col.square = "navyblue",
                   col.diamond = "green",
                   col.predict = "gold",
                   fixed = FALSE,
                   random = TRUE,
                   prediction = TRUE)  

meta_U          # print results of the random effect model (SMD, heterogeneity + test)
                # details: inverse variance method
                # restricted maximum-likelihood estimator for tau^2
                # Q-Profile method for confidence interval of tau^2 and tau
                # Hedges' g (bias corrected standardised mean difference; using exact formulae)

# Fancy Forest Plot -> Effect Size 

forest(meta_U, layout = "RevMan5", 
       digits.sd = 2,
       col.square = "blue",
       col.diamond = "turquoise",
       col.predict = "gold")

summary(meta_U)

### Test Funnel Plot Asymmetry

library(metafor)
UPDRS_df <- escalc(n1i = n_Ex, n2i = n_Co, m1i = mean_Ex_UPDRS, m2i = mean_Co_UPDRS, 
                   sd1i = sd_Ex_UPDRS, sd2i = sd_Co_UPDRS, data = UPDRS_df, measure = "SMD",
                   append = TRUE)
View(UPDRS_df)

# convert characters into numeric values for all DV columns
library(dplyr)
df %>% 
  mutate_at(vars(yi, vi), as.numeric)

num.cols <- c('yi', 'vi')
UPDRS_df[num.cols] <- sapply(UPDRS_df[num.cols], as.numeric)
lapply(UPDRS_df, class) 

model_U_re <- rma(yi, vi, data = UPDRS_df)
model_U_re
res1 <- rma(yi, vi, model_U_re, data = UPDRS_df)

### compute the diagnostics for influential studies
inf1 <- influence(res1)
inf1 # 1 influential study


# Regression Test of Publication Bias
regtest(model_U_re)
ranktest(model_U_re)

# Funnel Plot -> Publication Bias
funnel(meta_U)

# replace missing values with 0s
UPDRS_df[is.na(UPDRS_df)] = "0"
View(UPDRS_df)

# Sensitivity Analysis
install.packages('metasens')
library(metasens)
limitmeta(meta_U,
          method.adjust = "beta0",
          level = meta_U$level,
          level.ma = meta_U$level.ma,
          backtransf = meta_U$backtransf,
          title = meta_U$title,
          complab = meta_U$complab,
          outclab = meta_U$outclab
          )

###################################### Walking Time

### compute columns with standardized mean difference & variance:
# yi = observed effect sizes or outcomes - all negative values
# vi = corresponding sampling variances
library(metafor)
WalkTime_df <- escalc(n1i = n_Ex, n2i = n_Co, m1i = mean_Ex_WS, m2i = mean_Co_WS, 
                   sd1i = sd_Ex_WS, sd2i = sd_Co_WS, data = WalkTime, measure = "SMD", 
                   append = TRUE)
View(WalkTime_df)

### RUN META-ANALYSES
# Random Effects Model for Continuous Data Meta-Analyses

# Walking Time

meta_WT <- metacont(n_Ex, mean_Ex_WS, sd_Ex_WS, n_Co, mean_Co_WS, sd_Co_WS,
                   data = WalkTime_df, 
                   sm = "SMD", 
                   digits.sd=2,
                   method.smd = "Hedges",
                   leftcols = c("studlab", "TE", "seTE"),
                   leftlabs = c("Study", "g", "SE"),
                   title = "Walking Time",
                   xlab = "Standardized Mean Difference",
                   studlab = Study,
                   col.square = "navyblue",
                   col.diamond = "gold",
                   col.predict = "lightgreen",
                   fixed = FALSE,
                   random = TRUE,
                   prediction = TRUE)  

meta_WT          # print results of the random effect model (SMD, heterogeneity + test)
# details: inverse variance method
# restricted maximum-likelihood estimator for tau^2
# Q-Profile method for confidence interval of tau^2 and tau
# Hedges' g (bias corrected standardised mean difference; using exact formulae)

# Fancy Forest Plot -> Effect Size 

forest.meta(meta_WT, layout = "RevMan5",
            digits.sd = 2,
            title = "Walking Time",
            col.square = "blue",
            col.diamond = "turquoise",
            col.predict = "gold")

summary(meta_WT)

### Test Funnel Plot Asymmetry
# Regression test for publication bias

model_WT_re <- rma(yi, vi, data = WalkSpeed_df)
res_wt <- rma(yi, vi, model_WS_re, data = WalkSpeed_df)

### compute the diagnostics for influential studies
inf_wt <- influence(res_wt)
inf_wt

# Regression Test of Publication Bias
regtest(model_WT_re)
ranktest(model_WT_re)

# Funnel Plot -> Publication Bias
funnel(meta_WT)

# Sensitivity Analysis
library(metasens)
limitmeta(meta_WT,
          method.adjust = "betalim",
          level = meta_WT$level,
          level.ma = meta_WT$level.ma,
          backtransf = meta_WT$backtransf,
          title = meta_WT$title,
          complab = meta_WT$complab,
          outclab = meta_WT$outclab)



###################################### Cadence

### compute columns with standardized mean difference & variance:
# yi = observed effect sizes or outcomes - all negative values
# vi = corresponding sampling variances
library(metafor)
Cadence_df <- escalc(n1i = n_Ex, n2i = n_Co, m1i = mean_Ex_Cadence, m2i = mean_Co_Cadence, 
                   sd1i = sd_Ex_Cadence, sd2i = sd_Co_Cadence, data = Cadence, measure = "SMD", 
                   append = TRUE)
View(Cadence_df)

### RUN META-ANALYSES
# Random Effects Model for Continuous Data Meta-Analyses

# Cadence

meta_C <- metacont(n_Ex, mean_Ex_Cadence, sd_Ex_Cadence, n_Co, mean_Co_Cadence, sd_Co_Cadence,
                   data = Cadence_df, subset = (3:6), 
                   sm = "SMD", 
                   method.smd = "Hedges",
                   leftcols = c("studlab", "TE", "seTE"),
                   leftlabs = c("Study", "g", "SE"),
                   title = "Cadence",
                   xlab = "Standardized Mean Difference",
                   studlab = Study,
                   col.square = "navyblue",
                   col.diamond = "gold",
                   col.predict = "lightgreen",
                   fixed = FALSE,
                   random = TRUE,
                   prediction = TRUE)  

meta_C          # print results of the random effect model (SMD, heterogeneity + test)
# details: inverse variance method
# restricted maximum-likelihood estimator for tau^2
# Q-Profile method for confidence interval of tau^2 and tau
# Hedges' g (bias corrected standardised mean difference; using exact formulae)

# Fancy Forest Plot -> Effect Size 

forest.meta(meta_C, layout = "RevMan5", 
            digits.sd=2,
            col.square = "blue",
            col.diamond = "turquoise",
            col.predict = "gold")

summary(meta_C)

# Funnel Plot -> Publication Bias
funnel(meta_C)

### Test Funnel Plot Asymmetry
# Regression test for publication bias
library(metafor)
Cadence_df0 <- escalc(n1i = n_Ex, n2i = n_Co, m1i = mean_Ex_Cadence, m2i = mean_Co_Cadence, 
                   sd1i = sd_Ex_Cadence, sd2i = sd_Co_Cadence, data = Cadence_df, measure = "SMD",
                   append = TRUE)
View(Cadence_df0)

# convert characters into numeric values for all DV columns
library(dplyr)
df %>% 
  mutate_at(vars(yi, vi), as.numeric)

num.cols <- c('yi', 'vi')
Cadence_df0[num.cols] <- sapply(Cadence_df0[num.cols], as.numeric)
lapply(Cadence_df0, class) 

model_C <- rma(yi, vi, data = Cadence_df0)
model_C
res0 <- rma(yi, vi, model_C, data = Cadence_df0)

### compute the diagnostics for outliers & influential studies
inf0 <- influence(res0)
inf0 # 1 influential study
View(Cadence_df0)

# Regression Test of Publication Bias
regtest(model_C)
ranktest(model_C)

# Sensitivity Analysis 

limitmeta(meta_C,
          method.adjust = "beta0",
          level = meta_C$level,
          level.ma = meta_C$level.ma,
          backtransf = meta_C$backtransf,
          title = meta_C$title,
          complab = meta_C$complab,
          outclab = meta_C$outclab)

# The End
# Thank you for your attention :)
