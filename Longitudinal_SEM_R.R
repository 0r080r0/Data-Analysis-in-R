# Longitudinal SEM

install.packages("lcsm")
library(lcsm)
library(psych)
library(tidyverse)
library(lavaan)
library(ggplot2)
library(readxl)
data1 <- read_xlsx('/Users/evangelia_karakoliou/Downloads/data_nomiss.xlsx')
View(data1)

############### LATENT GROWTH CURVE MODELLING ################

### 4 main interests in latent growth curve:
# mean for intercept + slope for average person
# variance for intercept (how much do individuals differ where they start) + slope (how much do individuals differ in rates of change?)

### latent growth curve model (= LGCM)
# 2 latent factors: intercept & slope; residuals
# factor loadings for intercept set at = 1; slopes -> time invterval (month, year, etc)
# correlation btw intercept and slope

# start with repeated measurements of the same variable
# use the total scores!
# in latent growth curves we need at least 3 time points
# we have fixed and random time effects


# FIRST CHECK LONGITUDINAL INVARIANCE - is it weak or strong?



# Latent Regression Model

mod1 <- '
        # latent variables
         soccomp1 =~soccomp1_1 + soccomp2_1 + soccomp3_1
         selfest2 =~ selfest1_2 + selfest2_2 + selfest3_2 
         
         # regression
         selfest2 ~ soccomp1
'

fit1 <- growth(mod1, data = data1)
summary(fit1, standardized = TRUE)

semPaths(lavaanify(mod1))

# Visualize Change
pred_lgm <- predict(fit1) 
head(pred_lgm)

# Factorial Invariance

table(data1$gender) ##request a frequency summary
data1$gender <- as.factor(data1$gender)
levels(data1$gender) <- c("Girls", "Boys") ##assign level labels
table(data1$gender) ##request a frequency summary to verify assigned labels 

semTools::measurementInvariance(model = mod1, data = data1, group = "gender")


# Model 2
mod2 <- '
        # latent variables
         selfest1 =~ selfest1_1 + selfest2_1 + selfest3_1
         selfest2 =~ selfest1_2 + selfest2_2 + selfest3_2 
         
         # regression
         selfest2 ~ selfest1

         # covariance removed (by using the 0)
         selfest1 ~~ 0*selfest2 
'

fit2 <- growth(mod2, data = data1)
summary(fit2, standardized = TRUE)

semPaths(lavaanify(mod2))






