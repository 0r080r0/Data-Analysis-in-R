### CFA, Discriminant & Convergent Validity & SEM

# CFA: to confirm the fitness of data to the proposed hypothesised model
# Discriminant & Convergent Validity: along with composite reliability are estimated - to ensure the validity and reliability of the instrument
# SEM: used to test the proposed hypothesis of the study -> Use Regression, since we test influence of 2 factors on DV

#######

# load libraries
install.packages("lavaan", dependencies = TRUE)
install.packages("readxl")
install.packages("Hmisc")
install.packages("QuantPsyc")
install.packages("ggcorrplot")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggcorrplot")

# load lavaan
library(lavaan)
library(readxl)
library(Hmisc)
library(QuantPsyc)
library(ggcorrplot)

rm(list = ls()) # clean your environment from previously defined work in R

# import data
data <- read_excel('~/Downloads/Mumin/Rohdaten.xls')
View(data)

### Variablen
# LATENT FACTOR - Arbeitszufriedenheit (AZ): Kolumnen 3:29 -> 4-5 Subkonstrukte (MANIFEST INDICATORS: Resignation, Selbstverwirklichung, Bezahlung, Firma, nicht zuordbar)
# Kommunikationsqualität: 30:37
# LATENT FACTOR: Führungsbeziehungsqualität: 38:53 -> 4 Subskalen, aber 1-dimensionales Konstrukt (MANIFEST INDICATORS) Subskalen (Respekt, Vertrauen, Ermutigung und Zuneigung)
# LATENT FACTOR: Betriebsklima: 54:59 -> 2 Subkonstrukte (MANIFEST INDICATORS: Firma, nicht zuordbar)
# Work-Life-Balance: 60:64
# Gehalt: 65:68
# demographische Item: 69:73


### Faktoren
# Motivationsfaktoren: Selbstverwirklichung (Subkonstrukt von AZ) & Gehalt (?)
# Hygienefaktoren: Gehalt & Work-Life Balance & Betriebsklima - bzw. Bezahlung & Firma (Subkonstrukt von AZ); Führungsbeziehungsqualität

# Model für SEM: Einfluss von Führungsbeziehungsqualität (Hygienefaktor) & Gehalt (Motivationsfaktor) auf Arbeitszufriedenheit


# Neues Subset vom Datenset - Variablen von Interesse

AZ <- data[3:29]
KQ <- data[30:37]
FQ <- data[38:53]
GE <- data[65:68]
WLB <- data[60:64]

data2 <- cbind(AZ, KQ, FQ, GE, WLB)
View(data2)

### Reverse Code Items that were negatively phrased
# define columns to reverse code
reverse_cols = c("AZ01_02", "AZ01_03", "AZ01_07", "AZ01_08", "AZ01_09", "AZ01_10", "AZ01_11", "AZ01_15", "AZ03_01", "WL01_02")

# reverse code the previous columns and include in original dataset
data2[reverse_cols] = 6 - data2[reverse_cols] # Use 6, as we had rating scales ranging from 1 o 5

# Check normality of data: perform multivariate normality test
mult.norm(data2)$mult.test # p-value smaller then 0.05 -> reject H0 -> evidence that multivariate data is NOT normally distributed

# Check missing data
is.na(data2)
sum(is.na(data2))


# Impute missing data: replace missing values (use median, due to skewed data)
library(Hmisc)

data_imp <- impute(data2, fun=median)
View(data_imp)

# Check Covariance & Correlation of Variables
library(ggcorrplot)

cov <- round(cov(data_imp), 2)
head(cov[, 1:6]) # positive covariances

corr <- round(cor(data_imp), 2)
head(corr[, 1:6]) # positive correlations

corrAZ <- round(cor(data_imp[1:27]), 2) # correlation for AZ
corrKQ <- round(cor(data_imp[28:35]), 2) # correlation for KQ
corrFQ <- round(cor(data_imp[36:51]), 2) # correlation for FQ
corrGE <- round(cor(data_imp[52:55]), 2) # correlation for GE
corrWLB <- round(cor(data_imp[56:60]), 2) # correlation for GE

# Visualise Correlations
ggcorrplot(corrAZ) # quite some weak/moderate correlations
ggcorrplot(corrKQ) # Strong correlations
ggcorrplot(corrFQ) # mostly moderate/strong correlations
ggcorrplot(corrGE) # mostly strong correlations
ggcorrplot(corrWLB) # mostly strong correlations

# Lavaan Model
# Syntax: latent variable =~ indicator1 + indicator2 + indicator3 + etc.

# Model 1: The measurement model

model_1 <- "
            # measurement model (AZ)

            AZ =~ AZ01_03 + AZ01_04 + AZ01_05 + AZ01_07 + AZ01_09 + AZ01_10 + AZ01_11 + AZ01_12 + AZ01_13 + AZ01_14 + AZ01_15 + AZ02_01 + AZ03_01 + AZ04_01 + AZ05_01 + AZ05_02 + AZ05_03 + AZ05_04 + AZ05_05 + AZ05_06 + AZ06_01 + AZ06_02 + AZ07_01
            "

fit1 <- cfa(model_1, data = data2, estimator = "DWLS", likelihood = "WLSM", std.lv=TRUE) # #by adding std.lv = TRUE to the cfa() function we can fix the factor variance to 1

summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
coef(fit1) # extract coefficients

fit1_stats <- fitMeasures(fit1, c("cfi", "rmsea", "SRMR"))
fit1_stats # CFI: 0.985; RMSEA: 0.052; SRMR: 0.083; Interpretation: CFI ≥ 0.95 (good value); excellent RMSEA value, SRMR less than .05 is good, however values of up to 0.08 are acceptable -> good model fit

# Extract residuals
lavInspect(fit1, what = "resid")

# variance-covariance matrix of standardized residuals
resid(fit1, type = "standardized") # values with an absolute value ≥ 2.58, p ≤ 0.01 turn out to be significant

### Plot the Model

library(semPlot)
p <- semPaths(fit1,
              "std",
              whatLabels="est",
              sizeMan = 5,
              node.width = 1,
              edge.label.cex = .75,
              style = "ram",
              mar = c(5, 5, 5, 5))


############################################

# First structural equation model - three factor model

model2 <- "
            # latent variable definitions

            AZ =~ AZ01_03 + AZ01_04 + AZ01_05 + AZ01_07 + AZ01_09 + AZ01_10 + AZ01_11 + AZ01_12 + AZ01_13 + AZ01_14 + AZ01_15 + AZ02_01 + AZ03_01 + AZ04_01 + AZ05_01 + AZ05_02 + AZ05_03 + AZ05_04 + AZ05_05 + AZ05_06 + AZ06_01 + AZ06_02 + AZ07_01

            FQ =~ FQ01_01 + FQ01_02 + FQ01_03 + FQ01_04 + FQ01_05 + FQ01_06 + FQ01_07 + FQ01_08 + FQ01_09 + FQ01_10 + FQ01_11 + FQ01_12 + FQ01_13 + FQ01_14 + FQ01_15 + FQ01_16

            # regressions

            AZ ~ FQ

            "

fit2 <- cfa(model2, data = data2, estimator = "DWLS", likelihood = "WLSM", std.lv=TRUE) # #by adding std.lv = TRUE to the cfa() function we can fix the factor variance to 1

summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
coef(fit) # extract coefficients

fit2_stats <- fitMeasures(fit2, c("cfi", "rmsea", "SRMR"))
fit2_stats # CFI: 0.993; RMSEA: 0.038; SRMR: 0.079; Interpretation: CFI ≥ 0.95 (good value); excellent RMSEA value, SRMR less than .05 is good, however values of up to 0.08 are acceptable -> good / acceptable model fit

# Extract residuals
lavInspect(fit2, what = "resid")

# variance-covariance matrix of standardized residuals
resid(fit2, type = "standardized") # values with an absolute value ≥ 2.58, p ≤ 0.01 turn out to be significant

### Plot the Model

library(semPlot)
p <- semPaths(fit2,
              "std",
              whatLabels="est",
              sizeMan = 5,
              node.width = 1,
              edge.label.cex = .75,
              style = "ram",
              mar = c(5, 5, 5, 5))

# plot interpretation: negative values = as X increases, Y decreases

###############################################

# Second structural equation model - 3 factor model

model3 <- "
            # latent variable definitions

            AZ =~ AZ01_03 + AZ01_04 + AZ01_05 + AZ01_07 + AZ01_09 + AZ01_10 + AZ01_11 + AZ01_12 + AZ01_13 + AZ01_14 + AZ01_15 + AZ02_01 + AZ03_01 + AZ04_01 + AZ05_01 + AZ05_02 + AZ05_03 + AZ05_04 + AZ05_05 + AZ05_06 + AZ06_01 + AZ06_02 + AZ07_01

            KQ =~ KQ01_01 + KQ01_02 + KQ01_03 + KQ01_04 + KQ01_05 + KQ01_06 + KQ01_07 + KQ01_08

            # regressions

            AZ ~ KQ
            "

fit3 <- cfa(model3, data = data2, estimator = "DWLS", likelihood = "WLSM", std.lv=TRUE) #by adding std.lv = TRUE to the cfa() function we can fix the factor variance to 1

summary(fit3, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
coef(fit3) # extract coefficients

fit3_stats <- fitMeasures(fit3, c("cfi", "rmsea", "SRMR"))
fit3_stats # CFI: 0.986; RMSEA: 0.048; SRMR: 0.083; Interpretation: CFI ≥ 0.95 (good value); excellent RMSEA value, SRMR less than .05 is good, however values of up to 0.08 are acceptable -> good

# Extract residuals
lavInspect(fit3, what = "resid")

# variance-covariance matrix of standardized residuals
resid(fit3, type = "standardized") # values with an absolute value ≥ 2.58, p ≤ 0.01 turn out to be significant

### Plot the Model

library(semPlot)
p3 <- semPaths(fit3,
              "std",
              whatLabels="est",
              sizeMan = 5,
              node.width = 1,
              edge.label.cex = .75,
              style = "ram",
              mar = c(5, 5, 5, 5))

# Third structural equation model - MEDIATION - 3 factor model

mod4 <- "
            # Latent variable definitions

             AZ =~ AZ01_03 + AZ01_04 + AZ01_05 + AZ01_07 + AZ01_09 + AZ01_10 + AZ01_11 + AZ01_12 + AZ01_13 + AZ01_14 + AZ01_15 + AZ02_01 + AZ03_01 + AZ04_01 + AZ05_01 + AZ05_02 + AZ05_03 + AZ05_04 + AZ05_05 + AZ05_06 + AZ06_01 + AZ06_02 + AZ07_01

            FQ =~ FQ01_01 + FQ01_02 + FQ01_03 + FQ01_04 + FQ01_05 + FQ01_06 + FQ01_07 + FQ01_08 + FQ01_09 + FQ01_10 + FQ01_11 + FQ01_12 + FQ01_13 + FQ01_14 + FQ01_15 + FQ01_16

            KQ =~ KQ01_01 + KQ01_02 + KQ01_03 + KQ01_04 + KQ01_05 + KQ01_06 + KQ01_07 + KQ01_08

            # a path
            KQ ~ a * FQ

            # b path
            AZ ~ b * KQ

            # c prime path
            AZ ~ cp * FQ

            # indirect and total effects
            ab := a * b
            total := cp + ab"

fit4 <- cfa(mod4, data = data2, estimator = "DWLS", likelihood = "WLSM", std.lv=TRUE) #by adding std.lv = TRUE to the cfa() function we can fix the factor variance to 1

summary(fit4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
coef(fit4) # extract coefficients

fit4_stats <- fitMeasures(fit4, c("cfi", "rmsea", "SRMR"))
fit4_stats # CFI: 0.996; RMSEA: 0.027; SRMR: 0.076; Interpretation: CFI ≥ 0.95 (good value); excellent RMSEA value, SRMR less than .05 is good, however values of up to 0.08 are acceptable -> good

# Extract residuals
lavInspect(fit4, what = "resid")

# variance-covariance matrix of standardized residuals
resid(fit4, type = "standardized") # values with an absolute value ≥ 2.58, p ≤ 0.01 turn out to be significant

### Plot the Model

library(semPlot)
P4 <- semPaths(fit4,
               "std",
               whatLabels="est",
               sizeMan = 5,
               node.width = 1,
               edge.label.cex = .75,
               style = "ram",
               mar = c(5, 5, 5, 5))


# Fourth structural equation model - three factor model

model_5 <- "
            # latent variable definitions

            AZ =~ AZ01_03 + AZ01_04 + AZ01_05 + AZ01_07 + AZ01_09 + AZ01_10 + AZ01_11 + AZ01_12 + AZ01_13 + AZ01_14 + AZ01_15 + AZ02_01 + AZ03_01 + AZ04_01 + AZ05_01 + AZ05_02 + AZ05_03 + AZ05_04 + AZ05_05 + AZ05_06 + AZ06_01 + AZ06_02 + AZ07_01

            WLB =~ WL01_01 + WL01_02* + WL01_03 + WL01_04 + WL01_05

            GE =~ GE02_01 + GE03_01 + GE03_02 + GE03_03

            # regressions

            AZ ~ WLB + GE
            "

fit5 <- cfa(model_5, data = data, estimator = "DWLS", likelihood = "WLSM", std.lv=TRUE) # #by adding std.lv = TRUE to the cfa() function we can fix the factor variance to 1

summary(fit5, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
coef(fit) # extract coefficients

fit5_stats <- fitMeasures(fit5, c("cfi", "rmsea", "SRMR"))
fit5_stats # CFI: 0.984; RMSEA: 0.047; SRMR: 0.083; Interpretation: CFI ≥ 0.95 (good value); excellent RMSEA value, SRMR less than .05 is good, however values of up to 0.08 are acceptable -> good / acceptable model fit

# Extract residuals
lavInspect(fit5, what = "resid")

# variance-covariance matrix of standardized residuals
resid(fit5, type = "standardized") # values with an absolute value ≥ 2.58, p ≤ 0.01 turn out to be significant

### Plot the Model

library(semPlot)
p <- semPaths(fit5,
              "std",
              whatLabels="est",
              sizeMan = 5,
              node.width = 1,
              edge.label.cex = .75,
              style = "ram",
              mar = c(5, 5, 5, 5))
