### Topic: Rape Myth Acceptance

## Confirmatory Factor Analysis (CFA)

# install & load packages

install.packages("psych")
library(psych)
install.packages("lavaan")
library(lavaan)
install.packages("ggplot2")
library(ggplot2)
install.packages("EFAtools")
library(EFAtools)
install.packages("moments")
library(moments)
install.packages("ggcorrplot")
library(ggcorrplot) #plotting correlation matrices
install.packages("GPArotation")
library(GPArotation) #methods for factor rotation
install.packages("nFactors")
library(nFactors)

# Import csv.file dataset
data <- read.csv("/Users/evangelia_karakoliou/Desktop/PSY/Master/Y2/MV/EXAM/CriPsy/ammsa_data_clean.csv")
View(data)

# CREATE NEW DATASET WITH LATENT VARIABLES OF INTEREST________________________________________________

df <- data [c(4:33, 34:55, 88:103, 107)] # AMMSA, ASI, SDO & gender
write.csv(df, "homeexam.csv", row.names=F)
summary(df)

# Missing Cases Analysis
homeexam <- read.csv("homeexam.csv")

is.na(homeexam) %>%
  sum()             # 69 missing cases in my new dataset

sapply(homeexam, function(x) which(is.na(x))) # location of missing values per column -> ID no. 60 + 421 no values - empty row

cl_home <- na.omit(homeexam) # removes rows with missing data (2 rows)
summary(cl_home)

# Add ID column to make life easier
library(tidyverse)
cl_h <- tibble::rowid_to_column(cl_home, "ID")
View(cl_home)
# select items pertaining to each construct to run Cronbach's alpha per construct
# AMMSA: no reverse coded items
# AS & SDO: some reverse coded items

# Assess Internal Consistency Reliability; SDO: 9-16 are reverse coded; AS: items 3,6,7,13,18 are reverse coded

cl_home <-cl_home[!(cl_home$dem_01_gender =="3" | cl_home$dem_01_gender =="4"),]
gender <-cl_home[c(69)]
View(cl_home)

# Alternative way to remove gender groups 3 +4 
cl_home <- cl_home[!cl_home$dem_01_gender>'2',]     # remove all gender coding labels larger than 2

# New dataframes per scale
rma_s <- cl_home[c(1:30)]            # dataframe AMMSA scale
sdo_s <- cl_home[c(53:68)]           # dataframe SDO scale
asi_s <- cl_home[c(31:52)]           # dataframe ASI scale

### Cronbach's Alpha

library(ltm)
cronbach.alpha(rma_s)                # Cronbach's a = 0.942    -> HIGH = good, but too high, aka items are too similar (?)

### RECODE REVERSE CODED ITEMS
### REPLACE REVERSE CODED WITH NEW CODED 
## SDO: reverse code column numbers 9-16 & subtract the cells to recode them into the correct code "sdo_coded"

summary(sdo_s)                           # 7-point Likert scale - Min: 1, Max:7
sdo_s[,c(9:16)] <- abs(sdo_s[,c(9:16)]-8)  # Subtract from 8, as scale range = 1-7 -> otherwise you have 0s in original dataset

library(ltm)
cronbach.alpha(sdo_s)                   # Cronbach's a = .912 -> good, HIGH - REVERSE CODED ITEMS - ALPHA - real alpha

## AS (MODERATOR): reverse coded items: 3,6,7,13,18

summary(asi_s)                           # 7-point Likert scale - Min: 1, Max:7
View(asi_s)
asi_s[,c(3,6,7,13,18)] <- abs(asi_s[,c(3,6,7,13,18)]-8)     # recoded reversed items + replaced in the original data

View(asi_s[,c(3,6,7,13,18)])             # CORRECTLY NEW CODED !!!
library(ltm)
cronbach.alpha(asi_s)                   # Cronbach's alpha with recoded items: .790

# Create new dataset, clean & normally coded items

hope <- data.frame(rma_s, sdo_s, asi_s, gender)
write.csv(hope, "df_new.csv", row.names=F)
win <- read.csv("df_new.csv")                     # NEW DATASET - clean & correctly coded
summary(win)
View(win)

### Assumption Checks EFA - Normality, linearity, outliers

# Split data in 2 samples, 1 for EFA and 1 for CFA
set.seed(3245233)
grouping <- rbinom(n = (508), size = 1, prob = .5)
sum(grouping)# good, more or less divided in half - N = 256
df0 <- data.frame(win, grouping)
df0 <- abs(win[,-c(70)])
rm(grouping)
View(df0)

# group 0 is the EFA group // group 1 is the CFA group
library(dplyr)
efa_dat <- filter(df0, grouping == 0) # N = 254
efa_dat <- abs(efa_dat[,-c(70)]) # removing grouping variable
cfa_dat <- filter(df0, grouping == 1) # N = 257
cfa_dat <- abs(cfa_dat[,-c(70)]) # removing grouping variable
View(efa_dat)                 
View(cfa_dat)

# save EFA & CFA randomized datasets for future inspections
write.csv(efa_dat, "efa_ex.csv", row.names=F)                       # save efa data as file
write.csv(cfa_dat, "cfa_ex.csv", row.names=F)                       # save cfa data as file

# SAVE AS NEW DATASET FILES 

cfa <- read.csv("cfa_ex.csv")

#EFA ----
#Normality and assumptions______________________________________________________

efa <- read.csv("efa_ex.csv")

install.packages("moments")
library(moments)

# Kurtosis & Skewness
library("MVN")
norm_rma <- mvn(efa[,1:30],               
                mvnTest = 'mardia', 
                multivariatePlot = 'qq',
                univariateTest = 'AD',
                showOutliers = F, 
                showNewData = F, 
                multivariateOutlierMethod = 'adj',
                desc = T)
norm_rma$multivariateNormality
norm_rma$Descriptives
norm_rma$univariateNormality

##### NOTE OUTLIER REMOVAL WAS NOT NECSSARY AS I USED LIKERT SCALES (EXTREME SCORES)


# Check dataset after removal of Outliers
norm_rma$newData
no_out <- as.data.frame(norm_rma$newData)

norm_rma1 <- mvn(no_out, 
               mvnTest = 'mardia', 
               multivariatePlot = 'qq',
               univariateTest = 'AD',
               showOutliers = TRUE, 
               showNewData = TRUE, 
               multivariateOutlierMethod = 'adj')     # 53 new outliers

# BAD: AMMSA + SDO have most outliers -> extreme Likert scores

# Convert data to correlation matrix and look for correlations
EFA_cormat <- round(cor(efa[c(1:30, 31:46, 47:68)]), 3)  #AMMSA, ASI, SDO
View(EFA_cormat)

EFA_cor1 <- cor(efa[c(1:30)])
EFA_cor2 <- cor(efa[c(31:46)])
EFA_cor3 <- cor(efa[c(47:68)])
View(EFA_cor1)          # AMMSA has mostly moderate, but also many weak correlations
View(EFA_cor2)          # SDO has mostly moderate to strong correlations, some weak
View(EFA_cor3)          # AS has many weak, few moderate correlations

# Visually inspect low correlations to see if factor analysis makes sense

install.packages("ggcorplot")
library(ggplot2)
library(ggcorrplot)

# Plotting correlation heatmap per construct (AMMSA, SDO, AS)
EFA_cm <- round(cor(efa[c(1:30, 31:46, 47:68)]), 3)  #AMMSA, ASI, SDO

efa_cm1 <- round(cor(efa[c(1:30)]), 3)
efa_cm2 <- round(cor(efa[c(31:46)]), 3) 
efa_cm3 <- round(cor(efa[c(47:68)]), 3) 

# Correlation matrix plot
library(corrplot)
corrplot.mixed(efa_cm3)
corrplot(efa_cm3, order = 'hclust', col = COL2('BrBG', 10), addCoef.col = 1, number.cex = 0.35, tl.cex = 0.25, cl.cex = 0.35, tl.col = 'black', cl.ratio = 0.2, tl.srt = 45) # add method = 'number' if you want coloured numbers only

corrplot.mixed(efa_cm2)
corrplot(efa_cm2, order = "hclust", col = COL2('BrBG', 10), addCoef.col = 1, number.cex = 0.35, tl.cex = 0.25, cl.cex = 0.35, tl.col = 'black', cl.ratio = 0.2, tl.srt = 45)

corrplot.mixed(efa_cm1)
corrplot(efa_cm1, order = "hclust", col = COL2('BrBG', 10), addCoef.col = 1, number.cex = 0.35, tl.cex = 0.25, cl.cex = 0.35, tl.col = 'black', cl.ratio = 0.2, tl.srt = 45)

# GOOD: Corr Mat for AMMSA -> mod-strong
# BAD: Corr Mat for SDO & AS: weak-mod; AS many neg. corr.

## Sample Adequacy
library(EFAtools)
BARTLETT(EFA_cm, N = 508)
# Bartlett's Sphericity: tests H0 that correlation matrix is an identity matrix.
# The Bartlett's test of sphericity was significant at an alpha level of .05. These data are probably suitable for factor analysis. 𝜒²(2278) = 17918.93, p < .001

KMO(EFA_cm) # The overall KMO value for your data is meritorious. These data are probably suitable for factor analysis. Overall: 0.896

det(EFA_cm)
SCREE(EFA_cm)# Plot identified 3-4 factors

# Parallel analysis
install.packages("paran")
library(MASS)
library(paran)

paran(EFA_cm, iterations = 5000, centile = 0, quietly = FALSE,    # 4 Factors retained
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

# Model Fitting__________________________________________________________
efa.1 <- EFA(x = EFA_cm
            , N = 252         # will be random - look at the n of efa_dat
            , n_factors = 4
            , method = 'ML'
            , rotation = 'oblimin')
efa.1       # items with low factor loadings: AMMSA 11 + 19, ASI (2 until incl. 16; ASI 18 & 21) + SDO (2, 4, 5, 8)

# Set Communalities Cut-Off Point at .4
loadings <- print(efa.1$rot_loadings, digits=3, cutoff=.4, sort=F)

AMMSA_s <- efa[c(1:30)]
SDO_s <- efa[c(31:46)]
ASI_s <- efa[c(47:68)]

### HTML Table per Construct with Cronbach's Alpha
install.packages("sjPlot")
library(sjPlot)

# AMMSA
tab_fa(
  AMMSA_s,
  rotation = "oblimin",
  method = c("ml", "minres", "wls", "gls", "pa", "minchi", "minrank"),
  nmbr.fctr = NULL,                     # "NULL" chooses factors by parallel analysis
  fctr.load.tlrn = 0.1,
  sort = F,
  title = "AMMSA Factor Analysis",
  var.labels = NULL,
  wrap.labels = 40,
  show.cronb = T,
  show.comm = F,
  alternate.rows = F,
  digits = 3,
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = T,
  remove.spaces = T
) # results in 3 factors, Cronbach's α	0.902	0.846 0.801

# 9x LOW LOADINGS: ammsa_01_menlead ammsa_08_mediadepict ammsa_13_praiselooks ammsa_14_fascination ammsa_15_playcoy ammsa_17_urgepartner ammsa_20_marital ammsa_25_robbery ammsa_26_alcohol

# SDO
tab_fa(
  SDO_s,
  rotation = "oblimin",
  method = c("ml", "minres", "wls", "gls", "pa", "minchi", "minrank"),
  nmbr.fctr = NULL,                     # "NULL" chooses factors by parallel analysis
  fctr.load.tlrn = 0.1,
  sort = F,
  title = "SDO Factor Analysis",
  var.labels = NULL,
  wrap.labels = 40,
  show.cronb = T,
  show.comm = F,
  alternate.rows = F,
  digits = 3,
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = T,
  remove.spaces = T
) # results in 2 factors, Cronbach's α	0.896	0.871
# NO LOW LOADINGS

# ASI
tab_fa(
  ASI_s,
  rotation = "oblimin",
  method = c("ml", "minres", "wls", "gls", "pa", "minchi", "minrank"),
  nmbr.fctr = NULL,                     # "NULL" chooses factors by parallel analysis
  fctr.load.tlrn = 0.1,
  sort = F,
  title = "ASI Factor Analysis",
  var.labels = NULL,
  wrap.labels = 40,
  show.cronb = T,
  show.comm = F,
  alternate.rows = F,
  digits = 3,
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = T,
  remove.spaces = T
) # results in 4 factors, Cronbach's α	0.768	0.722	0.378	-0.363 -> remove last column items?

# 4-5 low loadings: asi_03_disaster_r asi_06_rominvlvd_r asi_13_complete_r asi_18_kickteasing_r asi_21_reasondemands	

# ALL CONSTRUCTS - Factor Loadings Table
library(sjPlot)
tab_fa(
  efa[c(1:68)],
  rotation = "oblimin",
  method = c("ml", "minres", "wls", "gls", "pa", "minchi", "minrank"),
  nmbr.fctr = 3,
  fctr.load.tlrn = 0.1,
  sort = F,
  title = "Rape Myth Acceptance - Exploratory Factor Analysis",
  var.labels = NULL,
  wrap.labels = 40,
  show.cronb = T,
  show.comm = F,
  alternate.rows = F,
  digits = 3,
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = T,
  remove.spaces = T
)

# 24 items low laod, if all constructs are in 1 EFA

### Remove low loading items from EFA_cormat/efa_dat

detach("package:paran", unload = TRUE)
library(dplyr)
efa_h <- efa[,-c(8, 9, 11, 12, 19, 25, 28:30, 32, 38, 48:49, 51:52, 54, 56:59, 61:62, 64)] # remove low factor loadings from data - 1st removal

efa_ammsa <- efa[,-c(8, 9, 11, 12, 19, 25, 28:30, 31:69)] # high loading AMMSA only
efa_sdo <- efa[,-c(1:30, 47:69)]
efa_as <- efa[,-c(1:47, 49, 52, 59, 64, 67, 69)] # high loading SDO
efa_high <- efa[,-c(1, 8, 13:15, 17, 20, 25:26, 49, 52, 59, 64, 67)] # high loading AS

# Re-run EFA per construct

efa_1 <- cor(efa_ammsa)
efa.1 <- EFA(x = efa_1
             , N = 252 # random - look at the n of efa_data
             , n_factors = 3
             , method = 'ML'
             , rotation = 'oblimin')
efa.1  # negative inter factor correlation

efa_2 <- cor(efa_sdo)
efa.2 <- EFA(x = efa_2
             , N = 252 # random - look at the n of efa_data
             , n_factors = 3
             , method = 'ML'
             , rotation = 'oblimin')
efa.2  # positive inter factor correlation

efa_3 <- cor(efa_as)
efa.3 <- EFA(x = efa_3
             , N = 252 # random - look at the n of efa_data
             , n_factors = 3
             , method = 'ML'
             , rotation = 'oblimin')
efa.3  # negative inter factor correlation

NEW <- cbind(efa_ammsa, efa_sdo, efa_as)
EFA_2 <- cor(NEW)
EFA.2 <- EFA(x = EFA_2
             , N = 252 # random - look at the n of efa_data
             , n_factors = 3
             , method = 'ML'
             , rotation = 'oblimin')
EFA.2 #

# DATASET FILE with high FA loadings
write.csv(efa_high, "efa_high.csv", row.names=F)
efa_high <- read.csv("efa_high.csv")

View(efa_h)
efa_cor2 <- cor(efa_high, method="pearson")
View(efa_cor2)
PosCor <- efa_cor2[efa_cor2 > 0]
PCor <- as.data.frame(PosCor)
View(PCor)

efa.h <- EFA(x = efa_cor2
            , N = 252 #will be random - look at the n of efa_data(or be smart and set.seed)
            , n_factors = 3
            , method = 'ML'
            , rotation = 'oblimin')           # use method ULS (unweighted least squares) rather than ML
efa.h                                        
# MODEL FIT: 𝜒𝜒²(900) = 1583.68, p < .001, CFI = .97, RMSEA [90% CI] = .05 [.05; .06], AIC = AIC =  -216.32 ,BIC = -3392.81, CAF = .46

# Set Communalities Cut-Off Point at .4
thr_f <- print(efa.h$rot_loadings, digits=3, cutoff=.4, sort=F) # few more low loading items

BARTLETT(efa_cor2, N = 252)

# Bartlett's Sphericity: tests H0 that correlation matrix is an identity matrix.
# The Bartlett's test of sphericity was significant at an alpha level of .05.
# 𝜒²(1035) = 5622.5, p < .001

KMO(efa_cor2) # The overall KMO value for data is "marvellous". Data are probably suitable for factor analysis. Overall: 0.911
det(efa_cor2)

SCREE(efa_cor2) # 3-4 factors

### UPDATED Cronbach's alpha - POST-EFA ANALYSES & ITEM REMOVAL
# Assess Internal Consistency Reliability
# Remember: we used the data which has already been correctly recoded - YAY

View(df_new)
# Remember low loading items: efa_h - low factor loadings
f_rma <- efa_h[c(1:21)]   # dataframe AMMSA scale - without low factor loading items from efa no. 1
f_sdo <- efa_h[c(22:35)]  # dataframe SDO scale - without low factor loading items from efa no. 1
f_asi <- efa_h[c(36:46)]  # dataframe ASI scale - without low factor loading items from efa no. 1
# efa_h -> includes all 3 scales + gender variable

library(ltm)
cronbach.alpha(f_rma)               # AMMSA Alpha = .926 (21 items)
cronbach.alpha(f_sdo)               # SDO Alpha = .915 (14 items)
cronbach.alpha(f_asi)               # ASI Alpha = .646 (11 items) 


tab_fa(
  efa_h[c(1:45)],
  rotation = "oblimin",
  method = c("ml", "minres", "wls", "gls", "pa", "minchi", "minrank"),
  nmbr.fctr = NULL,
  fctr.load.tlrn = 0.1,
  sort = F,
  title = "Rape Myth Acceptance - Exploratory Factor Analysis",
  var.labels = NULL,
  wrap.labels = 40,
  show.cronb = T,
  show.comm = F,
  alternate.rows = F,
  digits = 3,
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = T,
  remove.spaces = T
)

# 4 factors - low loadings again: ammsa_01_menlead, ammsa_24_hitbreaks, sdo_04_steponothers , asi_09_chersihed , asi_17_pedestal 

### CFA _________________________________________________________________

# The base of CFA and SEM is the variance covariance matrix, lets make it!

CFA <- cfa[,-c(1, 8, 9, 11, 12, 19, 24, 25, 28:30, 32, 34, 38, 48:49, 51:52, 54, 56:59, 61, 62, 63, 64)] # cfa dataset with low factor items from 2 EFAs removed!
cfa_covmat <- round(cov(CFA), 3)
View(cfa_covmat)
covcfa <- as.data.frame(cfa_covmat)
covcfa <- as.data.frame(as.table(cor(cfa_covmat)))
View(covcfa)

rownames(cfa_cov) <- NULL         # removes row names -> not a matrix any longer, but dataset (?)
View(cfa_cov)
NegV <- subset(cfa_cov, c(1:41) < 0) # subset of negative values

cfa_cov %>% 
  mutate(pos_neg_value = case_when(cfa_cov < 0 ~ 'Positive'
                                     , TRUE ~ 'Negative'))

CFA <- CFA[-c(42)] # remove gender variable
COR <- as.data.frame(rma_cor [,!apply(rma_cor, 2, function(x) any(abs(x) > 0.99, na.rm = F))])


# Confirm that your cov matrix is correct by testing a value and checking the matrix value
sd(cfa_new$ammsa_02_misgivings)^2 

# Covariance Matrix for each Construct/Factor
ammsa_cov <- round(cov(f_rma),3)          # remember to use the data with low factor loading items REMOVED
sdo_cov <- round(cov(f_sdo),3) 
asi_cov <- round(cov(f_asi),3)
View(ammsa_cov)

# Specify CFA Model as Mediated Model____________________________________________________

library(lavaan)
install.packages("semPlot")
library(semPlot)
install.packages("semPaths")
library(semPaths)

med_model = ' # direct effect
              AMMSA ~ c*SDO
              
              # mediator
              AS ~ a*SDO
              AMMSA ~ b*AS
             
              # indirect effect (a*b)
              indirect := a*b
             
              # total effect
              total := c + (a*b)
              
              # Covariance
              SDO ~~ AS
              
   SDO =~ sdo_01_simplyinferio + sdo_03_chanceinlife + sdo_06_topbottom + sdo_07_infstayplace + 
                sdo_09_groupsequal_r + sdo_10_ourideal_r + sdo_11_equalchance_r + sdo_12_eqlizeconds_r + 
                sdo_13_increased_r + sdo_14_fewerprblms_r + sdo_15_eqlincomes_r + sdo_16_dominate_r
                
              AMMSA =~ ammsa_02_misgivings + ammsa_03_emancipated + ammsa_04_custfalsacc + 
                ammsa_05_battlesexes + ammsa_06_bionecessit + ammsa_07_amplesuppor +
                ammsa_10_suggremarks + ammsa_13_praiselooks + ammsa_14_fascination + ammsa_15_playcoy + 
                ammsa_16_exaggerate + ammsa_17_urgepartner + ammsa_18_singleinvit + ammsa_20_marital + 
                ammsa_21_steamboil + ammsa_22_retailiate + ammsa_23_onthejob + ammsa_26_alcohol + 
                ammsa_27_wllmntgest

              AS =~ asi_01_nottrlycmplte + asi_19_moralsensib + asi_20_sacrifice + asi_21_reasondemands + 
                asi_22_refinedsense'

# Display Summary Output, CIs (estimated parameters) and goodness of fit
fit0 <- lavaan::cfa(med_model, data=CFA, se='boot')
lavaan::summary(fit0, fit.measures = T,  rsq=T, standardized = T, ci=TRUE)       # lavaan 0.6-12 ended normally after 55 iterations
standardizedSolution(fit0, type = "std.all" )
lavaan::parameterEstimates(fit0, ci = TRUE, level = 0.95)
lavaan::fitMeasures(fit0, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi"))
# 1348.280    0.074    0.094    0.746    5.837 
lavaan::residuals(fit0, type = "cor")

### CFA MODEL PLOT

# BEAUTIFUL plot ex.1 
install.packages("semPlot")
library(semPlot)
install.packages('semPaths')                    # ISSUES INSTALLING
library(semPaths)                               # ISSUES: no package available

semPaths(fit0,'model','est', curvePivot = F, edge.label.cex = 0.5)                    # triangle layout
semPaths(fit0,'model','est', layout = "circle", curvePivot = F, edge.label.cex = 0.5) # circle layout

library(semPlot)
library(semPaths)
semPaths(fit_lv,'model','est', curvePivot = F, edge.label.cex = 0.5)                    # triangle layout
semPaths(fit_lv,'model','std', layout = "circle", curvePivot = F, edge.label.cex = 0.5) # circle layout

library(pacman)
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson)

cols <- wes_palette(name = "Moonrise2", n = 4, type = "discrete")
colorlist <- list(man = cols[2], lat = cols[1])

semPaths(fit0, what = "col", whatLabels = "std", style = "mx",  
         color = colorlist, rotation =1 , layout = "circle", "par", weighted = F, nCharNodes = 7, 
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5) # layouts: "circle/spring/tree2"



#######################################################

LV <- win[c('ammsa_03_emancipated','ammsa_04_custfalsacc','ammsa_05_battlesexes','ammsa_10_suggremarks',
       'ammsa_16_exaggerate','ammsa_19_politicians','ammsa_22_retailiate','ammsa_23_onthejob','ammsa_27_wllmntgest',
       'sdo_09_groupsequal_r','sdo_10_ourideal_r','sdo_11_equalchance_r','sdo_12_eqlizeconds_r','sdo_13_increased_r',
       'sdo_14_fewerprblms_r','sdo_15_eqlincomes_r','sdo_16_dominate_r','asi_02_spclfavours','asi_04_inncntrem', 
       'asi_05_offended','asi_10_appreciate','asi_11_controlmen','asi_14_exgrtwork','asi_15_tightleash', 
       'asi_16_faircomp')]

# Indicators per construct USED IN REDUCED BASELINE MODEL FOR EXAM
ammsa <- win[c('ammsa_03_emancipated','ammsa_04_custfalsacc','ammsa_05_battlesexes','ammsa_10_suggremarks',
             'ammsa_16_exaggerate','ammsa_19_politicians','ammsa_22_retailiate','ammsa_23_onthejob','ammsa_27_wllmntgest')]
sdo <- win[c('sdo_09_groupsequal_r','sdo_10_ourideal_r','sdo_11_equalchance_r','sdo_12_eqlizeconds_r','sdo_13_increased_r',
             'sdo_14_fewerprblms_r','sdo_15_eqlincomes_r','sdo_16_dominate_r')]
asi <- win[c('asi_02_spclfavours','asi_04_inncntrem','asi_05_offended','asi_10_appreciate','asi_11_controlmen','asi_14_exgrtwork'
             ,'asi_15_tightleash','asi_16_faircomp')]

gender <- win[,c(69)]
lav <- cbind(ammsa, sdo, asi, gender)
lav[,c(1:25)]<-scale(lav[,c(1:25)],center=T,scale=F)

# USED MODEL IN EXAM - ONLY MEASUREMENT MODEL HERE
measure_model1 = '
              # Correlation & Covariance
              sdo ~ ammsa 
              asi ~ ammsa
              asi ~~ sdo

              # Measurement Model (Latent Variables)
  sdo =~ sdo_09_groupsequal_r + sdo_10_ourideal_r + sdo_11_equalchance_r + sdo_12_eqlizeconds_r +    
  sdo_13_increased_r + sdo_14_fewerprblms_r + sdo_15_eqlincomes_r + sdo_16_dominate_r
  
  ammsa =~ ammsa_03_emancipated + ammsa_04_custfalsacc + ammsa_05_battlesexes + ammsa_10_suggremarks +    
  ammsa_16_exaggerate + ammsa_19_politicians + ammsa_22_retailiate + ammsa_23_onthejob + ammsa_27_wllmntgest
  
  asi =~ asi_02_spclfavours + asi_04_inncntrem + asi_05_offended + asi_10_appreciate +   
  asi_11_controlmen + asi_14_exgrtwork + asi_15_tightleash + asi_16_faircomp
  '

fit_ms1 <- lavaan::cfa(measure_model1, data=lav, se='boot', bootstrap=1000, std.lv = TRUE)
lavaan::summary(fit_ms1, fit.measures = T,  rsq=T, standardized = T, ci=TRUE)
lavaan::fitMeasures(fit_ms1, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi"))   

# REDUCED BASELINE MODEL - AS USED IN EXAM ###########################################################
latent_model = '
              # direct effect
              ammsa ~ c*sdo
     
              # mediator
              asi ~ a*sdo
              ammsa ~ b*asi
          
              # indirect effect (a*b)
              indirect := a*b
             
              # total effect
              total := c + (a*b)
              
              # Covariance
              sdo ~~ asi

  ammsa =~ ammsa_03_emancipated + ammsa_04_custfalsacc + ammsa_05_battlesexes + ammsa_10_suggremarks + ammsa_16_exaggerate +
  ammsa_19_politicians + ammsa_22_retailiate + ammsa_23_onthejob + ammsa_27_wllmntgest

  sdo =~ sdo_09_groupsequal_r + sdo_10_ourideal_r + sdo_11_equalchance_r + sdo_12_eqlizeconds_r + sdo_13_increased_r + 
  sdo_14_fewerprblms_r + sdo_15_eqlincomes_r + sdo_16_dominate_r
  
  asi =~ asi_02_spclfavours + asi_04_inncntrem + asi_05_offended + asi_10_appreciate + asi_11_controlmen + asi_14_exgrtwork + 
  asi_15_tightleash + asi_16_faircomp
  '

fit_lv <- lavaan::cfa(latent_model, data=lav, se='boot', bootstrap=1000, std.lv = TRUE)
lavaan::summary(fit_lv, fit.measures = T,  rsq=T, standardized = T, ci=TRUE)
# lavaan 0.6-12 ended normally after 35 iterations
# very good RMSEA = 0.037, acceptable SRMR = 0.038, good CFI = 0.969

standardizedSolution(fit_lv, type = "std.all")
lavaan::parameterEstimates(fit_lv, ci = TRUE, level = 0.95)
lavaan::fitMeasures(fit_lv, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi"))   
# chisq   rmsea    srmr     gfi    ecvi     cfi 
# 457.687   0.037   0.038   0.929   1.114   0.969 
lavaan::residuals(fit_lv, type = "cor") # negative residuals
lavResiduals(fit_lv)
vcov(fit_lv) # negative values
parTable(fit_lv)

################### EQUIVALENCE TESTING

install.packages('truthiness')
library(truthiness)
truthiness::equivtest(fit_lv, lav, main_effect = FALSE, delta = 0.14)

library(semPlot)
library(semPaths)
semPaths(fit_lv,'model','est', curvePivot = F, edge.label.cex = 0.5)                    # triangle layout
semPaths(fit_lv,'model','std', layout = "circle", curvePivot = F, edge.label.cex = 0.5) # circle layout

library(pacman)
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson)

cols <- wes_palette(name = "Moonrise2", n = 4, type = "discrete")
colorlist <- list(man = cols[2], lat = cols[1])

semPaths(fit_lv, what = "col", whatLabels = "std", style = "mx",  
         color = colorlist, rotation =1 , layout = "circle", "par", weighted = F, nCharNodes = 7, 
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5) # layouts: "circle/spring/tree2"


# Inverse latent model WITH EQUALITY CONSTRAINTS -> ALTERNATIVE MODEL

latent_model2 = '
              # regression
              ammsa ~ c*asi
     
              # mediator
              sdo ~ a*asi
              ammsa ~ b*sdo
              
              # direct Effect
              direct := c 
          
              # indirect effect (a*b)
              indirect := a*b
             
              # total effect
              total := c + (a*b)
              
              # Correlation & Covariance
              asi ~~ sdo

              # Measurement Model (Latent Variables)
  sdo =~ sdo_09_groupsequal_r + sdo_10_ourideal_r + sdo_11_equalchance_r + sdo_12_eqlizeconds_r +    
  sdo_13_increased_r + sdo_14_fewerprblms_r + sdo_15_eqlincomes_r + sdo_16_dominate_r
  
  ammsa =~ ammsa_03_emancipated + ammsa_04_custfalsacc + ammsa_05_battlesexes + ammsa_10_suggremarks +    
  ammsa_16_exaggerate + ammsa_19_politicians + ammsa_22_retailiate + ammsa_23_onthejob + ammsa_27_wllmntgest
  
  asi =~ asi_02_spclfavours + asi_04_inncntrem + asi_05_offended + asi_10_appreciate +   
  asi_11_controlmen + asi_14_exgrtwork + asi_15_tightleash + asi_16_faircomp
  
            ### Variances & Covariances (IV & M)
            asi ~~ 1*sdo
            ammsa ~~ asi + sdo

            # Equality constraints: Covariances with equal parameter names
            ammsa ~~ a*asi
            ammsa ~~ b*sdo
  '

fit_lv2 <- lavaan::cfa(latent_model2, data=lav, se='boot', bootstrap = 1000, std.lv = TRUE)
lavaan::summary(fit_lv2, fit.measures = T,  rsq=T, standardized = T, ci=TRUE)       # lavaan 0.6-12 ended normally after 35 iterations; # very good RMSEA of 0.037, acceptable SRMR = 0.038, good CFI  0.969
standardizedSolution(fit_lv2, type = "std.all" )
lavaan::parameterEstimates(fit_lv2, ci = TRUE, level = 0.95)
lavaan::fitMeasures(fit_lv2, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi"))
# chisq   rmsea    srmr     gfi    ecvi     cfi 
# 457.687   0.037   0.038   0.929   1.121   0.969 
lavaan::residuals(fit_lv2, type = "cor")

library(pacman)
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson)

cols <- wes_palette(name = "Moonrise2", n = 4, type = "discrete")
colorlist <- list(man = cols[2], lat = cols[1])

semPaths(fit_lv, what = "col", whatLabels = "std", style = "mx",  
         color = colorlist, rotation =1 , layout = "circle", "par", weighted = F, nCharNodes = 7,
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5) # layouts: "circle/spring/tree2"

# constrained model has much lower standardized regression coefficients; but same model fit as not constrained model

##### REAL CONSTRAINED MODEL

constrained.group <- '
# mediator
asi ~ c(ag1,ag2)*sdo
ammsa ~ c(bg1,bg2)*asi

# direct effect
ammsa ~  c(cg1, cg2)*sdo

# indirect effect (a*b)
abg1 := ag1*bg1               # group 1 path comparison
abg2 := ag2*bg2               # group 2 path comparison

# total effect
totalg1 := cg1 + (ag1*bg1)
totalg2 := cg2 + (ag2*bg2)

# Measurement Model (Latent Variables)
sdo =~ sdo_09_groupsequal_r + sdo_10_ourideal_r + sdo_11_equalchance_r + sdo_12_eqlizeconds_r +    
  sdo_13_increased_r + sdo_14_fewerprblms_r + sdo_15_eqlincomes_r + sdo_16_dominate_r

ammsa =~ ammsa_03_emancipated + ammsa_04_custfalsacc + ammsa_05_battlesexes + ammsa_10_suggremarks +    
  ammsa_16_exaggerate + ammsa_19_politicians + ammsa_22_retailiate + ammsa_23_onthejob + ammsa_27_wllmntgest

asi =~ asi_02_spclfavours + asi_04_inncntrem + asi_05_offended + asi_10_appreciate +   
  asi_11_controlmen + asi_14_exgrtwork + asi_15_tightleash + asi_16_faircomp
'

fit.group <- lavaan::sem(constrained.group, data = lav, group = "gender", se="boot", bootstrap=1000, meanstructure = TRUE)
summary(fit.group, fit.measures = TRUE, rsquar = TRUE, standardized = TRUE, ci = TRUE) 
lavaan::fitMeasures(fit.group, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi"))
# worse model fit
# chisq   rmsea    srmr     gfi    ecvi     cfi 
# 858.777   0.048   0.049   0.946   2.305   0.945 

lavTestLRT(fit.group)

semPaths(fit.group, what = "col", whatLabels = "std", style = "mx",  
         color = colorlist, rotation = 3, layout = "tree", "par", weighted = F,
         nCharNodes = 7, shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)

# Testing Path Differences - no significant differences btw gender it seems
library(lavaan)
lavTestWald(fit.group, constraints = "ag1==ag2") 
#$stat [1] 2.36822, $df [1] 1, $p.value [1] 0.1238282, $se [1] "bootstrap" -> insignificant?
lavTestWald(fit.group, constraints = "bg1==bg2") # $stat [1] 0.7091868, $df [1] 1,  $p.value [1] 0.3997139; not sign. either



# Mediational Model - Direct Indirect Effects

MED0 = '      
              # mediator
              asi ~ c(ag1,ag2)*sdo
              ammsa ~ c(bg1,bg2)*asi

              # direct effect
              ammsa ~  c(cg1, cg2)*sdo

              # indirect effect (a*b)
              abg1 := ag1*bg1               # group 1 path comparison
              abg2 := ag2*bg2  
              
              # total effect
              totalg1 := cg1 + (ag1*bg1)
              totalg2 := cg2 + (ag2*bg2)

              # Measurement Model (Latent Variables)
  sdo =~ sdo_09_groupsequal_r + sdo_10_ourideal_r + sdo_11_equalchance_r + sdo_12_eqlizeconds_r +    
  sdo_13_increased_r + sdo_14_fewerprblms_r + sdo_15_eqlincomes_r + sdo_16_dominate_r
  
  ammsa =~ ammsa_03_emancipated + ammsa_04_custfalsacc + ammsa_05_battlesexes + ammsa_10_suggremarks +    
  ammsa_16_exaggerate + ammsa_19_politicians + ammsa_22_retailiate + ammsa_23_onthejob + ammsa_27_wllmntgest
  
  asi =~ asi_02_spclfavours + asi_04_inncntrem + asi_05_offended + asi_10_appreciate +   
  asi_11_controlmen + asi_14_exgrtwork + asi_15_tightleash + asi_16_faircomp
  
            # latent variable disturbance covariances
            sdo ~~ asi
            ammsa ~~ sdo
            ammsa ~~ asi
            ammsa ~~ asi + sdo
            
            # LV -> endogenous disturbance term
            ammsa ~~ ammsa
  '

fit.00 <- lavaan::sem(MED0, data = lav, group= "gender", se="boot", bootstrap=1000, meanstructure = TRUE)
summary(fit.00, fit.measures = TRUE, rsquar = TRUE, standardized = TRUE, ci = TRUE) 
lavaan::fitMeasures(fit.00, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi"))
# chisq   rmsea    srmr     gfi    ecvi     cfi 
# 858.777   0.048   0.049   0.946   2.328   0.944 

# Chi Square Difference

lavTestLRT(fit.gender.config, fit.gender.weak, fit.gender.strong, fit.gender.strict, method = "default", A.method = "delta", scaled.shifted = TRUE, H1 = TRUE, type = "Chisq",model.names = NULL) # some not all have scaled stats

anova(fit.gender.config, fit.gender.weak, fit.gender.strong, fit.gender.strict)

######### chck negative indefninite covariance matrix issues

PCor <- cor(lav, method = "pearson")
PosCor <- subset(PCor, !rowSums(PCor < 0))
View(PosCor)
Pcov <- cov(PCor)
View(Pcov) # negative values
PCov <- cov(PosCor)
eigen(PCor) # negative eigenvalues
eigen(PosCor) # non-square matrix in 'eigen'
eigen(PCov) # lots of negative values again
PosOnly <- as.data.frame(PCor[PCor < 0.8 | PCor ==1] <- "")

######################### MEASUREMENT INVARIANCE

library(dplyr)
invariance.data <- lav %>% 
  select(gender, starts_with("ammsa"), starts_with("sdo"), starts_with("asi")) %>% 
  mutate_all(as.numeric)

fit.gender.config0 <- cfa(constrained.group,
                             data = invariance.data,
                             meanstructure = TRUE,
                             estimator = 'MLM',
                             group = "gender")
summary(fit.gender.config0, standardized = TRUE, fit.measures = TRUE)
# Model Test User Model: Test statistic 862.791, df 545, p = 0.000
lavaan::fitMeasures(fit.gender.config0, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi"))
# chisq   rmsea    srmr     gfi    ecvi     cfi 
# 862.791   0.048   0.052   0.946   2.309   0.945 

fit.gender.weak <- cfa(constrained.group,
                           data = invariance.data,
                           meanstructure = TRUE,
                           se = "robust",
                           group = "gender",
                           group.equal = c("loadings"))

summary(fit.gender.weak, fit.measures = TRUE, standardized = TRUE, rsquar = TRUE, ci = TRUE)
# Model Test User Model: Test statistic 913.402, df 567,  P-value 0.000
lavaan::fitMeasures(fit.gender.weak, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi"))
# chisq   rmsea    srmr     gfi    ecvi     cfi 
# 913.402   0.049   0.069   0.942   2.322   0.940 

fit.gender.strong <- cfa(latent_model,
                             data = invariance.data,
                             meanstructure = TRUE,
                             se = "robust",
                             group = "gender",
                             group.equal = c("loadings", "intercepts"))
summary(fit.gender.strong, standardized = TRUE, fit.measures = TRUE, rsquar = TRUE, ci = TRUE)
# Model Test User Model: Test statistic 951.351, df  589, P-value  0.000
lavaan::fitMeasures(fit.gender.strong, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi")) 
# chisq   rmsea    srmr     gfi    ecvi     cfi 
# 951.351   0.049   0.071   0.940   2.310   0.937 

fit.gender.strict <- cfa(latent_model,
                             data = invariance.data,
                             meanstructure = TRUE,
                             se = "bootstrap",
                             bootstrap = 1000,
                             group = "gender",
                             group.equal = c("loadings", "intercepts", "residuals"))
summary(fit.gender.strict, standardized = TRUE, fit.measures = TRUE, rsquar = TRUE, ci = TRUE)
#Model Test User Model: Test statistic 1006.187, df 614, P-value  = 0.000
# Model Test Baseline Model: Test statistic  6352.088, df = 600, P-value  = 0.000
lavaan::fitMeasures(fit.gender.strict, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi")) 
# chisq    rmsea     srmr      gfi     ecvi      cfi 
# 1006.187    0.050    0.072    0.937    2.319    0.932 

################## DELTA CHI SQUARE DIFFERENCE OF MODELS -> EQUIVALENCE TESTING

conweak <- compareFit(fit.gender.config, fit.gender.weak)
summary(conweak)
weakscal <- compareFit(fit.gender.weak, fit.gender.strict)
summary(weakscal)
scalstrict <- compareFit(fit.gender.strict, fit.gender.strong)
summary(scalstrict)

library(semTools)
comp_all <- compareFit(fit.gender.config, fit.gender.weak, fit.gender.strong, fit.gender.strict, nested=T)
summary(comp_all)
showMethods(summary)

summary.FitDiff<-function(object){
  print(object@nested)
  return(object@fit)
}

a<-semTools::compareFit(fit.gender.config, fit.gender.weak, fit.gender.strong, fit.gender.strict, nested=T)
summary(a)

# plot model for latent variables mediation model

install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson)

cols <- wes_palette(name = "Moonrise2", n = 4, type = "discrete")
colorlist <- list(man = cols[2], lat = cols[1])

semPaths(fit_lv, what = "col", whatLabels = "std", style = "mx",  
         color = colorlist, rotation =1 , layout = "circle", "par", weighted = F, nCharNodes = 7, 
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5) # layouts: "circle/spring/tree2"

# plot constrained group model
semPaths(fit.group, what = "col", whatLabels = "std", style = "mx",  
         color = colorlist, rotation =1 , layout = "tree", "par", weighted = F, nCharNodes = 7, 
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5) # layouts: "circle/spring/tree2"

lavaan::fitMeasures(fit.group, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi"))
# chisq   rmsea    srmr     gfi    ecvi     cfi 
# 858.777   0.048   0.049   0.946   2.305   0.945 
lavaan::summary(fit.group, fit.measures = T,  rsq=T, standardized = T, ci=TRUE)

########################################################

# Local model fit: Comparing the empirical and implied variance-covariance matrix
library(lavaan)
lavInspect(fit_lv, what = "sampstat") # 1 negative value -> sdo 16 corr ammsa 19

# The variance-covariance matrix implied by the model is obtained with what = 'implied':
lavInspect(fit_lv, what = "implied") # all positive values

# res matrix implied by model
lavInspect(fit_lv, what = "resid") # lots of negative residuals

# Relevant for local fit diagnostics: variance-covariance matrix of stand. residuals, using function resid()
resid(fit_lv, type = "standardized") # Error in Q %*% ACOV.obs[[g]] : requires numeric/complex matrix/vector arguments

resid(fit_lv, type = "normalized") # Error in dimnames(x) <- dn : length of 'dimnames' [2] not equal to array extent

fitted(fit_lv) # all positive
coef(fit_lv)

lavaan::fitMeasures(fit_lv, c("chisq", "rmsea", "srmr", "gfi", "ecvi", "cfi"))

lavaan::summary(fit_lv, fit.measures = T,  rsq=T, standardized = T, ci=TRUE)

# plot the residuals

plot(latent_model) # errors


# R- Square of my model

inspect(fit_lv,'r2') # error for entire model; but works with fitted model

# If error for not positive definite values shows up
det(lavInspect(fit_lv, "cov.lv")) # 0.9488746
eigen(lavInspect(fit_lv, "cov.lv"))$values # all positive eigenvalues -> 3.2642743 0.7586177 0.3831768

# Check Residuals
cor_t <- residuals(fit_lv, type = "cor")$cov
print(cor_t) # seems like a good fit, no residuals equal or more than 0.10/.20 (5 corr pairs - sdo 16*ammsa 19; asi2*ammsa04; asi15*ammsa04; asi15*ammsa23; asi15*ammsa27; all below .2 -> GOOD); BUT MANY NEGATIVE VALUES

# Plotting residuals
res1 <- residuals(fit_lv, type = "cor")$cov  #extract the residuals from the model fit
res1[upper.tri(res1,diag = TRUE)] <- NA #get rid of the duplicates and diagonal values
v1 <- as.vector(res1) #create a vector for a
v2 <- v1[!is.na(v1)]
car::qqPlot(v2,id = F)

# Circle Layout
semPaths(fit_lv,'model','est', layout = "circle", curvePivot = F, edge.label.cex = 0.5) 

# Visualize Model as Path Diagram
install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson)

cols <- wes_palette(name = "Moonrise2", n = 4, type = "discrete")
colorlist <- list(man = cols[2], lat = cols[1])

semPaths(fit_lv, what = "col", whatLabels = "std", style = "mx",  
         color = colorlist, rotation =1 , layout = "circle", "par", weighted = F, nCharNodes = 7, 
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5) # layouts: "circle/spring/tree2"



############ FACTORIAL INVARIANCE

table(cfa_new$dem_01_gender) ##request a frequency summary
cfa_new$dem_01_gender <- as.factor(cfa$dem_01_gender)
levels(cfa_new$dem_01_gender) <- c("Girls", "Boys") ##assign level labels
table(cfa_new$dem_01_gender)

# Configural Invariance 

conf <- semTools::measurementInvariance(model = med3, data = cfa_new, group = "dem_01_gender")
std_conf <- semTools::measurementInvariance(model = med3, data = cfa_new,  std.lv = TRUE, group = "dem_01_gender") 
#the best way to do invariances: Configural, Metric(weak), Scalar(strong), Residual/Strict
##delta χ2, where a significant increase in χ2 indicates that the additional constraints imposed on the model (compared with the previous model) cannot be justified
##delta CFI, where a decrease in CFI of more than .01 is considered representative of a decrease in model fit to the extent that the additional constraints imposed on the model (compared with the previous model) cannot be justified
#cant go beyond Metric. Groups are measured the same way but cannot be compared (because fit.loadings 0.0869 non sig; we constrained the loadings for Metric)
#rmsea prefers model wtih more Dfs that is why it is senstive to them

# Configuration
invC <- cfa(model = med3, data = cfa_new, std.lv = TRUE,
            missing = "fiml", mimic = "Mplus", group = "dem_01_gender")
lavInspect(invC, "cov.lv")
summary(invC, standardized = TRUE, fit.measures = TRUE)

# Metric (weak)
invW <- cfa(med3, data = cfa_new, group = "dem_01_gender", 
            group.equal = c("loadings"), meanstructure = TRUE)
summary(invW, standardized = TRUE, fit.measures = TRUE)

# Scalar 
invS <- cfa(med3, data = cfa_new, group = "dem_01_gender", 
            group.equal = c("loadings", "intercepts"), meanstructure = TRUE)
summary(invS, standardized = TRUE, fit.measures = TRUE)

# Model comparison
compareFit(invW, invS)
lavTestScore(invS) # influential parameters are those with a p-value <.05 -> we can adjust scalar
### 14 .p32. == .p71. 5.709  1   0.017

parTable(invS)

# Adjust the Scalar Model
invS2 <- cfa(med3, data = cfa_new, estimator = "WLSMV", group = "dem_01_gender", group.equal = c
             ("loadings","intercepts"), group.partial = c("item14 ~ 1"))

# Model comparison
compareFit(invW, invS2) # error messages

fitMeasures(invC) # bad CFI, ral bad RMSEA .125
fitMeasures(invW) # good CFI, good SRMR: 0.065 + acceptable RMSEA: 0.071
fitMeasures(invS) # just so good CFI: .956, good SRMR: 0.068, acceptable RMSEA: 0.072

# Strict (= Based on ADJUSTED Scalar Model)

invS <- cfa(med3, data = cfa_new, estimator = "WLSMV", group = "dem_01_gender", 
            group.equal = c("loadings","intercepts", "residuals"), group.partial = c("item1 ~ 10", "item22 ~ 1"))
compWS <- compareFit(invW2, invS)
summary(compWS) # Error / Weird output

# Fitted Mediation Model SEM

fitmed <- lavaan::sem(med3, data = cfa_new, group = "dem_01_gender",)
summary(fitmed, fit.measures = TRUE, rsquar = TRUE, standardized = TRUE, ci = TRUE) 
lavaan::fitMeasures(fitmed, c("chisq", "rmsea", "srmr", "gfi", "ecvi"))

# Model Test User Model: T stat = 82.869, DF = 48,  P-value (Chi-square)= 0.001 - sign., T stat Group 2 (boys) = 36.976, (Group 1 (girls) = 45.893, MOdel Test Baseline Model: T stat=970.862, DF = 72, p-value 0.000; User Model vs Baseline Model (Comaprison) = good CFI: .961, Tucker Lewis Index (TLI): .942

# Just so acceptable RMSEA: 0.075 BUT NON-SIGN., good SRMR 0.055, good CFI: .961, Tucker Lewis Index (TLI): .942
# chisq  rmsea   srmr    gfi   ecvi 
# 83.175  0.075  0.055  0.977  0.792 

strict <- cfa(fitmed, data = cfa_new, estimator = "WLSMV", group = "dem_01_gender", group.equal = c("loadings","intercepts", "residuals"), group.partial = c("item14 ~ 1"))
summary(invSt)

# Model Comparison of ALL MODELS as NESTED MODELS
comp <- compareFit(invC, invW, invS, strict, nested = TRUE) # error messages
summary(comp)

anova(invC, invW, invS)

# Model Fit based on GENDER
fit_MED <- lavaan::sem(med3, data = cfa_new, group = "dem_01_gender")
summary(fit_MED, fit.measures = TRUE, rsquar = TRUE, standardized = TRUE, ci = TRUE)
#2 groups, check the regression coeff and CIs of groups, if CIs don't overlap it means that there is a sig diff between groups -> regression coeffificents are the same -> no difference btw gender

# Path Diagram

semPaths(fitmed, whatLabels = 'std', rotation = 3) # Cool looking SEM plot 

semPaths(fitmed, what = "col", whatLabels = "std", style = "mx",  
         color = colorlist, rotation = 3, layout = "circle2", "par", weighted = F,
         nCharNodes = 7, shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)



med4 = '
              ### Regressions
              
              # IV Regression
              AMMSA ~ c*AS
              
              # Mediator Regression - REVERSED
              SDO ~ a*AS
              AMMSA ~ b*SDO
              
              # Direct Effect
              direct := c
             
              # Indirect Effect (a*b)
              indirect := a*b
             
              # Total Effect
              total := c + (a*b)
              
              ### Measurement Model (Latent Variables)
              SDO =~ sdo_09_groupsequal_r + sdo_10_ourideal_r + sdo_12_eqlizeconds_r
                
              AMMSA =~ ammsa_03_emancipated + ammsa_04_custfalsacc + ammsa_27_wllmntgest

              AS =~ asi_01_nottrlycmplte + asi_14_exgrtwork + asi_20_sacrifice
          
              ### Variances & Covariances (IV & M)
              AS ~~ 1*SDO
              AMMSA ~~ AS + SDO
            
              # Equality constraints: Covariances with equal parameter names
              AMMSA ~~ a*AS
              AMMSA ~~ b*SDO
        '

fit_4 <- cfa(med4, data = cfa_new, se='boot', bootstrap=1000)
lavaan::summary(fit_4, fit.measures = T,  rsq=T, standardized = T, ci=TRUE)  # lavaan 0.6-12 ended normally after 43 iterations - Maximum Likelihood Estimation
standardizedSolution(fit_4, type = "std.all" )
lavaan::parameterEstimates(fit_4, ci = TRUE, level = 0.95)
lavaan::fitMeasures(fit_4, c("chisq", "rmsea", "srmr", "gfi", "ecvi"))

### NO DIFFERENCE BY SWAPPING IV AND MEDIATOR AT LEAST AT FIRST GLANCE, PERHAPS A/B PATH DIFFERENCES...

# Constrained Model

model.group <- ' 
# mediator
AS ~ c(ag1,ag2)*SDO
AMMSA ~ c(bg1,bg2)*AS

# direct effect
AMMSA ~  c(cg1, cg2)*SDO

# indirect effect (a*b)
abg1 := ag1*bg1               # group 1 path comparison
abg2 := ag2*bg2               # group 2 path comparison

# total effect
totalg1 := cg1 + (ag1*bg1)
totalg2 := cg2 + (ag2*bg2)

### Measurement Model (Latent Variables)
              SDO =~ sdo_09_groupsequal_r + sdo_10_ourideal_r + sdo_12_eqlizeconds_r
              AMMSA =~ ammsa_03_emancipated + ammsa_04_custfalsacc + ammsa_27_wllmntgest
              AS =~ asi_01_nottrlycmplte + asi_14_exgrtwork + asi_20_sacrifice
'

fit.group <- lavaan::sem(model.group, data = cfa_new, group = "dem_01_gender", se="boot", bootstrap=500, meanstructure = TRUE)
summary(fit.group, fit.measures = TRUE, rsquar = TRUE, standardized = TRUE, ci = TRUE) 
lavaan::fitMeasures(fit.group, c("chisq", "rmsea", "srmr", "gfi", "ecvi"))
# SLIGHTLY BETTER MODEL THAN PREVIOUS ONES - RMSEA a bit beter

semPaths(fit.group, what = "col", whatLabels = "std", style = "mx",  
         color = colorlist, rotation = 3, layout = "tree", "par", weighted = F,
         nCharNodes = 7, shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)

# Testing Path Differences
library(lavaan)
lavTestWald(fit.group, constraints = "ag1==ag2") 
#$stat [1] 0.02327439, $df [1] 1, $p.value [1] 0.8787457, $se [1] "bootstrap" -> insignificant?
lavTestWald(fit.group, constraints = "bg1==bg2") # not sign. either


# Configural Invariance 

conf2 <- semTools::measurementInvariance(model = model.group, data = cfa_new, group = "dem_01_gender")
std_conf2 <- semTools::measurementInvariance(model = model.group, data = cfa_new,  std.lv = TRUE, group = "dem_01_gender") 
#the best way to do invariances: Configural, Metric(weak), Scalar(strong), Residual/Strict
##delta χ2, where a significant increase in χ2 indicates that the additional constraints imposed on the model (compared with the previous model) cannot be justified
##delta CFI, where a decrease in CFI of more than .01 is considered representative of a decrease in model fit to the extent that the additional constraints imposed on the model (compared with the previous model) cannot be justified
#cant go beyond Metric. Groups are measured the same way but cannot be compared (because fit.loadings 0.0869 non sig; we constrained the loadings for Metric)
#rmsea prefers model wtih more Dfs that is why it is senstive to them

# Configuration
invC <- cfa(model = model.group, data = cfa_new, std.lv = TRUE,
            missing = "fiml", mimic = "Mplus", group = "dem_01_gender")
lavInspect(invC, "cov.lv")
summary(invC, standardized = TRUE, fit.measures = TRUE)

# Configural with robust se & MLM

invC2 <- cfa(model = model.group, data = cfa_new, estimator = "MLM", se = "robust", group = "dem_01_gender")
fitMeasures(invC2, c("chisq", "rmsea", "srmr", "gfi", "ecvi"))
summary(invC2, fit.measures = TRUE, rsquar = TRUE, standardized = TRUE, ci = TRUE) 

# chisq  rmsea   srmr    gfi   ecvi 
# 82.869  0.075  0.055  0.977  0.792 
# good fit RMSEA is alright, SRMR is good, CFI just so good

# Metric (weak)
invW <- cfa(med3, data = cfa_new, group = "dem_01_gender", 
            group.equal = c("loadings"), meanstructure = TRUE)
summary(invW, standardized = TRUE, fit.measures = TRUE) # good CFI, good srmr, accept rmsea

# Metric with robust se & MLM
invW2 <- cfa(model = model.group, data = cfa_new, estimator = "MLM", se = "robust", group = "dem_01_gender", group.equal = c("loadings"), meanstructure = TRUE)
fitMeasures(invW2, c("chisq", "rmsea", "srmr", "gfi", "ecvi"))
summary(invW2, fit.measures = TRUE, rsquar = TRUE, standardized = TRUE, ci = TRUE) 

# chisq  rmsea   srmr    gfi   ecvi 
# 86.244  0.068  0.062  0.976  0.759
# increased Chi-Square compared to Configural Model



# Scalar 
invS <- cfa(med3, data = cfa_new, group = "dem_01_gender", 
            group.equal = c("loadings", "intercepts"), meanstructure = TRUE)
summary(invS, standardized = TRUE, fit.measures = TRUE) # okay model


invS2 <- cfa(model = model.group, data = cfa_new, estimator = "MLM", se = "robust", group = "dem_01_gender", 
             group.equal = c("loadings", "intercepts"), meanstructure = TRUE)
fitMeasures(invS2, c("chisq", "rmsea", "srmr", "gfi", "ecvi"))
summary(invS2, fit.measures = TRUE, rsquar = TRUE, standardized = TRUE, ci = TRUE) 


# Model comparison
compareFit(invW, invS)
lavTestScore(invS) # influential parameters are those with a p-value <.05 -> we can adjust scalar
### 14 .p32. == .p71. 5.709  1   0.017

parTable(invS)

# Adjust the Scalar Model
invS2 <- cfa(med3, data = cfa_new, estimator = "WLSMV", group = "dem_01_gender", group.equal = c
             ("loadings","intercepts"), group.partial = c("item14 ~ 1"))

# Model comparison
compareFit(invW, invS2) # error messages

fitMeasures(invC) # ok CFI, ok rmsea, good srmr, but normed chi-square is bad (X2 divided by df)
fitMeasures(invW) # same here, all ok
fitMeasures(invS) # same again

com1 <- compareFit(invC, invW)


# Strict (= Based on ADJUSTED Scalar Model)

invS <- cfa(med3, data = cfa_new, estimator = "WLSMV", group = "dem_01_gender", 
            group.equal = c("loadings","intercepts", "residuals"), group.partial = c("item1 ~ 10", "item22 ~ 1"))
compWS <- compareFit(invW2, invS)
summary(compWS) # Error / Weird output

# Fitted Mediation Model SEM

fitmed <- lavaan::sem(model.group, data = cfa_new, group = "dem_01_gender",)
summary(fitmed, fit.measures = TRUE, rsquar = TRUE, standardized = TRUE, ci = TRUE) 
lavaan::fitMeasures(fitmed, c("chisq", "rmsea", "srmr", "gfi", "ecvi"))

# just so acceptabl RMSEA: 0.075, good SRMR 0.055, good CFI: .961
# chisq  rmsea   srmr    gfi   ecvi 
# 83.175  0.078  0.057  0.977  0.801 

strict <- cfa(fitmed, data = cfa_new, estimator = "WLSMV", group = "dem_01_gender", group.equal = c("loadings","intercepts", "residuals"), group.partial = c("item14 ~ 1"))
summary(invSt)

# Model Comparison of ALL MODELS as NESTED MODELS
comp <- compareFit(invC, invW, invS, strict, nested = TRUE) # error messages
summary(comp)

# Model Fit based on GENDER
fit7 <- lavaan::sem(model.group, data = cfa_new, group = "dem_01_gender")
summary(fit7, fit.measures = TRUE, rsquar = TRUE, standardized = TRUE, ci = TRUE)
#2 groups, check the regression coeff and CIs of groups, if CIs don't overlap it means that there is a sig diff between groups -> regression coeffificents are the same -> no difference btw gender

modindices(fit7,sort=TRUE)

# The End :)
