# Path Analysis in R

# install packages & load libraries
install.packages('lavaan', dependencies = TRUE)
library(lavaan)
install.packages('haven')
yes
library(haven)

# read in "sav." file
data <- read_sav('/Users/evangelia_karakoliou/Downloads/Album Sales.sav')
names(data) <- c('adverts', 'sales', 'airplay', 'image')
View(data)

# define model and parameters for regression
album_model <- ('
                # regression
                sales ~ adverts
                sales ~ image
                sales ~ airplay
                
                # covariates -> round arrows on picture
                adverts ~~ image
                adverts ~~ airplay
                adverts ~~ image
                ')

library(lavaan) # re-did function, because of trouble to use sem-function
# check model fit
fit_album <- sem(album_model, data)
summary (fit_album, standardize = TRUE, fit.measures = TRUE)

install.packages("zeallot")
install.packages('semPlot', dependencies = TRUE)
yes
library(semPlot)          # error
semPaths(fit_album
         , what = 'est, std'
         , style = 'lisrel')    # error

?semPaths

install.packages('effectsize')
library(effectsize)

parameterestimates(fit_album) # load lavaan using library function prior to this command

# create linear regression model
lm_model <- lm(sales ~., data)
effectsize(lm_model)

# bootstrapping with a random sample of 1000
boots <- bootstrapLavaan(fit_album, R = 1000)
boots <- data.frame(boots)

# packages for ggplot
install.packages('ggridges')
library(ggridges)

mahalanobis(data, center=TRUE, cov = vcov(data))

# new data 

library(haven)
data <- read_sav('/Users/evangelia_karakoliou/Downloads/path.sav')
View(data)
summary(data)

ggplot()

install.packages('MVN')
yes
library(MVN)                          # error
mvn(cfa_data, mvnTest = 'mardia')     # error
cor(data)

# define model
stress_model <- ('
                 #regression
                 stress ~ satisfaction
                 stress ~ support
                 stress ~ demands
                 
                 turnover_intent ~ stress
                 ')

# check model fit
stress.fit <- sem(stress_model, data) # error
semPaths(stress.fit
         , what = 'est, std'
         , nCharNotes = 0
         , sizeMan = 10
         , edge.label.cex = 1.4
         , residuals = FALSE
         , curvePivot = TRUE
         , rotation = 2)
summary(stress.fit, standardize = TRUE, fit.measures = TRUE)
modificationindices(stress.fit) %>% arrange(-mi) %>% head(20)

# define model
turnover_model <- ('
                   #regression
                   satisfaction ~ stress
                   satisfaction ~ support
                   satisfaction ~ demands
                   turnover_intent ~ stress
                   
                   #covariates
                   stress ~~ support
                   stress ~~ demands
                   support ~~ demands
                   ')
fit.turnover <-sem(turnover_model, data, estimator = 'MLM')
summary(fit.turnover, standardize = TRUE, fit.measures = TRUE)
