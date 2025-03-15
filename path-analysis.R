# ... = name of package
# install.packages('...')       
install.packages('...')
# check if installation was successful
library(insert packagename)

# install lavaan for SEM
install.packages('lavaan', dependencies = TRUE)
library(lavaan)

# read in "sav." file

install.packages('haven')
yes
library(haven)
data <- read_sav('/Users/evangelia_karakoliou/Downloads/Album Sales.sav')
names(data) <- c('adverts', 'sales', 'airplay', 'image')
View(data)

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

stress_model <- ('
                 #regression
                 stress ~ satisfaction
                 stress ~ support
                 stress ~ demands
                 
                 turnover_intent ~ stress
                 ')

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
