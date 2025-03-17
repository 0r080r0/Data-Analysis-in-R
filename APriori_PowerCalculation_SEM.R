# Power Calculation for CFA / SEM

# install packages

install.packages("semPower")

# load libraries
library(semPower)

# A priori power analysis (model-free)

ap <- semPower(type = "a-priori", effect = .05, effect.measure = "RMSEA", alpha = .05, power = .80, df = 50) # note: effect size 0.3 to 0.5 is considered small; 0.5 to 0.8 medium, and 0.8 and higher as large
summary (ap) # required no. of observations: N = 243


# A priori power analysis (seems the correct one)

ap2 <- semPower(type = "a-priori", effect = .05, effect.measure = "RMSEA", alpha = .05, power = .80, df = 75) # df = 75 -> based on total of 75 items across 3 measurement scales used in this study
summary (ap2) # required no. of observations: N = 193


