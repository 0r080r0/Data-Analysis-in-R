rm(list=ls())

# loading data from excel file

install.packages("readxl")
library("readxl")
Data <- read_excel(file.choose())
Data

#RT's

StroopSmall <- ((Data$RT_incongr_XS - Data$RT_congr_XS) + (Data$RT_incongr_S - Data$RT_congr_S))/2
StroopLarge <- ((Data$RT_incongr_L - Data$RT_congr_L) + (Data$RT_incongr_XL - Data$RT_congr_XL))/2
StroopDifference <- (StroopSmall - StroopLarge)

# T-tests & 95% CI's

install.packages("lsr")
library("lsr")
oneSampleTTest(StroopSmall, mu=0)
oneSampleTTest(StroopLarge, mu=0)
pairedSamplesTTest(~ StroopSmall + StroopLarge)

# Error Bar Plot - Attempts

install.packages("gplots")
library("gplots")

StroopAll <- data.frame(StroopSmall, StroopLarge, StroopDifference) # 3 variables, 200 observations
View(StroopAll)

install.packages("sciplot")
library(sciplot)

plot(StroopAll) # scatterplot matrix

install.packages("ggplot2")
library("ggplot2")

# 1st trial to compute error bar plot

SSciM <- ciMean(x = StroopSmall, conf = .95)
SSciM
SLciM <- ciMean(x = StroopLarge, conf = .95)
SLciM
SDifciM <- ciMean(x = StroopDifference, conf = .95)
SDifciM
ciMeansTotal <- data.frame (SSciM, SLciM, SDifciM) # problem: creates 1 variable with all 6 observations

bargraph.CI(StroopAll, Data, ci.fun = ciMeansTotal, "RTsIncongr - RTsCongr") # error: arguments length

# 2nd trial to compute error bar plot

plotmeans( StroopAll ~ StroopSmall + StroopLarge + StroopDifference, Data, n.label = FALSE ) # error: cannot find "plotmeans"


# Further steps - Cheat Sheet Codes:

d <- ggplot(mpg, aes(fl)) # creates 9 variables with strange content unrelated to Stroop Data

qplot(x = cty, y = hwy, data = mpg, geom = “point")
ggplot (data = Data, aes( x = StroopAll, y = ciMeansTotal))

# visualizing error

df <- data.frame(grp = c("A", "B"), fit = 4:5, se = 1:2)
j <- ggplot(df, aes(grp, fit, ymin = fit-se, ymax = fit+se))