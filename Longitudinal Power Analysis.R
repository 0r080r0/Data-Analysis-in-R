### App to perform longitudinal power analysis

list.of.packages = c("htmltools","shiny","htmltools","shiny","DT","nlme","ggplot2","gridExtra",
                     "data.table","plyr","dplyr","formattable","tidyr","MASS","shinyjs","compiler","future.apply","devtools")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(htmltools)
library(shiny)
library(DT)
library(nlme)
library(ggplot2)
library(gridExtra)
library(data.table)
library(plyr)
library(dplyr)
library(formattable)
library(tidyr)
library(MASS)
library(shinyjs)
library(compiler)
library(future.apply)

library(devtools)
devtools::install_github("ginettelafit/PowerAnalysisIL", force = T)

library(PowerAnalysisIL)

# Using Gist: users can launch this app with:
shiny::runGist('6bac9d35c2521cc4fd91ce4b82490236')




