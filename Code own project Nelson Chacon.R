##############################
#Own Project Script: Predicting labor informality
##############################

#Load database from github repository
#if(!require(haven)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

#library(haven)
#BOL_2018m11_BID <- read_dta("BOL_2018m11_BID.dta")
#View(BOL_2018m11_BID)

#Save data
#save.image("es_2018.Rdata")

#load data
#load(file = "es_2018.Rdata")

#Install required packages
if(!require(foreign)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gt)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#if(!require(scales)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

#Load libraries we will use
library(tidyverse)
library(caret)
#library(data.table)
#library(lubridate)
library(ggplot2)
library(gt)
library(foreign)
#library(scales)
