##############################
#Own Project Script: Predicting labor informality
##############################

#Install required packages
if(!require(foreign)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gt)) install.packages("gt", repos = "http://cran.us.r-project.org")
#if(!require(scales)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(repmis)) install.packages("repmis", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")


#Load libraries we will use
library(tidyverse)
library(caret)
#library(data.table)
#library(lubridate)
library(ggplot2)
library(gt)
library(foreign)
#library(scales)
library(repmis)
library (readr)


#Load database from github repository (csv format)

urlfile="https://raw.githubusercontent.com/nelchacon/My_project_edx/main/hs_2018.csv"

mydata<-read_csv(url(urlfile))


