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
View(mydata)

# Wrangling and descriptive analysis

summary(mydata)

# Percentage of informal workers by gender
table1 <- table(mydata$gender, mydata$formality)
table1
prop.table(table1,1)


# Percentage of informal workers by zone
table2 <- table(mydata$zone, mydata$formality)
table2
prop.table(table2,1)

# Percentage of informal workers by race
table3 <- table(mydata$indigenous, mydata$formality)
table3
prop.table(table3,1)


# Percentage of informal workers by labor category
table4 <- table(mydata$labcat, mydata$formality)
table4
prop.table(table4,1)


# Percentage of informal workers by labor branch
table5 <- table(mydata$labbranch, mydata$formality)
table5
prop.table(table5,1)

# Percentage of informal workers by firm size
table6 <- table(mydata$firm_size, mydata$formality)
table6
prop.table(table6,1)


# Percentage of informal workers by labor market
table7 <- table(mydata$labor_market, mydata$formality)
table7
prop.table(table7,1)

# Percentage of informal workers by labor market
table8 <- table(mydata$poverty, mydata$formality)
table8
prop.table(table8,1)

#average income for formal and informal workers
mydata %>% group_by(formality) %>% summarize(Avg = mean(work_income, na.rm=T))


#average working hours for formal and informal workers
mydata %>% group_by(formality) %>% summarize(Avg = mean(work_hours, na.rm=T))

#average labor seniority for formal and informal workers
mydata %>% group_by(formality) %>% summarize(Avg = mean(job_tenure, na.rm=T))

#Graphics

# Labor Informality by gender
inf_gender <- data.frame(Gender=c("Men", "Women"),
                 len=c(0.791,0.822))
head(inf_gender)

p<-ggplot(data=inf_gender, aes(x=Gender, y=len)) +
  geom_bar(stat="identity")
p

# Basic barplot
plot1 <- ggplot(data=mydata, aes(x=sexo_ci, y=formal_ci)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_bar(stat="identity")
plot1

# Horizontal bar plot
plot1 + coord_flip()


#test

ggplot(mydata, aes(x= formal_ci,  group=sexo_ci)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="gender") +
  facet_grid(~formal_ci) +
  scale_y_continuous(labels = scales::percent)






library(ggplot2)
library(scales)

ggplot(mydata, aes(x=sexo_ci, group=formal_ci)) +
  geom_bar(aes(y=..prop.., fill = factor(..x..)), stat="count") +
  scale_y_continuous(labels=percent_format())

#generating data for graphs

variable <- c(rep("gender" , 2) , rep("zone" , 2) , rep("race" , 2) , rep("firm size" , 3), rep("poverty" , 2), rep("labor market" , 6), rep("labor category" , 5) )
categories <- c("men" , "women" , "urban", "rural" , "indigenous", "not indigenous", "small", "medium", "large", "poor", "not poor", "doemstic", "government", "family", "pseudo business", "business", "unpaid", "other", "boss", "self-employed", "employee", "unpaid")
value <- abs(rnorm(22 , 0 , 15))
data <- data.frame(variable,categories,value)


#graphs for categorical variables
ggplot(data, aes(fill=categories, y=value, x=categories)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Studying 4 species..") +
  facet_wrap(~variable) +
  theme(legend.position="none") +
  xlab("")

# Pre-processing

#Working only with economically active population (EAP)

table(mydata$eapop) #We have 17659 EAP in our sample

eap.data <- filter(mydata, eapop==1)
View(eap.data)

#Cleaning outliers

boxplot(eap.data$age)  #there are some outliers
boxplot(eap.data$work_hours) #there are some outliers
boxplot(eap.data$work_income) #there are many outliers
boxplot(eap.data$educ) # no outliers
boxplot(eap.data$job_tenure) #there are many outliers too

#creating Tukey's rule for removing outliers
max_age <- quantile(eap.data$age, 0.75) + 1.5*IQR(eap.data$age)
max_age  #the maximum age for avoiding outlier values is 83 years

max_hours <- quantile(eap.data$work_hours, 0.75, na.rm=T) + 1.5*IQR(eap.data$work_hours, na.rm=T)
max_hours  #the maximum number of working hours per week for avoiding outliers is 90 hours

max_income <- quantile(eap.data$work_income, 0.75, na.rm=T) + 1.5*IQR(eap.data$work_income, na.rm=T)
max_income  #the maximum labor income for avoiding outliers is 7750 Bolivianos (around 1.100 US$)

max_senior <- quantile(eap.data$job_tenure, 0.75, na.rm=T) + 1.5*IQR(eap.data$job_tenure, na.rm=T)
max_senior  #the maximum labor seniority for avoiding outliers is 28 years

#Lets clean the data from these extreme values

eap.data.no <- filter(eap.data, age <= 83, work_hours <= 90, work_income <=7750, job_tenure <=28)
View(eap.data.no) #we deleted 2878 extreme observations from our data base

#Imputing missing values

#first lets define all non numeric variables as factors
str(eap.data.no)
eap.data.no$zone <- as.factor(eap.data.no$zone)
eap.data.no$gender <- as.factor(eap.data.no$gender)
eap.data.no$indigenous <- as.factor(eap.data.no$indigenous)
eap.data.no$labcat <- as.factor(eap.data.no$labcat)
eap.data.no$labbranch <- as.factor(eap.data.no$labbranch)
eap.data.no$firm_size <- as.factor(eap.data.no$firm_size)
eap.data.no$labor_market <- as.factor(eap.data.no$labor_market)
eap.data.no$poverty <- as.factor(eap.data.no$poverty)
eap.data.no$number_jobs <- as.factor(eap.data.no$number_jobs)
eap.data.no$house_prop <- as.factor(eap.data.no$house_prop)
eap.data.no$health_insure <- as.factor(eap.data.no$health_insure)

str(eap.data.no)  #now the variables are numeric or factors

#Now lets track which variables have missing data
summary(eap.data.no)
sum(is.na(eap.data.no))  # we have 562 missing data observations /(firm size, education and poverty)

eap.data.no$missFirm <- ifelse(is.na(eap.data.no$firm_size), "Y", "N") #want to know where are the NAs
eap.data.no$misseduc <- ifelse(is.na(eap.data.no$educ), "Y", "N") 
eap.data.no$missPov <- ifelse(is.na(eap.data.no$poverty), "Y", "N") 

#we erase the variables that not help for the imputation

forimputation <- select(eap.data.no, -6, -10, -20:-22)
View(forimputation)
str(forimputation)

#transforming all variables to dummies

dummy.vars <- dummyVars(~., data=forimputation)
train.dummy <- predict(dummy.vars, forimputation)
View(train.dummy)

#now impute

pre.process <- preProcess(train.dummy, method ="bagImpute")
imputed.data <- predict(pre.process, train.dummy)
View(imputed.data)








#Creating the final database for our models

#Split data into training and testing sets




#Running Model 1: CART

model1 <- train(formal_ci ~ .,
                method = "rpart",
                tuneGrid = data.frame(cp= seq(0, 0.05, len = 25)),
                data = mydata)


#Running Model 2: Random Forest
