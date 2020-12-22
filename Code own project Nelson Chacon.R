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
table1 <- table(mydata$sexo_ci, mydata$formal_ci)
table1
prop.table(table1,1)


# Percentage of informal workers by zone
table2 <- table(mydata$zona_c, mydata$formal_ci)
table2
prop.table(table2,1)

# Percentage of informal workers by race
table3 <- table(mydata$id_ind_ci, mydata$formal_ci)
table3
prop.table(table3,1)


# Percentage of informal workers by labor category
table4 <- table(mydata$categopri_ci, mydata$formal_ci)
table4
prop.table(table4,1)


# Percentage of informal workers by labor branch
table5 <- table(mydata$rama_ci, mydata$formal_ci)
table5
prop.table(table5,1)

# Percentage of informal workers by firm size
table6 <- table(mydata$tamemp_ci, mydata$formal_ci)
table6
prop.table(table6,1)


# Percentage of informal workers by labor market
table7 <- table(mydata$mt, mydata$formal_ci)
table7
prop.table(table7,1)

# Percentage of informal workers by labor market
table8 <- table(mydata$p0, mydata$formal_ci)
table8
prop.table(table8,1)

#average income for formal and informal workers
mydata %>% group_by(formal_ci) %>% summarize(Avg = mean(ylmpri_ci, na.rm=T))


#average working hours for formal and informal workers
mydata %>% group_by(formal_ci) %>% summarize(Avg = mean(horaspri_ci, na.rm=T))

#average labor seniority for formal and informal workers
mydata %>% group_by(formal_ci) %>% summarize(Avg = mean(antiguedad_ci, na.rm=T))

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

#Working only with economically active population

table(mydata$pea_ci) #We have 17659 EAP in our sample

eap.data <- filter(mydata, pea_ci==1)
View(eap.data)

#Cleaning outliers

boxplot(eap.data$edad_ci)  #there are some outliers
boxplot(eap.data$horaspri_ci) #there are some outliers
boxplot(eap.data$ylmpri_ci) #there are many outliers
boxplot(eap.data$aedu_ci) # no outliers
boxplot(eap.data$antiguedad_ci) #there are many outliers too

#creating Tukey's rule for removing outliers
max_age <- quantile(eap.data$edad_ci, 0.75) + 1.5*IQR(eap.data$edad_ci)
max_age  #the maximum age for avoiding outlier values is 83 years

max_hours <- quantile(eap.data$horaspri_ci, 0.75, na.rm=T) + 1.5*IQR(eap.data$horaspri_ci, na.rm=T)
max_hours  #the maximum number of working hours per week for avoiding outliers is 90 hours

max_income <- quantile(eap.data$ylmpri_ci, 0.75, na.rm=T) + 1.5*IQR(eap.data$ylmpri_ci, na.rm=T)
max_income  #the maximum labor income for avoiding outliers is 7750 Bolivianos (around 1.100 US$)

max_senior <- quantile(eap.data$antiguedad_ci, 0.75, na.rm=T) + 1.5*IQR(eap.data$antiguedad_ci, na.rm=T)
max_senior  #the maximum labor seniority for avoiding outliers is 28 years

#Lets clean the data from these extreme values

eap.data.no <- filter(eap.data, edad_ci <= 83, horaspri_ci <= 90, ylmpri_ci <=7750, antiguedad_ci <=28)
View(eap.data.no) #we deleted 2878 extreme observations from our data base

#Imputing missing values

#first lets define all non numeric variables as factors
str(eap.data.no)
eap.data.no$zona_c <- as.factor(eap.data.no$zona_c)
eap.data.no$sexo_ci <- as.factor(eap.data.no$sexo_ci)
eap.data.no$id_ind_ci <- as.factor(eap.data.no$id_ind_ci)
eap.data.no$categopri_ci <- as.factor(eap.data.no$categopri_ci)
eap.data.no$rama_ci <- as.factor(eap.data.no$rama_ci)
eap.data.no$sexo_ci <- as.factor(eap.data.no$sexo_ci)
eap.data.no$tamemp_ci <- as.factor(eap.data.no$tamemp_ci)
eap.data.no$mt <- as.factor(eap.data.no$mt)
eap.data.no$p0 <- as.factor(eap.data.no$p0)
eap.data.no$nempleos_ci <- as.factor(eap.data.no$nempleos_ci)
eap.data.no$viviprop_ch <- as.factor(eap.data.no$viviprop_ch)
eap.data.no$cobersal <- as.factor(eap.data.no$cobersal)

str(eap.data.no)  #now the variables are numeric or factors

#Now lets track which variables have missing data
summary(eap.data.no)
sum(is.na(eap.data.no))  # we have 562 missing data observations /(firm size, education and poverty)

eap.data.no$missFirm <- ifelse(is.na(eap.data.no$tamemp_ci), "Y", "N") #want to know where are the NAs
eap.data.no$misseduc <- ifelse(is.na(eap.data.no$aedu_ci), "Y", "N") 
eap.data.no$missPov <- ifelse(is.na(eap.data.no$p0), "Y", "N") 

#we erase the variables that not help for the imputation

forimputation <- select(eap.data.no, -6, -7, -11, -21:-23)
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
