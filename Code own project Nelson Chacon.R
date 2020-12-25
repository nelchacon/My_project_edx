##############################
#Own Project Script: Predicting labor informality
##############################

#Install required packages
#if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gt)) install.packages("gt", repos = "http://cran.us.r-project.org")
#if(!require(scales)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(repmis)) install.packages("repmis", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


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
library(rpart)
library(Rborist)
library(randomForest)


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
table7 <- table(mydata$poverty, mydata$formality)
table7
prop.table(table7,1)

#average income for formal and informal workers
mydata %>% group_by(formality) %>% summarize(Avg = mean(work_income, na.rm=T))

#average working hours for formal and informal workers
mydata %>% group_by(formality) %>% summarize(Avg = mean(work_hours, na.rm=T))

#average labor seniority for formal and informal workers
mydata %>% group_by(formality) %>% summarize(Avg = mean(job_tenure, na.rm=T))

#Graphics

characteristics <- c("rural","indigenous","self-employed","unpaid","agriculture","construction","trade","transportation","small firm","poor")
values <- c(92,87,96,99,98,91,92,91,95,95)
graph1 <- data.frame(characteristics, values)

#Plot 1: informality labour according to different economic characteristics (factors)

  ggplot(data=graph1, aes(x=reorder(characteristics, values), y= values)) +
  geom_bar(position = "dodge", stat="identity")+
  geom_text(aes(label=values), position=position_dodge(width=0.9), vjust=-0.25, hjust = -0.2)+
  coord_flip()+ scale_y_continuous(name="Labor informality (%)")+
  scale_x_discrete(name="Worker characteristics") +
  theme_minimal()

#Plot 2: informality labor according to numeric characteristics

  characteristics2 <- c(rep("labor income" , 2) , rep("hours per week" , 2) , rep("length of service" , 2))
  categories2 <- c("formal","informal","formal","informal","formal","informal")
  values2 <- c(638,305,41.9,43.3,8.2,9.3)
  graph2 <- data.frame(characteristics2,categories2,values2)

  
  ggplot(data=graph2, aes(x=categories2, y= values2)) +
    geom_bar(position = "dodge", stat="identity")+
    geom_text(aes(label=values2), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_y_continuous(name="Hours, dollars and years")+
    scale_x_discrete(name="Labor category") +
    facet_wrap(~characteristics2, scales = "free_y") +
    theme_minimal()

########################################
# Pre-processing
########################################  

#Working only with economically active population (EAP)

table(mydata$eapop) #We have 17659 EAP in our sample

eap.data <- filter(mydata, eapop==1)
#View(eap.data)

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

eap.data.no <- filter(eap.data, age <= max_age, work_hours <= max_hours, work_income <= max_income, job_tenure <= max_senior)
View(eap.data.no) #we deleted 2878 extreme observations from our data base

#Imputing missing values

#first lets define all non numeric variables as factors
str(eap.data.no)
eap.data.no$formality <- as.factor(eap.data.no$formality)
eap.data.no$zone <- as.factor(eap.data.no$zone)
eap.data.no$gender <- as.factor(eap.data.no$gender)
eap.data.no$indigenous <- as.factor(eap.data.no$indigenous)
eap.data.no$labcat <- as.factor(eap.data.no$labcat)
eap.data.no$labbranch <- as.factor(eap.data.no$labbranch)
eap.data.no$firm_size <- as.factor(eap.data.no$firm_size)
eap.data.no$poverty <- as.factor(eap.data.no$poverty)

str(eap.data.no)  #now the variables are numeric or factors

#Now lets track which variables have missing data
summary(eap.data.no)
sum(is.na(eap.data.no))  # we have 562 missing data observations /(firm size, education and poverty)

eap.data.no$missFirm <- ifelse(is.na(eap.data.no$firm_size), "Y", "N") #want to know where are the NAs
eap.data.no$misseduc <- ifelse(is.na(eap.data.no$educ), "Y", "N") 
eap.data.no$missPov <- ifelse(is.na(eap.data.no$poverty), "Y", "N") 

#we delete the variables that do not help for the imputation (economic active population indicator
#, dependent variable of formality, and missing value indicators)

forimputation <- select(eap.data.no, -6, -10, -16:-18)
#View(forimputation)
str(forimputation)

#transforming all variables to dummies

dummy.vars <- dummyVars(~., data=forimputation)
train.dummy <- predict(dummy.vars, forimputation)
#View(train.dummy)

#now impute

pre.process <- preProcess(train.dummy, method ="bagImpute")
imputed.data <- predict(pre.process, train.dummy)
#View(imputed.data)

#lets put in order the imputed data before replacing in the last database

imputed.data <- as.data.frame(imputed.data)
missimputed <- select(imputed.data, 22:24, 27:29)
#View(missimputed)

# recomputing the predicted missing values before including in data base

missimputed$new_educ <- round(missimputed$educ,0)
summary(missimputed$new_educ)

missimputed$new_poverty <- ifelse(missimputed$poverty.notpoor>0.5, "notpoor", "poor")

missimputed$new_firmsize <- ifelse(missimputed$firm_size.large>0.5, "large", ifelse(missimputed$firm_size.medium>0.5, "medium", "small"))


#Creating the final database for using in our models

#adding the new imputed variables to the database with no outliers

eap.data.no <- mutate(eap.data.no, new_edcu=missimputed$new_educ, new_poverty = missimputed$new_poverty, new_firmsize=missimputed$new_firmsize)
#View(eap.data.no)

#we exclude all the variables that wont be used in our model
#the economically active indicator, incomplete education, firm size and poverty, and missing values indicators)

finalbase <- eap.data.no[-c(6 ,9, 13, 14, 16:18)]
#View(finalbase)

#Split data into training and testing sets

#The idea is to have representative subsamples of the interest variable Formality both
#on training and testing sets

prop.table(table(finalbase$formality)) #We have 79% of informal workers on the final database

#Lets round it to 80% of informal and 20% of formal workers on the new splits

set.seed(1983)
test_index <- createDataPartition(finalbase$formality, times = 1, p = 0.2, list = FALSE)

test_set <- finalbase[test_index, ]
train_set <- finalbase[-test_index, ]

#checking that training and testing sets maintain the balance of 80% of informal workers

prop.table(table(train_set$formality))  #they are ok, around 80% informality
prop.table(table(test_set$formality))  #ok too, near 80% of informality

###############################
###MODELING LABOR INFORMALITY
###############################

# 1. FISRT MODEL: NAIVE LOGISTIC REGRESSION

glm_model <-  glm(formality ~ ., data= train_set, family = "binomial") #model

hat_logit <- predict(glm_model, newdata = test_set, type = "response") #prediction

hat_logit <- ifelse(hat_logit > 0.5, "1", "0") %>% factor # rounding the predictions for comparison
confusionMatrix(hat_logit, test_set$formality)  # accuracy

# 2. SECOND MODEL: CLASSIFICATION TREE








# tree graph
library(rpart.plot)
prp(cartmodel1, type = 2, extra = "auto" , nn = F, fallen.leaves = TRUE, faclen = 4, varlen = 8, shadow.col = "gray")


#Running Model 2: Random Forest

#Define control and grid for optimization and tuning parameters 
control <- trainControl(method="cv", number = 10)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))

train_rpart <- train(formality ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = train_set)
plot(train_rpart)

#predict
rpart_hat <- predict(train_rpart, test_set)

#Confusion matrix
confusionMatrix(rpart_hat, test_set$formality)$overall["Accuracy"]

#Best tuned model

best_rpart <- train(formality ~ .,
                     method = "rpart",
                     cp=0.004166667,
                     data = train_set)

rpart_hat_best <- predict(best_rpart, test_set)

confusionMatrix(rpart_hat_best, test_set$formality)$overall["Accuracy"]

















cartmodel <- rpart(formality ~ .,
                       data = train_set,
                      control = rpart.control(cp = 0, minsplit = 2))



#confusion matrix

formal_hat1new <- predict(cartmodel, test_set, type="class")

formal_hat1new <- factor(formal_hat1new)

confusionMatrix(data = formal_hat1new, reference=test_set$formality)






# 3. LAST MODEL: RANDOM FOREST

#control and grid
rfcontrol <- trainControl(method="cv", number = 10)
rfgrid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))


rf <- randomForest(formality ~ .,
                   data = train_set,
                   trControl = rfcontrol,
                   tuneGrid = rfgrid,
                   ntree = 150,
                   nSamp = 2000)

#predict
rf_predict <- predict(rf, test_set, type = "class")

#confusion matrix
rf_predict <- factor(rf_predict)

confusionMatrix(data = rf_predict, reference=factor(test_set$formality))

plot(rf)

imp <- importance(rf)
imp

varImpPlot(rf)

# best parameters for tuning

rf$mtry




#Best model tuned

rf_best <- randomForest(formality ~ .,
                        data = train_set,
                        trControl = rfcontrol,
                        ntree = 100,
                        minNode = rf$mtry)

rf_best
#predict
rf_predict_best <- predict(rf_best, test_set, type = "class")

#confusion matrix
rf_predict_best <- factor(rf_predict_best)

confusionMatrix(data = rf_predict_best, reference=factor(test_set$formality))
importance(rf_best)
