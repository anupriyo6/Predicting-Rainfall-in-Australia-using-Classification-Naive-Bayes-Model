## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions
library(data.table)
library(e1071)
rain <- fread('../input/weatherAUS.csv', data.table = FALSE)
head(rain)
names(rain)
sapply(rain,function(x)sum(is.na(x)))
nrow(rain)
#Remove the columns with greater number of missing data 
rain<-rain%>%
select(-c("Evaporation","Cloud3pm","Sunshine","Cloud9am"))
names(rain)
#Remove the missing data from other columns
rain<-na.omit(rain)
head(rain)
#Sample out the data
sample1<-sample(1:nrow(rain))
rain<-rain[sample1,]
head(rain)
str(rain)
#Since our target variable is RainTomorrow we will make it as a categorical variable.
#We will also make all non-numeric variables to categorical variables
rain$RainTomorrow<-as.factor(rain$RainTomorrow)
rain$RainToday<-as.factor(rain$RainToday)
rain$WindGustDir<-as.factor(rain$WindGustDir)
rain$WindDir9am<-as.factor(rain$WindDir9am)
rain$WindDir3pm<-as.factor(rain$WindDir3pm)
rain$Location<-as.factor(rain$Location)
#converting date to a date format
rain$Date<-as.Date(rain$Date)
str(rain)
table(rain$RainToday)
#Split into test and train set with 80-20 set
rain_train<-rain[1:90340,]
nrow(rain_train)
rain_test<-rain[90341:112925,]
rain_test1<-rain_test%>%select(-"RainTomorrow")
names(rain_test1)
table(rain_test$RainTomorrow)
#run naive bayes algorithm
nb_rain<-naiveBayes(RainTomorrow~.,data=rain_train)
nb_pred<-predict(nb_rain,rain_test1)
rain_test[1687,]
nb_pred[106]
#Checking the data in comfusion matrix
x<-table(nb_pred,rain_test$RainTomorrow)
x
acc<-((sum(diag(x)))/sum(x))
acc
#We get an accuracy of 94.59 by running the naive Bayes Model
#Lets check against the logistic regression
log_Rain<-glm(RainTomorrow~Location,family=binomial(link="logit"),rain_train)
summary(log_Rain)
library(pscl)
pR2(log_Rain)
#We find that Mcfadden index is 0.27.So the model is a good fit whrn it comes to predict rainfall in particular locations

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.