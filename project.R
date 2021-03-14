#clean the R environment
rm(list=ls())
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')


install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)
#setting working directory
setwd("/Users/alfazalm/Documents/bike_rent/bikerent")

#loading csv file
day=read.csv("day.csv",header = T)

#Missing Value analysis
sum(is.na(day))
#There is no missing value in the data(day.csv file)
#outlier Analysis
library(ggplot2)
boxplot(day$temp, day$atemp, day$hum, day$windspeed,
        names = c("Temperature", "ATemp", "Humidity", "Windspeed"),
        las = 2,
        col = c("blue","orange","red","green"),
        border = "brown",
        horizontal = FALSE,notch = FALSE
)
#There are outliers in windspeed and humidity
#outliers are saved in outlier vectors
outliers_w=boxplot(day$windspeed, plot=FALSE)$out
outliers_h=boxplot(day$hum, plot=FALSE)$out
day2=day
day2 = day2[-which(day2$windspeed %in% outliers_w),]
day2 = day2[-which(day2$hum %in% outliers_h),]
#box plot without outliers
boxplot(day2$temp, day2$atemp, day2$hum, day2$windspeed,
        names = c("Temp", "A Temp", "Humidity", "Windspd"),
        las = 2,
        col = c("blue","orange","red","green"),
        border = "brown",
        horizontal = FALSE,notch = FALSE
)
#putting NA in outlier values
day2=day
day2[,'windspeed'][day2[,'windspeed'] %in% outliers_w] = NA
day2[,'hum'][day2[,'hum'] %in% outliers_h] = NA
sum(is.na(day2))
#imputing outlier values using KNN imputation

library(DMwR)
library(rpart)
day3 = subset(day2, select = -c(instant, dteday, casual, registered))
day3 = knnImputation(day3, k = 5)
day2$windspeed=day3$windspeed
day2$hum=day3$hum
sum(is.na(day2))

#=========Feature selection=======
#correlational analysis
library(corrgram)
numeric=c('temp', 'atemp', 'hum', 'windspeed', 'cnt')
corrgram(day2[,numeric],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation Analysis")
#from the pie plot it is observed temp and atemp is highly correlated
#atemp is removed as part of feature selection
day2 = subset(day2, select=-atemp)

#anova test for categorical variables
categ = c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit')
for(i in categ){
  print(i)
  print(summary(aov(formula = cnt~day2[,i],day2))
  )
}
# The p values for  workingday,weekday and holiday variables are greater than 0.05
# The variables with p values greater than 0.05 are eliminated
day2 = subset(day2, select=-c(holiday,weekday,workingday))
#==============Feature Scaling=============
library(propagate)
numeric2=c('temp', 'hum', 'windspeed', 'cnt')
for(i in numeric2){
  print(i)
  print(skewness(day2[,i]))
}
#data is approximatey symmetric

#checking normality
hist(day2$temp)
hist(day2$hum)
hist(day2$windspeed)
hist(day2$cnt)
 # the distribution is approximately symmetric and normally distributed scaling not recquired

#################Modeling################

####Decision tree################
# mape = 26.76604
# mae = 796.4505777
# rsquare = 0.8171842

library(rpart)
library(MASS)
#######droping few columns for decision tree#####
day2= subset(day2, select = -c(instant, dteday, casual, registered))

categ2= c("season","yr","mnth","weathersit")

library(dummies)

day3 = dummy.data.frame(day2, categ2)


train_index=sample(1:nrow(day3),0.8*nrow(day3))
train= day3[train_index,]
test= day3[-train_index,]
###rpart for regression###

model_dt = rpart(cnt ~ .,data=train,method="anova")
# test values without target variable
test2= subset(test, select=-cnt)
# prediction using decision tree
predictions_dt = predict(model_dt,test2)

#####calculate mape#####
mape = function(y,y1){mean(abs((y-y1)/y))}*100
mape(test$cnt,predictions_dt)

regr.eval(test$cnt,predictions_dt,stats = c('mape','mae') )
cor(test$cnt,predictions_dt)^2


#========Random Forest=======
# MAPE = 19.51173
# MAE = 547.4096596
# Rsquare = 0.9167251


library(randomForest)
model_rf = randomForest(cnt~., train, ntree = 500, importance = TRUE)

# Prediction using random forest
predictions_rf = predict(model_rf, test2)

regr.eval(test$cnt,predictions_rf,stats = c('mape','mae') )
cor(test$cnt,predictions_rf)^2

#==========Linear Regression=======
# MAPE = 21.11433
# MAE = 642.7422879
# Rsquare = 0.86049

model_lr = lm(cnt~., train)
# Predictions using linear regression
predictions_lr = predict(model_lr, test2)

regr.eval(test$cnt,predictions_lr,stats = c('mape','mae') )
cor(test$cnt,predictions_lr)^2

###### Choosing Model#####
 # Random forest method is best suitable since MAPE,MAe is least in Random Forest method
# x is the sample input
x= data.frame("season"=1,"yr"=0,"mnth"=2,"weathersit"=2,"temp"=0.173,"hum"=0.796,"windspeed"=0.1323,"cnt"=NA)
# creating dataframe using values in x for deploying in random forest model we have created
day4=day2
bind = rbind(day4,x)
bind = dummy.data.frame(bind, categ2)
input = bind[-(1:nrow(day4)), -25]
output = predict(model_rf, input)
# output is 1439
#bar chart of season vs count 
ggplot(day, aes(x = season, y = cnt))+
  labs(title = "Count of Bike Rent of season ", x = "Season", y = "count")+
  geom_bar(stat = "identity", fill = "blue")
#bar chart of weekday vs count 
ggplot(day, aes(x = weekday, y = cnt))+
  labs(title = "Count of Bike Rent of weekday ", x = "weekday", y = "count")+
  geom_bar(stat = "identity", fill = "blue")
#bar chart of year vs count 
ggplot(day, aes(x = yr, y = cnt))+
  labs(title = "Count of Bike Rent of year ", x = "Year", y = "count")+
  geom_bar(stat = "identity", fill = "blue")
#bar chart of Month vs count 
ggplot(day, aes(x = mnth, y = cnt))+
  labs(title = "Count of Bike of Months ", x = "Month", y = "count")+
  geom_bar(stat = "identity", fill = "blue")
#bar chart of Weather Situation vs count 
ggplot(day, aes(x = weathersit, y = cnt))+
  labs(title = "Count of Bike Rent ", x = "Weather Situation", y = "count")+
  geom_bar(stat = "identity", fill = "blue")

hist(day$temp,main = "Frequency of Temperature",xlab = "Temperature")
hist(day$atemp,main = "Frequency of Actual Temperature",xlab = " Actual Temperature")
hist(day$hum,main = "Frequency of Humidity",xlab = "Humidity")
hist(day$windspeed,main = "Frequency of Windspeed",xlab = "Wind Speed")
model_dt
summary(model_lr)