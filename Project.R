library(randomForest)
library(magrittr)
library(Metrics)
library(plotly)
library(corrplot)
library(PerformanceAnalytics)
library(Hmisc)
library(knitr)
library(mctest)
library(ROCR)
library(data.table)
library(dplyr)
library(caret)
library(class)
library(e1071)
library(tree)
library(ISLR)

##Import The DataSet Clean the Data
df <- read.csv("2017_CapitalBike(for demand model).csv")
df$X <- NULL
df$holiday <- df$Weekend_and_holiday - df$weekend
df$Weekend_and_holiday <- as.factor(df$Weekend_and_holiday)
df$holiday <- as.factor(df$holiday)
df$weekend <- as.factor(df$weekend)
df$Season <- as.factor(df$Season)
df$Day <- as.factor(df$Day)
df$Month <- as.factor(df$Month)
df$hour <- as.factor(df$hour)
df$weather <- as.factor(df$weather)

df$Weekend_and_holiday <- NULL
df$Relative.Temperature_c <- NULL
df$Member.type_Casual <- NULL
df$Member.type_Member <- NULL
df$Year <- NULL

##split dataset into training and testing
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
df_train <- data.frame(df[inTrain,])
df_test <- data.frame(df[-inTrain,])
df_train

##Tune the Random Forest: The important part of this is mtryStart. By tweaking mtry, we change the number of features that may possibly be used in the best randomForest. 
##It takes multiple randomForests and based on their mtry value, it finds the randomForest with the least out of bag error.
set.seed(12345)
features <- setdiff(names(df_train), "Demand")
m2 <- tuneRF(
  x          = df_train[features],
  y          = df_train$Demand,
  ntreeTry   = 500,
  mtryStart  = 2,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)
m2
mtry_opt <- m2[,"mtry"][which.min(m2[,"OOBError"])]

##We take the mtry_opt and plug it into the randomForest to try and improve the error metrics.
set.seed(12345)
rf1<-randomForest(Demand~.,data=df_train,mtry=mtry_opt,importance=TRUE)
rf1
plot(rf1)
# Number of trees with the lowest RMSE
which.min(rf1$mse)
# RMSE of this optimal random forest
sqrt(rf1$mse[which.min(rf1$mse)])

##get the confusion matrix
pred=predict(rf1,df_test)
AE_test <- mean(df_test$Demand - pred)
RMSE_test <- sqrt(mean((df_test$Demand - pred) ^ 2))
MAE_test<- mean(abs(df_test$Demand - pred))
Values_test <- c(AE_test, RMSE_test, MAE_test)
Errors <- c("AE", "RMSE", "MAE")
Metrics_test <- data.frame(Errors, Values_test)
Metrics_test

##Now we try to see which factors out of all the features available have the most importance in predicting well.
##As we can see, based on the percent increase in MSE, the hour of the day has the most impact towards the actual demand. 
##The next important feature is whether the day was on a weekend or not. The third most important feature towards predicting demand is temperature. 
##In general, the other features had very little impact towards explaining the demand.

importance(rf1)
varImpPlot(rf1)


##Import The DataSet Clean the Data Use Casual Riders as the dependent Variable now. The number of casual riders is only a part of the total demand. 
##The total demand is the sum of casual riders and actual members who took a ride at the time period.
df <- read.csv("2017_CapitalBike(for demand model).csv")
df$X <- NULL
df$holiday <- df$Weekend_and_holiday - df$weekend
df$Weekend_and_holiday <- as.factor(df$Weekend_and_holiday)
df$holiday <- as.factor(df$holiday)
df$weekend <- as.factor(df$weekend)
df$Season <- as.factor(df$Season)
df$Day <- as.factor(df$Day)
df$Month <- as.factor(df$Month)
df$hour <- as.factor(df$hour)
df$weather <- as.factor(df$weather)
df$Weekend_and_holiday <- NULL
df$Relative.Temperature_c <- NULL
df$Demand <- NULL
df$Member.type_Member <- NULL
df$Year <- NULL

##Split the Dataset
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
df_train2 <- data.frame(df[inTrain,])
df_test2 <- data.frame(df[-inTrain,])
df_train2

##Use the same mtry for the randomForest to predict the number of casual users.
set.seed(12345)
rf2<-randomForest(Member.type_Casual~.,data=df_train2,mtry=mtry_opt,importance=TRUE)
rf2
plot(rf2)
# Number of trees with the lowest RMSE
which.min(rf2$mse)
# RMSE of this optimal random forest
sqrt(rf2$mse[which.min(rf2$mse)])

##Looking at the error values, the error measures for casual riders is much lower than the total demand. 
##Of course we should expect it being lower because Casual riders is only part of the total demand. 
##However, before we come to any conclusions we should first look at the other part of the total demand which is members as the dependent variable.
pred2=predict(rf2,df_test2)
AE_test2 <- mean(df_test2$Member.type_Casual - pred2)
RMSE_test2 <- sqrt(mean((df_test2$Member.type_Casual - pred2) ^ 2))
MAE_test2<- mean(abs(df_test2$Member.type_Casual - pred2))
Values_test2 <- c(AE_test2, RMSE_test2, MAE_test2)
Errors2 <- c("AE", "RMSE", "MAE")
Metrics_test2 <- data.frame(Errors2, Values_test2)
Metrics_test2

##Import The DataSet Clean the Data Use Members as the dependent Variable now
df <- read.csv("2017_CapitalBike(for demand model).csv")
df$X <- NULL
df$holiday <- df$Weekend_and_holiday - df$weekend
df$Weekend_and_holiday <- as.factor(df$Weekend_and_holiday)
df$holiday <- as.factor(df$holiday)
df$weekend <- as.factor(df$weekend)
df$Season <- as.factor(df$Season)
df$Day <- as.factor(df$Day)
df$Month <- as.factor(df$Month)
df$hour <- as.factor(df$hour)
df$weather <- as.factor(df$weather)
df$Weekend_and_holiday <- NULL
df$Relative.Temperature_c <- NULL
df$Demand <- NULL
df$Member.type_Casual <- NULL
df$Year <- NULL

##split the dataset
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
df_train3 <- data.frame(df[inTrain,])
df_test3 <- data.frame(df[-inTrain,])
df_train3

##Use the same mtry for the randomForest to predict the number of members.
set.seed(12345)
rf3<-randomForest(Member.type_Member~.,data=df_train3,mtry=mtry_opt,importance=TRUE)
rf3
plot(rf3)
# Number of trees with the lowest RMSE
which.min(rf3$mse)
# RMSE of this optimal random forest
sqrt(df_bag4$mse[which.min(rf3$mse)])


##Notice the huge disparity of the error measures between actual members and casual riders.
##At first, it may seem reasonable to conclude that our model should predict casual riders better than actual members. We will see that this is not exactly the case later.
pred3=predict(rf3,df_test3)
AE_test3 <- mean(df_test3$Member.type_Member - pred3)
RMSE_test3 <- sqrt(mean((df_test3$Member.type_Member - pred3) ^ 2))
MAE_test3<- mean(abs(df_test3$Member.type_Member - pred3))
Values_test3 <- c(AE_test3, RMSE_test3, MAE_test3)
Errors3 <- c("AE", "RMSE", "MAE")
Metrics_test3 <- data.frame(Errors3, Values_test3)
Metrics_test3

##Concluding that our model predicts casual riders better than actual members is fallacious because the amount for each category: casual vs member riders are disproportionate. 
##While casual riders have a mean of 112, the actual members have a mean of 318. The error measures are about the same proportion for both cases.
df <- read.csv("2017_CapitalBike(for demand model).csv")
mean(df$Member.type_Casual)
mean(df$Member.type_Member)

##prepare the dataset for predict total demand
df <- read.csv('~/Desktop/DM project/2017_CapitalBike(for demand model).csv')
set.seed(12345)
df$X <- NULL
df$holiday <- df$Weekend_and_holiday - df$weekend
df$Weekend_and_holiday <- as.factor(df$Weekend_and_holiday)
df$holiday <- as.factor(df$holiday)
df$weekend <- as.factor(df$weekend)
df$Season <- as.factor(df$Season)
df$Day <- as.factor(df$Day)
df$Month <- as.factor(df$Month)
df$hour <- as.factor(df$hour)

##split the dataset into training and testing
inTrain <- sample(nrow(df), 0.7*nrow(df))
df_train <- data.frame(df[inTrain,])
df_test <- data.frame(df[-inTrain,])

##No.1 linear regression, and get confusion matrix
fit_lm <- lm(Demand ~ Month+Day+hour+weekend+holiday+weather+Wind_km.h+Humidity_percentage+Temperature_c , data = df_train)
summary(fit_lm)
test_predicted <- predict(fit_lm, newdata = df_test)
summary(test_predicted)
actual_test <- df_test$Demand
AE_test <- mean(actual_test - test_predicted)
RMSE_test <- sqrt(mean((actual_test - test_predicted) ^ 2))
MAE_test<- mean(abs(actual_test - test_predicted))
Values_test <- c(AE_test, RMSE_test, MAE_test)
Errors <- c("AE", "RMSE", "MAE")
Metrics_test <- data.frame(Errors, Values_test)
Metrics_test

##log transformation since some predicted values are negative, and get confusion matrix
fit_lm <- lm(log(Demand) ~ Month+Day+hour+weekend+holiday+Season+weather+Wind_km.h+Humidity_percentage+Temperature_c , data = df_train)
summary(fit_lm)
test_predicted <- predict(fit_lm, newdata = df_test)
test_predicted <- exp(test_predicted)
summary(test_predicted)
actual_test <- df_test$Demand
AE_test <- mean(actual_test - test_predicted)
RMSE_test <- sqrt(mean((actual_test - test_predicted) ^ 2))
MAE_test<- mean(abs(actual_test - test_predicted))
Values_test <- c(AE_test, RMSE_test, MAE_test)
Errors <- c("AE", "RMSE", "MAE")
Metrics_test <- data.frame(Errors, Values_test)
Metrics_test

#No.2 random forest, and get confusion matrix
set.seed(12345)
fit_rf<-randomForest(Demand ~ Month+Day+hour+weekend+holiday+Season+weather+Wind_km.h+Humidity_percentage+Temperature_c, data=df_train,mtry=sqrt(10),importance=TRUE)
rf.pred=predict(fit_rf,df_test)
AE_test <- mean(actual_test - rf.pred)
RMSE_test <- sqrt(mean((actual_test - rf.pred) ^ 2))
MAE_test<- mean(abs(actual_test - rf.pred))
Values_test <- c(AE_test, RMSE_test, MAE_test)
Errors <- c("AE", "RMSE", "MAE")
Metrics_test <- data.frame(Errors, Values_test)
Metrics_test

#No.3 support vector machine, and get confusion matrix
fit_svm = svm(Demand ~ Month+Day+hour+weekend+holiday+Season+weather+Wind_km.h+Humidity_percentage+Temperature_c,
              data = df_train,
              type = 'eps-regression',
              kernel = 'radial')
summary(fit_svm)
svm_pred = predict(fit_svm, df_test)

AE_test <- mean(actual_test - svm_pred)
RMSE_test <- sqrt(mean((actual_test - svm_pred) ^ 2))
MAE_test<- mean(abs(actual_test - svm_pred))
Values_test <- c(AE_test, RMSE_test, MAE_test)
Metrics_test <- data.frame(Errors, Values_test)
Metrics_test






df <- read.csv('/Users/roy/Documents/umd2019Spring/758T/data/Capital_Bike_Share_Locations.csv')
df_distance <- df[,c(5,6)]
##calculate distance matrix of each station
library(geosphere)
distance_matrix = distm(df_distance)
df_distance['Distance'] <- NA
class(df_distance$Distance) <- 'list' 

##convert meters to miles
distance_matrix = distance_matrix[,]/1609

##select nearby stations of station's distance parameter
for (n in 1:539) {
  distance = list()
  for (m in 1:539) {
    if (distance_matrix[n,m] < 0.54 & distance_matrix[n,m] > 0){
      distance = append(distance,m)
      distance = paste(distance, collapse=',')
    }
  }
  df_distance[n,'Distance'] <- distance
}
df$distance <- df_distance$Distance

##prepare the dataset for balanced/unbalanced stations
library(pglm)
df1 <- read.csv('/Users/roy/Documents/umd2019Spring/758T/Project/weather_station_update.csv')
df1$X <- NULL
df1$Date <- gsub(pattern = "-", replacement="", x = df1$Date)
df1$Date <- gsub(pattern = " ", replacement="", x = df1$Date)
df1$Date <- as.numeric(df1$Date)
df1$Station.Number <- as.factor(df1$Station.Number)

##delete null-value rows
df1 <- df1[complete.cases(df1), ]

##use pglm to filter out unimportnat variables in the model for demand/return
pglm_demand <- pglm(Demand~ Daily.minimum.temperature+Daily.maximum.temperature+Maximum.steady.wind+Total.daily.precipitation+Pressure+holiday+Season,
                    family ='poisson',data = df1, index=c('Station.Number','Date'), model='within')
summary(pglm_demand)
pglm_return <- pglm(Return~Daily.minimum.temperature+Daily.maximum.temperature+Maximum.steady.wind+Total.daily.precipitation+Pressure+holiday+Season,
                    family ='poisson',data = df1, index=c('Station.Number','Date'), model='within')
summary(pglm_return)

##split raw dataset into training and testing
set.seed(12345)
inTrain <- sample(nrow(df1),nrow(df1)*0.7)
train <- df1[inTrain,]
test <- df1[-inTrain,]

##build predictive poisson model for demand and return in training dataset
glm_demand <- glm(Demand~Date+Daily.minimum.temperature+Daily.maximum.temperature+Maximum.steady.wind+Total.daily.precipitation+Pressure+holiday+Season+factor(Station.Number),
                  family ='poisson',data = train)

glm_return <- glm(Return~Date+Daily.minimum.temperature+Daily.maximum.temperature+Maximum.steady.wind+Pressure+holiday+Season+factor(Station.Number),family='poisson',data=train)

##predict the demand and rerturn in the testing dataset
test$predicted_demand <- predict(glm_demand,newdata = test,type = 'response')
test$predicted_return <- predict(glm_return,newdata = test,type = 'response')

##calculate the predicted usage percentage and unbalance/balance of each station
test$predicted_usage <- (test$predicted_demand - test$predicted_return)/test$Total.Slot
test$predicted_positive <- ifelse(test$predicted_usage >=1,0,1)

##build the confusion matrix for the prediction
Actual1 <- test$balanced_positive
Predicted1 <- test$predicted_positive
confusion1 <- table(Actual1,Predicted1)
rownames(confusion1) <- c('unbalanced','balanced')
colnames(confusion1) <- c('unbalanced','balanced')
confusion1