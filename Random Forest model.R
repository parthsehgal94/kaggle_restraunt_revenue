library(data.table) ## for loading data very fast using fread.
library(bit64) ## special features of data.table, supporting package.
library(mice) ## missing values functionality
library(lubridate)  #cool package for converting dates
library(glmnet)##contain LASO-for regularisation
library(randomForest)


#loading dataset
train <- fread("train.csv" , na.strings = c(" " , "NA" , "NaN" , "NULL"), stringsAsFactors = F)## na.string (part of data.table) tell all value to be taken as NA 
test <- fread("test.csv" ,na.strings = c(" " , "NA" , "NaN" , "NULL"), stringsAsFactors = F )
dim(train)##show dimensions of dataset

#getting a first view inside the dataset
names(train)## tell about all column names.
str(train)


#creating some new variables and formatting old variables for training set
train$`City Group` <- as.factor(train$`City Group`)
train$Type <- as.factor(train$Type)

train$`Open Date` <-  mdy(train$`Open Date`)##change date format, lubridate package
train$duration <- as.POSIXct(train$`Open Date`)## 
d <- Sys.Date()
l <- as.POSIXct(d)

train$time_since_opening <- NULL
for (i in 1:137) {
  train$time_since_opening[i] <- (l - train$duration[i])
}

colnames(train)[2] <- "open_date" ##2nd column renamed




#creating some new variables and formatting old variables for test set

test$`City Group` <- as.factor(test$`City Group`) 
test$Type <- as.factor(test$Type)

test$`Open Date` <-  mdy(test$`Open Date`)
test$duration <- as.POSIXct(test$`Open Date`)
d <- Sys.Date()
l <- as.POSIXct(d)

test$time_since_opening <- NULL
for (i in 1:137) {
  test$time_since_opening[i] <- (l - test$duration[i])
  
}

colnames(test)[2] <- "open_date"

#creating model using random forest 
train <- as.data.frame(train)## to convert dataset into data frame so that we can input in modeling functions.
mrf <- randomForest(revenue~.-Id-duration-City-open_date , train )##response variable is revenue. '.' means all variables are exploratory (minus sign used to remove some particular variables)
prediction <- predict(mrf , test) ## create a coloumn of output respone variable from test set. This column can be added to original dataset using cbind.

## Dealings
##1. Tunning parameters mtry-size of random subset, ntree - number of tree in forest.
##2. Special variables created. 
##3. Proper cleaning. 
##4. Tried using other algorithm.



