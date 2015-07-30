#install.packages("caret", dependencies = TRUE) #Install caret package
#install.packages("randomForest") #Install randomForest package
#install.packages("fields") #Install fields Package

library(caret)
library(randomForest)
library(fields)

setwd("C:\\Titanic") #Sets the working directory

trainSet <- read.table("train.csv", sep=",", header=TRUE)
testSet <- read.table("test.csv", sep=",", header=TRUE)

#Convert to Factor that way Caret treats it as a classification feature, not regression
trainSet$Survived <- factor(trainSet$Survived)

#Random Seed
set.seed(42)

#Trains a random forest (rf) classification model, using 5 folds for cross validation
model <- train(Survived ~ Pclass + Sex + SibSp + Embarked + Parch + Fare, 
               data = trainSet, 
               method = "rf", 
               trControl = trainControl(method = "cv", number = 5))

#Predict on the test set
#testSet$Survived <- predict(model, newdata = testSet)
#This fails since there are missing (NA) variables in our testSet!
#We need to find them and fix them.

#Using summary(testSet) we notice that the testSet has 1 NA value inside the Fare column.
#We should replace that NA with the mean of Fare.

testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)
testSet$Survived <- predict(model, newdata = testSet)

#Removing unnecessary columns
submission <- testSet[,c("PassengerId", "Survived")]

#SWAG
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
