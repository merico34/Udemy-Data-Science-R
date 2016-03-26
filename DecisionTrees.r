setwd("C:/Users/Stephen/Documents/udemy/Data Science with R/Linear Regression")
iris_data <- iris
str(iris_data)
summary(iris_data)
head(iris_data)

#Start analyzing
library(ggplot2)
#Plot Length vs Width, and distinguish species by coloring
qplot(Petal.Length, Petal.Width, data=iris_data, colour=Species, size=3)
#Same for Sepal length/width
qplot(Sepal.Length, Sepal.Width, data=iris_data, colour=Species, size=3)

#Boxplots
#Looking for clusters here. For example, if you can draw a horizontal line across
#the plots and hit more than one box, then it's not a good predictor
#Petal Length and Width are good, Sepal is not as good
par(mfrow=c(2,2))

boxplot(Petal.Length ~ Species, data=iris_data,col="red")
title("Petal Length")
boxplot(Petal.Width ~ Species, data=iris_data,col="green")
title("Petal Width")
boxplot(Sepal.Length ~ Species, data=iris_data, col="blue")
title("Sepal Length")
boxplot(Sepal.Width ~ Species, data=iris_data,col="maroon")
title("Sepal Width")

#Correlations
library(psych)
pairs.panels(iris_data)
#Correlations confirm that Petal Length and Width are correlated
#with Species 


#Modeling and Prediction
library(caret) #Used for splitting training and testing data
  #It preserves the characteristics of the source data set
inTrain <- createDataPartition(y=iris_data$Species, p=0.7,list=FALSE)
training <- iris_data[inTrain,]
testing <- iris_data[-inTrain,]
dim(training);dim(testing)

table(training$Species);table(testing$Species)
#Shows 1:1:1 ratio between species in each data set

library(C50)
model <- C5.0(training[-5], training$Species)
summary(model)

#Testing
predicted <- predict(model, testing)
table(predicted)
library(e1071)
confusionMatrix(predicted, testing$Species)

#Petal Length and Width are highly correlated, and sepal vars weren't used
#What happens if we remove the Petal variables

sub_data <- iris_data[, c(1,2,5)]
summary(sub_data)

inTrain <- createDataPartition(y=sub_data$Species, p=0.7,list=FALSE)
training <- sub_data[inTrain,]
testing <- sub_data[-inTrain,]

model <- C5.0(training[-3], training$Species)
summary(model)

predicted <- predict(model, testing)
confusionMatrix(predicted, testing$Species)

#Stav extra test
#Since Petal Length and Width are highly correlated,
#What if we combine them into a new variable, Petal "area"

sub_data2 <- iris_data
head(sub_data2)
sub_data2 <- cbind(sub_data2, Petal.Area = sub_data2$Petal.Length * sub_data2$Petal.Width)
head(sub_data2)

inTrain <- createDataPartition(y=sub_data2$Species, p=0.7,list = FALSE)
training <- sub_data2[inTrain,]
testing <- sub_data2[-inTrain,]

head(training[c(-3,-4,-5)])

model <- C5.0(training[c(-3,-4,-5)], training$Species)
summary(model)

predicted <- predict(model, testing)
confusionMatrix(predicted, testing$Species)

#Woah, I think it's a better model!