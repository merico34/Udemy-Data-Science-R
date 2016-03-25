setwd("C:/Users/Stephen/Documents/udemy/Data Science with R/Linear Regression")
auto_data <- read.csv("auto-miles-per-gallon.csv")
str(auto_data)
summary(auto_data)

#Replace non-numeric values with the average
auto_data$HORSEPOWER <- as.numeric(auto_data$HORSEPOWER)
auto_data$HORSEPOWER[is.na(auto_data$HORSEPOWER)] <- mean(auto_data$HORSEPOWER, na.rm=TRUE)

summary(auto_data)

#Box and Whisker Plot on Cylinders and MPG
library(ggplot2)
ggplot(auto_data, aes(factor(CYLINDERS), MPG)) + geom_boxplot(aes(fill=factor(CYLINDERS)))

#CORRELATIONS
library(psych)
pairs.panels(auto_data)

#Observation: Cylinders, Displacement, and Weight are highly correlated
#Action: Variable Reduction - Remove 2 from model

auto_data$DISPLACEMENT <- NULL
auto_data$CYLINDERS <- NULL
summary(auto_data)

#Modeling and Prediction
# -6 : Leave out "Names" because the model can't handle text
lm_model <- lm(MPG ~ ., auto_data[,-6])
summary(lm_model)
#Weight and Model Year are significant variables


#Testing the Model
predicted <- predict.lm(lm_model, auto_data)
#Creates a Vector of predicted values
summary(predicted)

#Assessment 1: Plot prediction vs actuals
plot(auto_data$MPG, predicted, col="red")
#Outcome: Predictions are in line with the actual

#Assessment 2: Check correlation
cor(auto_data$MPG, predicted)


