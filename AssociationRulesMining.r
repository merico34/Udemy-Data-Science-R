setwd("C:/Users/Stephen/Documents/udemy/Data Science with R/Repo")
accident_data <- read.csv("accidents.csv")
str(accident_data)
summary(accident_data)

#get column names of the data set
colnames <- names(accident_data)
#Start building a file in basket format - one row per transaction and each column value becoming
# a basket item in the format <column_name>=<column_value>
basket_str <- ""
for ( row in 1: nrow(accident_data)) {
  if ( row != 1) {
    basket_str <- paste0(basket_str, "\n")
  }
  basket_str <- paste0(basket_str, row, ",")
  for (col in 2: length(colnames)) {
    if ( col != 2) {
      basket_str <- paste0(basket_str, ",")
    }
    basket_str <- paste0(basket_str, colnames[col], "=",accident_data[row,col])
  }
}
write(basket_str, "accidents_basket.csv")

#Exploratory Data Analysis
library(arules)
accidents <- read.transactions("accidents_basket.csv",sep=",")
summary(accidents)

itemFrequencyPlot(accidents,topN=10,type="absolute", col="darkgreen",horiz=TRUE)

#Modeling and Prediction

rules <- apriori(accidents, parameter=list(supp=0.1,conf=0.3))
inspect(rules[1:40])
