setwd("C:/Users/Stephen/Documents/udemy/Data Science with R/Repo")
auto_data <- read.csv("auto-data.csv")
str(auto_data)
summary(auto_data)

#Data Cleansing
#K Means requires all numeric data to be in the same range
#Use Centering and Scaling

scaled_num <- scale(auto_data[8:12])
#Put the attributes back into the main data frame
auto_data[,8:12] <- scaled_num
summary(auto_data)

#Exploratory Data Analysis
par(mfrow=c(1,5))
boxplot(auto_data$HP, col = "red")
title("HP")

boxplot(auto_data$RPM,col="blue")
title("RPM")

boxplot(auto_data$MPG.CITY,col="green")
title("MGP City")

boxplot(auto_data$MPG.HWY,col="maroon")
title("MPG.HWY")

boxplot(auto_data$PRICE, col="cyan")
title("PRICE")

#Modeling and Prediction
library(class)
set.seed(11111)
auto_subset <- auto_data[1:100,c(8,12)]
clusters <- kmeans(auto_subset,4)
clusters

par(mfrow=c(1,1))
plot(auto_subset$HP, auto_subset$PRICE, col=clusters$cluster, pch=20, cex=2)
points(clusters$centers, col="purple",pch=17,cex=3)

#Clustering using all variables
for (i in 1:8) {
  auto_data[,i]=as.numeric(auto_data[,i])
}
summary(auto_data)

set.seed(11111)
clusters <- kmeans(auto_data[,7:12],4)
clusters


#Finding the ideal # of clusters

wssplot <- function(data, nc=15, seed=1234) {
  wss<- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type="b",xlab = "Number of Clusters",
        ylab= "Within groups sum of squares", col="red")
}

wssplot(auto_data)