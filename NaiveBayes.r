setwd("C:/Users/Stephen/Documents/udemy/Data Science with R/Repo")
sms_data <- read.csv("sms_spam_short.csv",stringsAsFactors=FALSE)
sms_data$type <- as.factor(sms_data$type)
str(sms_data)
summary(sms_data)
head(sms_data)

#Data/Text Cleansing
library(tm)
#Create a corpus
mesg_corpus <- Corpus(VectorSource(sms_data$text))
inspect(mesg_corpus[1:5])

#Cleanse Data

#remove punctuation
refined_corpus <- tm_map(mesg_corpus, removePunctuation)
#remove white space
refined_corpus <- tm_map(refined_corpus, stripWhitespace)
#convert to lower case
refined_corpus <- tm_map(refined_corpus, content_transformer(tolower))
#remove numbers in text
refined_corpus <- tm_map(refined_corpus, removeNumbers)
#remove stop words
refined_corpus <- tm_map(refined_corpus, removeWords, stopwords())
#remove specific words
refined_corpus <- tm_map(refined_corpus, removeWords, c("else","the","are","for","has","they","as","a","his","on","when","is","in","already"))

#Look at the processed text
inspect(refined_corpus[1:5])

#Create a document-term sparse matrix
dtm <- DocumentTermMatrix(refined_corpus)
dtm
dim(dtm)

#Remove all words who has occured less than 10 times to create a new DTM

filtered_dtm <- DocumentTermMatrix(refined_corpus, list(dictionary=findFreqTerms(dtm,10)))
dim(filtered_dtm)

#inspect the contents before converting to a matrix and transposing
t(inspect(filtered_dtm)[1:25,])

#WordCloud
library(wordcloud)
pal <- brewer.pal(8,"Dark2")
wordcloud(refined_corpus[sms_data$type=="ham"],min.freq=5,random.order=FALSE, colors=pal)
wordcloud(refined_corpus[sms_data$type=="spam"],min.freq=2,random.order=FALSE, colors=pal)

library(caret)
inTrain <- createDataPartition(y=sms_data$type, p=0.7,list=FALSE)

#Splitting the raw data
train_raw <- sms_data[inTrain,]
test_raw <- sms_data[-inTrain,]

#Splitting the corpus
train_corpus <- refined_corpus[inTrain]
test_corpus <- refined_corpus[-inTrain]

#Splitting the dtm
train_dtm <- filtered_dtm[inTrain,]
test_dtm <- filtered_dtm[-inTrain,]

#Convert word counts with categorical indicator
#Yes or No

conv_counts <- function(x) {
  x <- ifelse(x>0,1,0)
  x <- factor(x,levels=c(0,1),labels=c("No","Yes"))
}

train <- apply(train_dtm, MARGIN = 2, conv_counts)
test <- apply(test_dtm, MARGIN = 2, conv_counts)

#convert to a df and add the target variable
df_train <- as.data.frame(train)
df_test <- as.data.frame(test)

df_train$type <- train_raw$type
df_test$type <- test_raw$type

df_train[1:10,1:10]

#Model Building
library(e1071)
modFit <- naiveBayes(df_train[,-60], df_train$type)
modFit

#Testing
predictions <- predict(modFit, df_test)
confusionMatrix(predictions, df_test$type)
