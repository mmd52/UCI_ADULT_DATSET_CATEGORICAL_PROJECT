#Author @ Mohammed 23/12/2016

source("Libraries.R")

data<-read.csv("UCI_ADULT_CATEGORICAL_DATA.csv",header = T)

#Now that we have a data set lets run Naive Bayes on it.
#But we have to many explanotary variables here so we need to eliminate 
#a few. We could either rely on buisness or then Use more complex techniques
#Im going to eliminate capital gains and lossses here

training_data <- data[,-c(9,10)]

model<-naiveBayes(income~.,data = training_data)

prediction<-predict(model,training_data[,-11])

table(prediction,training_data[,11])

#Accuracy here is 80% which is good Way better than Logistic 
#regression

#But decision Tree seems to be the winner here