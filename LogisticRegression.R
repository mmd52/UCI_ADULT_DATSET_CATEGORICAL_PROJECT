#Author @ Mohammed 23/12/2016

source("Libraries.R")

data<-read.csv("UCI_ADULT_CATEGORICAL_DATA.csv",header = T)

#Now that we have a data set lets run Logistic Regression on it.
#But we have to many explanotary variables here so we need to eliminate 
#a few. We could either rely on buisness or then Use more complex techniques
#Im going to eliminate capital gains and lossses here
#Further splitting in traing and testing

set.seed(1001)
train<-sample(1:30162,24129,replace = F)
test<--train
training_data <- data[train,-c(9,10)]
testing_data <- data[test,-c(9,10)]


#Logistic Regression Magic Line
logr<-glm(income~.,data=training_data,family = binomial(link="logit"))
summary(logr)
vif(logr)
#Variance inflation factor 
# VIF = 1	Not correlated
# 1 < VIF < 5	Moderately correlated
# VIF > 5 to 10	Highly correlated

#Predicting
prediction<-round(predict(logr,testing_data[,-11],type = "response"))
cpred<-0
for(i in 1:length(prediction)){
  if(prediction[i]==0){
    cpred[i]="Low"
  }
  else if(prediction[i]==1){
    cpred[i]="High"
  }
}

table(testing_data[,11],cpred)
#Accuracy is 16.5% pretty bad prediction by logisttic
#Logistic regression is used when all explanotary variables
#are numerical , although it can accomodate categorical
#variables . Logistic regression here hence seems likes a very
#bad idea especially the data set where we have all categorical
#variables