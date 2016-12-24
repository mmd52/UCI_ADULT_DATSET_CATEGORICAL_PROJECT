#Author @ Mohammed 23/12/2016

source("Libraries.R")

data<-read.csv("UCI_ADULT_CATEGORICAL_DATA.csv",header = T)

#Now that we have a data set lets run a decision tree on it.
#But we have to many explanotary variables here so we need to eliminate 
#a few. We could either rely on buisness or then Use more complex techniques
#Im going to eliminate capital gains and lossses here

training_data <- data[,-c(9,10)]

#To run a decision tree and make a model im going to make use of 
#package RWEKA here

j48<-J48(income~.,data=training_data,control=Weka_control(),options=NULL)

summary(j48)

eval_j48 <- evaluate_Weka_classifier(j48, numFolds = 100,
                                     complexity = FALSE, 
                                     seed = 1, class = TRUE)
pred<-predict(j48,training_data[,-11])
table(pred,training_data[,11])

training_data$logit_f<-pred

model<-naiveBayes(income~.,data = training_data)

prediction<-predict(model,training_data[,-11])

table(prediction,training_data[,11])

acc.stacked <- caret::confusionMatrix(training_data[,11],
                                      prediction,
                                      mode = "prec_recall")

acc.stacked
#Accuracy here is 82% which is bad as we brought the 83% down
