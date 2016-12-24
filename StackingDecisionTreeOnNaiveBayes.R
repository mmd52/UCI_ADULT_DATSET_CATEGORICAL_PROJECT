#Author @ Mohammed 23/12/2016

source("Libraries.R")

data<-read.csv("UCI_ADULT_CATEGORICAL_DATA.csv",header = T)

#Now that we have a data set lets run a NaiveBayes on it.
#But we have to many explanotary variables here so we need to eliminate 
#a few. We could either rely on buisness or then Use more complex techniques
#Im going to eliminate capital gains and lossses here

training_data <- data[,-c(9,10)]

model<-naiveBayes(income~.,data = training_data)

prediction<-predict(model,training_data[,-11])

table(prediction,training_data[,11])

acc.nb <- caret::confusionMatrix(training_data[,11],
                                      prediction,
                                      mode = "prec_recall")

acc.nb
#Accuracy here is 80% which is OK lets see if by stacking on Dtree
#we can increase it

training_data$logit_f<-prediction

#To run a decision tree and make a model im going to make use of 
#package RWEKA here

j48<-J48(income~.,data=training_data,control=Weka_control(),options=NULL)

summary(j48)

eval_j48 <- evaluate_Weka_classifier(j48, numFolds = 100,
                                     complexity = FALSE, 
                                     seed = 1, class = TRUE)
eval_j48

#It just came till 83 % means we must do fe and Stacking didnt 
#help us here :P