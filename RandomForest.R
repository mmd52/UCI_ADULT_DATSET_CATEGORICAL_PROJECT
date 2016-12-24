source("Libraries.R")

data<-read.csv("UCI_ADULT_CATEGORICAL_DATA.csv",header = T)

#Now that we have a data set lets run a decision tree on it.
#But we have to many explanotary variables here so we need to eliminate 
#a few. We could either rely on buisness or then Use more complex techniques
#Im going to eliminate capital gains and lossses here

set.seed(999)
train<-sample(1:30162,24129,replace = F)
test<--train
training_data <- data[train,-c(9,10)]
testing_data <- data[test,-c(9,10)]

# Tuning takes factors as target variables
bestmtry <- tuneRF(training_data[,-11], as.factor(training_data[,11]), 
                   ntreeTry=100, stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE) 

rf.fit <- randomForest(income ~ ., data=training_data, 
                       mtry=2, ntree=1000, keep.forest=TRUE,
                       importance=TRUE, test=testing_data) 
summary(rf.fit)
importance(rf.fit)
varImpPlot(rf.fit)

# Confusion Matrix
preds <- predict(rf.fit, newdata=testing_data, type="response")
table(testing_data[,11], preds)
caret::confusionMatrix(testing_data[,11], preds, mode = "prec_recall")

#Accuracy thanks to random forst for all categorical 
#variables is just 77% and this is not a very good model
#as theres a big gap between Precision and Recall