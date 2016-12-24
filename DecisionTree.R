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
#plot(j48)

summary(j48)

# 
# === Summary ===
#   
#   Correctly Classified Instances       25290               83.8472 %
# Incorrectly Classified Instances      4872               16.1528 %
# Kappa statistic                          0.5382
# Mean absolute error                      0.238 
# Root mean squared error                  0.3449
# Relative absolute error                 63.6401 %
# Root relative squared error             79.7756 %
# Total Number of Instances            30162     
# 
# === Confusion Matrix ===
#   
#   a     b   <-- classified as
# 4346  3162 |     a = High
# 1710 20944 |     b = Low

eval_j48 <- evaluate_Weka_classifier(j48, numFolds = 100,
                                     complexity = FALSE, 
                                     seed = 1, class = TRUE)
eval_j48

# === 100 Fold Cross Validation ===
#   
#   === Summary ===
#   
#   Correctly Classified Instances       24869               82.4514 %
# Incorrectly Classified Instances      5293               17.5486 %
# Kappa statistic                          0.4942
# Mean absolute error                      0.2465
# Root mean squared error                  0.3559
# Relative absolute error                 65.9301 %
# Root relative squared error             82.3179 %
# Total Number of Instances            30162     
# 
# === Detailed Accuracy By Class ===
#   
#   TP Rate  FP Rate  Precision  Recall   F-Measure  MCC      ROC Area  PRC Area  Class
# 0.540    0.081    0.688      0.540    0.605      0.500    0.837     0.632     High
# 0.919    0.460    0.858      0.919    0.887      0.500    0.837     0.915     Low
# Weighted Avg.    0.825    0.366    0.815      0.825    0.817      0.500    0.837     0.844     
# 
# === Confusion Matrix ===
#   
#   a     b   <-- classified as
# 4052  3456 |     a = High
# 1837 20817 |     b = Low
