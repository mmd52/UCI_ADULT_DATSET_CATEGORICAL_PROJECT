#Author @ Mohammed 23/12/2016
#Code used to Fetch/install/Load all libraries required

#Set Working Directory
setwd("D:/RPROJECTS/UCI_Categorical_Models_run")

print("============== Beginning to load libraries  ===================")

#Function To check if 
checkInstallLoad <- function(libName) 
{
  if(!require(libName, character.only=TRUE)) 
  {
    install.packages(libName)
    require(libName, character.only=TRUE)
  }
}


#Calling the libraries for Decision tree
checkInstallLoad("RWeka")
checkInstallLoad("partykit")

#calling the libraries for Logistic Regression
checkInstallLoad("car")

print("============== Completed loading libraries  ===================")
