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


#Calling the libraries for random Forest
checkInstallLoad("RWeka")
checkInstallLoad("partykit")

print("============== Completed loading libraries  ===================")
