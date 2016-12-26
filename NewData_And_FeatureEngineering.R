# @Author Mohammed 25-12-2016
source("Libraries.r")

#Downloading adult income data set from UCI
print("=================Downloading Data=========================")

data = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                  sep=",",header=F,
                  col.names=c("age", "type_employer", "fnlwgt", "education", 
                              "education_num","marital", "occupation", "relationship", "race","sex",
                              "capital_gain", "capital_loss", "hr_per_week","country", "income"),
                  fill=FALSE,strip.white=T)

print("=====================Data Loaded===============================")

#Data Dictionary
View(head(data,20))
 
#So first things first lets understand each column
#1)Age-> Like i need to explian :p (continuous). 
#2)workclass-> Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked. 
#3)fnlwgt-> continuous (have not quite understood this). 
#4)education-> Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool. 
#5)education-num->continuous (A numeruic representation of education). 
#6)marital-status-> Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse. 
#7)occupation-> Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces. 
#8)relationship-> Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried. 
#9)race-> White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black. 
#10)sex-> Female, Male. 
#11)capital-gain-> continuous. 
#12)capital-loss-> continuous. 
#13)hours-per-week-> continuous. 
#14)native-country-> United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.


#As we can clearly see from above education num is a numeric 
#representation of education so we will go ahead and delete it
data[["education_num"]]=NULL


########### Binning
#What we will do here is convert our raw numeric fields into bins
#wel do this for the ones it makes sense like age
#=========================binning for Age==================
#Age<18=child   18<Age<=30=Young Adult       
#30<Age<=60=adult     Age>60=Senior
ndata<-data
for (i in 1:nrow(ndata)){
  if(ndata[i,1]<=18){
    ndata[i,1]="child"
  }else if (ndata[i,1]>18 && ndata[i,1]<=30){
    ndata[i,1]="young_adult"
  }else if (ndata[i,1]>30 && ndata[i,1]<=60){
    ndata[i,1]="adult"
  }else if (ndata[i,1]>61 ){
    ndata[i,1]="senior"
  }
}

#=========================binning for Hours worked per week=======
#HW<=25=Part Time   25<HW<=40=Full Time       
#40<HW<=60=Over_Time     HW>61=Time To Much
npdata<-ndata
for (i in 1:nrow(npdata)){
  if(npdata[i,12]<=25){
    npdata[i,12]="Part_Time"
  }else if (npdata[i,12]>25 && npdata[i,11]<=40){
    npdata[i,12]="Full_Time"
  }else if (npdata[i,12]>40 && npdata[i,11]<=60){
    npdata[i,12]="Over_Time"
  }else if (npdata[i,12]>61 ){
    npdata[i,12]="TIME_TOMUCH"
  }
}

data<-npdata

######################### Factor to Character
#Convert factor variables to character
#We do this for ease of processing
fctr.cols <- sapply(data, is.factor)
data[, fctr.cols] <- sapply(data[, fctr.cols], as.character)


######################## Missing Value Treatment

is.na(data) = data=='?'
is.na(data) = data==' ?'
data = na.omit(data)

#######################################################################
train_test<-data

#Now in Employer Type we have so many , options more complexity right
#So lets make just reduce them by using func GSUB
#So now my complexity is reduced
train_test$type_employer = gsub("^Federal-gov","Federal-Govt",train_test$type_employer)
train_test$type_employer = gsub("^Local-gov","Other-Govt",train_test$type_employer)
train_test$type_employer = gsub("^State-gov","Other-Govt",train_test$type_employer)
train_test$type_employer = gsub("^Private","Private",train_test$type_employer)
train_test$type_employer = gsub("^Self-emp-inc","Self-Employed",train_test$type_employer)
train_test$type_employer = gsub("^Self-emp-not-inc","Self-Employed",train_test$type_employer)
train_test$type_employer = gsub("^Without-pay","Not-Working",train_test$type_employer)
train_test$type_employer = gsub("^Never-worked","Not-Working",train_test$type_employer)

#Similarly here
train_test$occupation = gsub("^Adm-clerical","Admin",train_test$occupation)
train_test$occupation = gsub("^Armed-Forces","Military",train_test$occupation)
train_test$occupation = gsub("^Craft-repair","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Exec-managerial","White-Collar",train_test$occupation)
train_test$occupation = gsub("^Farming-fishing","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Handlers-cleaners","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Machine-op-inspct","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Other-service","Service",train_test$occupation)
train_test$occupation = gsub("^Priv-house-serv","Service",train_test$occupation)
train_test$occupation = gsub("^Prof-specialty","Professional",train_test$occupation)
train_test$occupation = gsub("^Protective-serv","Other-Occupations",train_test$occupation)
train_test$occupation = gsub("^Sales","Sales",train_test$occupation)
train_test$occupation = gsub("^Tech-support","Other-Occupations",train_test$occupation)
train_test$occupation = gsub("^Transport-moving","Blue-Collar",train_test$occupation)

#You are right counrty too
train_test$country[train_test$country=="Cambodia"] = "SE-Asia"
train_test$country[train_test$country=="Canada"] = "British-Commonwealth"    
train_test$country[train_test$country=="China"] = "China"       
train_test$country[train_test$country=="Columbia"] = "South-America"    
train_test$country[train_test$country=="Cuba"] = "Other"        
train_test$country[train_test$country=="Dominican-Republic"] = "Latin-America"
train_test$country[train_test$country=="Ecuador"] = "South-America"     
train_test$country[train_test$country=="El-Salvador"] = "South-America" 
train_test$country[train_test$country=="England"] = "British-Commonwealth"
train_test$country[train_test$country=="France"] = "Euro_1"
train_test$country[train_test$country=="Germany"] = "Euro_1"
train_test$country[train_test$country=="Greece"] = "Euro_2"
train_test$country[train_test$country=="Guatemala"] = "Latin-America"
train_test$country[train_test$country=="Haiti"] = "Latin-America"
train_test$country[train_test$country=="Holand-Netherlands"] = "Euro_1"
train_test$country[train_test$country=="Honduras"] = "Latin-America"
train_test$country[train_test$country=="Hong"] = "China"
train_test$country[train_test$country=="Hungary"] = "Euro_2"
train_test$country[train_test$country=="India"] = "British-Commonwealth"
train_test$country[train_test$country=="Iran"] = "Other"
train_test$country[train_test$country=="Ireland"] = "British-Commonwealth"
train_test$country[train_test$country=="Italy"] = "Euro_1"
train_test$country[train_test$country=="Jamaica"] = "Latin-America"
train_test$country[train_test$country=="Japan"] = "Other"
train_test$country[train_test$country=="Laos"] = "SE-Asia"
train_test$country[train_test$country=="Mexico"] = "Latin-America"
train_test$country[train_test$country=="Nicaragua"] = "Latin-America"
train_test$country[train_test$country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
train_test$country[train_test$country=="Peru"] = "South-America"
train_test$country[train_test$country=="Philippines"] = "SE-Asia"
train_test$country[train_test$country=="Poland"] = "Euro_2"
train_test$country[train_test$country=="Portugal"] = "Euro_2"
train_test$country[train_test$country=="Puerto-Rico"] = "Latin-America"
train_test$country[train_test$country=="Scotland"] = "British-Commonwealth"
train_test$country[train_test$country=="South"] = "Euro_2"
train_test$country[train_test$country=="Taiwan"] = "China"
train_test$country[train_test$country=="Thailand"] = "SE-Asia"
train_test$country[train_test$country=="Trinadad&Tobago"] = "Latin-America"
train_test$country[train_test$country=="United-States"] = "United-States"
train_test$country[train_test$country=="Vietnam"] = "SE-Asia"
train_test$country[train_test$country=="Yugoslavia"] = "Euro_2"


#Education is most important
train_test$education = gsub("^10th","Dropout",train_test$education)
train_test$education = gsub("^11th","Dropout",train_test$education)
train_test$education = gsub("^12th","Dropout",train_test$education)
train_test$education = gsub("^1st-4th","Dropout",train_test$education)
train_test$education = gsub("^5th-6th","Dropout",train_test$education)
train_test$education = gsub("^7th-8th","Dropout",train_test$education)
train_test$education = gsub("^9th","Dropout",train_test$education)
train_test$education = gsub("^Assoc-acdm","Associates",train_test$education)
train_test$education = gsub("^Assoc-voc","Associates",train_test$education)
train_test$education = gsub("^Bachelors","Bachelors",train_test$education)
train_test$education = gsub("^Doctorate","Doctorate",train_test$education)
train_test$education = gsub("^HS-Grad","HS-Graduate",train_test$education)
train_test$education = gsub("^Masters","Masters",train_test$education)
train_test$education = gsub("^Preschool","Dropout",train_test$education)
train_test$education = gsub("^Prof-school","Prof-School",train_test$education)
train_test$education = gsub("^Some-college","HS-Graduate",train_test$education)

# Similarly marital
train_test$marital[train_test$marital=="Never-married"] = "Never-Married"
train_test$marital[train_test$marital=="Married-AF-spouse"] = "Married"
train_test$marital[train_test$marital=="Married-civ-spouse"] = "Married"
train_test$marital[train_test$marital=="Married-spouse-absent"] = "Not-Married"
train_test$marital[train_test$marital=="Separated"] = "Not-Married"
train_test$marital[train_test$marital=="Divorced"] = "Not-Married"
train_test$marital[train_test$marital=="Widowed"] = "Widowed"

#Leaving race behind is racist no? :P
train_test$race[train_test$race=="White"] = "White"
train_test$race[train_test$race=="Black"] = "Black"
train_test$race[train_test$race=="Amer-Indian-Eskimo"] = "Amer-Indian"
train_test$race[train_test$race=="Asian-Pac-Islander"] = "Asian"
train_test$race[train_test$race=="Other"] = "Other"

#Getting income below or above 50K to High and low
train_test$income[train_test$income==">50K"]="High"
train_test$income[train_test$income=="<=50K"]="Low"

######################################################################
write.csv(x=train_test,file="ADULT_USI_FE_CATEGORICAL.csv")

######################################################################
#Doing Label Encoding
#This converts all categorical things to numeric
features = names(train_test[,-14])
for (f in features) {
  if (class(train_test[[f]])=="character") {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.numeric(as.integer(factor(train_test[[f]], levels=levels)))
  }
}
write.csv(x=train_test,file="ADULT_USI_FE_Numerical.csv")


print("=====================Loading datasets complete.=================")

