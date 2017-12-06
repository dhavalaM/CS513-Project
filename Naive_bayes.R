#  Course    : CS 513
#  First Name  : Sowmya 
#  Last Name  : Vijayakumar
#  CWId: 10421665
#Naive Bayes Classifier for young people survey data
rm(list=ls())

#Read the input data file
response<-read.csv("/Users/sowmyav/Desktop/Fall2017/KDD/YoungPeopleSurveyProject/young-people-survey/responses_remove_categorical.csv"
             ,na.strings = c(""," ", NA, NaN))

#bc<-read.csv("/Users/sowmyav/Desktop/Fall2017/KDD/breast-cancer-wisconsin_data.csv")
#response<-na.omit(response)
#Replacing the numerical NA values with mean
for(i in 1:139){
  response[is.na(response[,i]), i] <- round(mean(response[,i], na.rm = TRUE))
}

#Replacing the categorical NA values with mode #Gender colum
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

response[is.na(response[,140]), 140] <- Mode(response[,140])

#Create test and training set
set.seed(321) #set repeatable random numbers
index = sample(1:nrow(response), size=0.3*nrow(response))
test<-response[index,]
training <- response[-index,]


library(e1071)
model <- naiveBayes(Gender ~ ., data = training)
class(model)
#summary(model)
#print(model)

preds <- predict(model, newdata = test)

table(preds, test$Gender)



results<-cbind(test, as.character(preds))
dim(results)
#head(results)
?table()
#Measure the performance of svm
table(Actual=results[,140],Prediction=results[,141])

wrong<-results[,140]!=results[,141]
rate<-sum(wrong)/length(wrong)
rate #0.09756098
#Results:
#Without NAs
#  # Prediction
# Actual   female male
# female    105   11
# male        9   80

#Replacing NAs
# Prediction
# Actual   female male
# female    186    6
# male       11  100

#rate 0.05610561



