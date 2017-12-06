#  Course    : CS 513
#  First Name  : Sowmya 
#  Last Name  : Vijayakumar
#  CWId: 10421665
# KNN Classification for young people survey data


rm(list=ls())

#Read the input data file
response<-read.csv("/Users/sowmyav/Desktop/Fall2017/KDD/YoungPeopleSurveyProject/young-people-survey/responses_remove_categorical.csv"
                   ,na.strings = c(""," ", NA, NaN))

#response<-response[c(-136,-137,-138)]
#Display number of rows and columns
dim(response) #row:1010  cols:140

#Gives summary of the dataset
summary(response)
str(response)

#Replacing the numerical NA values with mean 
for(i in 1:139){
  response[is.na(response[,i]), i] <- round(mean(response[,i], na.rm = TRUE))
}
#response[c(136,137,138)]

#Replacing the categorical NA values with mode #Gender colum
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

response[is.na(response[,140]), 140] <- Mode(response[,140])
#response[is.na(response[,137]), 137] <- Mode(response[,137])

#response<-na.omit(response)
nrow(response) #1010

#response[,140] = as.integer(response[,140]) 
#Storing the column number of gender column 
gender_col<-ncol(response)

#Normalize the data using min max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

response_new_norm <- as.data.frame(lapply(response[c(-gender_col)], normalize))
response <- cbind(response_new_norm,Gender=response[,gender_col])


#str(response_new[c(4)])
#response_new <- cbind(resp_cat,response_new_norm)


#index <- seq(from = 1, to = nrow(response), by = 3)

#Create test and training set
set.seed(321) #set repeatable random numbers
index = sample(1:nrow(response), size=0.3*nrow(response))
response_new_test<-response[index,]
response_new_train <- response[-index,]
#response_new_train <- response[1:808,]
#response_new_test <- response[809:1010,]

nrow(response_new_test)
nrow(response_new_train)



library(class)
#Knn
predict<-knn(response_new_train[c(-gender_col)],response_new_test[c(-gender_col)],response_new_train[,gender_col],k=19)

# combine the prediction with the test data
results<-cbind(response_new_test, as.character(predict))
dim(results)
#head(results)
?table()
#Measure the performance of knn
table(Actual=results[,gender_col],Prediction=results[,gender_col+1])
#results[,149] <- factor(results[,141], levels=levels(results[,1]))

wrong<-results[,gender_col]!=results[,gender_col+1]
rate<-sum(wrong)/length(wrong)
rate #[1]   0.1056106 #predicting gender without other categories , k=19


#Results:
# Prediction
# Actual   female male
# female    176   16
# male       16   95


