rm(list=ls())

response<-read.csv("/Users/sowmyav/Desktop/Fall2017/KDD/YoungPeopleSurveyProject/young-people-survey/responses_knn.csv"
                   ,na.strings=c("","NA"))
summary(response)
response_new<-na.omit(response)
summary(response_new)
nrow(response) #1010
nrow(response_new)

response_new[,1] <- factor(response_new[,1], levels=c("female", "male"))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

response_new_norm <- as.data.frame(lapply(response_new[2:51], normalize))
response_new <- cbind(response_new[,1],response_new_norm)

response_new_train <- response_new[1:750,]
response_new_test <- response_new[751:848,]



library(class)

predict<-knn(response_new_train[c(-1)],response_new_test[c(-1)],response_new_train[,1],k=5)

# combine the prediction with the test data
results<-cbind(response_new_test, as.character(predict))
head(results)
?table()
#Measure the performance of knn
table(Actual=results[,1],Prediction=results[,52])

wrong<-results[,1]!=results[,52]
rate<-sum(wrong)/length(wrong)
rate #[1] 0.122449



