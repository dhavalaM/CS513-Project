#  Course    : CS 513
#  First Name  : Dhavala 
#  Last Name  : Manjunatha
#  CWId: 10421641

rm(list=ls())


library('C50')

response<-read.csv("C:/Dhavala/Courses/CS513/CS513-Project/responses_rf.csv",na.strings ="")
for(i in 1:41){
  response[is.na(response[,i]), i] <- round(mean(response[,i], na.rm = TRUE))
}
response_new<-response
response_new$Alcohol<-as.numeric(response_new[,42])


#create the formula


newdata<-data.frame(   Horror=response_new[,21] 
                       ,Thriller=response_new[,22] 
                       , Comedy=response_new[,23]
                       , Romantic=response_new[,24] 
                       ,alcohol=as.factor(response_new[,42])
)



mytree<-C5.0(alcohol~.,data=newdata)

summary(mytree)
mytree
plot(mytree)



