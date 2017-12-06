#  Course    : CS 513
#  First Name  : Dhavala 
#  Last Name  : Manjunatha
#  CWId: 10421641

rm(list=ls())


library('C50')

response<-read.csv("C:/Dhavala/Courses/CS513/CS513-Project/responses_knn.csv",na.strings ="")
for(i in 2:51){
  response[is.na(response[,i]), i] <- round(mean(response[,i], na.rm = TRUE))
}
response_new<-response[1:47]


#create the formula
n <- paste(names(response_new[2:47]), collapse = ' + ')
f <- as.formula(c("Gender ~ " , n))

newdata<-data.frame(   Horror=response_new[,22] 
                       ,Thriller=response_new[,23] 
                       , Comedy=response_new[,24]
                       , Romantic=response_new[,25] 
                       ,gender=response_new[,1]
)



mytree<-C5.0(gender~.,data=newdata)

summary(mytree)
mytree
plot(mytree)



