#  Course    : CS 513
#  First Name  : Dhavala 
#  Last Name  : Manjunatha
#  CWId: 10421641
#  Project         : Young people survey
#  Team            : Devanshu, Sowmya, Dhavala 

rm(list=ls())

#install.packages('neuralnet')

library(neuralnet)


response<-read.csv("C:/Dhavala/Courses/CS513/CS513-Project/responses_remove_categorical.csv",na.strings ="")
for(i in 1:139){
  response[is.na(response[,i]), i] <- round(mean(response[,i], na.rm = TRUE))
}
response_new<-response

response_new <- na.omit(response)

response_new$Gender <- as.numeric(response_new$Gender == 'female')

#create test and training sets
every_5th <- seq (1,nrow(response_new),by=5)
test<-response_new[every_5th,]
training<-response_new[-every_5th,]

#create the formula
n <- paste(names(response_new[1:139]), collapse = ' + ')
f <- as.formula(c("Gender ~ " , n))


#the actual values 
actual <- response_new[every_5th,140]


nn <- neuralnet(f,data= training,hidden=c(10),linear.output=FALSE,learningrate=0.05)

results1 <- compute(nn,test[1:139])


estimate1 <- round(results1$net.result)

#gender
#hidden =5 #0.8882353
#hidden=100 # 0.8470588235

#gender replacing with mean
#hidden=100 #0.80198
#hidden=100 learningrate=0.1 #0.8564356436
#hidden=5 learningrate=0.1 #0.8564356436
accuracy1 <- mean(estimate1 == actual) 
table(estimate1,actual)
plot(nn)
summary(nn)



