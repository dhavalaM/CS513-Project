rm(list=ls())

response<-read.csv("C:/Dhavala/Courses/CS513/CS513-Project/responses-shortened.csv")

library('C50')

y<-mean(as.numeric(response[,6]),na.rm=TRUE)

# round of the mean to 0 decimal points
x<-round(y,digits=0)

#replace the ? values with the mean values
response$Classical.music[response$Classical.music=="NA"]<-x

y<-mean(as.numeric(response[,8]),na.rm=TRUE)

# round of the mean to 0 decimal points
x<-round(y,digits=0)

#replace the ? values with the mean values
response$Pop[response$Pop=="NA"]<-x

y<-mean(as.numeric(response[,90]),na.rm=TRUE)

# round of the mean to 0 decimal points
x<-round(y,digits=0)

#replace the ? values with the mean values
response$Loneliness[response$Loneliness=="NA"]<-x

y<-mean(as.numeric(response[,92]),na.rm=TRUE)

# round of the mean to 0 decimal points
x<-round(y,digits=0)

#replace the ? values with the mean values
response$Health[response$Health=="NA"]<-x



###### Simple example to check the effect of music on puntuality ####
newdata<-data.frame(   Classical=response[,6],
                       Pop=response[,8],
                       punctuality=as.factor(response[,98] )
)

###### Simple example to check the effect of adrenalin sports liking on puntuality ####
newdata<-data.frame(   adrenalin=as.factor(response[,52] ),
                       punctuality=as.factor(response[,98] )
)

newdata<-data.frame(   loneliness=response[,90] ,
                       health=as.factor(response[,92]) 
)

mode(newdata)

View(newdata)

mytree<-C5.0(health~.,data=newdata)

summary(mytree)
mytree
plot(mytree)

#################################################################

