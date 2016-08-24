install.packages("plyr")
install.packages("reshape2")
install.packages("xlsx")
install.packages("maptools")
install.packages("ggmap")
install.packages("shiny")
install.packages("lubridate")
install.packages("ggplot2")


require(plyr)
require(reshape2)
require(xlsx)
require(maptools)
require(ggmap)
require(shiny)
require(lubridate)
require(ggplot2)

setwd("D:/Tarun/data")

data<-read.csv("raw_data_latest_wkend.csv")
colnames(data)
data1<-data[,c("occ","cnc")]

data2<-ddply(data1,c("occ","cnc"),summarize,
             count=length(occ))

zonalcodes<-read.csv("Zone Matrix - 10092015.csv")
zonalcodes<-zonalcodes[,1:3]
colnames(zonalcodes)<-c("occ","cnc","zonalcode")

mergeddata<-merge(data2,zonalcodes,by=c("occ","cnc"),all.x=TRUE)
mergeddata1<-mergeddata[,c(1,3,4)]

data3<-ddply(mergeddata1,c("occ","zonalcode"),summarize,
             count=sum(count))

data4<-data3[order(data3$count,decreasing = TRUE),]

data4[,1]<-as.character(data4[,1])
data4[,2]<-as.character(data4[,2])

data4[is.na(data4[,1]),1]<-"Undefined"
data4[is.na(data4[,2]),2]<-"Undefined"

data4[rownames(data4)==1,1]<-"Undefined"


data5<-dcast(data4,occ~zonalcode)
data5$Total<-rowSums(data5[,2:7],na.rm=TRUE)

data5<-data5[order(data5$Total,decreasing = TRUE),]

data5[is.na(data5)]<-0

colnames(data5)<-c("occ","zone 1","zone 2","zone 3","zone 4","zone 5","Undefined","Total")

data5[,1]<-as.character(data5[,1])
data5[(rownames(data5))==1,1]="Undefined"

###### Write output file######

write.csv(data5,"byzonalcode.csv",row.names=FALSE)
write.csv(data4,"byzonalmelted.csv",row.names=FALSE)



######## map india ##########

masterdata<-cbind(data4,geocode(data4$occ))
write.csv(masterdata,"datawithlatitudelongitude.csv",row.names=FALSE)


####### shiny ##############


runApp("app1")
