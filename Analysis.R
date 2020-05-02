filename <- "repdata_data_StormData.csv.bz2"
if (!file.exists(filename)){
  url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url, filename, method="curl")
}  
stormdata<-read.csv("repdata_data_StormData.csv.bz2")
injuries<-summarise(group_by(stormdata,EVTYPE,INJURIES))
fatality<-summarise(group_by(stormdata,EVTYPE,FATALITIES))
fatality<-fatality[fatality$FATALITIES>0,]
injuries<-injuries[injuries$INJURIES>0,]
fatality<-summarise(group_by(fatality,EVTYPE),FATALITIES=sum(FATALITIES))
injuries<-summarise(group_by(injuries,EVTYPE),INJURIES=sum(INJURIES))
injuries<-injuries[order(injuries$INJURIES,decreasing = TRUE),]
fatality<-fatality[order(fatality$FATALITIES,decreasing = TRUE),]
combineddata<-merge(injuries,fatality,by="EVTYPE")
combineddata$total<- combineddata$INJURIES+combineddata$FATALITIES
combineddata<-combineddata[order(combineddata$total,decreasing = TRUE),]
ggplot(fatality[1:10,],aes(x=reorder(EVTYPE, -FATALITIES),y=FATALITIES))+geom_bar(stat="identity",fill="red")+theme(text = element_text(size=10),axis.text.x = element_text(angle=45, hjust=1))
ggplot(injuries[1:10,],aes(x=reorder(EVTYPE, -INJURIES),y=INJURIES))+geom_bar(stat="identity",fill="red")+theme(text = element_text(size=10),axis.text.x = element_text(angle=45, hjust=1))
ggplot(combineddata[1:10,],aes(x=reorder(EVTYPE, -total),y=total))+geom_bar(stat="identity",fill="red")+theme(text = element_text(size=10),axis.text.x = element_text(angle=45, hjust=1))

#part2
a <- stormdata[stormdata$PROPDMGEXP %in% c("", "K", "M", "B") & stormdata$CROPDMGEXP %in% c("", "K", "M", "B"), ]
a1<-a[,c(8,25,26,27,28)]
propexp<-vector()
for ( i in 1:nrow(a1)){
  if (as.character(a1[i,3])=="K"){
    propexp[i]<-1000
  }
  if (as.character(a1[i,3])=="B"){
    propexp[i]<-1e+09
  }
  if (as.character(a1[i,3])=="M"){
    propexp[i]<-1e+06
  }
  if (as.character(a1[i,3])==""){
    propexp[i]<-1
  }
}
cropexp<-vector()
for ( i in 1:nrow(a1)){
  if (as.character(a1[i,5])=="K"){
    cropexp[i]<-1000
  }
  if (as.character(a1[i,5])=="B"){
    cropexp[i]<-1e+09
  }
  if (as.character(a1[i,5])=="M"){
    cropexp[i]<-1e+06
  }
  if (as.character(a1[i,5])==""){
    cropexp[i]<-1
  }
}
a1<-cbind(a1,propexp,cropexp)
a1$propdamage<-a1$PROPDMG*a1$propexp
a1$cropdamage<-a1$CROPDMG*a1$cropexp
propertydmg<-summarise(group_by(a1,EVTYPE,propdamage))
propertydmg<-propertydmg[propertydmg$propdamage>0,]
propertydmg<-summarise(group_by(propertydmg,EVTYPE),propdamage=sum(propdamage))
propertydmg<-propertydmg[order(propertydmg$propdamage,decreasing = TRUE),]
cropdmg<-summarise(group_by(a1,EVTYPE,cropdamage))
cropdmg<-cropdmg[cropdmg$cropdamage>0,]
cropdmg<-summarise(group_by(cropdmg,EVTYPE),cropdamage=sum(cropdamage))
cropdmg<-cropdmg[order(cropdmg$cropdamage,decreasing = TRUE),]

ggplot(propertydmg[1:10,],aes(x=reorder(EVTYPE, -propdamage),y=propdamage))+geom_bar(stat="identity",fill="red")+theme(text = element_text(size=10),axis.text.x = element_text(angle=45, hjust=1))
ggplot(cropdmg[1:10,],aes(x=reorder(EVTYPE, -cropdamage),y=cropdamage))+geom_bar(stat="identity",fill="red")+theme(text = element_text(size=10),axis.text.x = element_text(angle=45, hjust=1))

combinedecodamage<-merge(propertydmg,cropdmg,by="EVTYPE")
combinedecodamage$total<-combinedecodamage$propdamage+combinedecodamage$cropdamage
combinedecodamage<-combinedecodamage[order(combinedecodamage$total,decreasing = TRUE),]
ggplot(combinedecodamage[1:10,],aes(x=reorder(EVTYPE, -total),y=total))+geom_bar(stat="identity",fill="red")+theme(text = element_text(size=10),axis.text.x = element_text(angle=45, hjust=1))


