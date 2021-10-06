library(reshape2)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(foreign)
library(GGally)
library(lme4)
library(dplyr)
library(tidyr)
#Reading in data, generating variables
makeDF<-function(fileList) {
files<-NULL
dfs<-NULL
countStarts<-NULL
countEnds<-NULL
names<-NULL
daysStart<-NULL
daysEnd<-NULL
dfsNew<-NULL
minCell<-NULL
maxCell<-NULL
counts<-NULL
endDFs<-NULL
for(i in 1:length(fileList)){
  files[i]<-list(read.csv(fileList[i],header=FALSE))
}
for(i in 1:length(files)){
  countStarts[i]<-min(which(files[[i]][5:nrow(files[[i]]),4]!="NaN"))
}
countStart<-max(countStarts)
for(i in 1:length(files)){
  names[i]<-list.files(path="./CSVs")[i]
  dfs[[i]]<-data.frame("day"=as.numeric(na.omit(files[[i]][countStart+5:nrow(files[[i]]),1])),
                       "hour"=as.numeric(na.omit(files[[i]][countStart+5:nrow(files[[i]]),2])),
                       "mins"=as.numeric(na.omit(files[[i]][countStart+5:nrow(files[[i]]),3])),
                       "counts"=as.numeric(na.omit(files[[i]][countStart+5:nrow(files[[i]]),4])))
}
names <- names[c(-28)]
for(i in 1:length(files)){
  daysStart[i]<-min(as.numeric(dfs[[i]]$day[dfs[[i]]$hour==0]))
  daysEnd[i]<-max(dfs[[i]]$day[dfs[[i]]$hour==23])
}
dayStart<-min(as.numeric(daysStart))
dayEnd<-min(as.numeric(daysEnd))
for(i in 1:length(files)){
  minCell[i]<-as.numeric(min(which((files[[1]][,1]==dayStart)==TRUE)))
  maxCell[i]<-as.numeric(max(which((files[[1]][,1]==dayEnd)==TRUE)))
  dfsNew[[i]]<-data.frame("day"=as.numeric(na.omit(files[[i]][minCell[1]+1:maxCell[1]-1,1])),
                          "hour"=as.numeric(na.omit(files[[i]][minCell[1]+1:maxCell[1]-1,2])),
                          "mins"=as.numeric(na.omit(files[[i]][minCell[1]+1:maxCell[1]-1,3])),
                          "counts"=as.numeric(na.omit(files[[i]][minCell[1]+1:maxCell[1]-1,4])))
}
for(i in 1:length(dfsNew)){
  obj <-dfsNew[[i]]
  print(fileList[i])
  for(day in min(dfsNew[[i]]$day):max(dfsNew[[1]]$day-1)){
    for(hour in 0:23) {
      counts$name <- fileList[i]
      counts$day[(((day)-2)*24)+hour+1] <- day
      counts$hour[(((day)-2)*24)+hour+1] <- hour
      counts$counts[(((day)-2)*24)+hour+1] <- sum(na.omit(obj$counts[obj$day==day&obj$hour==hour]))
    }
  }
  endDFs[[i]] <- data.frame("name"=counts$name, day=counts$day,"hour"=counts$hour,"counts"=counts$counts)
}
#View(endDFs[[1]])
done<-NULL
for(i in 1:length(endDFs)) {
  done<-rbind(done,endDFs[[i]])
}
return(done)
}
cdat<-makeDF(paste("./CSVs/",list.files(path="./CSVs"),sep=""))

write.csv(cdat,"longDat.csv")

wideDat<-cdat %>% group_by(name,hour,cohort,round) %>% summarize(counts=mean(counts))
wideDat2<-dcast(wideDat,name+cohort+round~hour)
View(wideDat2)
write.csv(wideDat2,"wideDat.csv")
