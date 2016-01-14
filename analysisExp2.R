library("ggplot2")
rawData<-read.table("completeExp2.csv",sep=",",header=TRUE)

# clean up data

dataStep2<-subset(rawData,subset=block==1)
dataStep3<-subset(dataStep2,subset=error!=1) # remove errors

meanRT<-mean(dataStep3$RT)
sdRT<-sd(dataStep3$RT)
data<-subset(dataStep3,subset=RT<meanRT+3*sdRT & RT>meanRT-3*sdRT) # remove 3 SD outliers
attach(data)

# first analyze time measures
# PERFORMANCE MEASURES
# RT
agg=aggregate(RT~subject+distance+condition+response,data=data,FUN="mean") # RT performance data aggregated by subject
RT.aov=aov(RT~as.factor(distance)*as.factor(condition)*as.factor(response)+Error(as.factor(subject)/(as.factor(distance)*as.factor(condition)*as.factor(response))),data=agg)
summary(RT.aov)
print(model.tables(RT.aov,"means"),digits=3)

summary=summarySEwithin(agg,measurevar="RT",withinvars=c("condition","distance","response"),idvar="subject")
summary$condition<-c("congruent","congruent","congruent","congruent","congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent")
summary$response<-c("downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward")

ggplot(summary,aes(x=distance,y=RT,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=RT-ci,ymax=RT+ci))+facet_grid(~response)+labs(x="Numerical distance",y="Mean RT (ms)")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme(strip.text=element_text(face="bold",size=rel(1.3)))

# init
agg=aggregate(init.time~subject+distance+condition+response,data=data,FUN="mean") # RT performance data aggregated by subject
init.aov=aov(init.time~as.factor(distance)*as.factor(condition)*as.factor(response)+Error(as.factor(subject)/(as.factor(distance)*as.factor(condition)*as.factor(response))),data=agg)
summary(init.aov)
print(model.tables(init.aov,"means"),digits=3)

summary=summarySEwithin(agg,measurevar="init.time",withinvars=c("condition","distance","response"),idvar="subject")
summary$condition<-c("congruent","congruent","congruent","congruent","congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent")
summary$response<-c("downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward")

ggplot(summary,aes(x=distance,y=init.time,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=init.time-ci,ymax=init.time+ci))+facet_grid(~response)+labs(x="Numerical distance",y="Mean Inititation Time (ms)")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme(strip.text=element_text(face="bold",size=rel(1.3)))+ylim(70,100)

# movement time
agg=aggregate(RT-init.time~subject+distance+condition+response,data=data,FUN="mean") # RT performance data aggregated by subject
names(agg)[5]<-c("MT")
MT.aov=aov(MT~as.factor(distance)*as.factor(condition)*as.factor(response)+Error(as.factor(subject)/(as.factor(distance)*as.factor(condition)*as.factor(response))),data=agg)
summary(MT.aov)
print(model.tables(MT.aov,"means"),digits=3)

# test linear trend in Distance factor
dist<-as.factor(agg$distance)
contrasts(dist)<-contr.poly(4)
distanceTrendMT<-aov(MT~dist,data=agg)
summary.lm(distanceTrendMT)

summary=summarySEwithin(agg,measurevar="MT",withinvars=c("condition","distance","response"),idvar="subject")
summary$condition<-c("congruent","congruent","congruent","congruent","congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent")
summary$response<-c("downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward")

ggplot(summary,aes(x=distance,y=MT,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=MT-ci,ymax=MT+ci))+facet_grid(~response)+labs(x="Numerical distance",y="Mean MT (ms)")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme(strip.text=element_text(face="bold",size=rel(1.3)))


# second, graph downward and upward trajectories separately
# congruent versus incongruent mapping

# plot hand trajectories

dataLeftCongruent1<-subset(data,response==1 & condition==1 & distance==1)
dataLeftCongruent2<-subset(data,response==1 & condition==1 & distance==2)
dataLeftCongruent3<-subset(data,response==1 & condition==1 & distance==3)
dataLeftCongruent4<-subset(data,response==1 & condition==1 & distance==4)

dataLeftIncongruent1<-subset(data,response==1 & condition==2 & distance==1)
dataLeftIncongruent2<-subset(data,response==1 & condition==2 & distance==2)
dataLeftIncongruent3<-subset(data,response==1 & condition==2 & distance==3)
dataLeftIncongruent4<-subset(data,response==1 & condition==2 & distance==4)

dataRightCongruent1<-subset(data,response==2 & condition==1 & distance==1)
dataRightCongruent2<-subset(data,response==2 & condition==1 & distance==2)
dataRightCongruent3<-subset(data,response==2 & condition==1 & distance==3)
dataRightCongruent4<-subset(data,response==2 & condition==1 & distance==4)

dataRightIncongruent1<-subset(data,response==2 & condition==2 & distance==1)
dataRightIncongruent2<-subset(data,response==2 & condition==2 & distance==2)
dataRightIncongruent3<-subset(data,response==2 & condition==2 & distance==3)
dataRightIncongruent4<-subset(data,response==2 & condition==2 & distance==4)

xCoords=rep(0,1616)
yCoords=rep(0,1616)
side=rep(0,1616)
condition=rep(0,1616)
distance=rep(0,1616)

for (i in 1:101){
  xCoords[i]=mean(dataLeftCongruent1[,i+20])
  yCoords[i]=mean(dataLeftCongruent1[,i+121])
  side[i]="downward"
  condition[i]="congruent"
  distance[i]="1"
  
  xCoords[i+101]=mean(dataLeftCongruent2[,i+20])
  yCoords[i+101]=mean(dataLeftCongruent2[,i+121])
  side[i+101]="downward"
  condition[i+101]="congruent"
  distance[i+101]="2"
  
  xCoords[i+202]=mean(dataLeftCongruent3[,i+20])
  yCoords[i+202]=mean(dataLeftCongruent3[,i+121])
  side[i+202]="downward"
  condition[i+202]="congruent"
  distance[i+202]="3"
  
  xCoords[i+303]=mean(dataLeftCongruent4[,i+20])
  yCoords[i+303]=mean(dataLeftCongruent4[,i+121])
  side[i+303]="downward"
  condition[i+303]="congruent"
  distance[i+303]="4"
  
  xCoords[i+404]=mean(dataLeftIncongruent1[,i+20])
  yCoords[i+404]=mean(dataLeftIncongruent1[,i+121])
  side[i+404]="downward"
  condition[i+404]="incongruent"
  distance[i+404]="1"
  
  xCoords[i+505]=mean(dataLeftIncongruent2[,i+20])
  yCoords[i+505]=mean(dataLeftIncongruent2[,i+121])
  side[i+505]="downward"
  condition[i+505]="incongruent"
  distance[i+505]="2"
  
  xCoords[i+606]=mean(dataLeftIncongruent3[,i+20])
  yCoords[i+606]=mean(dataLeftIncongruent3[,i+121])
  side[i+606]="downward"
  condition[i+606]="incongruent"
  distance[i+606]="3"
  
  xCoords[i+707]=mean(dataLeftIncongruent4[,i+20])
  yCoords[i+707]=mean(dataLeftIncongruent4[,i+121])
  side[i+707]="downward"
  condition[i+707]="incongruent"
  distance[i+707]="4"
  
  xCoords[i+808]=mean(dataRightCongruent1[,i+20])
  yCoords[i+808]=mean(dataRightCongruent1[,i+121])
  side[i+808]="upward"
  condition[i+808]="congruent"
  distance[i+808]="1"
  
  xCoords[i+909]=mean(dataRightCongruent2[,i+20])
  yCoords[i+909]=mean(dataRightCongruent2[,i+121])
  side[i+909]="upward"
  condition[i+909]="congruent"
  distance[i+909]="2"
  
  xCoords[i+1010]=mean(dataRightCongruent3[,i+20])
  yCoords[i+1010]=mean(dataRightCongruent3[,i+121])
  side[i+1010]="upward"
  condition[i+1010]="congruent"
  distance[i+1010]="3"
  
  xCoords[i+1111]=mean(dataRightCongruent4[,i+20])
  yCoords[i+1111]=mean(dataRightCongruent4[,i+121])
  side[i+1111]="upward"
  condition[i+1111]="congruent"
  distance[i+1111]="4"
  
  xCoords[i+1212]=mean(dataRightIncongruent1[,i+20])
  yCoords[i+1212]=mean(dataRightIncongruent1[,i+121])
  side[i+1212]="upward"
  condition[i+1212]="incongruent"
  distance[i+1212]="1"
  
  xCoords[i+1313]=mean(dataRightIncongruent2[,i+20])
  yCoords[i+1313]=mean(dataRightIncongruent2[,i+121])
  side[i+1313]="upward"
  condition[i+1313]="incongruent"
  distance[i+1313]="2"
  
  xCoords[i+1414]=mean(dataRightIncongruent3[,i+20])
  yCoords[i+1414]=mean(dataRightIncongruent3[,i+121])
  side[i+1414]="upward"
  condition[i+1414]="incongruent"
  distance[i+1414]="3"
  
  xCoords[i+1515]=mean(dataRightIncongruent4[,i+20])
  yCoords[i+1515]=mean(dataRightIncongruent4[,i+121])
  side[i+1515]="upward"
  condition[i+1515]="incongruent"
  distance[i+1515]="4"
}



library("ggplot2")
trajectoryData=data.frame(xCoords,yCoords,side,condition,distance)
trajectoryData$side<-factor(trajectoryData$side,levels=c("upward","downward"))


plot=ggplot(trajectoryData,aes(x=xCoords,y=yCoords,group=condition))+xlim(-1,1)+ylim(0,1.5)
paths=geom_path(aes(linetype=condition),size=1.3)
labels=labs(x="x-coordinates",y="y-coordinates")
faceting=facet_grid(side~distance)
stripFormat=theme(strip.text=element_text(face="bold",size=rel(1.5)))
legendFormat=theme(legend.title=element_text(face="bold",size=rel(1.5)),legend.text=element_text(size=rel(1.5)))
axesFormat=theme(axis.title=element_text(size=rel(1.4)))


basePlot=plot+paths+labels+faceting+stripFormat+legendFormat+axesFormat
basePlot+labs(colour="Condition")+theme(legend.position=c(0.5,0.5))+theme(legend.background=element_rect(fill="white",colour="black"))

# notes: export as 700 x 800


# aggregate AUC and MD measures by distance, side, and condition
agg=aggregate(AUC~subject+distance+condition+response,data=data,FUN="mean") # AUC performance data aggregated by subject
AUC.aov=aov(AUC~as.factor(distance)*as.factor(condition)*as.factor(response)+Error(as.factor(subject)/(as.factor(distance)*as.factor(condition)*as.factor(response))),data=agg)
summary(AUC.aov)
print(model.tables(AUC.aov,"means"),digits=3)

# test linear trend in Distance factor
dist<-as.factor(agg$distance)
contrasts(dist)<-contr.poly(4)
distanceTrendAUC<-aov(AUC~dist,data=agg)
summary.lm(distanceTrendAUC)

summary=summarySEwithin(agg,measurevar="AUC",withinvars=c("condition","distance","response"),idvar="subject")
summary$condition<-c("congruent","congruent","congruent","congruent","congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent")
summary$response<-c("downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward")

ggplot(summary,aes(x=distance,y=AUC,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=AUC-ci,ymax=AUC+ci))+facet_grid(~response)+labs(x="Numerical distance",y="Mean Area Under Curve")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme(strip.text=element_text(face="bold",size=rel(1.3)))

# MD
agg=aggregate(MD~subject+distance+condition+response,data=data,FUN="mean") # AUC performance data aggregated by subject
MD.aov=aov(MD~as.factor(distance)*as.factor(condition)*as.factor(response)+Error(as.factor(subject)/(as.factor(distance)*as.factor(condition)*as.factor(response))),data=agg)
summary(MD.aov)
print(model.tables(MD.aov,"means"),digits=3)

summary=summarySEwithin(agg,measurevar="MD",withinvars=c("condition","distance","response"),idvar="subject")
summary$condition<-c("congruent","congruent","congruent","congruent","congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent")
summary$response<-c("downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward","downward","upward")

ggplot(summary,aes(x=distance,y=MD,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=MD-ci,ymax=MD+ci))+facet_grid(~response)+labs(x="Numerical distance",y="Mean Maximum Deviation")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme(strip.text=element_text(face="bold",size=rel(1.3)))





# compute Asymmetry scores

# large targets, large distance

numParticipants<-32
  
subjAsymmetryScoreLargerLarge=rep(0,numParticipants)
for (i in 1:numParticipants){
  yC=rep(0,101)
  yI=rep(0,101)
  subjectCong<-subset(data,code=="large-consistent" & distance>2 & subject==i)
  subjectIncong<-subset(data,code=="large-inconsistent" & distance>2 & subject==i)
  
  for (j in 1:101){
    yC[j]<-mean(subjectCong[,j+121])  
    yI[j]<-mean(subjectIncong[,j+121])
  }
  
  subjAsymmetryScoreLargerLarge[i]=mean((yC+yI)/2-0.75)
}


# large targets, small distance

subjAsymmetryScoreLargerSmall=rep(0,numParticipants)
for (i in 1:numParticipants){
  yC=rep(0,101)
  yI=rep(0,101)
  subjectCong<-subset(data,code=="large-consistent" & distance<3 & subject==i)
  subjectIncong<-subset(data,code=="large-inconsistent" & distance<3 & subject==i)
  
  for (j in 1:101){
    yC[j]<-mean(subjectCong[,j+121])  
    yI[j]<-mean(subjectIncong[,j+121])
  }
  
  subjAsymmetryScoreLargerSmall[i]<-mean((yC+yI)/2-0.75)
}

# small targets, large distance

subjAsymmetryScoreSmallerLarge=rep(0,numParticipants)
for (i in 1:numParticipants){
  yC=rep(0,101)
  yI=rep(0,101)
  subjectCong<-subset(data,code=="small-consistent" & distance>2 & subject==i)
  subjectIncong<-subset(data,code=="small-inconsistent" & distance>2 & subject==i)
  
  for (j in 1:101){
    yC[j]<-mean(subjectCong[,j+121])  # x coords span from column 21 to 121
    yI[j]<-mean(subjectIncong[,j+121])
  }
  
  subjAsymmetryScoreSmallerLarge[i]<-mean((yC+yI)/2-0.75)
}


# small targets, small distance


subjAsymmetryScoreSmallerSmall=rep(0,numParticipants)
for (i in 1:numParticipants){
  yC=rep(0,101)
  yI=rep(0,101)
  subjectCong<-subset(data,code=="small-consistent" & distance<3 & subject==i)
  subjectIncong<-subset(data,code=="small-inconsistent" & distance<3 & subject==i)
  
  for (j in 1:101){
    yC[j]<-mean(subjectCong[,j+121])  
    yI[j]<-mean(subjectIncong[,j+121])
  }
  
  subjAsymmetryScoreSmallerSmall[i]<-mean((yC+yI)/2-0.75)
}

subject=rep(0,numParticipants*4)
distance=rep(0,numParticipants*4)
decision=rep(0,numParticipants*4)
asymmetry=rep(0,numParticipants*4)

for (i in 1:numParticipants){
  subject[i]<-i
  subject[i+numParticipants]<-i
  subject[i+2*numParticipants]<-i
  subject[i+3*numParticipants]<-i
  
  distance[i]<-"small"
  distance[i+numParticipants]<-"small"
  distance[i+2*numParticipants]<-"large"
  distance[i+3*numParticipants]<-"large"
  
  decision[i]<-"smaller"
  decision[i+numParticipants]<-"larger"
  decision[i+2*numParticipants]<-"smaller"
  decision[i+3*numParticipants]<-"larger"
  
  asymmetry[i]<-subjAsymmetryScoreSmallerSmall[i]
  asymmetry[i+numParticipants]<-subjAsymmetryScoreLargerSmall[i]
  asymmetry[i+2*numParticipants]<-subjAsymmetryScoreSmallerLarge[i]
  asymmetry[i+3*numParticipants]<-subjAsymmetryScoreLargerLarge[i]
  
}

x<-data.frame(subject,distance,decision,asymmetry) # for ANOVA in R
x<-data.frame(subjAsymmetryScoreSmallerSmall,subjAsymmetryScoreSmallerLarge,subjAsymmetryScoreLargerSmall,subjAsymmetryScoreLargerLarge) # for ANOVA in JASP
write.csv(x,"~/Dropbox/experiments/mouseTrackComparisonData/Rscripts/asymmetryScores.csv")


asym.aov<-aov(asymmetry~as.factor(distance)*as.factor(decision)+Error(as.factor(subject)/(as.factor(distance)*as.factor(decision))),data=x)
summary(asym.aov)
print(model.tables(asym.aov,"means"),digits=3)

summary=summarySEwithin(x,measurevar="asymmetry",withinvars=c("distance","decision"),idvar="subject")

summary$distance<-factor(summary$distance,levels=c("small","large"))

ggplot(summary,aes(x=distance,y=asymmetry,shape=decision))+geom_line(aes(group=decision,linetype=decision))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=asymmetry-ci,ymax=asymmetry+ci))+labs(x="Numerical distance",y="Asymmetry score")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))




# convert AUC to z-scores BY PARTICIPANT

z.agg.mean<-aggregate(AUC~subject,data=data,FUN="mean")
z.agg.sd<-aggregate(AUC~subject,data=data,FUN="sd")
z.AUC=c(rep(0,length(data$subject)))
for(i in 1:length(data$subject)){
  z.AUC[i]=(data$AUC[i]-z.agg.mean$AUC[data$subject[i]])/z.agg.sd$AUC[data$subject[i]]
}


# assess bimodality of AUC values 

library("diptest")
dip.test(z.AUC[data$condition==2])
dip.test(z.AUC)


# Bimodality coefficient (SAS)

library("moments")

s=skewness(z.AUC[data$condition==2])
k=kurtosis(z.AUC[data$condition==2])
n=length(z.AUC[data$condition==2])

BC=(s^2+1)/(k+(3*(n-1)^2)/((n-2)*(n-3)))
BC




# helper function for error bars above

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}
