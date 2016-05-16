############################################################################################################
## Analysis for Experiment 1 of "Testing a direct mapping versus..."
## Note: before plotting any graphs, be sure to execute the "helper functions" at the bottom of this script
############################################################################################################


library("ggplot2")
rawData<-read.table("completeExp1.csv",sep=",",header=TRUE)

# clean up data

dataStep3<-subset(rawData,subset=error!=1) # remove errors

meanRT<-mean(dataStep3$RT)
sdRT<-sd(dataStep3$RT)
data<-subset(dataStep3,subset=RT<meanRT+3*sdRT & RT>meanRT-3*sdRT) # remove 3 SD outliers
attach(data)

# first analyze time measures
# PERFORMANCE MEASURES

# movement time
agg=aggregate(RT-init.time~subject+distance+condition+response,data=data,FUN="mean") # RT performance data aggregated by subject
names(agg)[5]<-c("MT")
MT.aov=aov(MT~as.factor(distance)*as.factor(condition)*as.factor(response)+Error(as.factor(subject)/(as.factor(distance)*as.factor(condition)*as.factor(response))),data=agg)
summary(MT.aov)
print(model.tables(MT.aov,"means"),digits=3)

summary=summarySEwithin(agg,measurevar="MT",withinvars=c("condition","distance","response"),idvar="subject")
summary$condition<-c("congruent","congruent","congruent","congruent","congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent")
summary$response<-c("leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward")

# test linear trend in Distance factor
dist<-as.factor(agg$distance)
contrasts(dist)<-contr.poly(4)
distanceTrendMT<-aov(MT~dist,data=agg)
summary.lm(distanceTrendMT)

ggplot(summary,aes(x=distance,y=MT,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=MT-ci,ymax=MT+ci))+facet_grid(~response)+labs(x="Numerical distance",y="Mean MT (ms)")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme(strip.text=element_text(face="bold",size=rel(1.3)))+theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))

# init
agg=aggregate(init.time~subject+distance+condition+response,data=data,FUN="mean") # RT performance data aggregated by subject
init.aov=aov(init.time~as.factor(distance)*as.factor(condition)*as.factor(response)+Error(as.factor(subject)/(as.factor(distance)*as.factor(condition)*as.factor(response))),data=agg)
summary(init.aov)
print(model.tables(init.aov,"means"),digits=3)

summary=summarySEwithin(agg,measurevar="init.time",withinvars=c("condition","distance","response"),idvar="subject")
summary$condition<-c("congruent","congruent","congruent","congruent","congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent","incongruent")
summary$response<-c("leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward")


################################################################################################################
## Plot single graph of hand trajectories with error bands (collapsing over numerical distance)
## SE width at each timestep computed as standard error of mean x-coordinates over a sample of 64 participants
#########################################################################################################

dataLeftCongruent<-subset(data,response==1 & condition==1)
dataLeftIncongruent<-subset(data,response==1 & condition==2)
dataRightCongruent<-subset(data,response==2 & condition==1)
dataRightIncongruent<-subset(data,response==2 & condition==2)

# SE measures for each subset

SEmatrixLeftCongruent<-matrix(rep(0,64*101),nrow=64,ncol=101,byrow=TRUE)
SEmatrixLeftIncongruent<-matrix(rep(0,64*101),nrow=64,ncol=101,byrow=TRUE)
SEmatrixRightCongruent<-matrix(rep(0,64*101),nrow=64,ncol=101,byrow=TRUE)
SEmatrixRightIncongruent<-matrix(rep(0,64*101),nrow=64,ncol=101,byrow=TRUE)

for (i in 1:64){
  leftCongruent<-subset(dataLeftCongruent,subject==i)
  leftIncongruent<-subset(dataLeftIncongruent,subject==i)
  rightCongruent<-subset(dataRightCongruent,subject==i)
  rightIncongruent<-subset(dataRightIncongruent,subject==i)
  
  for (j in 1:101){
    SEmatrixLeftCongruent[i,j]<-mean(leftCongruent[,j+20])#/sqrt(length(leftCongruent[,j+20]))
    SEmatrixLeftIncongruent[i,j]<-mean(leftIncongruent[,j+20])#/sqrt(length(leftIncongruent[,j+20]))
    SEmatrixRightCongruent[i,j]<-mean(rightCongruent[,j+20])#/sqrt(length(rightCongruent[,j+20]))
    SEmatrixRightIncongruent[i,j]<-mean(rightIncongruent[,j+20])#/sqrt(length(rightIncongruent[,j+20]))
  }
}



xCoordsLeft=rep(0,202)
xCoordsRight=rep(0,202)
yCoordsLeft=rep(0,202)
yCoordsRight=rep(0,202)
SEleft=rep(0,202)
SEright=rep(0,202)
condition=rep(0,202)

for (i in 1:101){
  xCoordsLeft[i]=mean(dataLeftCongruent[,i+20])
  xCoordsRight[i]=-mean(dataRightCongruent[,i+20])
  yCoordsLeft[i]=mean(dataLeftCongruent[,i+121])
  yCoordsRight[i]=mean(dataRightCongruent[,i+121])
  SEleft[i]=sd(SEmatrixLeftCongruent[,i])/sqrt(64)
  SEright[i]=sd(SEmatrixRightCongruent[,i])/sqrt(64)
  condition[i]="congruent"
  
  xCoordsLeft[i+101]=mean(dataLeftIncongruent[,i+20])
  xCoordsRight[i+101]=-mean(dataRightIncongruent[,i+20])
  yCoordsLeft[i+101]=mean(dataLeftIncongruent[,i+121])
  yCoordsRight[i+101]=mean(dataRightIncongruent[,i+121])
  SEleft[i+101]=sd(SEmatrixLeftIncongruent[,i])/sqrt(64)
  SEright[i+101]=sd(SEmatrixRightIncongruent[,i])/sqrt(64)
  condition[i+101]="incongruent"
}


library("ggplot2")
trajectoryData=data.frame(yCoordsLeft,yCoordsRight,xCoordsLeft,xCoordsRight,SEleft,SEright,condition)
plot=ggplot(trajectoryData,aes())
pathLeft=geom_path(aes(x=yCoordsLeft,y=xCoordsLeft,linetype=condition),size=0.6)
ribbonLeft=geom_ribbon(aes(x=yCoordsLeft,y=xCoordsLeft,ymin=xCoordsLeft-SEleft,ymax=xCoordsLeft+SEleft,linetype=condition),alpha=0.2)
pathRight=geom_path(aes(x=yCoordsRight,y=xCoordsRight,linetype=condition),size=0.6)
ribbonRight=geom_ribbon(aes(x=yCoordsRight,y=xCoordsRight,ymin=xCoordsRight-SEright,ymax=xCoordsRight+SEright,linetype=condition),alpha=0.2)
axisLabels=labs(y="x-coordinates",x="y-coordinates")
#theme(strip.text=element_text(face="bold",size=rel(1.5)))
legendFormat=theme(legend.title=element_text(face="bold",size=rel(1.5)),legend.text=element_text(size=rel(1.5)))
axisFormat=theme(axis.title=element_text(size=rel(1.4)))
legend=labs(linetype="Condition")+theme(legend.position=c(0.5,1))+theme(legend.background=element_rect(fill="white",colour="black"))
classic=theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))

plot+pathLeft+ribbonLeft+pathRight+ribbonRight+axisLabels+axisFormat+legend+legendFormat+classic+coord_flip()+xlim(0,1.5)+ylim(-1,1)



# aggregate AUC  by distance, side, and condition
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
summary$response<-c("leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward","leftward","rightward")

ggplot(summary,aes(x=distance,y=AUC,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=AUC-ci,ymax=AUC+ci))+facet_grid(~response)+labs(x="Numerical distance",y="Mean Area Under Curve")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme(strip.text=element_text(face="bold",size=rel(1.3)))+theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))


# subset on only INCONGRUENT trials

aggSubset<-subset(agg,subset=condition==2)
AUC.aov=aov(AUC~as.factor(distance)*as.factor(response)+Error(as.factor(subject)/(as.factor(distance)*as.factor(response))),data=aggSubset)
summary(AUC.aov)

# test linear trend in Distance factor
dist<-as.factor(aggSubset$distance)
contrasts(dist)<-contr.poly(4)
distanceTrendAUC<-aov(AUC~dist,data=aggSubset)
summary.lm(distanceTrendAUC)


# compute Asymmetry scores

# large targets, large distance

subjAsymmetryScoreLargerLarge=rep(0,64)
for (i in 1:64){
  xC=rep(0,101)
  xI=rep(0,101)
  subjectCong<-subset(data,code=="large-consistent" & distance>2 & subject==i)
  subjectIncong<-subset(data,code=="large-inconsistent" & distance>2 & subject==i)
  
  for (j in 1:101){
    xC[j]<--mean(subjectCong[,j+20])  # x coords span from column 21 to 121
    xI[j]<-mean(subjectIncong[,j+20])
  }
  
  subjAsymmetryScoreLargerLarge[i]<-mean(xC+xI)
}


# large targets, small distance

subjAsymmetryScoreLargerSmall=rep(0,64)
for (i in 1:64){
  xC=rep(0,101)
  xI=rep(0,101)
  subjectCong<-subset(data,code=="large-consistent" & distance<3 & subject==i)
  subjectIncong<-subset(data,code=="large-inconsistent" & distance<3 & subject==i)
  
  for (j in 1:101){
    xC[j]<--mean(subjectCong[,j+20])  # x coords span from column 21 to 121
    xI[j]<-mean(subjectIncong[,j+20])
  }
  
  subjAsymmetryScoreLargerSmall[i]<-mean(xC+xI)
}

# small targets, large distance

subjAsymmetryScoreSmallerLarge=rep(0,64)
for (i in 1:64){
  xC=rep(0,101)
  xI=rep(0,101)
  subjectCong<-subset(data,code=="small-consistent" & distance>2 & subject==i)
  subjectIncong<-subset(data,code=="small-inconsistent" & distance>2 & subject==i)
  
  for (j in 1:101){
    xC[j]<-mean(subjectCong[,j+20])  # x coords span from column 21 to 121
    xI[j]<--mean(subjectIncong[,j+20])
  }
  
  subjAsymmetryScoreSmallerLarge[i]<-mean(xC+xI)
}


# small targets, small distance


subjAsymmetryScoreSmallerSmall=rep(0,64)
for (i in 1:64){
  xC=rep(0,101)
  xI=rep(0,101)
  subjectCong<-subset(data,code=="small-consistent" & distance<3 & subject==i)
  subjectIncong<-subset(data,code=="small-inconsistent" & distance<3 & subject==i)
  
  for (j in 1:101){
    xC[j]<-mean(subjectCong[,j+20])  # x coords span from column 21 to 121
    xI[j]<--mean(subjectIncong[,j+20])
  }
  
  subjAsymmetryScoreSmallerSmall[i]<-mean(xC+xI)
}

subject=rep(0,256)
distance=rep(0,256)
decision=rep(0,256)
asymmetry=rep(0,256)

for (i in 1:64){
  subject[i]<-i
  subject[i+64]<-i
  subject[i+128]<-i
  subject[i+192]<-i
  
  distance[i]<-"small"
  distance[i+64]<-"small"
  distance[i+128]<-"large"
  distance[i+192]<-"large"
  
  decision[i]<-"smaller"
  decision[i+64]<-"larger"
  decision[i+128]<-"smaller"
  decision[i+192]<-"larger"
  
  asymmetry[i]<-subjAsymmetryScoreSmallerSmall[i]
  asymmetry[i+64]<-subjAsymmetryScoreLargerSmall[i]
  asymmetry[i+128]<-subjAsymmetryScoreSmallerLarge[i]
  asymmetry[i+192]<-subjAsymmetryScoreLargerLarge[i]
  
}

x<-data.frame(subject,distance,decision,asymmetry) 

asym.aov<-aov(asymmetry~as.factor(distance)*as.factor(decision)+Error(as.factor(subject)/(as.factor(distance)*as.factor(decision))),data=x)
summary(asym.aov)
print(model.tables(asym.aov,"means"),digits=3)

summary=summarySEwithin(x,measurevar="asymmetry",withinvars=c("distance","decision"),idvar="subject")

summary$distance<-factor(summary$distance,levels=c("small","large"))

ggplot(summary,aes(x=distance,y=asymmetry,shape=decision))+geom_line(aes(group=decision,linetype=decision))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=asymmetry-ci,ymax=asymmetry+ci))+labs(x="Numerical distance",y="Asymmetry score")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))



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

######################################################################
## plot histogram of AUC values

AUCplotData<-data.frame(z.AUC,data$condition)

basePlot<-ggplot(AUCplotData,aes(x=z.AUC,fill=as.factor(data.condition)))+geom_histogram(color="black",binwidth=0.3,alpha=0.6,position="identity")+xlim(-2,5)

colors<-scale_fill_manual(values=c("white","gray"),name="condition",breaks=c(1,2),labels=c("congruent","incongruent"))
labels<-labs(x="AUC z-scores",y="frequency")
publicationScheme<-theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))


basePlot+colors+labels+publicationScheme+theme(legend.position=c(0.75,0.75))

### export 700 x 500





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
