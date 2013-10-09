load("/Users/gregorymatthews/Dropbox/SPORTS/FOOTBALL/Kick_Database.RData")
kick.dat[1:10,]
#Some simple stuff
summary(kick.dat$yards)
table(kick.dat$good)
table(kick.dat$year)
table(kick.dat$good,kick.dat$year)
#Averages by year
table(kick.dat$good,kick.dat$year)
#Averages by year
tapply(kick.dat$good,kick.dat$year,mean)
#Average distance by year
tapply(kick.dat$yards,kick.dat$year,mean)
#Boxplots of distance by year
boxplot(kick.dat$yards~kick.dat$year)
#Just fixed effects
summary(glm(good~yards,data=kick.dat,family="binomial"))
summary(glm(good~I(yards^0.5),data=kick.dat,family="binomial"))#Lower AIC
#Add in year
a<-glm(good~I(yards^0.5),data=kick.dat,family="binomial")
b<-glm(good~I(yards^0.5)+year,data=kick.dat,family="binomial")
anova(a,b)
1-pchisq(18.766,8)#0.01616243

#Adding in year as numeric
#Kickers getting better?
a<-glm(good~I(yards^0.5)+name,data=kick.dat,family="binomial")
b<-glm(good~I(yards^0.5)+name+as.numeric(year),data=kick.dat,family="binomial")
anova(a,b)

library(lme4)  
#Use random effects for kicker
a<-glmer(good~I(yards^0.5)+(1|name),data=kick.dat,family="binomial")
summary(a)


#Do we need a random yard variable? Maybe
a<-glmer(good~I(yards^0.5)+(1|name),data=kick.dat,family="binomial")  
b<-glmer(good~I(yards^0.5)+(1+yards|name),data=kick.dat,family="binomial")  
anova(a,b)

#Are kickers different from year to year? 
a<-glmer(good~I(yards^0.5)+year+(1+yards|name),data=kick.dat,family="binomial")  
b<-glmer(good~I(yards^0.5)+year+(1+yards|name)+(1|name:year),data=kick.dat,family="binomial")
anova(a,b)

#Final Model without controlling for year
kickingMer<-glmer(good~I(yards^0.5)+(1|name)+(1|name:year),data=kick.dat,family="binomial")  
b<-kickingMer
pdist<-function(dist){
  logitp<-fixef(b)%*%c(1,sqrt(dist))+rnorm(1000,0,sqrt(VarCorr(b)[1][[1]][1]))
  p<-exp(logitp)/(1+exp(logitp))
  p
}
pdistList<-list()
for (i in 17:60){
  pdistList[[i]]<-cbind(pdist(i),i)
}
pdist<-do.call(rbind,pdistList)
pdist<-as.data.frame(pdist)
names(pdist)<-c("y","x")
aaa<-tapply(pdist$y,pdist$x,quantile,c(0.05,0.95))
bbb<-do.call(rbind,aaa)
#png("KickerAndYear.png")
plot(c(17:60),bbb[,1],type="l",xlab="Distance of Kick",ylab="Probabilty of making the kick",col="blue",main="Kicker and year variability")
points(c(17:60),bbb[,2],type="l",col="red")
abline(h=seq(0,1,0.1),col="gray50",lty=3)
legend(20,0.6,legend=c("95th percentile","5th percentile"),col=c("red","blue"),lwd=3)
#dev.off()

kickingMerYear<-glmer(good~I(yards^0.5)+year+(1|name)+(1|name:year),data=kick.dat,family="binomial")  
b<-kickingMerYear
#Fixed Effects
#Only fixed effects
logitp<-fixef(b)%*%c(1,sqrt(40),1,0,0,0,0,0,0,0)
p<-exp(logitp)/(1+exp(logitp))

#Random Effects: Player ability held constant
#40 Yards FG in 2010
logitp<-fixef(b)%*%c(1,sqrt(40),1,0,0,0,0,0,0,0)
p<-exp(logitp)/(1+exp(logitp))

pdist<-function(dist){
logitp<-fixef(b)%*%c(1,sqrt(dist),1,0,0,0,0,0,0,0)+rnorm(1000,0,sqrt(VarCorr(b)[1][[1]][1]))
p<-exp(logitp)/(1+exp(logitp))
p
}
pdistList<-list()
for (i in 17:60){
  pdistList[[i]]<-cbind(pdist(i),i)
}
pdist<-do.call(rbind,pdistList)
pdist<-as.data.frame(pdist)
names(pdist)<-c("y","x")
aaa<-tapply(pdist$y,pdist$x,quantile,c(0.05,0.95))
bbb<-do.call(rbind,aaa)
#png("KickerAndYearFEyear.png")
plot(c(17:60),bbb[,1],type="l",xlab="Distance of Kick",ylab="Probabilty of making the kick",col="blue",main="Kicker and year variability")
points(c(17:60),bbb[,2],type="l",col="red")
abline(h=seq(0,1,0.1),col="gray50",lty=3)
legend(20,0.6,legend=c("95th percentile","5th percentile"),col=c("red","blue"),lwd=3)
#dev.off()




####################################
##By year.  Are kickers getting better?
####################################
kickingMer<-glmer(good~I(yards^0.5)+year+(1|name)+(1|name:year),data=kick.dat,family="binomial")  
b<-kickingMer
pdist<-function(dist,year){
  yyy<-c(2010:2003)
  logitp<-fixef(b)%*%c(1,sqrt(dist),yyy==year)
  p<-exp(logitp)/(1+exp(logitp))
  p
}
pdistList<-list()
for (year in 2003:2010){
  pdistList[[year]]<-list()
for (i in 30:60){
  pdistList[[year]][[i]]<-cbind(pdist(i,year),i)
}
}

pdist<-do.call(rbind,pdistList[[2003]])
pdist<-as.data.frame(pdist)
names(pdist)<-c("y","x")
aaa<-tapply(pdist$y,pdist$x,quantile,c(0.05,0.95))
bbb<-do.call(rbind,aaa)
#png("KickerAndYearFEyear.png")
plot(c(30:60),bbb[,1],type="l",xlab="Distance of Kick",ylab="Probabilty of making the kick",col="red",main="Probabilty of average kicker making a field goal",lwd=5,sub="Kicker ability held constant")

cols<-c("red","red","red","blue","blue","blue","blue")
for (year in 2004:2010){
pdist<-do.call(rbind,pdistList[[year]])
pdist<-as.data.frame(pdist)
names(pdist)<-c("y","x")
aaa<-tapply(pdist$y,pdist$x,quantile,c(0.05,0.95))
bbb<-do.call(rbind,aaa)
points(c(30:60),bbb[,1],type="l",col=cols[year-2003],lwd=5)
}
abline(h=seq(0,1,0.1),col="gray50",lty=3)
legend(30,0.6,legend=c("2003-2006","2007-2010"),col=c("red","blue"),lwd=3)
#dev.off()




