getwd()
setwd('/Users/hj1/Desktop/R/datamining')
#############productivity pre-processing###############
library(MASS)
prod<-read.csv("productivityORG.csv")
head(prod)
prod$date<-as.Date(prod$date, format='%m/%d/%Y')
prod$quarter<-factor(prod$quarter)#factor:가변수
prod$department<-factor(prod$department)
prod$day<-factor(prod$day)
prod$team<-factor(prod$team)
summary(prod)
attach(prod)
####box plot#####
par(mfrow=c(3,4))
boxplot(targeted_productivity, col="cyan3", xlab="Target Productivity")
boxplot(smv, col="cyan3", xlab="Standard Minute Value")
boxplot(wip, col="cyan3", xlab="Work in Progress")
boxplot(over_time, col="cyan3", xlab="Overtime")
boxplot(incentive, col="cyan3", xlab="Incentive")
boxplot(idle_time, col="cyan3", xlab="Idle Time")
boxplot(idle_men, col="cyan3", xlab="Idle Men")
boxplot(no_of_style_change, col="cyan3", xlab="Number of Chnages in Style")
boxplot(no_of_workers, col="cyan3", xlab="Number of Workers")
boxplot(actual_productivity, col="cyan3", xlab="Productivity")
####이상치 제거######
###이상치 기준을 계산
dout <- rep(0,15)
dout2 <- rep(0,15)
for(i in 6:15){
  t3<-quantile(prod[,i], 0.75, na.rm = TRUE)#quantile:분위수 측정
  t1<-quantile(prod[,i], 0.25, na.rm = TRUE)
  tq<-IQR(prod[,i], 0.75)
  dout[i] <- t3+1.5*tq
  dout2[i] <- t1-1.5*tq
}

###matrix는 계산된 것을 받아서 전체 데이터 observation에 적용해서 이상치를 
###제거한 데이터를 받게 된다
outindex<-matrix(0,1197,15)
for(i in 1:1197){
  for(j in 6:15){
    if(prod[i,j]>dout[j]||prod[i,j]<dout2[j]) outindex[i,j]<-1
  }
}

prod2 <- prod[apply(outindex, 1, sum)==0,]
prodnew <- prod2[,-c(1,11:13)]
head(prodnew)
write.csv(prodnew,"/Users/hj1/Desktop/R/datamining/productivityREG.csv", quote=F, row.names=F)

detach(prod)

############## dummy #######################
install.packages("dummy")

library(dummy)
prod =  read.csv("productivityREG.csv")
summary(prod)
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)
summary(prod)

# Generate dummy variables
dvar = c(1:4)
prod2 = dummy(x=prod[,dvar])
prod2 = prod2[,-c(5, 7, 13, 25)]
prod = cbind(prod[,-dvar], prod2)
for(i in 1: ncol(prod)) if(!is.numeric(prod[,i])) prod[,i] = as.numeric(prod[,i])
