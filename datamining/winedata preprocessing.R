#############wine quality pre-processing###############
setwd('/Users/hj1/Desktop/R/datamining')
library(MASS)
wine<-read.csv("winequalityORG.csv")
head(wine)
attach(wine)
summary(wine)
####box plot#####
par(mfrow=c(3,4))
boxplot(fixed, col="cyan3", xlab="Fixed Acidity")
boxplot(volatile, col="cyan3", xlab="Volatile Acidity")
boxplot(citric, col="cyan3", xlab="Citric Acid")
boxplot(residsugar, col="cyan3", xlab="Residual Sugar")
boxplot(chlorides, col="cyan3", xlab="Chlorides")
boxplot(freeSD, col="cyan3", xlab="Free Sulfur Dioxide")
boxplot(totalSD, col="cyan3", xlab="Total Sulfur Dioxide")
boxplot(density, col="cyan3", xlab="Density")
boxplot(pH, col="cyan3", xlab="pH")
boxplot(sulphates, col="cyan3", xlab="Sulphates")
boxplot(alcohol, col="cyan3", xlab="Alcohol")
####이상치 제거######
###이상치 기준을 계산
dout <- rep(0,12)
dout2 <- rep(0,12)
for (i in 1:11){
  t3 <- quantile(wine[,i], 0.75, na.rm = TRUE)
  t1 <- quantile(wine[,i], 0.25, na.rm = TRUE)
  tq <- IQR(wine[,i], 0.75)
  dout[i] <- t3 + 1.5*tq
  dout2[i] <- t1 - 1.5*tq
}
print(wine[133,2]>dout[2]||wine[133,2]<dout2[2])

outindex <- matrix(0,1599, 12)
for (i in 1:1599){
  for (j in 1:11){
    if (wine[i,j] > dout[j] || wine[i,j] < dout2[j]) {outindex[i,j] <- 1}
  }
}
wine2 <- wine[apply(outindex,1,sum)==0,]
table(wine2$quality)
write.csv(wine2,"winequalityCLASS.csv",quote=F,row.names=F)
detach(wine)
