##############################
# model development
# - 다항회귀모형
# - 가변수 회귀모형
##############################

### traffic crime

tcrime <- read.table('/Users/hj1/Desktop/R/reg/tcrime.txt', header=T)
head(tcrime, 3)
attach(tcrime)
plot(motor, tcratio, pch=19)
tcrime.lm = lm(tcratio ~ motor + I(motor^2), data=tcrime)
summary(tcrime.lm)


### tokyo international marathon
marathon = read.table('/Users/hj1/Desktop/R/reg/tcrime.txt', header=T)
head(marathon, 2)
plot(marathon$sect, marathon$m1990, pch=19)
marathon.lm=lm(m1990~sect+I(sect^2)+I(sect^3), data=marathon)
summary(marathon.lm)

### dummy variable regression model
soap = read.table('/Users/hj1/Desktop/R/reg/soap.txt', header=T)
soap[c(1,15,16,27),]
soap$D = factor(soap$D, levels=c(0,1), label=c("Line0", "Line1"))
plot(soap$X, soap$Y, type="n")
points(soap$X[soap$D=="Line1"], soap$Y[soap$D=="Line1"], pch=17, col="BLUE")
points(soap$X[soap$D=="Line0"], soap$Y[soap$D=="Line0"], pch=19, col="RED")
legend("bottomright", legend=levels(soap$D), pch=c(19,17), col=c("RED", "BLUE"))
# 회귀모형 적합하기 
soap.lm = lm(Y ~ X+D, data=soap)
summary(soap.lm)
abline(27.28179, 1.23074, lty=2, col="RED")
abline(27.28179+53.1292, 1.23074, lty=2, col="BLUE")

# 교호작용을 고려한 경우(두 생산라인의 기울기가 다른 경우)
soup2.lm = lm(Y ~ X+D+X:D, data=soup)
summary(soup2.lm)

### Mulitlevel factor
library(faraway)
data(fruitfly)
fruitfly[c(1,26,51,75,101),]
# plot of data
attach(fruitfly)
plot(thorax, longevity, type="n")
points(thorax[activity=="many"], longevity[activity=="many"], pch="m", col=1)
points(thorax[activity=="isolated"], longevity[activity=="isolated"], pch="i", col=2)
points(thorax[activity=="one"], longevity[activity=="one"], pch="o", col=3)
points(thorax[activity=="low"], longevity[activity=="low"], pch="w", col=4)
points(thorax[activity=="high"], longevity[activity=="high"], pch="h", col=5)
# Fit general linear model considering interaction effects
g = lm(longevity ~ thorax * activity, data=fruitfly)
summary(g)
# ANOVA table
anova(g)
# Refit model without interaction term
gb = lm(longevity~thorax+activity, data=fruitfly)
summary(gb)