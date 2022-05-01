########################################
# multiple regression model 중회귀 모형 적합하기
# dataset: gym health data
# author: hyeji choi
# purpose : midterm assignment
#########################################

# 자료 파일 읽기(mac)
#setwd('/Users/hj1/Desktop/R/reg')
#install.packages("xlsx")
#library(xlsx)
#excel <- "p162.csv"
#gym <- read.xlsx(excel, 1)

# 자료 파일 읽기(window)
gym = read.csv("c:/data/reg/p162.csv", header=T)
head(gym)

# 기술통계량 및 상관계수 보기
summary(gym[,-1]) # 번호라서 첫번째 열 빼준다 
cor(gym[,-1])

# 산점도 그리기
par(mfrow=c(2,2), pty="s")
plot(gym$X1, gym$Y, xlab="weight", pch=19)
plot(gym$X2, gym$Y, xlab="pulse",pch=19)
plot(gym$X3, gym$Y, xlab="muscle power",pch=19)
plot(gym$X4, gym$Y, xlab="speed",pch=19)

# 회귀모형 적합하기 
gym.lm = lm(Y ~ X1+X2+X3+X4, data=gym)
summary(gym.lm)
library(car)
avPlots(gym.lm)

# 분산분석표
anova(gym.lm)

# 잔차산점도 그리기(독립변수, 잔차)
par(mfrow=c(2,2), pty="s")
plot(gym$X1, gym.lm$residuals, xlab="weight", pch=19)
abline(h=0, lty=2)

plot(gym$X2, gym.lm$residuals, xlab="pulse",pch=19)
abline(h=0, lty=2)

plot(gym$X3, gym.lm$residuals, xlab="muscle power",pch=19)
abline(h=0, lty=2)

plot(gym$X4, gym.lm$residuals, xlab="speed",pch=19)
abline(h=0, lty=2)

# 잔차산점도 그리기(추정값, 잔차)
par(mfrow=c(1,1), pty="s")
plot(gym.lm$fitted.values, gym.lm$residuals, pch=19)
abline(h=0, lty=2)

