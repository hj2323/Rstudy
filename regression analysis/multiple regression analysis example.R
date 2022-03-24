####################################################
### 중회귀모형 분석사례(multiple regression analysis)
### dataset : chemical
### X1:speed, X2:temp, Y:loss
####################################################
### 1. 자료파일 읽기 
install.packages("xlsx")
library(xlsx)
chemical = read.xlsx('/Users/hj1/Desktop/R/reg/chemical.xlsx', 1)
head(chemical)

### 2. 기술통계량 및 상관계수 보기
summary(chemical[,-1])
cor(chemical[,-1])

# 산점도 그리기
par(mfrow=c(1,2), pty="s")
plot(chemical$speed, chemical$loss, pch=19)
plot(chemical$temp, chemical$loss, pch=19)

### 3. 회귀모형 적합하기 
chemical.lm = lm(loss ~ speed+temp, data = chemical)
summary(chemical.lm)
library(car)
avPlots(chemical.lm)

### 4. 분산분석표
anova(chemical.lm)

### 5. 잔차산점도 그리기(독립변수, 잔차)
par(mfrow=c(1,2), pty="s")
plot(chemical$speed, chemical.lm$resid, pch=19)
abline(h=0, lty=2)
identify(chemical$speed, chemical.lm$resid)

plot(chemical$temp, chemical.lm$resid, pch=19)
abline(h=0, lty=2)
identify(chemical$temp, chemical.lm$resid)

plot(chemical.lm$fitted, chemical.lm$resid)
abline(h=0, lty=2)
identify(chemical.lm$fitted, chemical.lm$resid)
