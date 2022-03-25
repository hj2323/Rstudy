################################################
##########multiple regression model###############
################################################

#행렬연산
#mac
market2 = read.table('/Users/hj1/Desktop/R/reg/market-2.txt', header=T)
#window
market2 = read.table('C:/data/reg/market-2.txt', header=T)
head(market2, 3)
X = market2[, c(2:3)] #X1, X2열에 대한 값만 담아줌 ㄱ
X = cbind(1,X) # 첫번쨰행을 1로 해서  덧붙여라
Y = market2[,4] # Y열에 대한 값만 담아줌
X = as.matrix(X)
Y = as.matrix(Y)
XTX = t(X) %*% X
XTX
XTXI = solve(XTX)
XTY = t(X) %*% Y
beta = XTXI %*% XTY
beta = round(beta, 3)
beta


#######################
###회귀방정식의 신뢰성
########################

###1gtf4855574. 분산분석표에 의한 F-검정
#window
#market2 = read.table('C:/data/reg/market-2.txt', header=T)
market2 = read.table('/Users/hj1/Desktop/R/reg/market-2.txt', header=T)
head(market2, 2)
market2.lm = lm(Y~X1+X2, data=market2)
summary(market2.lm)
anova(market2.lm)
###2. 결정계수
#결정계수(중상관계수)Multiple R-squared = 상관계수^2
names(market2.lm)
yhat=market2.lm$fitted
cor(market2$Y, yhat)
cor(market2$Y, yhat)^2
#수정결정계수Adjusted R-squared
#잔차평균제곱근Residual standard error
sqrt(10.42/12)

#######################
###표준화 회귀모형
########################
install.packages("lm.beta")
library(lm.beta)
market2.lm = lm(Y~X1+X2, data=market2)
market2.beta = lm.beta(market2.lm) # 표준화된 회귀모형 
print(market2.beta)
summary(market2.beta)


#######################
### 추정과 검정
########################
# 1 :95% (99% 신뢰구간 추가)
pred.x = data.frame(X1=10, X2=10)
pc = predict(market2.lm, int="c", newdata=pred.x)
pc
pc99 = predict(market2.lm, int="c", level=0.99, newdata=pred.x)
pc99
summary(market2.lm)

#######################
### 변수 추가 
######################## 
# 추가 제곱합
health = read.table('/Users/hj1/Desktop/R/reg/health.txt', header=T)
head(health,3)
h1.lm = lm(Y~X1, data=health)
h2.lm = lm(Y~X1+X4, data=health)
anova(h1.lm, h2.lm)
# X1 모형에서 X4가 추가된 경우의 추가제곱합 : sum of sq

# 추가 변수 그림
install.packages("car")
library(car)
h4.lm = lm(Y~X1+X2+X3+X4, data=health)
avPlots(h4.lm)
