##########single regression model###############
################################################
###scatter plot########
market = read.table('/Users/hj1/Desktop/R/reg/market-1.txt', header=T)
head(market)
plot(market$X, market$Y, xlab="광고료", ylab="총판매액", pch=19)
title("광고료와 판매액의 산점도")
#그래프 마진 재설정하는 코드
#par('mar')
#par(mar=c(5.1,4.1,4.1,2.1))

#회귀직선 그리기
market.lm=lm(Y~X, data=market)
summary(market.lm)
abline(market.lm)
identify(market$X, market$Y)

#잔차 residual
#점 x bar, y bar는 적합된 회귀선상에 있음
xbar=mean(market$X)
ybar=mean(market$Y)
xbar
ybar
points(xbar, ybar, pch=17, cex=2.0, col="RED")
text(xbar, ybar, "(8.85, 19.36)")
fx <- "Y-hat = 0.328+ 2.14*X"
text(locator(1), fx)

#분산분석표
anova(market.lm)
#분산분석 결과 해석 : p value = 3.554e-09(3.554*10^-9)로 
#매우 작은 값이므로 H0:B1=0을 기각

#참고 1:유의수준 0.05애서 F-기각역
qf(0.95, 1, 13)
#F-value = 192.9 > F(1,13,0.05) 이므로 귀무가설을 기각함.

#참고2: p-값 구하기
1-pf(192.9, 1, 13)

#결정계수:Multiple R-squared
summary(market.lm)

#R결과에서 B1, B0 신뢰구간 구하기 
summary(market.lm)
#B1(기울기-> summary에서 X)의 95% 신뢰구간
q.val = qt(0.975, 8)
2.6087-q.val*0.3878
2.6087+q.val*0.3878
#B0(절편->summary에서 intercept)의 95% 신뢰구간 
q.val = qt(0.975, 8)
-2.2696-q.val*3.2123
-2.2696+q.val*3.2123

###X의 주어진 값에서 신뢰대 그리기
pred.frame = data.frame(X=seq(3.5,14.5,0.2))
pc = predict(market.lm, int="c", newdata=pred.frame) #기댓값 신뢰구간
pp = predict(market.lm, int="p", newdata=pred.frame) #새로운 값 신뢰구간
head(pc, 3)
head(pp, 3)
pred.X = pred.frame$X
pred.X
plot(market$X, market$Y, ylim=range(market$Y, pp))
matlines(pred.X, pc, lty=c(1,2,2), col="BLUE")
matlines(pred.X, pp, lty=c(1,3,3), col="RED")

#R결과 : B1 검정
market.lm = lm(Y~X, data=market)
summary(market.lm)
#t값은 Estimate(X)/ Std. Error(X)

#가중회귀
x = c(1,2,3,4,5)
y = c(2,3,5,8,7)
w = 1/x
w.lm = lm(y~x, weight=w)
summary(w.lm)

######################
###분석사례##########
######################

#자료를 읽어 산점도 그리기
super = read.table('/Users/hj1/Desktop/R/reg/supermarket.txt', header=T)
head(super, 3)
attach(super)#변수를 직접 사용하려면 attach 함수 사용
plot(price, time, pch=19)

#회귀모형 적합하기
super.lm = lm(time ~ price, data = super)
summary(super.lm)

#분산분석표 구하기
anova(super.lm)

#잔차 및 추정값 보기
names(super.lm)
cbind(super, super.lm$resid, super.lm$fitted)

#잔차 그림 그리기
plot(super$price, super.lm$resid, pch=19)
abline(h=0, lty=2)

#추정값의 신뢰대 그리기
p.x = data.frame(price = c(1,45))
pc = predict(super.lm, int = "c", newdata = p.x)
pred.x = p.x$price
plot(super$price, super$time, ylim = range(super$time, pc), pch=19)
matlines(pred.x, pc, lty = c(1,2,2), col="BLUE")
