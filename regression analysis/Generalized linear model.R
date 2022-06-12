#####################################
### regression analysis
### 7. generalized linear model
### dataset : glider, 
#####################################

### 로지스틱 회귀모형
# 자료읽기
glider = read.csv("c:/data/reg/sugar_glider_binomial.csv")
head(glider, 3)
# 모형적합
logit_m1 = glm(occurr~p_size_km+con_metric, family=binomial(link=logit), data=glider)
# 적합 결과 요약
summary(logit_m1)

### 모형의 유의성 검정
# 유의 확률(p-value)구하기
1-pchisq(68.994-54.661,2)
# anova 함수 사용 - 유의성 검정
logit_m0 = glm(occurr~1, family=binomial(link=logit), data=glider)
anova(logit_m0, logit_m1, test="Chisq")

### 모형의 적합도(goodness-of-fit) 평가
1-pchisq(54.661, 47)

### 모형의 선택 
# anova
logit_m2 = glm(occurr~p_size_km, family=binomial(link=logit), data=glider)
logit_m1 = glm(occurr~p_size_km+con_metric, family=binomial(link=logit), data=glider)
anova(logit_m2, logit_m1, test='Chisq')

# AIC 함수 사용
AIC(logit_m2, logit_m1)

# 변수선택방법 이용
library(MASS)
stepAIC(logit_m1, direction='both')

### 로지스틱 모형의 해석
p_size = seq(20, 230, 1)
hat_eta = predict(logit_m2, list(p_size_km=p_size), type="link")
par(mfrow=c(1,2))
plot(glider$p_size_km, glider$occurr, xlab='구획의 크기(x)', ylab="hat pi(x) \\ occurr", sub='(a)', pch=20)
lines(p_size, exp(hat_eta)/(1+exp(hat_eta)), lwd=1.5, col='red')
glider_g=read.csv('c:/data/reg/sugar_glider_binomial_g.csv')
plot(glider_g$p_size_med, glider_g$cases/glider_g$count, 
xlab='구획의 크기(x)', 
ylim=c(0,1), ylab="hat pi(x) \\ sample prop.", sub='(b)', pch=20, col='blue')
lines(p_size, exp(hat_eta)/(1+exp(hat_eta)), lwd=1.5, col='red')

# 정리된 자료의 로지스틱 회귀모형 적합
y = cbind(glider_g$case, glider_g$count-glider_g$cases)
logit_mg = glm(y~glider_g$p_size_med, family=binomial(link=logit))
summary(logit_mg)

### 승산비(odds ratio)
logit_m2 = glm(occurr ~ p_size_km, family=binomial(link = logit), data=glider)
exp(coef(logit_m2)) # 계수에 대한 지수함수
exp(confint(logit_m2, parm="p_size_km", level=0.95)) # 95% 신뢰구간
# 출현확률 - 크기 x=150km인 구획
x = 150
predict(logit_m2, list(p_size_km=x, type="response"))

### 프로빗 모형
glider = read.csv("c:/data/reg/sugar_glider_binomial.csv")
attach(glider)
# 프로빗 모형 적합
probit_m = glm(occurr~p_size_km, family=binomial(link=probit))
summary(probit_m)
# 원 자료 jitter 형식으로 그리기
plot(p_size_km, occurr, type="n", xlab="구획의 크기(x)", ylab="hat pi(x)\\ occurr")
rug(jitter(p_size_km[occurr==0]))
rug(jitter(p_size_km[occurr==1]), side=3)
# 확률추정곡선 그리기
x = seq(23,226,1)
hat.pi.p = predict(probit_m, list(p_size_km=x), type="response")
lines(x, hat.pi.p, col="blue", lty=1, lwd=1.5)
hat.pi.l = predict(logit_m2, list(p_size_km=x), type="response")
lines(x, hat.pi.l, col="red", lty=2, lwd=1.5)
# 표본비율 그리기
lines(md.size, s.prop, type="p", pch=20, cex=1.5, col="blue")
legend(locator(1), c("프로빗모형", "로지스틱회귀모형"), lty=c(1,2), col=c("blue", "red"), cex=0.7)
predict(probit_m, list(p_size_km=150), type="response")

### 개수형 자료 - 로그선형모형
library(MASS)
data(Traffic)
head(Traffic, 3)
# day와 year를 factor 변수로 바꾸기
day = as.factor(Traffic$day)
year = as.factor(Traffic$year)
log_m = glm(y~Traffic$limit+day+year, family=poisson(link=log))
summary(log_m)
log_m1 = glm(y~limit+day, family=poisson(link=log))
summary(log_m1)
exp(coef(log_m1, parm="limit"))
exp(confint(log_m1, parm="limityes", level=0.95))
par(mfrow=c(2,2))
plot(log_m1)