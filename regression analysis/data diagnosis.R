#####################################
### regression analysis
### 5. diagnosis of data
### dataset : forbes, Prater
#####################################

# 자료 읽기
forbes = read.table("c:/data/reg/forbes.txt", header=T)
forbes$Lpress = 100*log10(forbes$press)
head(forbes, 3)

plot(forbes$temp, forbes$Lpress, pch=19)
identify(forbes$temp, forbes$Lpress)

# 잔차 분석
forbes.res = ls.diag(forbes.lm)
names(forbes.res)
resid.result = cbind(forbes.res$std.res, forbes.res$stud.res, forbes.res$hat)
colnames(resid.result)=c("standardized resid", "student resid", "Hat")
resid.result = round(resid.result, 3)
print(resid.result)

# 스튜던트화 잔차
rstudent(forbes.lm)

# Bonferroni 유의수준 0.01에서 기각치
qt(0.01/(2*17), 14)

# Bonferroni p-value for obs.12
2*17*(1-pt(12.374,14))

# 특이값 검정
library(car) # car(companion to Application Regression)
outlierTest(forbes.lm)

# 영향력이 큰 관측값(influential observation)
prater = read.table("c:/data/reg/prater.txt", header=T)
head(prater, 2)

prater.lm=lm(Y~X1+X2+X3+X4, data=prater)
summary(prater.lm)

# 단계별 선택방법을 이용하여 모형 적합
start.lm=lm(Y~1, data=prater)
full.lm=lm(Y~X1+X2+X3+X4, data=prater)
step(start.lm, scope=list(upper=full.lm), data=prater, direction="both")


# Cook의 통계량 D
soil = read.table("c:/data/reg/soil.txt", header=T)
head(soil, 3)
soil.lm = lm(SL~SG+LOBS+PGC, data=soil)
summary(soil.lm)
# 잔차분석 및 cook 통계량
soil.diag = ls.diag(soil.lm)
names(soil.diag)
diag.st = cbind(soil.diag$hat, soil.diag$std.res, soil.diag$stud.res, soil.diag$cooks)
colnames(diag.st)=c("Hii", "ri", "ti", "Di")
round(diag.st, 3)

Di = cooks.distance(soil.lm)
round(Di, 3)
library(car)
outlierTest(soil.lm)