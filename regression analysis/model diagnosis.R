#####################################
### regression analysis
### 6. diagnosis of model
### dataset : goose, tree
#####################################

### 오차의 등분산
goose = read.table("c:/data/reg/goose.txt", header=T)
head(goose, 3)

goose.lm = lm(photo ~ obsA, data=goose)
plot(goose.lm$fitted, goose.lm$resid, pch=19)
# 스코어 검정
library(car)
ncvTest(goose.lm)

### 회귀모형의 선형성
tree = read.table("c:/data/reg/tree.txt", header=T)
head(tree, 3)
tree.lm = lm(V~D+H, data=tree)
plot(tree$D, tree.lm$resid, pch=19)
plot(tree$H, tree.lm$resid, pch=19)


### 오차의 정규성 
goose.lm = lm(photo~obsA, data=goose)
qqPlot(goose.lm)
# 정규성 검정
package.install(mvnormtest)
library(mvnormtest)
goose.rstudent = rstudent(goose.lm)
shapiro.test(goose.rstudent)


### 반응변수의 변환: Box-Cox 변환
energy = read.table("c:/data/reg/energy.txt", header=T)
head(energy, 3)
energy.lm = lm(Y~X, data=energy)
plot(energy.lm$fitted, energy.lm$resid, pch=19) # 잔차산점도 

install.packages("MASS")
library(MASS)
boxcox(Y~X, data=energy, lambda=seq(-2,2, 1/2), plotit=TRUE)