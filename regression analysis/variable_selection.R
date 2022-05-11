##############################
# variable selection
##############################

hospital <- read.table('/Users/hj1/Desktop/R/reg/hospital.txt', header=T)
head(hospital, 3)

# 회귀적합
hospital.lm <- lm(Y~., data=hospital)
summary(hospital.lm)

# 분산팽창인자 계산
install.packages(fmsb)
library(fmsb)
VIF(lm(X1~X2+X3+X4+X5, data=hospital))
VIF(lm(X2~X1+X3+X4+X5, data=hospital))
VIF(lm(X3~X1+X2+X4+X5, data=hospital))
VIF(lm(X4~X1+X2+X3+X5, data=hospital))
VIF(lm(X5~X1+X2+X3+X4, data=hospital))

cor(hospital[,-6])


# 설명변수 X1을 제거하는 경우
summary(lm(Y~X2+X3+X4+X5, data=hospital))


### 변수선택의 방법

# Hald 자료 읽기
hald = read.table('/Users/hj1/Desktop/R/reg/hospital.txt', header=T)
head(hald,3)

# 모든 가능한 회귀
install.packages("leaps")
library(leaps)
all.lm = regsubsets(Y ~., data=hald)
(rs=summary(all.lm))
names(rs)
rs$rsq
rs$adjr2
rs$cp

# 앞으로부터의 선택
start.lm = lm(Y~1, data=hald)
full.lm = lm(Y~., data=hald)
step(start.lm, scope=list(lower=start.lm, upper=full.lm, direction="forward"))

# 뒤로부터 제거
full.lm = lm(Y~., data=hald)
step(full.lm, data=hald, direction="backward")

# 단계별 회귀(stepwise selection)
start.lm = lm(Y~1, data=hald)
full.lm = lm(Y~., data=hald)
step(start.lm, scope=list(upper=full.lm), data=hald, direction="both")
