####################################################
### 인자 분석(factor analysis)
### dataset : state.x77(미국 50개 주의 정보)
### method : 최우추정법(Maximum-linkelihood method)
###실제로 인자분석을 이행할때는 그 분야에 대해 잘 알고 있어야 한다. 변수에대해서도 잘알고 분야에 대한 지식이 있어야 결과를 분석할 수 있다. 
####################################################
### 1. 자료 읽기 및 요약 통계량
state = state.x77
summary(state)
head(state)
boxplot(state)
hist(state)


### 2. 고윳값 구하기
library(stats)
state_fact0 = factanal(state, factors=4)
sosq = function(v) { sum(v^2) }
loadings = as.matrix(state_fact0$loadings)
eigen_value = apply(loadings, 2, sosq)
eigen_value
#eigen_value = round(apply(state_fact0$loadings^2, 2, sum),3)

### 3. 인자분석 실행 
library(stats)
state_fact = factanal(state, factors=3, rotation="none")
state_fact1 = factanal(state, factors=3, rotation="varimax")
state_fact2 = factanal(state, factors=3, rotation="promax")
names(state_fact)


### 4. varimax 회전결과(population 제외 전)
state_fact1 = factanal(state, factors=3, rotation="varimax")
state_fact1

### 5. varimax 회전결과(population 제외 후)
state_fact1_1 = factanal(state[,-1], factors=3, rotation="varimax",
                         scores="Bartlett")
state_fact1_1

### 6. varimax 인자회전 Bartlett method로 인자점수 추청
head(state_fact1_1$scores)

### 7. 인자패턴
# plot (factor1, factor2)
namev = colnames(state)
fa = state_fact1_1
plot(fa$loadings[,1], fa$loadings[,2], xlab="factor1", ylab="factor2", pch=19)
text(x=fa$loadings[,1], y=fa$loadings[,2], labels=namev, adj=-0.1, cex=0.8)
abline(h=0, v=0, lty=2)
# plot (factor1, factor3)
plot(fa$loadings[,1], fa$loadings[,3], xlab="factor1", ylab="factor3", pch=19)
text(x=fa$loadings[,1], y=fa$loadings[,3], labels=namev, adj=-0.1, cex=0.8)
abline(h=0, v=0, lty=2)