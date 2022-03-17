####################################################
### 인자 분석(factor analysis)
### dataset : Positive Health Inventory(PHI)
### 주성분 인자법(principal factor method)
###실제로 인자분석을 이행할때는 그 분야에 대해 잘 알고 있어야 한다. 변수에대해서도 잘알고 분야에 대한 지식이 있어야 결과를 분석할 수 있다. 
####################################################
### 1. 자료 읽기 및 요약 통계량
med_data = read.csv("c:/data/mva/medFactor.csv")
head(med_data)
boxplot(med_data)
summary(med_data)

### 2. 초기 인자분석 실행하기

install.packages("GPArotation")
library(psych)
library(GPArotation)#인자 회전을 위한 라이브러리

# 인자분석 적정성 검정 (추가)
cortest.bartlett(med_data) # 유의 확률 p값이 굉장히 작다는 말은 단위행렬과 다르다-> 인자분석이 가능하다
KMO(med_data)

med_factor = principal(med_data, rotate="none")
names(med_factor)
med_factor$values
plot(med_factor$values, type="b", pch=19) 

### 3. varimax 인자회전
med_varimax = principal(med_data, nfactors=3, rotate="varimax",
                        scores=T, method="regression")
med_varimax

### 4. varimax 인자회전 regression method로 인자점수 추청
head(med_varimax$scores)

### 5. oblimin 인자회전
med_oblimin = principal(med_data, nfactors=3, rotate="oblimin",
                        scores=T, method="regression")
med_oblimin

### 6. oblimin 인자회전 regression method로 인자점수 추청
head(med_oblimin$scores)

### 7. 행렬도
biplot(med_varimax)