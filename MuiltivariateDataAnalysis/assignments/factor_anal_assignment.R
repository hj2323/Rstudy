####################################################
### 인자 분석(factor analysis)과제 
####################################################
### 1. 자료읽기 및 요약 통계량
fifa = read.csv('C:/data/mva/FIFA21_official_data_ex35.csv')
head(fifa)
summary(fifa)
boxplot(fifa)
### 2. 초기 인자분석 실행
install.packages("GPArotation")
library(psych)
library(GPArotation)#인자 회전을 위한 라이브러리

# 인자분석 적정성 검정 (추가)
cortest.bartlett(fifa) # 유의 확률 p값이 굉장히 작다는 말은 단위행렬과 다르다-> 인자분석이 가능하다
KMO(fifa) #0.69이므로 유의성 검정 통과

fifa_factor = principal(fifa, nfactors=2, rotate="none")
fifa_factor
names(fifa_factor)
fifa_factor$values
plot(fifa_factor$values, type="b", pch=19)


### 3. varimax 인자회전
fifa_varimax = principal(fifa, nfactors=3, rotate="varimax",
                        scores=T, method="regression")
fifa_varimax

### 4. varimax 인자회전 regression method로 인자점수 추청
head(fifa_varimax$scores)

### 5. 행렬도
biplot(fifa_varimax)

