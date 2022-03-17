####################################################
### 인자 분석(factor analysis)과제 
####################################################
### 1. 자료읽기 및 요약 통계량
fifa = read.csv('C:/data/mva/FIFA21_official_data_ex35.csv')
head(fifa)
summary(fifa)

### 2. 초기 인자분석 실행
install.packages("GPArotation")
library(psych)
library(GPArotation)
fifa_factor = principal(fifa, rotate="none")
names(fifa_factor)
fifa_factor$values
plot(fifa_factor$values, type="b", pch=19)