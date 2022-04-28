########################################
# multiple regression model 중 회귀 모형 적합하기
# dataset: gym health data
# author: hyeji choi
# purpose : midterm assignment
#########################################

# 자료 파일 읽기
setwd('/Users/hj1/Desktop/R/reg')
install.packages("xlsx")
library(xlsx)
excel <- "p162.csv"
gym <- read.xlsx(excel, 1)
head(gym)

# 기술통계량 및 상관계수 보기
summary(gym[,-1]) # 번호라서 ㄱ첫번째 열 빼준다 
cor(gym[,-1])

# 회귀모형 적합하기 
gym.lm = lm(Y ~ X1+X2+X3+X4, data=gym)
summary(gym.lm)
library(car)
avPlots(gym.lm)

# 분산분석표
anova(gym.lm)

# 잔차산점도 그리기
plot()