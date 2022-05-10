########################################
# single regression model 단순 회귀 모형 적합하기
# dataset: achievement
# author: hyeji choi
# purpose : midterm assignment(intro Statistics)
#########################################

# 자료를 읽어 산점도 그리기
achiv = read.table("C:\\data\\R_IntroStat\\achievement.txt", header=T)
head(achiv, 3)
attach(achiv)
plot(before, after, pch=19, xlab='방과후 학습 전 성취도',
     ylab='방과후 학습 후 성취도')

# 회귀모형 적합하여 회귀 직선그리기
achiv.lm = lm(after ~ before, data=achiv)
abline(achiv.lm)

# 서머리에서 상관계수 확인하기
summary(achiv.lm)
