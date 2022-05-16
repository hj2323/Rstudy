#################################
# 통계적 비교
#################################

###  두 모평균에 대한 비교(대응표본)
# 데이터 입력
pre <- c(72,80,83,63,66,76,82)
post <- c(78,82,82,68,70,75,88)
exam1 <- data.frame(pre, post)

# 두 모평균의 비교
t.test(exam1$pre, exam1$post, mu=0, alternative="less", paried=T)


### 일원배치법 데이터에 대한 분산분석
x<-c(84,83,82,85,89,86,93,94,96,89,89,87)
A<-c(rep(1,3), rep(2,3), rep(3,3), rep(4,3))
A<-factor(A) # 각 수준이 의미없는 명목적 수준임
aovdat1<-data.frame(x,A)
aovmodel1<-aov(x~A, data=aovdat1) # 분산분석 
summary(aovmodel1) # 분석 결과 출력 


### 이원배치법 데이터에 대한 분산분석



### 범주형 데이터에 대한 독립성 검정
dept<- c(rep("Stat", 50), rep("DS", 25))
regi<- c(rep("Y", 20), rep("N", 30), rep("Y", 13), rep("N", 12))
deptregi<- data.frame(dept, regi)
# xtabs: 범주형 변수를 가지고 분할표를 작성하는 기능을 수행 
rtable<- xtabs(~dept+regi, data=deptregi) 
rtable
ctest<- chisq.test(rtable, correct=F) # 분할표를 카이제곱 검정 실행 
ctest

### 적합도 검정


