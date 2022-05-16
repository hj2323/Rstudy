###  두 모평균에 대한 비교(대응표본) - 운동 데이터
# 데이터 입력
# 운동요법 전 체중
pre <- c(80,56,49,82,70)
# 운동요법 후 체중
post <- c(76,55,52,79,72)
workout <- data.frame(pre, post)

d<- post-pre
d

# 평균 구하기
md = mean(d)
# 표준편차 구하기 
sd = sd(d)

# 검정통계량과 기각역의 임계값 구하기
ttest = (md)/(sd/sqrt(5))
ttest
ttest_cr = qt(1-0.05/2, 5-1)
ttest_cr

# 두 모평균의 비교
t.test(workout$pre, workout$post, mu=0, alternative="greater", paried=T)


### 일원배치법 데이터에 대한 분산분석
# dataset: 월급 데이터
x<-c(269,196,254,226,215,228,251,217,260,240,320,
     281,336,303,294,354,315,259,283,268,357,325,
     288,295,272,245,275,246,341)
A<-c(rep(1,10), rep(2,8), rep(3,11))
A<-factor(A) # 각 수준이 의미없는 명목적 수준임
aovdat1<-data.frame(x,A)
aovmodel1<-aov(x~A, data=aovdat1) # 분산분석 
summary(aovmodel1) # 분석 결과 출력 


### 범주형 데이터에 대한 독립성 검정
vacc<- c(rep("A", 30), rep("B", 30))
sideeffect<- c(rep("N", 12), rep("S", 18), rep("N", 15), rep("S", 15))
vaccside<- data.frame(vacc, sideeffect)
# xtabs: 범주형 변수를 가지고 분할표를 작성하는 기능을 수행 
rtable<- xtabs(~vacc+sideeffect, data=vaccside) 
rtable
ctest<- chisq.test(rtable, correct=F) # 분할표를 카이제곱 검정 실행 
ctest
#임계값 계산
vtest_cr = qchisq(1-0.05, 2-1)
vtest_cr