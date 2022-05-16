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


