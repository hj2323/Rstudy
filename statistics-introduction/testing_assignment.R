### 모평균에 대한 가설검정
n=10
s=12.2
bar_x = 32.3
alpha = 0.05  # 유의수준
ttest = (bar_x - 30)/(12.2/sqrt(10)) # 검정통계량
ttest_cr = qt(1-alpha/2, n-1)       # 기각역
ttest_pv = (1-pt(ttest, n-1))*2     # 유의확률
cat("검정통계량값: ", ttest, "기각역: ", ttest_cr, "유의확률: ", ttest_pv)
