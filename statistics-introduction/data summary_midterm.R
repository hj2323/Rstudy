################################
# 데이터 요약
################################

### 히스토그램 그리기
numberofbook <- c(8,1,10,15,15,10,5,19,20,9,10)
hist(numberofbook, main="")


### 상자그림 그리기
boxplot(numberofbook, ylab="학생 11명의 1년 동안 읽은 책 수")



### 다섯수치요약 산출
fivenum(numberofbook)

