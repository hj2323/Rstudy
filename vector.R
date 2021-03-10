 #벡터의 생성
x<-c(1,2,3)
y<-c(4,5,6)
x+y
x*y
x^2
log(x^2)
x**2

#백터의 연산
x<-c(7,2,4,9,8,6,1,5,10,3)
sort(x)
sum(x)
mean(x)#표본평균
sum(x)/length(x)#표본평균
var(x)#표본분산
min(x)#최솟값
which.min(x)#최솟값이 있는 위치
summary(x)#기초통계량

#(1,3,5,NA,9,NA,11)로 이루어진 벡터 y
#평균을 구하기 위해 mean()실행
y<-c(1,3,5,NA,9,NA,11)
mean(y)
mean(y, na.rm=TRUE)
is.na(y)
which(is.na(y))
sum(is.na(y))
#무한대
1/0
#이상치
log(-1)
#도움말
? mean
 example(log2)