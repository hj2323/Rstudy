####################################
# 2강 다변량 시각화
####################################
# 단변량 그래프
# 이변량 그래프
# 다차원 그래프

####################################
###단변량 그래프
survey = read.csv('/Users/hj1/Desktop/mva/survey.csv')
survey = read.csv("C:\data\mva\survey.csv")
# R 막대그림 및 원그림
edu_tb = table(survey$edu) # table 빈도표를 구하는 명령
edu_tb
rownames(edu_tb) = c("무학", "초졸", "중졸", "고졸", "대졸")
barplot(edu_tb)
dev.new() # new graphic device
pie(edu_tb.main="교육정도 원그림")
dev.off()
# R 겹친막대그림
sex_edu = list(survey$sex, survey$edu)
sex_edu_tb = table(sex_edu)
sex_edu_tb
barplot(sex_edu_tb, legend.text=rownames(sex_edu_tb), col=c(2,4))
title("Statcked Barplot")
# 한 화면에 여러 개의 그림 그리기:par문 이용
par(mfrow=c(1,2))
pie(sex_edu_tb[1,])
title("Educaton of Male")
pie(sex_edu_tb[2,])
title("Education of Female")

par(mfrow=c(1,1))
# 히스토그램, 줄기-잎 그림
hist(survey$salary)
stem(survey$salary)
stem(survey$salary, scale=2)

# 상자그림
boxplot(salary ~ sex, data=survey)
title("Boxplot of Salary")

####################################
### 다변량 그래프
# plot using lines
plot(co2)
lines(smooth(co2), col="BLUE")
# plot of mathematical functions
x<-seq(0, 20, 0.1)
y<-exp(-x/10)*cos(2*x)
plot(x,y, type="l")
# R bivariate boxplot
install.packages("HSAUR2")
library(HSAUR2)
install.packages("MVA")
library(MVA)
data(USairpollution)
head(USairpollution, 3)
x=USairpollution[,c(3,4)]
bvbox(x, xlab="manu", ylab="popul", pch=19)
title("Bivariate Boxplot")
identify(x) # 점찍어서 interactive하게 확인 가능
rownames(x)[c(7,9,14,30)]

# R Bubble plot
# (temp, wind)의 산점도에 제3의 변수인 SO2의 정보의 크기에 따라 운으로 나타낸 그림 
plot(wind~temp, data=USairpollution, pch=9)
# symbols(USairpollution$temp, USairpollution$wind, USairpollution$circle=SO2,
# inches=0.5, add=T))
with(USairpollution, symbols(temp, wind, circle=SO2, inches=0.5, add=T))
title("Bubble plot")

####################################
### 다차원 그래프
# 산점도 행렬
social = read.table('/Users/hj1/Desktop/mva/social.txt', header=T)
head(social, 3)
pairs(social)
round(cor(social, use="complete.obs"),3)

# 별그림
social2 = social[,-1]
year = social[,1]
rownames(social2) = year
stars(social2)

# 얼굴 그림
install.packages("aplpack")
library(aplpack)
#install.packages("XQuartz")
