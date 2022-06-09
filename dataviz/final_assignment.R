######################################
# data visualization fianl assignment
######################################

### 3. 류마티스 관절염 환자 임상시험 결과 데이터, 
### 새로운 치료제 투약 여부가 치료 결과와 연관이 있는지,
### 성별과 치료 결과 간에 연관이 있는지 

install.packages("vcd")
library(vcd)
data <- Arthritis
head(data)
summary(data)

# 원 그래프로 데이터의 대략적인 분포 파악하기
sort.sex = sort(table(data$Sex))
sort.treat = sort(table(data$Treatment))
sort.improved = sort(table(data$Improved))
par(mfrow=c(1,2))
pie(sort.sex, radius=1, main="성별")
pie(sort.treat, radius=1, main="치료제 투약 여부")
pie(sort.improved, radius=1, main="치료결과")


# plot() 사용 그리기
par(mfrow=c(1,2))
plot(data$Treatment, data$Improved, pch=19)
plot(data$Sex, data$Improved, pch=19)

# 모자이크 플롯 그리기
mosaicplot(~ Treatment+Improved, data=Arthritis, color=c("grey", "blue"))
mosaicplot(~ Sex+Improved, data=Arthritis, color=c("grey", "blue"))


### 3. 뉴욕의 대기질에 관한 데이터셋, 

# 산점도 행렬 그리기
airquality
data(airquality)
attach(airquality)
pairs(airquality[1:4], main="AirQuality")

pairs(airquality[1:4], main="AirQuality", pch=21, 
      bg=c("red","yellow","green","cyan","blue")[unclass(airquality$Month)])


