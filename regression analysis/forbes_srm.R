########################################
# single regression model 단순 회귀 모형 적합하기
# dataset: forbes
# author: hyeji choi
# purpose : midterm assignment
#########################################

# 자료를 읽어 산점도 그리기
forbes = read.table("c:/data/reg/forbes.txt", header=T)
head(forbes, 3)
attach(forbes)
Lpress = 100 * log(10*press, base=10)
Lpress
plot(temp, Lpress, pch=19)

# 회귀모형 적합하기
forbes.lm = lm(Lpress ~ temp, data=forbes)
summary(forbes.lm)

# 분산분석표 구하기
anova(forbes.lm)

# 잔차 및 추정값 보기
names(forbes.lm)
cbind(forbes, forbes.lm$residuals, forbes.lm$fitted.values)

# 잔차 그림 그리기
plot(temp, forbes.lm$residuals, pch=19)
abline(h=0, lty=2)

# 추정값의 신뢰대 그리기
p.x = data.frame(temp=c(1, 45))
pc=predict(forbes.lm, int="c", newdata=p.x)
pred.x = p.x$temp
plot(temp, Lpress, ylim = range(Lpress, pc))
matlines(pred.x, pc, lty=c(1,2,2), col="BLUE")