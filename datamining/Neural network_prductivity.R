install.packages('neuralnet')
install.packages("dummy")
library(neuralnet)
library(dummy)
setwd("C:\\chj\\Rstudy\\datamining")
prod = read.csv("productivityREG.csv", header=TRUE)
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)
# Create dummy variables
dvar = c(1:4)
prod2 = dummy(x=prod[,dvar])
prod2 = prod2[,-c(5,7,13,25)] # 모형비교를 위한 데이터세트 생성(범주수-1)
                              # 이를 삭제하면 범주 수만큼의 가변수 생성
prod2 = cbind(prod[,-dvar], prod2)
for(i in 1: ncol(prod2)) if(!is.numeric(prod2[,i])) prod2[,i] = as.numeric(prod2[,i])

# Standardization
max1 = apply(prod2, 2, max)
min1 = apply(prod2, 2, min)
sdat = scale(prod2, center = min1, scale = max1 - min1)
sdat = as.data.frame(sdat)
pn = names(sdat)
f = as.formula(paste("productivity~", paste(pn[!pn %in% "productivity"], collapse = " + ")))
set.seed(1234)
fit.nn = neuralnet(f, data = sdat, hidden=c(3,1), linear.output=T)
plot(fit.nn)

# Prediction
pred.nn = predict(fit.nn, sdat)
pred.nn = pred.nn * (max1[7]-min1[7])+min1[7]

# Mean Squared Error(MSE)
mean((prod2$productivity-pred.nn)^2)
# Scatter plot (Observed vs. Fitted)
plot(prod2$productivity, pred.nn, xlab="Observed Values", ylab="Fitted Values")
abline(0,1)

