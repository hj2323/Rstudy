################################################
##########multiple regression model###############
################################################

#행렬연산
market2 = read.table('/Users/hj1/Desktop/R/reg/market-2.txt', header=T)
head(market2, 3)
X = market2[, c(2:3)] #X1, X2열에 대한 값만 담아줌 ㄱ
X = cbind(1,X) # 첫번쨰행을 1로 해서  덧붙여라
Y = market2[,4] # Y열에 대한 값만 담아줌
X = as.matrix(X)
Y = as.matrix(Y)
XTX = t(X) %*% X
XTX
XTXI = solve(XTX)
XTY = t(X) %*% Y
beta = XTXI %*% XTY
beta = round(beta, 3)
beta
