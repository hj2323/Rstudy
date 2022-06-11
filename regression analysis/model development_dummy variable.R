#####################################
### regression analysis
### 4. model development
### dataset : soap
#####################################

### dummy variable regression model

# 교호작용이 없는 경우
soap = read.table("c:/data/reg/soup.txt", header=T)
soap[c(1,15,16,27),]

soap$D = factor(soap$D, levels=c(0,1), label=c("Line0", "Line1"))
plot(soap$X, soap$Y, type="n")
points(soap$X[soap$D=="Line1"], soap$Y[soap$D=="Line1"], pch=17, col="BLUE")
points(soap$X[soap$D=="Line0"], soap$Y[soap$D=="Line0"], pch=19, col="RED")
legend("bottomright", legend=levels(soap$D), pch=c(19,17), col=c("RED", "BLUE"))

soap.lm = lm(Y~X+D, data=soap)
summary(soap.lm)
abline(27.2817, 1.23074, lty=2, col="RED")
abline(27.2817+53.1292, 1.23074, lty=2, col="BLUE")


soap2.lm = lm(Y~X+D+X:D, data=soap) # soap.lm = lm(Y~X*D, data=soap)와 같다
summary(soap2.lm)
