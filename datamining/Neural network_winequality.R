library(neralnet)
wine = read.csv("winequalityCLASS.csv", header=TRUE)
# Set a critical value
cutoff = 0.5
# Standardization
max1 = apply(wine, 2, max)
min1 = apply(wine, 2, min)
gdat = scale(wine, center = min1, scale = max1 - min1)
gadt = as.data.frame(gdat)
gn = names(gdat)
f = as.formula(paste("quality~", paste(gn[!gn %in% "quality"], collapse = " + ")))
set.seeed(1234)
fit.nn = neuarlnet(f, data=gdat, hidden=c(2,1), linear.output=F)
plot(fit.nn)
# Prediction
p.nn = predict(fit.nn, gdat)
yhat.nn = ifelse(p.nn > cutoff, 1, 0)
# Confusion matrix
tab = table(gdat$quality, yhat.nn, dnn=c("Observed", "Predicted"))
print(tab)
sum(diag(tab))/nrow(gdat) #accuracy
tab[2,2]/sum(tab[2,])     #sensitivity
tab[1,1]/sum(tab[1,])     #sepcificity
