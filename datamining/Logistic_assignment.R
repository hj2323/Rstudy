####################################
# Assignment
# Logistic Regression with wine data
####################################

setwd("C://chj//Rstudy//datamining")
# Importing data
wine = read.csv("winequalityCLASS.csv", header=TRUE)

# Fitting a logistic regression model
fit.all = glm(quality ~ alcohol+sulphates, family = binomial, data = wine)
fit.step = step(fit.all, direction="both") # stepwise vaiable selection
fit.step$anova
summary(fit.step)


# Making predictions
p = predict(fit.step, newdata=wine, type="response") # prediction
p
cutoff = 0.5 #cutoff
yhat = ifelse(p > cutoff, 1, 0)

# Evaluation
tab = table(wine$quality, yhat, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
tab[2,2]/sum(tab[2,])   # sensitivity
tab[1,1]/sum(tab[1,])   # specificity


### END
### 전체 오분류율이 최소화되는 것을 선택할 수 도 있지만 오분류율이 조금 크더라도 나무구조 조금 간단하고 해석이 용이한 것을 선택