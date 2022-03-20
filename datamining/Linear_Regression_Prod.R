###########################################
# Linear Regression with productivity data
###########################################

setwd("C://chj//Rstudy//datamining")
# Importing data
prod = read.csv("productivityREG.csv", header=TRUE)

# Factorizing predictor variables
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)

# Fitting a linear regression model
fit.all = lm(productivity ~ ., data = prod)
head(prod)
summary(fit.all) #fit한 결과값 보기기
fit.step = step(fit.all, direction="both") 
summary(fit.step)
fit.step$anova

# Making predictions
pred.reg = predict(fit.step, newdata=prod, type="response") 
print(pred.reg)

# Evaluation
mean((prod$productivity - pred.reg)^2)  # MSE
mean(abs(prod$productivity - pred.reg)) # MAE


### END
