#########################################
# Decision Tree with group data
#########################################
install.packages("rpart")
install.packages("rpart.plot")

setwd('C:\\chj\\Rstudy\\datamining')

# importing data
group1 = read.csv("group1.csv", header=TRUE)
head(group1)

# Factorize for classification
group1$Y = factor(group1$Y)

# Classification Tree
library(rpart)
set.seed(1234) # random number generate-> 결과값이 일관성을 유지
c <- rpart(Y~., data=group1)
my.control = rpart.control(xval=10, cp=0, minsplit=20)
# cp=0 최대나무구조까지 출력
tree.group1 = rpart(Y~., data=group1, method="class", control=my.control)
print(tree.group1)

# Display tree
library(rpart.plot)
prp(tree.group1, type=4, extra=1, digits=2, box.palette="Grays")



# Making predictions - probability prediction
prob.tree.group1 = predict(tree.group1, newdata=group1, type="prob")
head(prob.tree.group1, 5)
cutoff = 0.5 # cutoff
yhat.tree.group1 = ifelse(prob.tree.group1[,2] > cutoff, 1, 0)
yhat.tree.group1

#Evalutaaion
tab = table(group1$Y, yhat.tree.group1, dnn=c("Observed", "Predicted"))
print(tab)  # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
