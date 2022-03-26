#########################################
# Decision Tree with wine data
#########################################
install.packages("rpart")
install.packages("rpart.plot")

setwd('C:\\chj\\Rstudy\\datamining')

# importing data
wine = read.csv("winequalityCLASS.csv", header=TRUE)

# Factorize for classification
wine$quality = factor(wine$quality)

# Classification Tree
library(rpart)
set.seed(1234) # random number generate-> 결과값이 일관성을 유지
my.control = rpart.control(xval=10, cp=0, minsplit=20)
# cp=0 최대나무구조까지 출력
tree.wine = rpart(quality~., data=wine, method="class", control=my.control)
print(tree.wine)

# Display tree
library(rpart.plot)
prp(tree.wine, type=4, extra=1, digits=2, box.palette="Grays")

### 가지고 있는 데이터는 잘 설명하지만 새로운 데이터가 
### 들어왔을때 엉뚱한 예측값을 산출 -> 예측값이 굉장히 떨어짐-> 가지치기 필요성


# Pruning with c-s.e.
cps = printcp(tree.wine)
k = which.min(cps[,"xerror"])
err = cps[k, "xerror"]; se = cps[k, "xstd"]
err
se
c = 1 # 1-s.e.
k1 = which(cps[,"xerror"] <= err+c*se)[1]
k1
cp.chosen = cps[k1, "CP"]
tree.pruned.wine = prune(tree.wine, cp=cp.chosen)
print(tree.pruned.wine)
#Display tree
prp(tree.pruned.wine, type=4, extra=1, digits=2, box.palette="Gray")


# Making predictions - probability prediction
prob.tree.wine = predict(tree.pruned.wine, newdata=wine, type="prob")
head(prob.tree.wine, 5)
cutoff = 0.5 # cutoff
yhat.tree.wine = ifelse(prob.tree.wine[,2] > cutoff, 1, 0)
yhat.tree.wine

#Evalutaaion
tab = table(wine$quality, yhat.tree.wine, dnn=c("Observed", "Predicted"))
print(tab)  # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
tab[2,2]/sum(tab[2,]) #sensitivity
tab[1,1]/sum(tab[1,]) #specificity
