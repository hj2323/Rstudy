####################################################
### 주성분 분석(principal component analysis)
####################################################
# 1. 자료 가져오기
data("USArrests")
head(USArrests)
summary(USArrests)
boxplot(USArrests)

# 2. 주성분 분석 실행
# Principal component analysis using prcomp (using SVD)
usarr_pca =  prcomp(USArrests, scale=TRUE)
names(usarr_pca)
usarr_pca

# 3. 주성분 분석 결과
summary(usarr_pca)
eig_val = usarr_pca$sdev^2
round(eig_val, 3)

# 5. 스크리그림과 누적분산
# 스크리그림
screeplot(usarr_pca, type="lines", pch=19, main="Scree plot")
# 누적분산 그림 그리기
usarr_var = usarr_pca$sdev^2
usarr_var_ratio = usarr_var/sum(usarr_var)
round(usarr_var_ratio, 3)
plot(cumsum(usarr_var_ratio), type='b', pch=19, xlab='Component', 
     ylab='Cumulative Proporton')
title('Variance Explained')

# 6. 주성분 계수
round(usarr_pca$rotation[, c(1:2)], 3)

# 7. 주성분 점수 및 행렬도(biplot)
usarr_pca$scores[c(1:7), c(1:2)]
biplot(usarr_pca, cex=0.7, col=c("Red", "Blue"))
title("Biplot")
