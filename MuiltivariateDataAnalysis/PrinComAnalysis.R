####################################################
### 주성분 분석(principal component analysis)
####################################################
# 1. 자료 가져오기
install.packages("HSAUR2")
library(HSAUR2)
data(heptathlon)
summary(heptathlon)
write.csv(heptathlon, file='/Users/hj1/Desktop/mva/heptathlon.csv')

# 2. 자료 변형하기
heptathlon$hurdles = max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m = max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m = max(heptathlon$run800m) - heptathlon$run800m
head(heptathlon)

# 3. 주성분 분석 실행
hep_data = heptathlon[,-8]
hep_pca = princomp(hep_data, cor=T, scores=T)
names(hep_pca)
hep_pca

# 4. 주성분 분석 결과
summary(hep_pca)
eig_val = hep_pca$sdev^2
round(eig_val, 3)

# 5. 스크리그림과 누적분산
# 스크리그림
screeplot(hep_pca, type="lines", pch=19, main="Scree plot")
# 누적분산 그림 그리기
hep_var = hep_pca$sdev^2
hep_var_ratio = hep_var/sum(hep_var)
round(hep_var_ratio, 3)
plot(cumsum(hep_var_ratio), type='b', pch=19, xlab='Component', 
     ylab='Cumulative Proporton')
title('Variance Explained')


# 6. 주성분 계수
round(hep_pca$loadings[,c(1:2)], 3)

# 7. 주성분 점수 및 행렬도(biplot)
hep_pca$scores[c(1:5), c(1:2)]
biplot(hep_pca, cex=0.7, col=c("Red", "Blue"))
title("Biplot")
