#############################
# 군집분석cluster analysis
#############################

### 계층형 군집분석
# 자료 읽기
beer = read.csv("/Users/hj1/Desktop/mva/beerbrand.csv", header=T, row.names=1)
head(beer)
summary(beer)

#
zbeer = scale(beer)
round(apply(zbeer, 2, mean), 3)
round(apply(zbeer, 2, sd), 3)

#
zbeer_euc = dist(zbeer)
zbeer_euc[1]
zbeer_man = dist(zbeer, "manhattan")
zbeer_man[1]

# R 4.4
hc_s = hclust(zbeer_euc, method='single')
hc_s
plot(hc_s, hang=-1)

# R 4.5
hc_c = hclust(zbeer_euc, method='complete')
hc_c
plot(hc_c, hang=-1)

# R 4.6
hc_cen = hclust(zbeer_euc, method="centroid")
hc_cen
plot(hc_cen, hang=-1)


# R 4.7
hc_w = hclust(zbeer_euc, method="ward.D")
hc_w
plot(hc_w, hang=-1)

# R 4.8
hc_cen24 = cutree(hc_cen, 2:4)
hc_cen24

# R 4.9
kmc = kmeans(zbeer, centers=2)
kmc

# R 4.10
plot(zbeer, col=kmc$cluster, pch=16)

# R 4.11
pairs(zbeer, col=kmc$cluster, pch=16, cex.labels=1.5)

### 비계층형 군집분석(k평균)
# R 4.12
head(USArrests)
summary(USArrests)
zUSArrests=scale(USArrests)

# R 4.13
hc_a = hclust(dist(zUSArrests), method="average")
hc_a
plot(hc_a, hang=-1) 

# R 4.14
hcmember <- cutree(hc_a, k=5)
hc_member

# R 4.15
data_combined = cbind(USArrests, hcmember)
aggregate(.~hcmember, data_combined, mean)

# R 4.16
zUSArrests = scale(USArrests)
kmc1 = kmeans(zUSArrests, 4)
kmc1

# R 4.17
pairs(USArrests, col=kmc1$cluster, pch=16, cex.labels=1.5)

