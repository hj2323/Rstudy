####################################################
### 군집 분석(cluster analysis)
### 과제 
####################################################
### 1. 데이터 파일 읽기
mall = read.csv('C:/data/mva/mall_customer.csv', header=T, row.names=1)
head(mall)
sapply(mall, class)
# 성별 가변수 변환
mall <- transform(mall, 
                  Gender = ifelse(Gender == 'Male', 1, 0))
head(mall)
summary(mall)

# 자료 표준화
zmall = scale(mall)
round(apply(zmall, 2, mean), 3)
round(apply(zmall, 2, sd), 3)

# 거리행렬 계산
zmall_euc = dist(zmall) #euclidean는 디폴트
zmall_man = dist(zmall, "manhattan") 

### 2. 계층적 군집분석 - 와드의 방법
hc_w = hclust(zmall_euc, method="ward.D")
hc_w
plot(hc_w, hang=-1)


### 3. 계층적 군집분석 - 최장연결법
hc_c = hclust(zmall_euc, method="complete")
hc_c
plot(hc_c, hang=-1)

### 5. k-평균 군집분석
kmc = kmeans(zmall, 6)
kmc

# 소속 군집집 산점도
pairs(zmall, col=kmc$cluster, pch=16, cex.labels=1.5)

