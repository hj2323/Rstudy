###################################
# Multidimensional scaling,MDS
###################################

# 자료 읽기
auto = read.csv("/Users/hj1/Desktop/mva/auto.csv")
head(auto)

#
X = auto[,-1]
autoName = auto[,-1]
# z-standardization
zX = scale(X, center=TRUE, scale=TRUE)
# 0-1 transformation
maxX = apply(X, 2, max)
minX = apply(X, 2, min)
z01X = scale(X, center=minX, scale=maxX - minX)

#
z01X_dist = dist(z01X, method='euclidean')
z01X_dist = as.matrix(z01X_dist)
colnames(z01X_dist) = autoName
rownames(z01X_dist) = autoName

#
mds1 = cmdscale(z01X_dist, k=2)
plot(mds1[,1], mds1[,2], type='n', xlab='', ylab='', main='cmdscale(Auto)')
text(mds[,1], mds1[,2], rownames(z01X_dist), cex=0.9)
abline(h=0, v=0, lty=2)

#
library(smacof)
mds2 = mds(z01X_dist, ndim=2)
names(mds2)
plot(mds2$conf[,1], mds$conf[,2], type='n', xlab='', ylab='', main="smacof(Auto)")
text(mds2$conf[,1], mds2$conf[,2], rownames(z01X_dist), cex=0.9)
abline(h=0, v=0, lty=2)
mds2$stress

#
mds2_1 = mds(z01X_dist, ndim=1)
mds2_2 = mds(z01X_dist, ndim=2)
mds2_3 = mds(z01X_dist, ndim=3)
mds2_4 = mds(z01X_dist, ndim=4)
stress = c(mds2_1$stress, mds2_2$stress, mds2_3$stress, mds2_4$stress)
plot(stress, type="l")
points(stress, cex=0.9)

#
readMatrix <- function(datam, nrows, cname, lower=1)
{
  # lower=1 : Lower Triangular Matrix (default)
  # lower=2 : upper Triangular Matrix
  n <- nrows
  if( lower == 1)
  { DistanceArray <- array(0, n*(n-1)/2 )
  for(i in 1:(n-1) )
    for(j in (i+1):n )
    { kk1 <- (j-1)*(j-2)/2 + i
    kk2 <- n*(i-1)- i*(i-1)/2 + j-i
    DistanceArray[kk2] <- datam[kk1] }
  }
  else
    DistanceArray <- datam
  
  DistanceArray = 10 - DistanceArray
  MD <- matrix(0, nrow=n, ncol=n)
  
  for(j in 1:(n-1) )
    for(k in (j+1):n)
    { kk <- n*(j-1) - j*(j-1)/2 + k-j
    MD[j,k] <- MD[k,j] <- DistanceArray[kk]
    } 
  
  colnames(MD) = cname
  rownames(MD) = cname
  return(MD)
}

#
source("/Users/hj1/Desktop/mva/nreadMatrix.r")
datam = scan("/Users/hj1/Desktop/mva/country1968.txt")
country_name = scan("/Users/hj1/Desktop/mva/countryname.txt", what="")
country_name
cdata = readMatrix(datam, nrow=12, country_name)
cdata

#
library(MASS)
nmds = isoMDS(cdata, k=2)
names(nmds)
nmds
x = nmds$points[,1]
y = nmds$points[,2]
plot(x,y, xlab="", ylab="", main="isoMDS", type="n")
text(x,y, labels=row.names(cdata), cex=0.9)
abline(h=0, v=0, lty=2)