setwd("C:\\chj\\Rstudy\\dataviz")

install.packages("datarium")
library(datarium)

dat <- marketing
View(dat)

# 1.R 함수 사용
plot(dat$facebook, dat$sales, main="202135-368401", xlab="facebook", ylab="sales", pch=20)
lmobj <- lm(sales~facebook, data=dat)
abline(lmobj, col=5)


# 2.ggplot2 사용
ggplot(data=dat, mapping = aes(x=facebook, y=sales)) + geom_point() + 
   stat_smooth(method =lm)





