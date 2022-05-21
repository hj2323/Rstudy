##############
# Star plot
##############

baseball <- read.csv("프로야구 20060602.csv", header=T)
x<-baseball[,2:4]
x[,1]<-0.2+0.8*(x[,1]-min(x[,1]))/(max(x[,1])-min(x[,1]))
x[,2]<-0.2+0.8*(x[,2]-min(x[,2]))/(max(x[,2])-min(x[,2]))
x[,3]<-0.2+0.8*(max(x[,3])-x[,3])/(max(x[,3])-min(x[,3]))
rownames(x) <- baseball[,1]
stars(x, scale=F, key.loc = c(7,2), col.stars = 2:9)

x11(); stars(x, scale=F, draw.segments=T, full=F,
             key.loc = c(7,2))

##############
# scatterplot matrix
##############
data(iris) 
attach(iris) 
pairs(iris[1:4], main="Iris Data")

# 품종별 색구분 산점도 행렬
pairs(iris[1:4], main = "Iris Data", pch
      = 21, bg = c("red", "green3",
                   "blue")[unclass(iris$Species)])


#######################
# conditioning plot
#######################

library(lattice) 
data(quakes) 
str(quakes)
attach(quakes) 
xyplot(lat ~ long,main="Earthquakes in Fiji", pch="*",
                      cex=2)

