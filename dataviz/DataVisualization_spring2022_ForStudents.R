setwd("C:\\Users\\SYP\\Dropbox\\KNOU_2022_spring\\학부\\데이터시각화")

dat.sah<-read.table("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data",
                    sep=",",head=T,row.names=1)
dat.sah$BMI.cat<-cut(dat.sah$obesity, breaks=c(0, 25, 30, Inf))
dat.sah$chd<-as.factor(dat.sah$chd)
summary(dat.sah)

plot(dat.sah$age, dat.sah$sbp)
plot(sbp~age, data=dat.sah)

plot(dat.sah$age, dat.sah$sbp, 
     main="Scatterplot", 
     xlab="Age", ylab="SBP", xlim=c(10, 70), ylim=c(100, 220))


plot(sbp~age, data=dat.sah, col="blue")
plot(sbp~age, data=dat.sah, col=2)

library(plyr)

levels(dat.sah$BMI.cat)

BMI.col=as.character(mapvalues(dat.sah$BMI.cat, 
                               from=levels(dat.sah$BMI.cat),
                               to=c("black", "orange", "red")))

table(dat.sah$BMI.cat, BMI.col)


plot(sbp~age, data=dat.sah, col=BMI.col)
legend(17, 220, pch=1, col=c("black", "orange", "red"),
       legend=c("Normal", "Overweight", "Obese"))

BMI.pch=as.double(mapvalues(dat.sah$BMI.cat, 
                            from=levels(dat.sah$BMI.cat),
                            to=c(1, 2, 3)))
plot(sbp~age, data=dat.sah, pch=BMI.pch)
legend(17, 220, pch=1:3,
       legend=c("Normal", "Overweight", "Obese"))

plot(sbp~age, data=dat.sah)
lm.obj<-lm(sbp~age, data=dat.sah)
lm.obj$coefficients

abline(a=lm.obj$coefficients[1], b=lm.obj$coefficients[2])
abline(lm.obj)

plot(sbp~age, data=dat.sah)
abline(h=120, col="gray", lty=3)
abline(v=40, col="gray", lty=5)


par(mfrow=c(2, 2))
plot(sbp~age, data=dat.sah)
plot(sbp~obesity, data=dat.sah)
plot(sbp~alcohol, data=dat.sah)
plot(sbp~ldl, data=dat.sah)
dev.off()

matrix(c(1, 1, 2, 3), nrow=2, byrow=T)


layout(matrix(c(1, 1, 2, 3), nrow=2, byrow=T))
plot(sbp~age, data=dat.sah)
plot(sbp~obesity, data=dat.sah)
plot(sbp~alcohol, data=dat.sah)
dev.off()


hist(dat.sah$sbp)
hist(dat.sah$sbp, breaks=seq(100, 220, by=20))


hist(dat.sah$sbp[dat.sah$chd==0], col=rgb(0, 0, 1, 0.5),
     main="Histogram of SBP", xlab="SBP")
hist(dat.sah$sbp[dat.sah$chd==1], col=rgb(1, 0, 0, 0.5), add=T)

legend(180, 70, fill=c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)),
       legend=c("No CHD", "CHD"))

d<-density(dat.sah$sbp)
plot(d)

d1<-density(dat.sah$sbp[dat.sah$chd==0])
d2<-density(dat.sah$sbp[dat.sah$chd==1])

plot(d1, col="blue", main="Kernel Density Plot", xlab="SBP")
lines(d2, col="red")
legend(180, 0.025, lty=1, col=c("blue", "red"), legend=c("No CHD", "CHD"))

plot(d1, main="Kernel Density Plot", xlab="SBP")
lines(d2, lty=2)
legend(180, 0.025, lty=1:2, legend=c("No CHD", "CHD"), bty="n")

boxplot(sbp~BMI.cat, data=dat.sah, xlab="BMI group", ylab="SBP")


library(supclust)

data(leukemia)
xdat<-leukemia.x
ydat<-leukemia.y
dim(xdat)

heatmap(xdat)  
heatmap(xdat, scale="column")

rowcol=ifelse(ydat==1, "red", "blue")
heatmap(xdat, scale="column", RowSideColors=rowcol)  ### 1 = AML, 0 = ALL
heatmap(xdat, scale="column", RowSideColors=rowcol, Colv = NA, Rowv = NA) 
heatmap(xdat, scale="column", 
        RowSideColors=rowcol,
        col=terrain.colors(256)) 


library(RColorBrewer)

display.brewer.all()

brewer.pal(5, "RdBu")
display.brewer.pal(5, "RdBu")

brewer.pal(10, "RdBu")
display.brewer.pal(10, "RdBu")


brewer.pal(256, "RdBu")  ### too large
colorRampPalette(brewer.pal(10, "RdBu"))(256)

heatmap(xdat, scale="column", 
        RowSideColors=rowcol,
        col=colorRampPalette(brewer.pal(10, "RdBu"))(256)) 

pdf("sbp_age_plot.pdf", width=5, height=5)
plot(sbp~age, data=dat.sah)
dev.off()


library(ggplot2)

mpg
?mpg
head(mpg)

ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, color=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, shape=class)) +
  scale_shape_manual(values=1:7)

ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, color="blue"))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy), color="blue")

ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_wrap(~class, nrow=2)

ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ, y=hwy)) 
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ, y=hwy, linetype=drv)) 

ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy)) +
                    geom_smooth(mapping=aes(x=displ, y=hwy))

ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) + geom_point(mapping=aes(color=class)) +
  geom_smooth()

ggplot(data=mpg) + geom_histogram(mapping=aes(x=hwy))
ggplot(data=mpg) + geom_histogram(mapping=aes(x=hwy), binwidth = 2)
ggplot(data=mpg) + geom_histogram(mapping=aes(x=hwy, fill=drv), binwidth = 2, alpha=0.5, position="identity")

ggplot(data=mpg) + geom_boxplot(mapping=aes(x=class, y=hwy))
ggplot(data=mpg) + geom_boxplot(mapping=aes(x=reorder(class, hwy, FUN=median), y=hwy))
ggplot(data=mpg) + geom_boxplot(mapping=aes(x=reorder(class, hwy, FUN=median), y=hwy)) + coord_flip()


diamonds
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut))

ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, fill=cut))
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, fill=clarity))

economics
ggplot(economics, aes(date, unemploy)) + geom_line()





