#####################################
### regression analysis
### 4. model development
### dataset : traffic crime, tokyo international marathon
#####################################

# example 1 :  2차 다항회귀모형
tcrime = read.table("c:/data/reg/tcrime.txt", header=T)
head(tcrime, 3)

attach(tcrime)
plot(motor, tcratio, pch=19)

tcrime.lm = lm(tcratio ~ motor + I(motor^2), data=tcrime)
summary(tcrime.lm)

# example 2 :  3차 다항회귀모형
maraton = read.table("c:/data/reg/maraton.txt", header=T)
head(maraton, 2)

plot(maraton$sect, maraton$m1990, pch=19)

maraton.lm=lm(m1990 ~ sect+I(sect^2)+I(sect^3), data=maraton)
summary(maraton.lm)



