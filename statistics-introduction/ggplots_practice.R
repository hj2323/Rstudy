library(ggplot2)
transp<-c("bicycle", "bus","bus", "walking", "bus","bicycle","bicycle","walking","bus","bus","bus","bicycle","bus","bicycle","bicycle","walking","walking","bus","bus","bicycle","bicycle","bicycle","bus","bus","bus","bus","bicycle","bus","bus","bicycle","bicycle","bicycle")
dat1<-data.frame(transp)

library(forcats)
ggplot(data=dat1) +geom_bar(mapping=aes(x=transp)) +
  xlab("transportation")

obesity<-factor(c("underweight", "normal", "overweight", "obese"),
                levels=c("underweight", "normal", "overweight", "obese"))
count<-c(6,69,27,13)
perc<-count/sum(count)*100
dat2<-data.frame(obesity, count, perc)
ggplot(data=dat2) + geom_bar(mapping=aes(x=obesity, y=perc),
                             stat="identity") +xlab("Obesity")
                  +ylab("Percentage(%)")

#stat="identity??? y?????? ???????????? ?????? ????????? ???????????? ??????
table(transp)
dat3<-data.frame(transportation=c("bus", "bicycle", "walking"),
                 count=c(15, 13, 4))

ggplot(data=dat3)+geom_bar(mapping=aes(x="", y=count, fill=transportation),
                           stat="identity") + coord_polar("y", start=0) +
                xlab("") + ylab("") +
                theme(axis.text=element_blank(),
                      axis.ticks=element_blank(),
                      panel.grid=element_blank())
#coord_polar: pie chart
