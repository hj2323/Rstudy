setwd("C:\\chj\\Rstudy\\dataviz")

install.packages("ggplot2")
library(ggplot2)

#외부 데이터 불러들일때 가장 안정적인 함수
dat <- read.csv("owid-covid-data.csv")
View(dat)

library(dplyr) # 데이터 클리닝할 때 아주 유용한 패키지 

dat1 <- dat %>% filter(iso_code=="KOR" |
                         iso_code=="USA" |
                         iso_code=="FRA" |
                         iso_code=="JPN") %>%
                mutate(date=as.Date(date))

table(dat1$iso_code)

# 날짜가 문자열로 인식되어 있기 때문에 날짜로 인식 시켜주는 작업을 해주어야 한다. 

ggplot(data=dat1, mapping = aes(x=date, y=new_cases, color=iso_code)) + 
  geom_line() + geom_smooth(span=0.3) +
  facet_wrap(~iso_code)

ggplot(data=dat1, mapping = aes(x=date, y=new_cases, color=iso_code)) + 
  geom_line() + geom_smooth(span=0.3) 