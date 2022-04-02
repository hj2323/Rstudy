##################################
### EDA
##################################
### pie chart
# bloodtype
set.seed(2022)
df <- data.frame(blood_type = sample(c('A', 'B', 'AB', 'O'), replace = TRUE, size = 100))
sort.df = sort(table(df), decreasing = T)
sort.df
par(mfrow = c(1,2))
slices = c("red", "blue", "yellow", "green")
pie(sort.df, col=slices, radius=1, main="PIE CHART")

# election
require(grDevices)
par(mfrow = c(1,1))
pie.vote <- c(0.5067, 0.0167, 0.0100, 0.0433, 0.4233)
names(pie.vote) <- c("새누리 152명", "선진 5명","무 3명",
                     "진보 13명","민주 127명")
pie(pie.vote)
pie(pie.vote, col=c("red3", "blue", "green3", "magenta", "yellow"),
    main = "19대 국회의원 선거")
# 원띠그래프
par(new=T)
pie(c(152, 127, 13, 5, 3), radius = 0.8, col="white", label=NA, border=NA)
text(0,0, "총 300석")

### barplot
barplot(sort.df, col=slices, main="막대그래프")
barplot(pie.vote, col=c("red3", "blue", "green3", "magenta", "yellow"), main="막대그래프")


### hist
# 외부파일을 읽어 데이터프레임을 만들기
담즙과포화비율 = read.table("C:\\chj\\Rstudy\\dataviz\\담즙과포화비율.txt", header=T)
담즙과포화비율
attach(담즙과포화비율)
str(담즙과포화비율)
# 담즙과포화비율 - 자료의 크기
