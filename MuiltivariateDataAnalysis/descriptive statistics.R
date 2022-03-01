survey = read.csv("c:/data/mva/survey.csv")
head(survey,3)
mean(survey$age)
sd(survey$age)
nlevels(survey$sex)
survey$sex = factor(survey$sex, levels=c(1:2), labels=c("Male", "Female"))
survey$marriage = factor(survey$marriage, levels=c(1:3),
                         labels=c("Unmarried","Married","Divorced"))
survey$job = factor(survey$job, levels=c(1:8), 
                    labels=c('a','b','c','d','e','f','g','other'))
survey$edu = ordered(survey$edu, levels=c(1:5), 
                     labels=c('none','elem','med','high','college')) 
summary(survey[,-1])

###그룹별 기술통계량 구하기
tapply(survey$age, survey$sex, mean)
with(survey, tapply(age, sex, sd))
with(survey, tapply(age, marriage, mean))
with(survey, tapply(age, marriage, sd))

###두개의 변수 연결
sex_ma = list(survey$sex, survey$marriage)
table(sex_ma)
with(survey, tapply(age, sex_ma, mean))
with(survey, tapply(age, sex_ma, sd))

###빈도표 및 분할표(성별, 교육)
table(survey$sex)
table(survey$edu)
table(survey$sex, survey$edu)
sex_edu = table(survey$sex, survey$edu)
summary(sex_edu)
