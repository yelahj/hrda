
# EDA 
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)

# 한글깨짐 (http://blog.daum.net/pingpu/13718306)
install.packages("extrafont")

library(extrafont)
font_import('D2')

theme_set(theme_grey(base_family='NanumGothic'))

theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
theme.leti<-element_text(family="NanumGothic", face="bold") #범례 제목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요
theme.lete<-element_text(family="NanumGothic") #범례 항목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요



counts <- table(data$Gender)
counts
barplot(counts,
        main = "Gender Count",
        xlab = "Gender", ylab = "count")

counts <- (table(data$Attrition, data$Gender))
counts
barplot(counts,
        main = "Gender Count",
        xlab = "Gender", ylab = "Attrition",
        col=c("red", "green"),
        legend=rownames(counts)
)

barplot(counts, main="Grouped Bar Plot",
        xlab="Gender", ylab="Attrition",
        col=c("red", "green"),
        legend=rownames(counts), beside=TRUE)

apply(is.na(data), 2, sum)


# 퇴사자 수, 비율
data %>%
  group_by(Attrition) %>%
  tally() %>%
  ggplot(aes(x = Attrition, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="퇴사여부", y="퇴사자 수")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9)) +
  theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트

prop.table(table(data$Attrition))

# Age 확인 > 분포도 
ggplot(data=data, aes(data$Age)) + 
  geom_histogram(breaks=seq(20, 50, by=2), 
                 aes(fill=..count..))+
  labs(x="연령대", y="퇴사자") +
  scale_fill_gradient("Count", low="skyblue", high="navy")
  scale_color_steps()

# Business Travel
ggplot(data,aes(BusinessTravel,fill=Attrition))+geom_bar()
ggplot(data,aes(Department,fill = Attrition))+geom_bar()
ggplot(data,aes(DistanceFromHome,fill=Attrition))+geom_bar()
## grid.arrange(travelPlot,depPlot,distPlot, nrow=3)


data %>% 
  group_by(data$Department,data$Attrition) %>% 
  summarise(count=n()) %>% 
  mutate(grp_pct=count/sum(count)*100)

data %>%
  ggplot(aes(x=data$Department)) + 
  geom_bar(aes(fill=data$Attrition),position = position_dodge())

ggplot(aes(x=data$OverTime))+geom_bar(aes(fill=data$Attrition),position = position_dodge(),color="grey")+facet_grid(data$Gender~.)

library(corrplot)
library(psych)

attach(data)

table(data$Attrition)
table(data$Department)

xtabs(~JobSatisfaction+PerformanceRating)
xtabs(~JobInvolvement+EnvironmentSatisfaction)
xtabs(~JobRole+Department+JobSatisfaction)
xtabs(~OverTime+WorkLifeBalance)

chisq.test(Age, Attrition)
chisq.test(Age, Attrition)

chisq.test(JobSatisfaction, Attrition)

chisq.test(YearsAtCompany, Attrition)
chisq.test(YearsInCurrentRole, Attrition)
chisq.test(YearsSinceLastPromotion, Attrition)

t.test(PerformanceRating~Attrition,data=data)


frame <-data[,c("Age","DistanceFromHome","Education",
                "EnvironmentSatisfaction","JobInvolvement","JobLevel",
                "JobSatisfaction","MonthlyIncome","NumCompaniesWorked",
                "PercentSalaryHike","PerformanceRating","RelationshipSatisfaction",
                "StandardHours","TotalWorkingYears",
                "TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany",
                "YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager")]
round(cor(frame),2)

library(corrplot)
corrplot(cor(frame))


ggplot(data,aes(YearsSinceLastPromotion,fill=Attrition))+geom_bar()


v_data <- data[c("Attrition","JobSatisfaction", "MonthlyIncome", "YearsAtCompany", "NumCompaniesWorked","JobRole", "PerformanceRating","RelationshipSatisfaction","YearsSinceLastPromotion","Education")]



v_ds <- v_data %>% 
  summarise_each(funs(chisq.test(., 
                                 v_data$Attrition)$p.value), -one_of("Attrition"))

v_ds



head(data)

# Feature Selection

# Feature Engineering




