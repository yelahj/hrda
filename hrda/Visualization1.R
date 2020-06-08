# Visualizatoin 1

library(ggplot)
library(reshape2)
library(ggplot2)
library(extrafont)
library(GGally)


font_import(pattern = "D2")

theme_set(theme_grey(base_family='NanumGothic'))

theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
theme.leti<-element_text(family="NanumGothic", face="bold") #범례 제목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요
theme.lete<-element_text(family="NanumGothic") #범례 항목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요

# numeric 변수들의 corr
ggcorr(data)

option(digit=2)
cor(data)


# 1. 인구통계학적 특성에 따른 현황 (성별, 교육 등 개인적인 요인들) 

# 그래프 1. 각 성별 연령대 분포도 (평균) 
# 그래프 2. 학력별 퇴사율 

table(data$Age)
raw =
raw %>%
  mutate(Ages = case_when(
    Age %in%  20:29  ~ "20대",
    Age %in%  30:39 ~ "30대",
    Age %in%  40:49 ~ "40대",
    Age %in%  50:59 ~ "50대",
    Age %in%  60:69 ~ "60대",
    Age %in%  70:79 ~ "70대",
    Age %in%  0:19 ~ "10대",
  ))

da_yes <- 
data %>%
  filter(Attrition ==  "Yes")

gg1_1_1 <- da_yes %>% 
  ggplot(aes(x=Age)) + 
  geom_density(alpha = 0.8) +
  geom_histogram(fill="cornsilk", colour="grey", size=.2) +
  labs(title = "연령대별 퇴사자") +
  #geom_vline(aes(xintercept = mean(Age))) +
  theme_light() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트

gg1_1_2 <- da_yes %>%
  filter(Gender == "Male") %>%
  ggplot(aes(x=Age)) + 
  geom_density(fill = "lightblue", alpha = 0.5) + 
  geom_vline(aes(xintercept = mean(Age))) +
  labs(title = "연령대별 퇴사자(남성)") +
  theme_minimal() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트

gg1_1_3 <- da_yes %>%
  filter(Gender == "Female") %>%
  ggplot(aes(x=Age)) + 
  geom_density(fill = "tomato", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Age))) +
  labs(title = "연령대별 퇴사자(여성)") +
  theme_minimal() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트

ggarrange(gg1_1_1,ggarrange(gg1_1_2, gg1_1_3),nrow = 2)

학력별

gg1_2 =
data %>% 
  ggplot(aes(x=Education)) +
  geom_bar(aes(fill=Attrition), position = position_dodge(), alpha = 0.8) +
  labs(title = "학력별", x="학력", y="수") +
  scale_fill_manual(values=c("grey", "tomato")) + 
  theme_minimal() + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트



# 2. 소속 집단에 따른 현황 (사내 환경적 요인)

#그래프1 부서별 연봉? 연봉과 퇴사 정보 비교 (gg2_1)

gg2_1 <- data %>% select(Department, MonthlyIncome, Attrition) %>% group_by(Attrition, Department) %>%
  summarize(avg.inc=mean(MonthlyIncome)) %>%
  ggplot(aes(x=reorder(Department, avg.inc), y=avg.inc, fill=Attrition)) + geom_bar(stat="identity", position="dodge", alpha = 0.5) + facet_wrap(~Attrition) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.8)) + 
  scale_fill_manual(values=c("tomato", "grey")) + 
  labs(y="평균임금", x="부서", title="퇴사/재직자 별 부서별 임금비교") + 
  geom_text(aes(x=Department, y=0.01, label= paste0("$ ", round(avg.inc,2))),
            hjust=-0.5, vjust=0, size=3, 
            colour="black", fontface="bold",
            angle=90) + 
  theme_minimal() + 
  theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트


gg2_1

# 그래프2 overtime 현황별 퇴사 (gg2_2)

ggplot(data, 
       aes(x = OverTime, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.8) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "OverTime") +
  facet_grid(~Attrition) +
  scale_fill_manual(values = c("grey","tomato")) + 
  theme_minimal() + 
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트

# 3. 만족도에 따른 현황 - 사내 환경적 (주관)

# 그래프 1. role 별 만족도 (업무, 환경)
# 그래프 2. 만족도에 따른 퇴사율


# 1


# 4 라이프 스타일에 따른 현황
### WorkLifeBalance

#그래프 1. 부서별 워라밸
#- 부서별 overtime
#- 부서별 refresh 발생율
#- 부서별 refresh 소진율

