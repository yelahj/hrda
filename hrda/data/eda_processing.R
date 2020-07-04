
# EDA 
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)

# 한글깨짐 (http://blog.daum.net/pingpu/13718306)
#install.packages("extrafont")
# 
# library(extrafont)
# font_import('D2')
# 
# theme_set(theme_grey(base_family='NanumGothic'))
# 
# theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
# theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
# theme.leti<-element_text(family="NanumGothic", face="bold") #범례 제목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요
# theme.lete<-element_text(family="NanumGothic") #범례 항목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요
# 
# 

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
ggplot(data=data, aes(data$Age, data$Gender)) + 
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


## 
# 0. 환경설정 ------
library(tidyverse)
library(caret)
library(janitor)
library(ggridges)
library(ggthemes)
library(cowplot)
library(corrplot)
library(corrr)
library(plotly)
library(crosstalk)

# 1. 데이터 ------
idx <- createDataPartition(data$Attrition, 
                           p = 0.7, 
                           list = FALSE, 
                           times = 1)

data_train <- data[ idx,]
data_test  <- data[-idx,]


## 2.2. 모형적합 ------
fit_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

data_rf <- train(Attrition ~ ., 
                 data = data_training, 
                 method = "rf", 
                 preProcess = c("scale", "center"),
                 trControl = fit_ctrl,
                 verbose = FALSE)

## 2.3. 모형성능평가 ------
test_predict <- predict(data_rf, data_testing)
confusionMatrix(test_predict, data_testing$Attrition)



# 3. 모형 설명 -----
## 3.1. 중요변수 추출 -----
data_rf_imp <- varImp(data_rf, scale = TRUE)

(top_ten_variable_v <- data_rf_imp$importance %>%
    as.data.frame() %>%
    rownames_to_column(var="variable") %>% 
    top_n(10, Overall) %>% 
    pull(variable))


data_rf_imp$importance %>%
  as.data.frame() %>%
  rownames_to_column(var="variable") %>%
  ggplot(aes(x = reorder(variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#1F77B4", alpha = 0.8) +
  coord_flip() +
  labs(y="중요도", x="요소") +
  theme_minimal(base_family="NanumGothic")



data_rf_imp


# 2. 탐색적 데이터 분석 ------
## 2.1. 정적 시각화 -----
y_p <- 
  data %>%
  ggplot(aes(x = Attrition, fill = Attrition)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("grey", "red")) +
  guides(fill = FALSE)

x_p <- 
  data %>%
  gather(variable, value, Age:OverTimeHours) %>%
  ggplot(aes(x = value, y = Attrition, color = Attrition, fill = Attrition)) +
  facet_wrap( ~ variable, scale = "free", ncol = 3) +
  scale_color_manual(values = c("red", "grey")) +
  scale_fill_manual(values = c("red", "grey")) +
  geom_density_ridges(alpha = 0.8) +
  guides(fill = FALSE, color = FALSE)

plot_grid(y_p, x_p, rel_widths = c(1,3))



suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggcorrplot))
library(RColorBrewer)
library(rpart)

suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(rpart.plot))
library(ggplot2)
suppressPackageStartupMessages(library(tree))
suppressPackageStartupMessages(library(ggcorrplot))
# 각 변수별 상관관계
options(repr.plot.width=10, repr.plot.height=7) 

nums <- select_if(data, is.numeric)

corr <- round(cor(nums), 1)

ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2, 
           method="square", 
           colors = c("tomato2", "white", "#01A9DB"), 
           title="각 변수별 상관관계", 
           ggtheme=theme_minimal()
           + theme(axis.title = theme.ax, plot.title = theme.ti)  # 한글 폰트
)


library("rpart.tree")
#install.packages("rpart.tree")
#install.packages("RColorBrewer")
library(rpart)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

options(repr.plot.width=10, repr.plot.height=8) 

rpart.tree <- rpart(Attrition ~ ., data=data_training)
plot(rpart.tree, uniform=TRUE, branch=0.6, margin=0.05)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("의사결정트리")

var_imp <- data.frame(rpart.tree$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$rpart.tree.variable.importance, 2)
var_imp$rpart.tree.variable.importance <- NULL

colorCount <- length(unique(var_imp$features))

feature_importance <- var_imp %>%
  ggplot(aes(x=reorder(features, importance), y=importance, fill=features)) + geom_bar(stat='identity') + coord_flip() + 
  theme_minimal() + theme(legend.position="none", strip.background = element_blank(), strip.text.x = element_blank(), 
                          plot.title=theme.ti, plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#FFFFFF"),
                          axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black"),
                          axis.title=theme.ax, 
                          legend.background = element_rect(fill="#FFFFFF",
                                                           size=0.5, linetype="solid", 
                                                           colour ="black")) + scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(colorCount)) + 
  geom_label(aes(label=paste0(importance, "%")), colour = "black", fontface = "italic", hjust=0.6) + 
  labs(title="요인별 중요도", x="요인", y="중요도")


feature_importance




# Random Forest 
data_rf_imp

feature_importance <- data_rf_imp %>%
  ggplot(aes(x=reorder(features, importance), y=importance, fill=features)) + geom_bar(stat='identity') + coord_flip() + 
  theme_minimal() + theme(legend.position="none", strip.background = element_blank(), strip.text.x = element_blank(), 
                          plot.title=theme.ti, plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#FFFFFF"),
                          axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black"),
                          axis.title=theme.ax, 
                          legend.background = element_rect(fill="#FFFFFF",
                                                           size=0.5, linetype="solid", 
                                                           colour ="black")) + scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(colorCount)) + 
  geom_label(aes(label=importance), colour = "black", fontface = "italic", hjust=0.6) + 
  labs(title="요인별 중요도", x="요인", y="중요도")


feature_importance




