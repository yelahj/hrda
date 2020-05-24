
library(ROCR)
library(ggplot2)
library(tidyverse)


library(tidyverse)
library(gridExtra)
library(DMwR) ## for knnimputation function
library(dplyr) ## for data preprocessing
library(stringr) #for str_trim in ticket
library(caret) ## for createDataPartition
library(gridExtra)

data <- read_csv("data/dataset.csv")

# Data 미리보기
data

# 전체 데이터셋에 ID 변수 추가 (index)
data$ID <- seq.int(nrow(data))
# 전체 데이터셋에 ID 변수 추가 (index)
data$Attrition <- as.integer(as.character(data$Attrition)=="Yes")
data$Attrition <-as.factor(data$Attrition)


########################### doesn't work...
data <- within(data, {
  data$Gender[ data$Gender == "female" ] <- 1
  data$Gender[ data$Gender == "male" ] <- 0
})

data<- mutate(data, Gender == "female", 1)
data$Gender
if (data$Gender == 1) {
  data$Gender <- 1
} else {
  data$Gender <- 0
  
}
data$Gender <- as.integer(as.character(data$Gender)=="Female")
data$Gender <-as.factor(data$Gender)

###########################

## 75%로 샘플 사이즈 변경하기 smp_size는 75% 로 실행
smp_size <- floor(0.75 * nrow(data))

## 난수 설정 (나눌때 중복된값이 들어가면 안된다 하여...)
set.seed(123)

## train_ind <- sample 인덴ㅇㄱ
train_ind <- sample(seq_len(nrow(data)), size = smp_size)


library(plyr)
library(dplyr)


train <- data[train_ind, ]
test <- data[-train_ind, ]

dim(train)

test_label <- test$ID
train_label <- train$ID

# test$Id <- NULL
# train$Id <- NULL

# test$Attrition <- NA

all <- rbind(train, test)
dim(all)

dim(train)
dim(test)
test

model <- glm(Attrition ~ JobSatisfaction + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + WorkLifeBalance, data = train, family = "binomial")
summary(model)


model <- glm(Attrition ~ JobSatisfaction + WorkLifeBalance, data = train, family = "binomial")

summary(model)


anova(model, test="Chisq")




p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$Attrition)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



install.packages("scales") 

library(scales)

s_data <-   data.frame(  data$Attrition,
                         data$JobSatisfaction , 
               data$PercentSalaryHike , 
               data$PerformanceRating ,
               data$RelationshipSatisfaction ,
               data$WorkLifeBalance)

aa <- data.frame(  data$Attrition,
             data$Gender
)

data$Gender
 
summary(s_data)

plot(formula = s_data$data.Attrition ~ s_data$data.JobSatisfaction,
     data = s_data,
     col = alpha(c("red", "gray"), 0.8)[s_data$data.Attrition],
     xlab = "만족도",
     ylab = "퇴사율",
     main = "만족도에 따른 퇴사율"
     )



legend("topleft", 
       legend = levels(s_data$data.Attrition),
       pch = 1,
       cex = 0.9,
       bty = "n")



# k-Fold Cross Validation

set.seed(9876)

library(tidyverse)
idx <- sample(x = c("train_valid", "test"),
              size = nrow(data),
              replace = TRUE,
              prob = c(7, 3))


train_valid <- data[idx == "train_valid", ]
test <- data[idx == "test", ]

test_x <- test[, -3]
test_y <- test[, 3]

library(scales)

plot(formula = train_valid$data.RelationshipSatisfaction  ~ train_valid$data.JobSatisfaction,
     data = s_data,
     col = alpha(c("red", "gray"), 0.8)[train_valid$Attrition],
     pch = 0,
     main = "퇴사 분포도",
     x_lab = "만족도",
     y_lab = "퇴사"
)



legend("topleft", 
       legend = levels(s_data$data.Attrition),
       pch = 1,
       cex = 0.9,
       bty = "n")



# EDA 

head(data, 10)
summary(data)

# 종속변수 탐색
table(data$Attrition)

ggplot(data = data) +
  geom_bar(aes(x = Attrition, fill = Attrition)) +
  labs(title = 'Distribution of Attrition')

# 독립변수 탐색
# unique(중복값 제거) 함수를 거친 벡터의 길이와, 원본 벡터의 길이를 비교합니다.
# TRUE 시 Unique 성질을 확인
length(unique(data$EmployeeNumber)) == length(data$EmployeeNumber)


# 범주형 JobSatisfaction

table(data$JobSatisfaction)
ggplot(data = data) + geom_bar(aes(x = JobSatisfaction, fill = JobSatisfaction))
with(data, table(Attrition, JobSatisfaction))

mosaicplot(JobSatisfaction~Attrition, data = data)

# 범주형이 아닌 데이터는 전처리를 통해 char 형으로 변환시켜준다

data$EmployeeNumber <- as.character(data$EmployeeNumber)
head(data$EmployeeNumber)

# 다른거 
table(data$Gender)
with(data, table(Gender, Attrition))

mosaicplot(Gender~Attrition, data = data)

# 서로 독립이라는 귀무가설을 기각하였습니다. 서로 연관성이 있다고 할 수 있습니다.?
with(data, chisq.test(Gender, Attrition))


a <- ggplot(data = data) +
  geom_histogram(aes(x = YearsAtCompany),binwidth = 10) +
  labs(title = 'a')
b <- ggplot(data = data) +
  geom_histogram(aes(x = YearsAtCompany),binwidth = 1) +
  labs(title = 'b')

grid.arrange(a, b, ncol = 2)

mean(data$YearsAtCompany)

data[1:split_num, ] %>%
  mutate(Year_group = cut(YearsAtCompany, c(seq(0, 80, by = 5)))) %>%
  group_by(Year_group) %>%
  summarise(Attrition = sum(as.numeric(Survived)-1, na.rm = T),
            count = n()) %>% #Survived에서 No : 1, Yes : 2의 값을 갖는다. 1을 빼주어 생존자의 비율만 찾는다.
  mutate(Attr_rat = Attrition / count) %>%
  ggplot() +
  geom_col(aes(x = Year_group, y = Attr_rat, fill = Year_group))




# Attrition
chisq.test(data$YearsAtCompany, data$Attrition)

chisq.test(data$YearsInCurrentRole, data$Attrition)
chisq.test(data$YearsSinceLastPromotion, data$Attrition)

mosaicplot(YearsAtCompany~Attrition, data = data)


# EDA 
Joblevel



https://www.kaggle.com/jrchun1/titanic-tutarial-with-r-basic
