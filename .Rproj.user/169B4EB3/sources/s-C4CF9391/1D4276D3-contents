# dashboard.r

# 그래프-부서별 인원수

install.packages("ggplot2")
library(ggplot2)
library(Mass)

count.gender <- ggplot(data = raw, aes(x = Gender)) + geom_bar(fill="#F8766D") +
  labs(title = "EmployeeCount by Gender")

count.dp <- ggplot(data = raw, aes(x = Department, y = EmployeeCount)) + geom_col(colour="#F8766D") +
  labs(title = "EmployeeCount by Department")

