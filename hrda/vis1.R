


# 전체 YES/NO

stack_Attrition <- raw %>% 
  mutate(Attrition = as.factor(Attrition)) %>% 
  count(Attrition) %>%
  ggplot(aes(x="", y=n, fill=Attrition)) +
  geom_bar(position = "stack", stat = "identity")

pie_Attrition <- raw %>% 
  mutate(Attrition = as.factor(Attrition)) %>% 
  count(Attrition) %>% 
  ggplot(aes(x="", y=n, fill=Attrition)) +
  geom_col() +
  coord_polar("y", start = 0)

gridExtra::grid.arrange(stack_Attrition, pie_Attrition)

# 1. 부서별 통계
# 1-1. 부서별 연령-성별-만족도




# 2. 인사적 background 환경 요인
# 2-1. 

