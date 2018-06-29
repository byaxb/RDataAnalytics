#读取数据
library(readxl)
cjb <- readxl::read_excel("data/cjb.xlsx")
View(cjb)
#对数据进行探索性分析
library(tidyverse)
cjb %>%
  select(sx, wlfk) %>%
  ggplot(aes(
    x = wlfk, 
    y = sx, 
    fill = wlfk)) +
  geom_boxplot(width = 0.5)
#数据预处理
as_five_grade_scores <- function(x) {
  cut(x, breaks = c(0, seq(60, 100, by = 10)),
      labels = c("不及格", "及格", "中", "良", "优"))
}
cjb <- cjb %>%
  mutate(zcj = rowSums(.[4:12])) %>%
  filter(zcj != 0) %>%#剔除脏数据
  mutate_at(vars(xb, wlfk), factor) %>% #类型转换
  mutate_at(vars(yw:sw), as_five_grade_scores)#数据分箱
View(cjb)
#建模
library(arulesViz)
my_model <- cjb %>%
  select(xb:wlfk) %>%
  apriori(parameter = list(supp = 0.06, conf = 0.8), 
  appearance = list(rhs = paste0("wlfk=", c("文科", "理科"))))
#模型评估
inspectDT(my_model)
#可视化
plot(my_model)
