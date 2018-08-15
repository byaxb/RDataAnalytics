

#######################################################
#######################################################
##
## 名称：《R语言数据分析·Mini Case》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V9
## 时间：2018年8月
##
##*****************************************************
##
## ch00_Mini Case_V9
## Data Analytics with R
## Instructed by Xinbo Ai
## Beijing University of Posts and Telecommunications
##
##*****************************************************
##
## Author: byaxb
## Email:axb@bupt.edu.cn
## QQ:23127789
## WeChat:13641159546
## URL:https://github.com/byaxb
##
##*****************************************************
##
## (c)2012~2018
##
#######################################################
#######################################################



#读取数据
library(readxl)
#请从github中下载cjb.xlsx文件
#在当前工作路径创建data子文件夹并将该excel文件置于其中
cjb <- readxl::read_excel("data/cjb.xlsx")
View(cjb) #如果使用的是RStudio，将在代码面板中展示
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
  cut(x, 
      breaks = c(0, seq(60, 100, by = 10)),
      include.lowest = TRUE, 
      right = FALSE,
      ordered_result = TRUE,
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
#模型评估：看规则的支持度、置信度和提升度
inspectDT(my_model)
#可视化
plot(my_model, method = "graph")
#当然，也可采用交互的方式
plot(my_model,
     method = "graph",
     engine = "htmlwidget")
