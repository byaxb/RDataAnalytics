
# test --------------------------------------------------------------------



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
