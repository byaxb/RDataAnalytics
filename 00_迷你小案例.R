


# Data Import -------------------------------------------------------------
library(readr)
#默认将cjb.csv文件置于getwd()路径下的data文件夹中
#也可以改为绝对路径
#从R4.0之后，可以改为：
#cjb_url <- r'[D:\\desktop\data\cjb.csv]'
cjb_url <- "data/cjb.csv"
cjb <- read_csv(cjb_url,
                locale = locale(encoding = "CP936"))

View(cjb)


# Data Exploration --------------------------------------------------------

library(tidyverse)
cjb %>%
    dplyr::select(sx, wlfk) %>%
    ggplot(aes(x = wlfk,
               y = sx,
               fill = wlfk)) +
    geom_boxplot(width = 0.5)


# Data Preparation --------------------------------------------------------
as_five_grade_scores <- function(x) {
    cut(
        x,
        breaks = c(0, seq(60, 100, by = 10)),
        include.lowest = TRUE,
        right = FALSE,
        ordered_result = TRUE,
        labels = c("不及格", "及格", "中", "良", "优")
    )
}
cjb <- cjb %>%
    mutate(zcj = rowSums(.[4:12])) %>%
    filter(zcj != 0) %>% #剔除脏数据
    mutate_at(vars(xb, wlfk), factor) %>% #类型转换
    mutate_at(vars(yw:sw), as_five_grade_scores)#数据分箱
View(cjb)


# Model -------------------------------------------------------------------

library(arulesViz)
my_model <- cjb %>%
    select(xb:wlfk) %>%
    apriori(parameter = list(supp = 0.06, conf = 0.8),
            appearance = list(rhs = paste0("wlfk=", c("文科", "理科"))))


# Visualization -----------------------------------------------------------

inspectDT(my_model)
plot(my_model, method = "graph")
#当然，也可采用交互的方式
plot(my_model,
     method = "graph",
     engine = "htmlwidget")

# The End ^-^ -------------------------------------------------------------


