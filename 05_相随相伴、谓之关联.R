




# 05_相随相伴、谓之关联----------------------------------------------------

#在观察完数据的长相之后，便开始深入其内在的关系结构了
#本次实验聚焦的是关联规则
#关联规则所表达的联系，本质上是伴随关系
#因此，本章节名称为《相随相伴、谓之关联》

#教材上的名称频繁项集、关联规则
#关联规则可能是机器学习/数据挖掘领域最为知名的算法了
#啤酒和尿不湿的故事，提供了“发现数据背后意想不到的模式”的范本，
#也让关联规则成为数据挖掘最好的科（guang）普（gao）

# Data Import -------------------------------------------------------------

#清空内存
rm(list = ls())
#蛮力搜索可能的规则数
n_item <- c(2:5, 10, 20, 50, 100)
n_rules <- 3 ^ n_item - 2 ^ (n_item + 1) + 1
View(data.frame(n_item, n_rules))

library(tidyverse)
library(readr)
cjb_url <- "data/cjb.csv"
cjb <- read_csv(cjb_url,
                locale = locale(encoding = "CP936"))

# Discretization ----------------------------------------------------------

#数据离散化
#arules包只能对离散数据进行关联规则挖掘
#离散化有专用的包discretization
#当然，对于大部分的任务而言，
#cut()函数已经够用了
#定义一个百分制转成五分制成绩的函数
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

cjb %<>%
    mutate_at(vars(xb, wlfk), factor) %>% #类型转换
    mutate_at(vars(yw:sw), as_five_grade_scores) %>% #数据分箱
    select(-c(1:2))#姓名、班级两列不参与规则挖掘


# Types of data -----------------------------------------------------------

library(arules)
#转换为transaction
cjb_trans <- as(cjb, "transactions")
#查看数据
cjb_trans
#> transactions in sparse format with
#> 775 transactions (rows) and
#> 49 items (columns)

inspect(cjb_trans[1:5])
inspect(head(cjb_trans))




#转换为数据框
cjb_trans %>%
    as("data.frame") %>%
    View()
#转换为矩阵
cjb_trans %>%
    as("matrix") %>%
    View()
#转换为列表
cjb_trans %>%
    as("list") %>%
    head(n = 2)



#无论是列表、矩阵、数据框
#还是最直接的事务记录transactions
#都可以直接用来挖掘


# Model with default args -------------------------------------------------
#关于Apriori算法的原理，请参阅课程讲义
#R中的具体实现，则简单得超乎人们的想象
#首先是加载包
#对于关联规则的挖掘和可视化
#主要用arules和arulesViz两个包
#加载后者时，前者自动加载

library(arulesViz)
#调用apriori()函数进行挖掘
#算法实现，只是一句话的事儿
irules_args_default <- apriori(cjb_trans)
irules_args_default <- apriori(cjb)
? apriori

irules_args_default <- apriori(cjb, parameter = list(ext = TRUE))
quality(irules_args_default)


#看一看挖出来的规则
irules_args_default
#> set of 3775 rules

#查看具体的规则
inspect(head(irules_args_default))



# Rules information -------------------------------------------------------

#关于规则的一些基本信息
irules_args_default@info
#> $`data`
#> cjb_trans
#>
#> $ntransactions
#> [1] 775
#>
#> $support
#> [1] 0.1
#>
#> $confidence
#> [1] 0.8


# Parameters --------------------------------------------------------------

#定制其中的参数
#设置支持度、置信度、最小长度等
irules <- apriori(cjb_trans,
                  parameter = list(
                      minlen = 2,
                      supp = 50 / length(cjb_trans),
                      #最小支持度，减少偶然性
                      conf = 0.8 #最小置信度，推断能力
                  ))
length(irules)
inspectDT(irules)
#> [1] 8651


#也可以进一步设定前项和后项
irules <- apriori(
    cjb_trans,
    parameter = list(
        minlen = 2,
        supp = 50 / length(cjb_trans),
        conf = 0.8
    ),
    appearance = list(rhs = paste0("wlfk=", c("文科", "理科")),
                      default = "lhs")
)
inspectDT(irules)
#对规则进行排序
irules_sorted <- sort(irules, by = "lift")
inspectDT(irules_sorted)



# Pruned Rules ------------------------------------------------------------

subset.matrix <-
    is.subset(irules_sorted, irules_sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA

View(subset.matrix)
redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
as.integer(which(redundant))


(irules_pruned <- irules_sorted[!redundant])
#> set of 107 rules
inspect(irules_pruned)
inspectDT(irules_pruned)
#当然，很多时候，我们只想查看其中部分规则
inspect(head(irules_pruned))
inspect(tail(irules_pruned))


# Model Evaluation --------------------------------------------------------

#查看评估指标
quality(irules_pruned)


str(quality(irules_pruned))


#更多评估指标
(
    more_measures <- interestMeasure(
        irules_pruned,
        measure = c("support", "confidence", "lift", "casualConfidence"),
        transactions = cjb_trans
    )
)

quality(irules_pruned) <- more_measures %>%
    mutate_at(vars(1:3),
              funs(round(., digits = 2)))



# Rules Filtering ---------------------------------------------------------

#比如仅关心文科相关的规则
irules_sub <- subset(irules_pruned,
                     items %in% c("wlfk=文科"))
inspect(irules_sub)
inspectDT(irules_sub)

irules_sub <- subset(irules_pruned,
                     items %pin% c("文科"))
inspectDT(irules_sub)
#当然也可以同时满足多种搜索条件
#比如性别和确信度
irules_sub <- subset(irules_pruned,
                     lhs %pin% c("sw") &
                         lift > 1.8)
inspectDT(irules_sub)
inspect(irules_sub)
#> lhs                                      rhs         support confidence
#> [1]  {xb=男,sx=优,ls=优,wl=优,hx=优,sw=优} => {wlfk=理科} 0.074   0.93
#> [2]  {xb=男,sx=优,dl=优,wl=优,hx=优,sw=优} => {wlfk=理科} 0.090   0.93
#> [3]  {xb=男,dl=优,wl=优,hx=优,sw=优}       => {wlfk=理科} 0.099   0.93
#> [4]  {xb=男,sx=优,wl=优,hx=优,sw=优}       => {wlfk=理科} 0.092   0.92
#> [5]  {xb=男,sx=优,dl=优,wl=优,sw=优}       => {wlfk=理科} 0.090   0.92
#> [6]  {xb=男,sx=优,ls=优,wl=优,sw=优}       => {wlfk=理科} 0.074   0.92
#> [7]  {xb=女,zz=优,sw=中}                   => {wlfk=文科} 0.070   0.95


# Frequent Itemsets -------------------------------------------------------

#从规则中提取频繁项集
itemsets <- unique(generatingItemsets(irules_pruned))
itemsets
#> set of 107 itemsets
itemsets_df <- as(itemsets, "data.frame")
View(itemsets_df)
inspect(itemsets)

#反过来，先挖掘频繁项集
#再导出关联规则
#生成频繁项集，而不是规则
itemsets <- apriori(cjb_trans,
                    parameter = list(
                        minlen = 2,
                        supp = 50 / length(cjb_trans),
                        target = "frequent itemsets"
                    ))
inspect(itemsets)
irules_induced <- ruleInduction(itemsets,
                                cjb_trans,
                                confidence = 0.8)
irules_induced
#> set of 8651 rules

#显然，只要参数是一样的
#得到规则条数也是一样的

#1-项集的频繁程度
itemFrequency(cjb_trans, type = "relative")
itemFrequencyPlot(cjb_trans)
#当然我们更愿意统一成ggplot2的风格
item_freq <- itemFrequency(cjb_trans, type = "relative")
library(tidyverse)
item_freq %>%
    as.data.frame %>%
    rownames_to_column(var = "item") %>%
    mutate(item = factor(item, levels = item)) %>%
    ggplot(aes(x = item, y = item_freq, fill = item_freq)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x  = element_text(
        angle = 60,
        vjust = 1,
        hjust = 1
    ))
#保留现有的因子水平，也有下述方法
item_freq %>%
    as.data.frame %>%
    rownames_to_column(var = "item") %>%
    mutate(item = forcats::fct_inorder(item)) %>%
    ggplot(aes(x = item, y = item_freq, fill = item_freq)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x  = element_text(
        angle = 60,
        vjust = 1,
        hjust = 1
    ))


# Rules Viz ---------------------------------------------------------------

library(arulesViz)
plot(irules_pruned[1:10],
     method = "graph")#最常用的一种方式
plot(irules_pruned, method = "grouped")
plot(irules_pruned, method = "paracoord")

#交互式的规则可视化
library(tcltk2)
plot(irules_pruned,
     method = "graph",
     interactive = TRUE)


# Rules Export ------------------------------------------------------------

#这些规则怎么保存呢？
#当然可以console输出之后复制、或是截图，
#但效果并不好
#稍微好一点的办法是直接将console的结果捕获
out <- capture.output(inspect(irules_pruned))
out
writeLines(out, con = "Rules.txt")

save(irules_pruned,
     file = "rules.rda")

#更好的办法，应该是将规则转换成数据框
#然后另存为csv文件
irules_pruned_in_df <-
    as(irules_pruned, "data.frame")
View(irules_pruned_in_df)
#考虑到规则中也包含逗号,
#在另存为csv文件时，一般需要设置参数quote=TRUE
write.csv(
    irules_pruned_in_df,
    file = "Rules.csv",
    quote = TRUE,
    row.names = FALSE
)
#当然，在另存为csv之前，也可以对规则进行必要的处理
irules_pruned_in_df %<>%
    separate(rules,
             sep = "=>",
             into = c("LHS", "RHS")) %>%
    mutate_at(vars("LHS", "RHS"),
              funs(gsub("[\\{\\} ]", "", .)))
View(irules_pruned_in_df)

#转换成data.frame之后
#自然可以随意处置了
#比如可以通过正则表达式任意抽取自己想要的规则
#请小伙伴们自行练习
#当然，arules包中write()函数也可以将规则直接写到本地
write.csv(
    irules_pruned_in_df,
    file = "Rules2.csv",
    quote = TRUE,
    row.names = FALSE
)

#以上是R中关于关联规则的基本实现
#感兴趣的同学，可以进一步阅读：
#序列模式arulesSequences等主题
#当然，即便是关联规则，arules当然使用最多
#但也并非是唯一的选择，比如RKEEL等均可尝试


# The End ^-^ -------------------------------------------------------------
