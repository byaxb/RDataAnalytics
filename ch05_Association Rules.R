

#######################################################
#######################################################
##
## 名称：《R语言数据分析·关联规则》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V7
## 时间：2017年9月
##
##*****************************************************
##
## ch05_Association Rules_V7
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
## (c)2012~2017
##
#######################################################
#######################################################


#在观察完数据的长相之后，便开始深入其内在的关系结构了
#本次实验聚焦的是伴随关系
#教材上的名称频繁项集、关联规则
#关联规则可能是机器学习/数据挖掘领域最为知名的算法了
#啤酒和尿不湿的故事，提供了“发现数据背后意想不到的模式”
#的范本，也让关联规则成为数据挖掘最好的
#科（guang）普（gao）

#######################################################
##数据读取与类型转换
#######################################################
#清空内存
rm(list = ls())
load("cj.rda", verbose = TRUE)
#对数据进行简单描述
str(cheng_ji_biao)
summary(cheng_ji_biao)
View(cheng_ji_biao)
library(Hmisc)
describe(cheng_ji_biao)

#数据离散化
#arules包只能对离散数据进行关联规则挖掘
#离散化有专用的包discretization
#当然，对于大部分的任务而言，
#cut()函数已经够用了
for (i in 4:12){
  cheng_ji_biao[,i]<- cut(cheng_ji_biao[,i],
                          breaks = c(0, 60, 70, 80, 90, 100),
                          labels = c("不及格", "及格", "中", "良", "优"),
                          include.lowest = FALSE, 
                          right = FALSE,
                          ordered_result = TRUE)
  #对于形如[a,b)的方式，也可以用Hmisc中的cut2()函数
  #请小伙伴自行实现
}
View(cheng_ji_biao)
cheng_ji_biao <- cheng_ji_biao[, -c(1:2, ncol(cheng_ji_biao))]

#对转换后的数据进行简单描述
str(cheng_ji_biao)
summary(cheng_ji_biao)
library(Hmisc)
describe(cheng_ji_biao)

library(arules)
#转换为transaction
cjb_trans <- as(cheng_ji_biao, "transactions")
#查看数据
cjb_trans
inspect(cjb_trans)
#转换为矩阵
cjb_matrix <- as(cjb_trans, "matrix")
View(cjb_matrix)
#转换为列表
cjb_list <- as(cjb_trans, "list")
cjb_list
#无论是列表、矩阵、数据框
#还是最直接的事务记录transactions
#都可以直接用来挖掘

#######################################################
##关联规则挖掘
#######################################################
#关于Apriori算法的原理，请参阅课程讲义
#R中的具体实现，则简单得超乎人们的想象
#首先是加载包
#对于关联规则的挖掘和可视化
#主要用arules和arulesViz两个包
#加载后者时，前者自动加载
library(arulesViz)
#调用apriori()函数进行挖掘
irules_args_default <- apriori(cjb_trans)
#看一看挖出来的规则
irules_args_default
#关于规则的一些基本信息
irules_args_default@info
#查看具体的规则
inspect(irules_args_default)


######################################################
##参数设定
#######################################################
#定制其中的参数
#设置支持度、置信度、最小长度等
irules <- apriori(
  cjb_trans,
  parameter = list(
    minlen = 2,
    supp = 100 / length(cjb_trans), #最小支持度，减少偶然性
    conf = 0.8 #最小置信度，推断能力
  ))

#也可以进一步设定前项和后项
irules <- apriori(
  cjb_trans,
  parameter = list(
    minlen = 2,
    supp = 50 / length(cjb_trans),
    conf = 0.8
  ),
  appearance = list(rhs = paste0("文理分科=", c("文科", "理科")),
                    default = "lhs"))

#对规则进行排序
irules_sorted <- sort(irules, by = "lift")
inspect(irules_sorted)


#######################################################
##删除冗余规则
#######################################################
subset.matrix <-
  is.subset(irules_sorted, irules_sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
as.integer(which(redundant))
# [1]   4   7   8   9  10  15  18  23  24  26  30  31  32  35  36  37  38  39  40  41  43  44  47  48  49  50  54  55  56
# [30]  57  59  63  64  65  68  72  73  74  75  76  77  78  80  82  83  84  87  89  90  92  95  96  97 100 101 102 103 104
# [59] 108 109 110 111 112 113 114 117 118 123 126
(irules_pruned <- irules_sorted[!redundant])
# set of 57 rules 
inspect(irules_pruned)


#######################################################
##评估指标
#######################################################
#查看评估指标
quality(irules_pruned)
#更多评估指标
(more_measures <- interestMeasure(irules_pruned,
                                  measure = c("conviction",
                                              "casualConfidence", 
                                              "mutualInformation"),
                                  transactions = cjb_trans))
#增加评估指标
quality(irules_pruned) <- cbind(quality(irules_pruned),
                         more_measures)
#格式化规则评估指标
View(quality(irules_pruned))
#小数点后三位
quality(irules_pruned) <- round(quality(irules_pruned), digits = 5)
View(quality(irules_pruned))


#######################################################
##规则搜索
#######################################################
#比如仅关心文科相关的规则
irules_sub1 <- subset(irules_pruned,
                      items %in% c("文理分科=文科"))
irules_sub2 <- subset(irules_pruned,
                      items %pin% c("文科"))
#当然也可以同时满足多种搜索条件
#比如性别和确信度
irules_sub3 <- subset(irules_pruned, 
                      lhs %pin% c("性别") &
                        conviction > 3)
inspect(irules_sub3)

#######################################################
##频繁项集与关联规则
#######################################################
#从规则中提取频繁项集
itemsets <- unique(generatingItemsets(irules_pruned))
itemsets
# set of 57 itemsets
itemsets_df <- as(itemsets, "data.frame")
View(itemsets_df)
inspect(itemsets)

#反过来，先挖掘频繁项集
#再导出关联规则
#生成频繁项集，而不是规则
itemsets <- apriori(cjb_trans,
                    parameter = list(
                      minlen = 2,
                      supp = 100 / length(cjb_trans),
                      target = "frequent itemsets"
                    ))
inspect(itemsets)
irules_induced <- ruleInduction(itemsets, 
                                cjb_trans,
                                confidence = 0.8)
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
  theme(axis.text.x  = element_text(angle=60, vjust=1, hjust = 1))
#保留现有的因子水平，也有下述方法
item_freq %>%
  as.data.frame %>%
  rownames_to_column(var = "item") %>%
  mutate(item = forcats::fct_inorder(item)) %>%
  ggplot(aes(x = item, y = item_freq, fill = item_freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x  = element_text(angle=60, vjust=1, hjust = 1))



#######################################################
##规则可视化
#######################################################
library(arulesViz)
plot(irules_pruned, method = "graph")#最常用的一种方式
plot(irules_pruned, method = "grouped")
plot(irules_pruned, method = "paracoord")

#交互式的规则可视化
library(tcltk2)
plot(irules_pruned, 
            method="graph", 
            interactive=TRUE)


#######################################################
##规则的导出与保持
#######################################################
#这些规则怎么保存呢？
#当然可以console输出之后复制、或是截图，
#但效果并不好
#稍微好一点的办法是直接将console的结果捕获
out <- capture.output(inspect(irules_pruned))
out
writeLines(out, con = "Rules.txt")
#更好的办法，应该是将规则转换成数据框
#然后另存为csv文件
irules_pruned_in_df <- as(irules_pruned, "data.frame")
View(irules_pruned_in_df)
#考虑到规则中也包含逗号,
#在另存为csv文件时，一般需要设置参数quote=TRUE
write.csv(irules_pruned_in_df, 
          file = "Rules.csv",
          quote = TRUE,
          row.names = FALSE)
#当然，在另存为csv之前，也可以对规则进行必要的处理
col1_without_braces <-
  gsub("[\\{\\}]", "", irules_pruned_in_df$rules)
left_and_right <-
  do.call("rbind", strsplit(col1_without_braces, split = " => "))
left_and_right <- as.data.frame(left_and_right)
names(left_and_right) <- c("LHS", "RHS")
irules_in_df <-
  cbind(left_and_right, irules_pruned_in_df[, -1])
View(irules_in_df)
#转换成data.frame之后
#自然可以随意处置了
#比如可以通过正则表达式任意抽取自己想要的规则
#请小伙伴们自行练习
#当然，arules包中write()函数也可以将规则直接写到本地
write(irules_pruned, 
      file="Rules2.csv", 
      sep=",", 
      quote=TRUE,
      row.names=FALSE)  


#以上是R中关于关联规则的基本实现
#感兴趣的同学，可以进一步阅读：
#序列模式arulesSequences等主题
#当然，即便是关联规则，arules当然使用最多
#但也并非是唯一的选择，比如RKEEL等均可尝试

#######################################################
##The End ^-^
#######################################################
