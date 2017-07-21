

#######################################################
#######################################################
##
## 名称：《R语言数据分析·关联规则》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V6
## 时间：2017年6月
##
##*****************************************************
##
## ch05_Association Rules_V6
## Data Analytics with R
## Instructed by Xinbo Ai
## Beijing University of Posts and Telecommunications
##
##*****************************************************
##
## Author: byaxb
## Email:axb@bupt.edu.cn
## QQ:23127789
##
##*****************************************************
##
## (c)2012~2017
##
#######################################################
#######################################################


#清空内存
rm(list = ls())
library(arules)
#加载数据
data(Titanic)
#查看数据结构
str(Titanic)
#查看具体数据
Titanic


#将列联表数据，展开成原始记录
#转变成数据框
Titanic_df <- as.data.frame(Titanic)
titanic_raw <- NULL
#注意：此处是按列进行伸展
for (i in 1:4) {
  titanic_raw <-
    cbind(titanic_raw, rep(as.character(Titanic_df[, i]), Titanic_df$Freq))
}
str(titanic_raw)
View(titanic_raw)
titanic_raw <- as.data.frame(titanic_raw)
names(titanic_raw) <- names(Titanic_df)[1:4]

#当然，也有更简单的办法
#利用epitools直接进行转换
library(epitools)
titanic_raw <- expand.table(Titanic)


#对转换后的数据进行简单描述
str(titanic.raw)
summary(titanic_raw)
library(Hmisc)
describe(titanic_raw)


#将titanic.raw转换成因子
titanic_raw <- as.data.frame(lapply(titanic_raw, factor))
str(titanic_raw)

#转换为transaction
titanic_trans <- as(titanic_raw, "transactions")
#查看数据
titanic_trans
inspect(titanic_trans)
#转换为矩阵
titanic_matrix <- as(titanic_trans, "matrix")
View(titanic_matrix)
#转换为列表
titanic_list <- as(titanic_trans, "list")
titanic_list
#以下代码中，titanic_raw可以替换成titanic_trans
#titanic_matirx, 或是titanic_list


library(arules)
irules_args_default <- apriori(titanic_raw)
inspect(irules_args_default)
#可以将规则转换成数据框
irules_args_default_in_df <- as(irules_args_default, "data.frame")
View(irules_args_default_in_df)
col1_without_braces <-
  gsub("[\\{\\}]", "", irules_args_default_in_df$rules)
left_and_right <-
  do.call("rbind", strsplit(col1_without_braces, split = " => "))
left_and_right <- as.data.frame(left_and_right)
names(left_and_right) <- c("LHS", "RHS")
irules_in_df <-
  cbind(left_and_right, irules_args_default_in_df[, -1])
#转换成data.frame之后
#自然可以随意处置了
#比如显示、或是另存为csv文件等
#也可以通过正则表达式任意抽取自己想要的规则
#请小伙伴们自行练习
View(irules_in_df)

#定制其中的参数
irules <- apriori(
  titanic_raw,
  parameter = list(
    minlen = 2,
    supp = 0.005,
    conf = 0.8
  ),
  appearance = list(rhs = paste0("Survived=", c("No", "Yes")),
                    default = "lhs"))
inspect(irules)

#也可以时候提取你想要的规则
inspect(subset(irules, subset = lhs %pin% "Age=" ))

#格式化规则评估指标
quality(irules) <- round(quality(irules), digits = 3)
#对规则进行排序
irules_sorted <- sort(irules, by = "lift")
inspect(irules_sorted)

#删除冗余规则
subset.matrix <-
  is.subset(irules_sorted, irules_sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
which(redundant)
irules_pruned <- irules_sorted[!redundant]
inspect(irules_pruned)



itemFrequency(titanic_trans, type = "relative")
itemFrequencyPlot(titanic_trans)


#生成频繁项集，而不是规则
itemsets <- apriori(titanic_raw,
                    parameter = list(
                      minlen = 2,
                      supp = 0.005,
                      target = "frequent itemsets"
                    ))
inspect(itemsets)

#从规则中提取频繁项集
itemsets <- unique(generatingItemsets(irules_pruned))
itemsets
inspect(itemsets)



#规则可视化
library(arulesViz)
plot(irules_pruned, method = "graph")#最常用的一种方式
plot(irules_pruned, method = "grouped")
plot(irules_pruned, method = "paracoord")

#交互式的规则可视化
sel <- plot(irules_pruned, method="graph", 
            interactive=TRUE)


#######################################################
##The End ^-^
#######################################################