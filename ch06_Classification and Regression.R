

#######################################################
#######################################################
##
## 名称：《R语言数据分析·分类与回归》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V6
## 时间：2017年6月
##
##*****************************************************
##
## ch06_Classification and Regression_V6
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



#######################################################
##读入数据并进行观察
#######################################################

rm(list = ls())
#读入数据
ctg <- read.csv("https://raw.githubusercontent.com/byaxb/RDataAnalytics/master/data/ctg.csv")
#ctg数据集也可以从以下网站下载：
#http://archive.ics.uci.edu/ml/machine-learning-databases/00193/CTG.xls
#由于是excel数据，可以通过下述方法读取
library(readxl)
ctg_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00193/CTG.xls"
#将文件download至本地
download.file(ctg_url, "ctg.xls", mode="wb")
#读取数据文件中，比较干净的那一部分
ctg <- read_xls("ctg.xls",
                sheet = "Data",
                range = "K2:AT2128")[, c(1:21, 34, 36)]
#至于为何range = "K2:AT2128")
#并且只选取[, c(1:21, 34, 36)]这些列
#小伙伴可打开文件进行对照
shell("ctg.xls")


#首先查看数据的结构
str(ctg)
# LB, FHR基线（心跳数/分）
# AC, 每秒钟加速数目
# FM, 每秒钟胎儿动作数
# UC, 每秒钟子宫收缩数目
# DL, 每秒钟轻微减速数目
# DS, 每秒钟严重减速数目
# DP, 每秒钟持续减速数目
# ASTV, 短期变异的时间百分比
# MSTV, 短期变异的平均值
# ALTV, 长期变异的时间百分比
# MLTV, 长期变异的平均值
# Width, FHR直方图的宽度
# Min, FHR直方图最小值
# Max, FHR直方图最大值
# Nmax, FHR直方图峰值数
# Nzeros, FHR直方图零值数
# Mode, FHR直方图众数
# Mean, FHR直方图均值
# Median, FHR直方图中位数
# Variance, FHR直方图方差
# Tendency, FHR直方图趋势
# CLASS, FHR分类代码
# NSP, 胎儿状态分类代码
#N=normal; S=suspect; P=pathologic
#更详尽的解释，请执行以下代码：
browseURL("http://archive.ics.uci.edu/ml/datasets/Cardiotocography")

#可以看出，ctg数据集共有23个变量
#其中，前22个变量为自变量
#第23个变量NSP为胎儿状态分类代码

#先做必要的数据转换
#把用整数代表的分类变量水平转换成factor
ctg$CLASS <- factor(ctg$CLASS)
ctg$NSP <- factor(ctg$NSP,
                  levels = c(3, 2, 1),
                  labels = c("病态", "疑似", "正常"))
ctg$Tendency <- factor(ctg$Tendency,
                       levels = c(-1, 0, 1),
                       labels = c("左不对称", "对称", "右不对称"))

#处理完数据之后，
#通常做法是把它另存为rda文件，
#方便日后处理
#免得以后每次都需要进行数据下载/转换等操作
#save(ctg, file = "ctg.rda")

#以后，每次只需要执行以下代码即可
rm(list = ls())
load("ctg.rda")

levels(ctg$Tendency)
View(ctg)

#观察数据
summary(ctg)

#进一步对数据进行描述
library(Hmisc)
describe(ctg)

#通过describe()的结果，可以看出，
#数据几乎没有缺失
#也可以进一步验证一下：
which(!complete.cases(ctg))
#输出结果为：integer(0)
#遇有缺失的数据集，
#可通过以下扩展包观察其缺失模式
library(mice)
View(md.pattern(ctg))


#从describe()结果可以看得出来
#其中一个显著的问题是：
#类不平衡
#用下边的图形来表达则更加直观
library(ggplot2)
ggplot(ctg, aes(x = NSP, fill = NSP)) +
  geom_bar(width = 0.25) +
  geom_text(aes(label = ..count.., y=..count..+30), stat="count")
ggplot(ctg, aes(x = NSP, fill = NSP)) +
  geom_bar(width = 1) +
  geom_text(aes(label = ..count.., y=..count..+30), stat="count")+
  coord_polar()

#里边也有很多数据取值非常之少
#（describe中的unique数量）
#比如：
table(ctg$Nzeros)

#比如，DS每秒钟严重减速数目
#绝大部分情况下取值为零
table(ctg$NSP, c("取值为零", "取值非零")[(ctg$DS != 0) + 1])
#         取值非零 取值为零
# 正常        1     1654
# 疑似        0      295
# 病态        6      170
#找出这7条具体的记录
View(ctg[ctg$DS != 0, c("DS", "NSP")])
#正常婴儿中，取值不为零的，只1个(1/7)
#病态婴儿中，取值不为零的，有6个(6/7)
#由此可见，ctg$DS极少出现不为零的情况，
#一旦不为零，出现病态的可能性极大
#当然，由于历史上总共才出现7次这种现象，
#我们并没有足够的证据来作出相应的推断
#在某种程度上讲，DS这个属性/特征对于后续建模来讲，
#用途不大

#那哪些特征，对于我们最后的分类是有效的呢？
#一般来讲，特征的选取，要与具体的问题情境相结合，
#究竟选择哪些属性特征，要籍助领域专家的帮助

#就数据本身而言，也有很多工具进行特征的分析，
#比如caret包中的featurePlot,
#和plotluck包中的plotluck
library(caret)
featurePlot(
  x = ctg[, 1:20],
  y = ctg$NSP,
  plot = "density",
  ## Pass in options to xyplot() to
  ## make it prettier
  scales = list(
    x = list(relation = "free"),
    y = list(relation = "free")
  ),
  adjust = 1.5,
  pch = "|",
  auto.key = list(columns = 3))
#假如觉得图太小，看不清楚的话，可以分组观测
featurePlot(
  x = ctg[, 17:20], #分四组观测：1:4、5:8、9:12、13:16、17:20
  y = ctg$NSP,
  plot = "density",
  ## Pass in options to xyplot() to
  ## make it prettier
  scales = list(
    x = list(relation = "free"),
    y = list(relation = "free")
  ),
  adjust = 1.5,
  pch = "|",
  auto.key = list(columns = 3))

#第21、22列乃是类别变量
#看看相应的马赛克图
library(devtools)
devtools::install_github("stefan-schroedl/plotluck")
library(plotluck)
plotluck(ctg, NSP~Tendency + CLASS)
#通过这个马赛克图可以看出：
#红色部分，也就是病态，集中于下部和左部
#并且可以看出，CLASS是决定性的
#通过CLASS和Tendency，几乎就可以把病态/疑似/正常进行清晰划分
#当CLASS取值为8、9时，几乎全是病态
#当CLASS取值为5、10，且Tendency为左不对称时，也几乎全是病态

#当然，其实把所有变量喂给plotluck函数也是可以的
plotluck(ctg, NSP~.)
#这个图所包含的大量信息，请小伙伴们细细玩味

#通过前述分析，小伙伴基本应该有一个概念：
#对于分类回归问题，观察数据分布时，
#通常需要观察不同因变量，相对于不同自变量取值时的数据分布
#考察其分类的潜力


#######################################################
##模型评估
#######################################################

#在建模之前就说模型评估，
#仿佛为时过早
#实际上，模型评估和模型建立是同等重要的
#道理很简单：
#All models are wrong, but some are useful~
#模型之所以称之为模型，就是因为它只不过是近似、逼近而已
#机器学习，无非是在我们已知的模型集里边，
#找到那个最逼近的而已

#有别于传统统计看p值、看统计的显著性
#机器学习/数据挖掘领域的模型评估
#主要是看实际效果
#看模型在实际数据上的性能指标
#通常的做法是：把数据分为训练集和测试集
#在训练集上训练、或者说让机器学习出一个模型
#然后在测试集上看其具体的性能指标：如正确率

#具体而言，有三种做法：
#1、留出法hold out:
#将数据集一分为二，一般是训练集70%，测试集50%
#2、交叉验证法cross validation:
#将数据分为k折，每一次都用当前折一折作为测试集
#其余的k-1折作为训练集，
#最后通过k折测试集上性能指标的平均值作为模型
#最终的性能指标
#显然，交叉验证比留出法更加稳定
#3、自助法out-of-bag
#主要是应用于组合学习之中，进行有放回抽样时，
#有36%左右的数据抽取不到，这些数据天然作为
#测试集
#从这三种方法的描述可以看出，交叉验证法
#是适用范围最广的方法

#下边看看如何得到这k折数据
#从前边的describe()可以看出，
#我们所面临的问题：类不平衡
#因此，在进行k折交叉检验时，
#也要采用分层抽样的方法
#什么是分层抽样？就是一锅八宝粥，尽量搅和匀
#这样，每一次崴一勺，都具有代表性
#具体怎么做呢？
#第1步：把不同标签的下标都取出来
#第2步：分别将这些下标分成k份
#第3步：将他们合在一起
#第1步：
idx_set <- list()#用来存放不同取值水平的下标
for(curLevel in levels(ctg$NSP)) {
  idx_set[[curLevel]] <- which(ctg$NSP == curLevel)
}
#显然，idx_set包含正常、意思、病态三个组成部分，
#每个组成部分分别是这三类记录的下标
#第2步：
k <- 5 #人为设定k值
k_fold_idx <- list()#用以存放每一折的下标
n_kfold <- sapply(idx_set, function(x) {#计算出每一个水平每一折的长度
  floor(length(x) / k)
})
for(i in 1:(k-1)) {
  idx_cur_fold <- NULL #当前折一折的下标
  for(j in 1:length(idx_set)) {
    tmp_sample <- sample(idx_set[[j]], n_kfold[j]) #抽样
    idx_set[[j]] <- setdiff(idx_set[[j]], tmp_sample) #抽到的下标，从下标集中剔除
    idx_cur_fold <- c(idx_cur_fold, tmp_sample)#这里隐藏着第3步：将他们合而为一
  }
  k_fold_idx[[i]] <- idx_cur_fold 
}
k_fold_idx[[k]] <- unlist(idx_set,use.names = FALSE)

#咱们接下来验证一下，是不是每一折里边，
#成分比例和总体是一致的
level_proportion_list <- lapply(k_fold_idx, function(x) {
  table(ctg[x, "NSP"])
})
View(do.call(rbind, level_proportion_list))

#当然，在caret里边，
#早就做了类似的动作
library(caret)
k_fold_idx <- createFolds(ctg$NSP, 
                         k = 5, 
                         list = TRUE, 
                         returnTrain = FALSE)
level_proportion_list <- lapply(k_fold_idx, function(x) {
  table(ctg[x, "NSP"])
})
View(do.call(rbind, level_proportion_list))


#对于类不平衡问题，仅仅用正确率/错误率来评价是不够的
#比如：10000人中，有10个人得SARS。现在不采用任何模型，
#只是用众数进行预测，也就是判定所有人都不得SARS，
#此时模型的正确率为(10000 - 10) / 10000 = 99.90%，
#正确率达到99.9%，然而，这种预测没有任何意义
#故此，还需要引入召回率Recall和Precision，
#以及二者的调和平均数F1值
#关于这些指标的具体含义，请参阅课程讲义
#或是其他相关书籍

#就实现而言，我们当然可以用简单的table()进行计算
#不过，MLmetrics和caret包早就已经实现了
#我们就不必要去重复造轮子了
#以下是实现过程
#采用MLmetrics扩展包
global_performance <- NULL
imetrics <- function(method, type, yhat, y, positive = "病态") {
  library(MLmetrics)
  curMetrics <- c(
    Method = method, #所采用的方法
    Type = type,
    Accuracy = Accuracy(y_true = y, y_pred = yhat), #正确率
    Recall = Recall(y_true = y, y_pred = yhat, positive = positive), #召回率
    Precision = Precision(y_true = y, y_pred = yhat, positive = positive), #精确率
    F1_Score = F1_Score(y_true = y, y_pred = yhat, positive = positive)
    ) #F1值：即召回率和精确率的调和平均数
  global_performance <<- rbind(global_performance, curMetrics)
}



#采用caret扩展包
imetrics <- function(method, type, yhat, y, positive = "病态") {
  con_table <- table(yhat, y)
  curMetrics <- c(
    Method = method, #所采用的方法
    Type = type,
    Accuracy = sum(diag(con_table)) / sum(con_table), #正确率
    Recall = caret::recall(con_table, relevant = positive), #召回率，注意：设置beta=Inf返回错误结果
    #Recall = caret::F_meas(con_table, beta = Inf), #召回率，注意：设置beta=Inf返回错误结果
    Precision = caret::F_meas(con_table, beta = 0), #精确率，当然也可以用caret::precision()
    F1_Score = caret::F_meas(con_table, beta = 1)) #F1值：即召回率和精确率的调和平均数
  global_performance <<- rbind(global_performance, curMetrics)
}
#小伙伴可以对照一下，二者结果是否一致



#######################################################
##决策树
#######################################################
#关于（决策树）算法原理的阐述，
#请参阅PPT讲义，在此不再赘述

#了解一下什么是熵、基尼指数、分类错误率
p1 <- seq(0.001, 0.999, len = 1000)
p2 <- 1 - p1
#熵
entropy <- -p1*log2(p1) - p2*log2(p2)
#基尼指数
gini <- 1- (p1^2 + p2^2)
max(gini)
#分类错误率
classerr <- 1- apply(data.frame(p1, p2), 1, max)
plot(0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
lines(p1, entropy, col = "blue", lty = 2)
lines(p1, gini, col = "red", lty = 2)
lines(p1, classerr)
abline(h = 0, lwd = 2)
text(0.25, 1, label = "Entropy", col = "blue")
text(0.25, max(gini), label = "Gini", col = "red")
text(0.25, 0.25, label = "Classification Error")

calEntropy <- function(prob) {
  sum(sapply(prob, function(x) -x*log2(x)))
}
root.en <- calEntropy(c(4/9, 5/9))
child <- 4/9*calEntropy(c(3/4, 1/4)) + 5/9*calEntropy(c(1/5, 4/5)) 
root.en - child

#决策树的构建
library(rpart.plot)
(m_tree <- rpart(NSP~., ctg))
#绘制决策树的基本方法
plot(m_tree)
text(m_tree)

#终极大法，当属rpart.plot的使用
split.fun <- function(x, labs, digits, varlen, faclen) {
  labs <- paste0(substring(labs, 1, 20), "...等")
  labs
}

rpart.plot(m_tree, type=4, fallen=T, branch=.5, round=0, 
           leaf.round=6, clip.right.labs=F, cex = 0.75,
           under.cex=0.75, box.palette="GnYlRd", prefix="胎儿状态\n", 
           branch.col="gray",  branch.lwd=2, extra=101, under=T, 
           lt=" < ",  ge=" >= ", split.cex=0.85,
           split.fun=split.fun)


#对决策树进行剪枝
opt <- which.min(m_tree$cptable[,"xerror"])
cp <- m_tree$cptable[opt, "CP"]
m_tree_pruned <- prune(m_tree, cp = cp)
print(m_tree_pruned)
rpart.plot(m_tree_pruned, type=4, fallen=T, branch=.5, round=0, 
           leaf.round=6, clip.right.labs=F, cex = 0.75,
           under.cex=0.75, box.palette="GnYlRd", prefix="胎儿状态\n", 
           branch.col="gray",  branch.lwd=2, extra=101, under=T, 
           lt=" < ",  ge=" >= ", split.cex=0.85,
           split.fun=split.fun)


#进行k-折交叉检验k-fold cross validation
library(rpart)
for(i in 1:length(k_fold_idx)) {
  #获取当前折一折
  curr_fold <- k_fold_idx[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- ctg[-curr_fold, ] #训练集
  test_set <- ctg[curr_fold, ] #测试集
  #建模
  m_tree_k_fold <- rpart(NSP~., train_set) #模型训练
  #在训练集上做模型评估
  yhat_train <- predict(m_tree_k_fold, train_set, type = "class")
  imetrics("rpart", "Train", yhat_train, train_set$NSP)
  #在测试集上做模型评估
  yhat_test <- predict(m_tree_k_fold, test_set, type = "class")
  imetrics("rpart", "Test", yhat_test, test_set$NSP)
}



#######################################################
##近邻法
#######################################################
library(kknn)
set.seed(1)
m_knn <- kknn(NSP~., train = ctg, test = ctg)
knn_presults <- m_knn$fit
(knn_performance <- table(ctg$NSP, knn_presults))
sum(diag(knn_performance)) / sum(knn_performance)


(train_con <- train.kknn(NSP~., data = ctg, 
                         kmax = 25, kernel = c("rectangular", "triangular",  
                                               "epanechnikov", "gaussian", "rank", "optimal")))
#通过图形来展示k值及kernel的选择
plot(train_con, main = "K及Kernel的选择")
#小伙伴们肯定觉得上边的图形不好看
#好吧，改用ggplot2
str(train_con)
View(train_con$MISCLASS)
train_con_misclass <- as.data.frame(train_con$MISCLASS)
train_con_misclass$k <- 1:nrow(train_con_misclass)
library(tidyr)
train_con_misclass_gather <- gather(train_con_misclass, "k")
names(train_con_misclass_gather) <- c("k", "kernel", "misclass")
ggplot(train_con_misclass_gather, aes(x = k, y = misclass, group = kernel, colour = kernel)) +
  geom_point(size = 2) +
  geom_line() +
  geom_abline(slope = 0, 
              intercept = min(train_con_misclass_gather$misclass), 
              size = 2, 
              colour = "blue",
              alpha = 0.25)+
  theme(legend.position = c(0.2, 0.8))


train_con$best.parameters$kernel
train_con$best.parameters$k

m_knn_xp <- kknn(NSP~., train = ctg, test = ctg, 
                 k = train_con$best.parameters$k, 
                 kernel = train_con$best.parameters$kernel)
knn_presults_xp <- m_knn_xp$fit
(knn_performance_xp <- table(ctg$NSP,
                             knn_presults_xp))
sum(diag(knn_performance_xp)) / sum(knn_performance_xp)



#进行k-折交叉检验k-fold cross validation
library(kknn)
sp <- Sys.time() #记录开始时间
cat("\n[Start at:", as.character(sp))
for(i in 1:length(k_fold_idx)) {
  #获取当前折一折
  curr_fold <- k_fold_idx[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- ctg[-curr_fold, ] #训练集
  test_set <- ctg[curr_fold, ] #测试集
  #在训练集上做训练和评估
  m_kknn_k_fold_train <- kknn(NSP~., train = train_set, test = train_set, 
                        k = train_con$best.parameters$k, 
                        kernel = train_con$best.parameters$kernel)
  yhat_train <- m_kknn_k_fold_train$fit
  imetrics("kknn", "Train", yhat_train, train_set$NSP)
  #在训练集上做训练和评估
  m_kknn_k_fold_test <- kknn(NSP~., train = train_set, test = test_set, 
                              k = train_con$best.parameters$k, 
                              kernel = train_con$best.parameters$kernel)
  yhat_test <- m_kknn_k_fold_test$fit
  imetrics("kknn", "Test", yhat_test, test_set$NSP)
}
ep <- Sys.time()
cat("\tFinised at:", as.character(ep), "]\n")
cat("[Time Ellapsed:\t",
    difftime(ep, sp, units = "secs"),
    " seconds]\n")

# #除了前述的for循环，
# #对于k折交叉检验，也可以采用并行计算模式
# library(foreach)
# library(doParallel)
# cl <- makeCluster(detectCores())
# registerDoParallel(cl, cores = detectCores())
# clusterExport(cl, 
#               varlist = c("global_performance", "k_fold_idx", "ctg"),
#               envir=environment()) 
# sp <- Sys.time() #记录开始时间
# cat("\n[Start at:", as.character(sp))
# kknnPerformance <- foreach(
#     i = 1:length(k_fold_idx),
#     .combine = "rbind",
#     .packages = c("kknn", "doParallel")) %dopar% {
#       #获取当前折一折
#       curr_fold <- k_fold_idx[[i]] #当前这一折
#       #划分测试集和训练集
#       train_set <- ctg[-curr_fold, ] #训练集
#       test_set <- ctg[curr_fold, ] #测试集
#       #在训练集上做训练和评估
#       m_kknn_k_fold_train <- kknn(NSP~., train = train_set, test = train_set, 
#                                   k = train_con$best.parameters$k, 
#                                   kernel = train_con$best.parameters$kernel)
#       yhat_train <- m_kknn_k_fold_train$fit
#       imetrics("kknn", "Train", yhat_train, train_set$NSP)
#       #在训练集上做训练和评估
#       m_kknn_k_fold_test <- kknn(NSP~., train = train_set, test = test_set, 
#                                  k = train_con$best.parameters$k, 
#                                  kernel = train_con$best.parameters$kernel)
#       yhat_test <- m_kknn_k_fold_test$fit
#       imetrics("kknn", "Test", yhat_test, test_set$NSP)
#       return(tail(global_performance, n = 2))
#     }
# #stop clusters
# stopCluster(cl)
# ep <- Sys.time()
# cat("\tFinised at:", as.character(ep), "]\n")
# cat("[Time Ellapsed:\t",
#     difftime(ep, sp, units = "secs"),
#     " seconds]\n")
# View(kknnPerformance)
# #毫无疑问，对于其他的一些模型
# #我们同样可以进行并行计算
# #小伙伴们可自行练习

#######################################################
##朴素贝叶斯
#######################################################
library(e1071)
m_nb <- naiveBayes(NSP~., data = ctg)
m_nb <- naiveBayes(NSP~., data = ctg, laplace = 1e-20)
nb_presults <- predict(m_nb, ctg)
(nb_performance <- table(ctg$NSP, nb_presults))
sum(diag(nb_performance)) / sum(nb_performance)
# [1] 0.9082785
# [1] 0.9360301

#进行k-折交叉检验k-fold cross validation
library(e1071)
for(i in 1:length(k_fold_idx)) {
  #获取当前折一折
  curr_fold <- k_fold_idx[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- ctg[-curr_fold, ] #训练集
  test_set <- ctg[curr_fold, ] #测试集
  #建模
  m_nb_k_fold <- naiveBayes(NSP~., data = train_set, laplace = 1e-20) #模型训练
  #在训练集上做模型评估
  yhat_train <- predict(m_nb_k_fold, train_set)
  imetrics("naiveBayes", "Train", yhat_train, train_set$NSP)
  #在测试集上做模型评估
  yhat_test <- predict(m_nb_k_fold, test_set)
  imetrics("naiveBayes", "Test", yhat_test, test_set$NSP)
}



#######################################################
##人工神经网络
#######################################################
library(nnet)
set.seed(1)
m_ann <- nnet(NSP~., 
              data = ctg, 
              decay = 0.01,
              maxit = 2000,
              size = 7)
ann_presults <- predict(m_ann, ctg, type = "class")
(ann_performance <- table(ctg$NSP, ann_presults))
1- sum(diag(ann_performance)) / sum(ann_performance)
sum(diag(ann_performance)) / sum(ann_performance)

library(NeuralNetTools)
plotnet(m_ann,
        cex = 0.75,
        circle_cex = 2,
        alpha_val = 0.25)

#神经网络参数的设置相对比较复杂
#一般来讲，没有绝对的套路可循
#我们当然可以写一些循环，来进行参数的选择
#不过，类似于e1071::tune.nnet()已经替我们作了很多工作
#下面，采用的是caret包中的方法
#通过caret包中的grid搜索来进行参数选择
library(caret)
nnGrid <- expand.grid(size = c(3, 5, 7), 
                      decay = c(0.5, 0.1, 0.01))
nnTune <- train(
  NSP ~ .,
  data = ctg,
  method = "nnet",
  maxit = 2000,
  tuneGrid = nnGrid)
ann_presults <- predict(nnTune, ctg)
(ann_performance <- table(ctg$NSP, ann_presults))
1- sum(diag(ann_performance)) / sum(ann_performance)

nnTune$bestTune

#进行k-折交叉检验k-fold cross validation
library(nnet)
for(i in 1:length(k_fold_idx)) {
  #获取当前折一折
  curr_fold <- k_fold_idx[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- ctg[-curr_fold, ] #训练集
  test_set <- ctg[curr_fold, ] #测试集
  #建模
  m_nnet_k_fold <- nnet(NSP~., data = train_set,
                        decay = as.numeric(nnTune$bestTune["decay"]),
                        size = as.integer(nnTune$bestTune["size"]),
                        maxit = 2000)#模型训练
  #在训练集上做模型评估
  yhat_train <- predict(m_nnet_k_fold, train_set, type = "class")
  imetrics("nnet", "Train", yhat_train, train_set$NSP)
  #在测试集上做模型评估
  yhat_test <- predict(m_nnet_k_fold, test_set, type = "class")
  imetrics("nnet", "Test", yhat_test, test_set$NSP)
}




#######################################################
##支持向量机
##Support Vector Machine
#######################################################
library(kernlab)
set.seed(1)
m_svm <- ksvm(NSP~., 
              data = ctg,
              kernel = "rbfdot",
              C = 1)
svm_presults <- predict(m_svm, ctg, type = "response")
(svm_performance <- table(ctg$NSP, svm_presults))
1- sum(diag(svm_performance)) / sum(svm_performance)

set.seed(1)
m_svm <- ksvm(NSP~., 
              data = ctg,
              kernel = "polydot",
              C = 1)
svm_presults <- predict(m_svm, ctg, type = "response")
(svm_performance <- table(ctg$NSP, svm_presults))
1- sum(diag(svm_performance)) / sum(svm_performance)

#当然也可以通过caret来进行调参
svmGrid <- expand.grid(sigma= 2^c(-5, 0), C= 2^c(0, 5))
set.seed(1)
m_svm <- train(NSP ~ ., data = ctg, 
             method = "svmRadial",
             preProc = c("center", "scale"),
             tuneGrid = svmGrid)
m_svm$bestTune
svm_presults <- predict(m_svm, ctg)
(svm_performance <- table(ctg$NSP, svm_presults))
1- sum(diag(svm_performance)) / sum(svm_performance)


#进行k-折交叉检验k-fold cross validation
library(kernlab)
for(i in 1:length(k_fold_idx)) {
  #获取当前折一折
  curr_fold <- k_fold_idx[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- ctg[-curr_fold, ] #训练集
  test_set <- ctg[curr_fold, ] #测试集
  #建模
  m_svm_k_fold <- ksvm(NSP~., 
                       data = train_set,
                       kernel = "rbfdot",
                       sigma = m_svm$bestTune["sigma"],
                       C = m_svm$bestTune["C"])#模型训练
  #在训练集上做模型评估
  yhat_train <- predict(m_svm_k_fold, train_set, type = "response")
  imetrics("svm", "Train", yhat_train, train_set$NSP)
  #在测试集上做模型评估
  yhat_test <- predict(m_svm_k_fold, test_set, type = "response")
  imetrics("svm", "Test", yhat_test, test_set$NSP)
}


#######################################################
##随机森林
##RandomForest
#######################################################
library(randomForest)
m_rf <- randomForest(NSP~., data = ctg,
                     importance = TRUE, 
                     proximity = TRUE,
                     ntree = 1000)
rf_presults <- predict(m_rf, ctg)
(rf_perf <- table(ctg$NSP, rf_presults))
(rf_err_rate <- (sum(rf_perf) -
                   sum(diag(rf_perf))) / sum(rf_perf)) 
sum(diag(rf_perf)) / sum(rf_perf)


#当然，即便只有一个参数，也是需要调优的
rfGrid <- expand.grid(.mtry = c(50, 100, 500, 1000))
m_rf <- train(NSP ~ ., data = ctg, 
               method = "rf",
               tuneGrid = rfGrid)
m_rf$bestTune

#进行k-折交叉检验k-fold cross validation
library(randomForest)
for(i in 1:length(k_fold_idx)) {
  #获取当前折一折
  curr_fold <- k_fold_idx[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- ctg[-curr_fold, ] #训练集
  test_set <- ctg[curr_fold, ] #测试集
  #建模
  m_rf_k_fold <- randomForest(NSP~., 
                       data = train_set,
                       ntree = as.integer(m_rf$bestTune["mtry"]))#模型训练
  #在训练集上做模型评估
  yhat_train <- predict(m_rf_k_fold, train_set)
  imetrics("randomForest", "Train", yhat_train, train_set$NSP)
  #在测试集上做模型评估
  yhat_test <- predict(m_rf_k_fold, test_set)
  imetrics("randomForest", "Test", yhat_test, test_set$NSP)
}



#######################################################
##GBDT
#######################################################
require(xgboost)
require(Matrix)
library(data.table)
ctg <- data.table(ctg, keep.rownames = F)
train_set_m <- sparse.model.matrix(~.-NSP-1, data = ctg)
train_set <- xgb.DMatrix(data = train_set_m, label= ctg$NSP)
test_set <- xgb.DMatrix(data = train_set_m)
m_xgb <- xgboost(data =train_set, 
                  objective = "multi:softmax", 
                  nrounds = 50, 
                  eval_metric = "merror", 
                  num_class = 4)
xgb_presults <- levels(ctg$NSP)[predict(m_xgb, test_set)]
(xgb_perf <- table(ctg$NSP, xgb_presults))
(xgb_err_rate <- (sum(xgb_perf) -
                   sum(diag(xgb_perf))) / sum(xgb_perf)) 
sum(diag(xgb_perf)) / sum(xgb_perf)


#进行k-折交叉检验k-fold cross validation
require(xgboost)
require(Matrix)
library(data.table)
for(i in 1:length(k_fold_idx)) {
  #获取当前折一折
  curr_fold <- k_fold_idx[[i]] #当前这一折
  #划分测试集和训练集
  train_set_df <- ctg[-curr_fold, ] #训练集
  test_set_df <- ctg[curr_fold, ] #测试集
  train_set <- sparse.model.matrix(~.-NSP-1, data = train_set_df)
  train_set <- xgb.DMatrix(data = train_set, label= train_set_df$NSP)
  test_set <- sparse.model.matrix(~.-NSP-1, data = test_set_df)
  #建模
  m_xgb_k_fold <- xgboost(data =train_set, 
                   objective = "multi:softmax", 
                   nrounds = 50, 
                   eval_metric = "merror", 
                   num_class = 4)#模型训练
  #在训练集上做模型评估
  yhat_train <- levels(ctg$NSP)[predict(m_xgb_k_fold, train_set)]
  imetrics("xgboost", "Train", yhat_train, train_set_df$NSP)
  #在测试集上做模型评估
  yhat_test <- levels(ctg$NSP)[predict(m_xgb_k_fold, test_set)]
  imetrics("xgboost", "Test", yhat_test, test_set_df$NSP)
}



#######################################################
##模型综合比较
#######################################################
library(ggplot2)
library(tidyr)
library(dplyr)
#求取k折的平均值
global_performance <- as.data.frame(global_performance)
row.names(global_performance) <- NULL
gp <- global_performance
gp$Accuracy <- as.numeric(gp$Accuracy)
gp$Recall <- as.numeric(gp$Recall)
gp$Precision <- as.numeric(gp$Precision)
gp$F1_Score <- as.numeric(gp$F1_Score)
gp <- gp %>% 
  group_by(Method, Type) %>%
  summarise(Accuracy = mean(Accuracy),
            Recall = mean(Recall),
            Precision = mean(Precision),
            F1_Score = mean(F1_Score))
gp <- as.data.frame(gp)
gp <- gather(gp, Method, Type)
names(gp) <- c("Method","Type", "Metric", "Value")
gp$Method <- factor(
  gp$Method,
  levels = c(
    "rpart",
    "kknn",
    "naiveBayes",
    "nnet",
    "svm",
    "randomForest",
    "xgboost"
  )
)
gp$Metric <- factor(gp$Metric,
                    levels = c("Accuracy", "Recall", "Precision", "F1_Score"))
library(ggplot2)
ggplot(gp, aes(x = Method, y = Value, group = Metric, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = format(Value, digits = 4), y = 0), 
            cex = 3, 
            angle = 90, hjust = -0.5, 
            position=position_dodge(width=1)) +
  facet_grid(Type ~ .) #采用分面的方式来展示训练集和测试集上的性能指标


#完成了模型训练、模型评估，故事基本告一段落
#再回顾一下本讲开始所讲的featurePlot
#进行完模型训练之后，咱们再通过变量重要性印证一下
#变量重要性，有很多评价方法
#既有 Model Specific Metrics，也有Model Independent Metrics
#如果是采用caret框架进行训练的话，多种指标可选
#具体请参阅
#http://topepo.github.io/caret/variable-importance.html

#在此，仅采用randomForest和xgboost扩展包自带的变量重要性指标
library(randomForest)
m_rf <- randomForest(NSP~., data = ctg,
                     importance = TRUE, 
                     proximity = TRUE,
                     ntree = 1000)
rf_var_importance <- importance(m_rf)
#可以直接绘制图形
varImpPlot(m_rf)
#也可以提取其中的数据，自行进行绘制
rf_var_importance <- caret::varImp(m_rf)#其实就是前述importance()的前3列
class(rf_var_importance)
rf_var_importance$variable <- row.names(rf_var_importance)
row.names(rf_var_importance) <- NULL
library(tidyr)
rf_imp_gather <- rf_var_importance %>%
  gather(variable)
colnames(rf_imp_gather) <- c("var", "cls", "imp")
ggplot(rf_imp_gather, aes(x = var, y = imp, group = cls, colour = cls)) +
  geom_point() +
  geom_abline(slope = 0, 
              intercept = c(min(rf_imp_gather$imp), max(rf_imp_gather$imp)),
              colour = c("red", "blue"),
              size = 2,
              alpha = 0.5) +
  geom_line()

#xgboost
require(xgboost)
require(Matrix)
library(data.table)
ctg <- data.table(ctg, keep.rownames = F)
train_set_m <- sparse.model.matrix(~.-NSP-1, data = ctg)
train_set <- xgb.DMatrix(data = train_set_m, label= ctg$NSP)
test_set <- xgb.DMatrix(data = train_set_m)
m_xgb <- xgboost(data =train_set, 
                 objective = "multi:softmax", 
                 nrounds = 50, 
                 eval_metric = "merror", 
                 num_class = 4)
xgb_var_importance <- xgb.importance(feature_names = train_set_m@Dimnames[[2]], model = m_xgb)
library(tidyr)
xgb_imp_gather <- xgb_var_importance %>%
  gather(Feature)
colnames(xgb_imp_gather) <- c("var", "typ", "imp")
ggplot(xgb_imp_gather, aes(x = var, y = imp, group = typ, colour = typ)) +
  geom_point() +
  geom_abline(slope = 0, 
              intercept = c(min(xgb_imp_gather$imp), max(xgb_imp_gather$imp)),
              colour = c("red", "blue"),
              size = 2,
              alpha = 0.5) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#关于xgboost变量重要性更详细的阐述，请参阅：
#http://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html#


#分类与回归（实际上本讲只是涉及到分类，不过二者本质一致）到此结束
#代码中，算法原理等阐述较少，请小伙伴们参照PPT讲义，
#或是相应的机器学习/数据挖掘教材
#当然，几乎所有的包、函数的帮助文档中，都列举了相应的参考文献，
#小伙伴们可自行参考

#分类与回归算法，其体量应该是数以百计的，
#caret包中列举了百余种算法
#本讲中，只是列举了比较经典的集中。有很多算法并未考虑纳入，
#比如：
#装袋法：adabag::bagging()
#助推法：adabag::boosting()
#即便是演示过得算法，参数调优过程也显得比较粗糙
#更多的精彩，由小伙伴们自行探索吧


#######################################################
##The End ^-^
#######################################################