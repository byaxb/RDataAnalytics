

#######################################################
#######################################################
##
## 名称：《R语言数据分析·分类与回归》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V8
## 时间：2018年3月
##
##*****************************************************
##
## ch06_Classification and Regression_V8
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


#分类与回归，几乎是有监督学习的代名词
#也是机器学习/数据挖掘最核心的内容
#旨在揭示自变量与因变量之间的映射关系
#因变量为类别变量时，称之为分类
#因变量为连续变量时，称之为回归
#本实验以分类为主

#在R语言里，caret包提供了分类与回归的统一框架
#caret包也是R里边使用最广泛的包之一
#本实验将主要基于caret包开展
#除了caret这些工业级的函数外，也将提供一些手工版代码
#帮助小伙伴们理解其中的算法模型

#######################################################
##认识数据
#######################################################
#清空内存
rm(list = ls())
library(tidyverse)
library(magrittr)
cjb_url <-"https://github.com/byaxb/RDataAnalytics/raw/master/data/cjb.csv"

cjb <- read_csv(cjb_url,
                locale = locale(encoding = "CP936"))

head(cjb)

#查看数据结构
str(cjb)

#作必要的数据预处理
#比如：
#这份数据中，虽然没有直接标记为NA的缺失值，
#但容易看出，总成绩为0的同学，未参加考试
#对于这种缺失情况，直接删除
cjb %<>%
  mutate(zcj = rowSums(.[4:12])) %>%
  filter(zcj != 0) %>%#剔除脏数据
  mutate_at(vars(xb, wlfk), factor) #类型转换


#数据汇总描述
summary(cjb)


#进一步对数据进行描述
# library(Hmisc)
# describe(cjb)
# #里边有一些量，不那么直观，比如Gmd和Info
# #没关系，手工实现一下，就彻底理解了
# #Gmd: Gini mean difference?
# #the mean absolute 
# #difference between any pairs of observations
# #可自行实现如下
# #找出所有两两组合
# zcj_cb <- combn(cjb$sw, 2)
# #求出两两之间的绝对差
# absolute_difference <- apply(zcj_cb, 2, function(x) {
#   abs(diff(x))
# })
# #求出Gmd
# (Gmd <- mean(absolute_difference))
# #Info:
# #one minus the sum of 
# #the cubes of relative frequencies of values 
# #divided by 
# #one minus the square of the reciprocal of the sample size
# cal_info <- function(x) {
#   freqs <- table(x) / sum(table(x))
#   (1-sum(freqs^3)) / (1 - (1/nrow(cjb))^2)
#   
# }
# apply(cjb, 2, cal_info)
# #分位数信息
# quantile(cjb$zcj,
#          c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95))
# #求出最低和最高的五个分数
# lowest_highest <- function(x) {
#   uniques <- unique(sort(x))
#   lowest <- head(uniques, 5)
#   highest <- tail(uniques, 5)
#   return(list(lo = lowest, hi = highest))
# }
# lowest_highest(cjb$zcj)


#tidyverse所提供的summarize函数，可用于揭示数据分布信息
#分班级统计总成绩
library(tidyverse)
zcj_stats <- cjb %>%
  group_by(bj) %>%
  arrange(desc(zcj)) %>%
  summarise(
    count = n(),
    highest = first(zcj),
    high03 = nth(zcj, 3),
    high10 = nth(zcj, 10),
    mean = mean(zcj),
    median = median(zcj),
    lowest = last(zcj),
    sd = sd(zcj),
    iqr = IQR(zcj),
    mad = mad(zcj))
View(zcj_stats)
#按照班级、性别进行分组统计
sx_stats <- cjb %>%
  group_by(xb) %>%
  summarise(
    count = n(),
    max = max(sx),
    mean = mean(sx),
    median = median(sx),
    min = min(sx),
    sd = sd(sx),
    iqr = IQR(sx),
    mad = mad(sx))
sx_stats
#> # A tibble: 2 x 9
#> xb    count   max  mean median   min    sd   iqr
#> <fct> <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
#>   1 男      368   100  86.2     90    26 11.0   14.2
#> 2 女      406   100  86.2     89    51  9.90  13  
#> # ... with 1 more variable: mad <dbl>
View(sx_stats)

#根据我们的问题情境，
#容易看出，姓名、班级是不需要作为特征的
#根据姓名判断是否文理科，
#要么是缘木求鱼，要么是八卦算命
#都不是科学
cjb %<>%
  select(-(1:2))#剔除无关变量
#那哪些特征，对于我们最后的分类是有效的呢？
#一般来讲，特征的选取，要与具体的问题情境相结合，
#究竟选择哪些属性特征，要籍助领域专家的帮助


#虽然我们在《认识数据》一章中，
#看到有众多的异常值（野值/离群点）
#我们先标记出这些离群点，在建模的过程中，
#可以对照删除离群点前后准确率的变化
#找出离群值
(outliers <- boxplot.stats(cjb$zcj)$out)
#离群值标签

(outliers_idx <- which(cjb$zcj
                       %in% outliers))
#如有必要，可以考虑剔除异常点
# cjb %<>%
#   slice(-outliers_idx)#剔除离群值


#简单找一下相互之间的关系
mosaic::favstats(sx~wlfk, data = cjb)
#> wlfk min Q1 median Q3 max     mean        sd   n
#> 1 理科  46 86     93 96 100 89.78158  8.728818 380
#> 2 文科  26 75     84 92 100 82.73096 10.814123 394
#看得出来，理科生的数学成绩高于文科生，

#对于分类与回归问题，除了认识数据中的其他一些数据探索外
#通常需要观察不同自变量，相对于不同因变量取值时的数据分布
#考察其分类的潜力
#我们可以借助caret::featurePlot()和plotluck::plotluck()来进行观察
library(tidyverse)
library(caret)
#install.packages("ellipse")
featurePlot(
  x = cjb %>%
    select(yw:sw),
  y = cjb[, "wlfk"] %>%
    as_vector(),
  plot = "density",
  scales = list(
    x = list(relation = "free"),
    y = list(relation = "free")
  ),
  adjust = 1.5,
  pch = "|")
# library(devtools)
# devtools::install_github("stefan-schroedl/plotluck")
library(plotluck)
plotluck(cjb, wlfk~xb)
#绘制不同所有自变量、因变量各自分布
plotluck(cjb, .~1)
#绘制自变量相对于因变量的分组分布
plotluck(cjb, wlfk~., 
         opts = plotluck.options(verbose=TRUE))
#上述代码出图顺序，并非变量原有顺序，
#而是conditional entropy从小打到排列的结果
plotluck(cjb, wlfk~.,
         opts=plotluck.options(verbose=TRUE,
                               multi.entropy.order = FALSE))
plotluck(cjb, wlfk~yw+sx)
plotluck(cjb[-outliers_idx, ], wlfk~yw+sx)


#######################################################
##k折交叉检验
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

#留出法hold-out
#手工版
set.seed(2012)
train_set_idx <- 
  sample(nrow(cjb), nrow(cjb)*0.7)

#工业级
train_set_idx <-
  caret::createDataPartition(cjb$wlfk,
                      p = 0.7, list = FALSE)
str(train_set_idx)
length(train_set_idx) / nrow(cjb)
train_set <- cjb[train_set_idx, ]
# test_set <- ?

#k折交叉检验
cv_kfold <- function(data, k = 10, seed = 2012) {
  #计算数据的行数
  n <- nrow(data)
  #重复1:k共计ceiling(n/k)次
  #若能整除，则长度正好为n；若不能整除，则取其中前1:n个元素
  n_foldmarkers <- rep(1:k, ceiling(n / k))[1:n]
  set.seed(seed)
  #打乱顺序，作好下标1:n属于某一折的标记
  n_foldmarkers <- sample(n_foldmarkers)
  kfold <- lapply(1:k, function(i){
    (1:n)[n_foldmarkers == i]
  })
  return(kfold)
}
sapply(cv_kfold(iris), length)

#k的具体取值，没有统一的标准，视数据量大小，
#可以取5、10等
#对于少量数据，甚至可以将其推到极致，
#取nrow(cjb)折
#也就是我们常说的留一法
kfolds <- cv_kfold(cjb, nrow(cjb))
#我们这里取k=10
kfolds <- cv_kfold(cjb)
sapply(kfolds, length)


#像k折交叉检验这么经典的方法，在很多扩展包中
#都有实现，比如caret或modelr等
# library(caret)
# kfolds <- createFolds(cjb$wlfk, k = 10)
# sapply(kfolds, length)

#对于分类模型的评估，首先需要看是否存在类不平衡问题，
#如果存在类不平衡，评估指标的选取，
#不能单纯用正确率、错误率来评估，
#比如：10000人中，有10个人得SARS。现在不采用任何模型，
#只是用众数进行预测，也就是判定所有人都不得SARS，
#此时模型的正确率为(10000 - 10) / 10000 = 99.90%，
#正确率达到99.9%，然而，这种预测没有任何意义
#故此，还需要引入召回率Recall和Precision，
#以及二者的调和平均数F1值等
#从plotluck()的结果可以看出，我们所拿到的数据，并不存在
#类不平衡问题
plotluck(cjb, .~1)
#由于类是相对均衡的，本实验仅采用分类正确率和错误率
#手工版如下：
global_performance <- NULL
imetrics <- function(method, type, predicted, actual) {
  con_table <- table(predicted, actual)
  cur_one <- data.frame(
    method = method, #算法模型的名称
    type = type, #取值为train或是test
    accuracy = sum(diag(con_table)) / sum(con_table),
    error_rate = 1 - sum(diag(con_table)) / sum(con_table)
  )
  assign("global_performance", 
         rbind(get("global_performance", envir = .GlobalEnv) , cur_one), 
         envir = .GlobalEnv)
}
#有很多专门的包，已经实现了各种模型评估指标
#在实际的数据分析项目中，就不用去重复造轮子了
#拿来主义，直接用就好
#工业级
# imetrics <- function(method, type, predicted, actual) {
#   cur_one <- data.frame(
#     method = method,
#     type = type,
#     accuracy = MLmetrics::Accuracy(y_true = actual, y_pred = predicted),
#     # accuracy = 1 - Metrics::ce(actual, predicted),
#     # accuracy = 1 - ModelMetrics::ce(actual, predicted),
#     error_rate = ModelMetrics::ce(actual, predicted)
#   )
#   global_performance <<- rbind(global_performance, cur_one)
# }

#分类回归模型，有数十种
#各类改进的模型，更是数以百计
available_models <- modelLookup()
unique(available_models$model)
length(unique(available_models$model))
#> [1] 237
#想穷尽所有的算法模型，几乎是不可能的
#本实验仅涉及部分经典算法模型
#包括决策树、近邻法、朴素贝叶斯、
#人工神经网络、支持向量机和随机森林

#######################################################
##近邻法
#######################################################
library(kknn)
set.seed(2012)
imodel <- kknn(wlfk ~ .,
                   train = cjb[train_set_idx, ],
                   test = cjb[train_set_idx, ])
predicted_train <- imodel$fit
#ce: classification error
Metrics::ce(cjb$wlfk[train_set_idx], predicted_train)
#> [1] 0.1107011
imodel <- kknn(wlfk ~ .,
                   train = cjb[train_set_idx, ],
                   test = cjb[-train_set_idx, ])
predicted_test <- imodel$fit
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#[1] 0.2586207

#选取最优的k和核
train_kk <- train.kknn(
  wlfk  ~ .,
  data = cjb,
  kmax = 100,
  kernel = c(
    "rectangular" ,
    "epanechnikov" ,
    "cos",
    "inv",
    "gaussian" ,
    "optimal"
  )
)
#查看具体结果
train_kk
#通过ggplot2进行绘制
ce_kk <- train_kk$MISCLASS
#最佳的k值
best_k <- train_kk$best.parameters$k
best_kernel <- train_kk$best.parameters$kernel
#最小的误分率
min_ce <- train_kk$MISCLASS[best_k,
                            train_kk$best.parameters$kernel]
#下边这种方法更简单
min_ce <- min(train_kk$MISCLASS)
View(ce_kk)
str(ce_kk)
ce_kk %>%
  as.data.frame() %>%
  mutate(k = 1:nrow(ce_kk)) %>%
  gather(key = "kernel", value = "ce", -k) %>%
  ggplot(aes(x = k, y = ce, colour = kernel)) +
  geom_vline(aes(xintercept = best_k), 
             linetype = "dashed") +
  geom_hline(aes(yintercept = min_ce), 
             linetype = "dashed") +
  geom_line() +
  geom_point(aes(shape = kernel)) +
  theme(legend.position = c(0.8,0.8))

#进行k-折交叉检验k-fold cross validation
library(kknn)
sp <- Sys.time() #记录开始时间
cat("\n[Start at:", as.character(sp))
for (i in 1:length(kfolds)) {
  #获取当前折一折
  curr_fold <- kfolds[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- cjb[-curr_fold,] #训练集
  test_set <- cjb[curr_fold,] #测试集
  #没有训练的过程
  #在训练集上做模型评估
  predicted_train <-  kknn(
    wlfk ~ .,
    train = train_set,
    test = train_set,
    k = best_k,
    kernel = best_kernel
  )$fit
  imetrics("kknn", "Train", predicted_train, train_set$wlfk)
  #在测试集上做模型评估
  predicted_test <- kknn(
    wlfk ~ .,
    train = train_set,
    test = test_set,
    k = best_k,
    kernel = best_kernel
  )$fit
  imetrics("kknn", "Test", predicted_test, test_set$wlfk)
}
ep <- Sys.time()
cat("\tFinised at:", as.character(ep), "]\n")
cat("[Time Ellapsed:\t",
    difftime(ep, sp, units = "secs"),
    " seconds]\n")

#######################################################
##决策树
#######################################################
View(cjb[, -ncol(cjb)])
#几种不纯度的比较
#信息熵、基尼指数、分类错误率
p1 <- seq(0.001, 0.999, len = 5000)
p2 <- 1 - p1
#信息熵
entropy <- 0.5*(-p1*log2(p1) - p2*log2(p2))
#基尼指数
gini <- 1- (p1^2 + p2^2)
#分类错误率
classification_error <- 1- apply(data.frame(p1, p2), 1, max)
egc <- data.frame(
  p = p1,
  entropy = entropy,
  gini = gini,
  ce = classification_error)
egc_gathered <- egc %>%
  gather(key = "type", value = "value", -p) %>%
  mutate(type = forcats::fct_inorder(type))

library(ggplot2)
ggplot(egc_gathered, 
       aes(x = p, y = value, colour = type)) +
  geom_line(size = 1.5)


#决策树的生长
#rpart.plot包会自动加载rpart包
library(rpart.plot)
imodel <-
  rpart(wlfk   ~ .,
            data = cjb[train_set_idx,])
imodel
predicted_train <-
  predict(imodel,
          newdata = cjb[train_set_idx,],
          type = "class")
Metrics::ce(cjb$wlfk[train_set_idx],
            predicted_train)
#> [1] 0.1715867
predicted_test <-
  predict(imodel,
          newdata = cjb[-train_set_idx, ],
          type = "class")
Metrics::ce(cjb$wlfk[-train_set_idx],
            predicted_test)
#> [1] 0.2844828

#决策树剪枝
printcp(imodel, digits = 6)
plotcp(imodel)
opt <- which.min(imodel$cptable[, "xerror"])
cp <- imodel$cptable[opt, "CP"]
imodel_pruned <- prune(imodel, cp = cp)
print(imodel_pruned)

#剪枝前后效果对比
predicted_train <- predict(imodel_pruned,
      newdata = cjb[train_set_idx,],
      type = "class")
Metrics::ce(cjb$wlfk[train_set_idx],
      predicted_train)
#> [1] 0.1715867
predicted_test <- predict(imodel_pruned,
      newdata = cjb[-train_set_idx,],
      type = "class")
Metrics::ce(cjb$wlfk[-train_set_idx], 
      predicted_test)
#> [1] 0.2844828


#绘制决策树的基本方法
plot(imodel)
text(imodel)
#上边的效果小伙伴们肯定是不满意的
rpart.plot(imodel_pruned, 
    type=4, fallen=F,
    branch=0.5, round=0, 
    leaf.round=2, clip.right.labs=T, 
    cex = 0.85, under.cex=0.75,
    box.palette="GnYlRd", 
    branch.col="gray", 
    branch.lwd=2, 
    extra=108, #extra参数的含义需留意
    under=T,split.cex=0.8)

#除了可视化之外，我们还希望把这个树导成规则
library(rattle)
rules <- asRules(imodel_pruned, compact=TRUE)
#> R  7 [22%,0.90] sx< 85.5 xb=女
#> R 11 [11%,0.85] sx>=85.5 wl< 86.5 ls>=92.5
#> R 51 [ 4%,0.79] sx< 85.5 xb=男 hx>=83 wy>=81.5 sx< 76.5
#> R 13 [ 5%,0.75] sx< 85.5 xb=男 hx< 83
#> R101 [ 1%,0.75] sx< 85.5 xb=男 hx>=83 wy>=81.5 sx>=76.5 sw>=89.5
#> R 21 [ 3%,0.75] sx>=85.5 wl< 86.5 ls< 92.5 sw< 80.5
#> R 19 [ 3%,0.75] sx>=85.5 wl>=86.5 ls>=95.5 sw< 92.5
#> R 24 [ 5%,0.28] sx< 85.5 xb=男 hx>=83 wy< 81.5
#> R 20 [13%,0.24] sx>=85.5 wl< 86.5 ls< 92.5 sw>=80.5
#> R 18 [ 7%,0.22] sx>=85.5 wl>=86.5 ls>=95.5 sw>=92.5
#> R100 [ 3%,0.19] sx< 85.5 xb=男 hx>=83 wy>=81.5 sx>=76.5 sw< 89.5
#> R  8 [23%,0.13] sx>=85.5 wl>=86.5 ls< 95.5

#进行k-折交叉检验k-fold cross validation
sp <- Sys.time() #记录开始时间
cat("\n[Start at:", as.character(sp))
library(rpart)
for(i in 1:length(kfolds)) {
  #获取当前折一折
  curr_fold <- kfolds[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- cjb[-curr_fold, ] #训练集
  test_set <- cjb[curr_fold, ] #测试集
  #生长树
  imodel_kfold <- rpart(wlfk~., train_set) #模型训练
  #剪枝
  opt <- which.min(imodel_kfold$cptable[, "xerror"])
  cp <- imodel_kfold$cptable[opt, "CP"]
  imodel_kfold <- prune(imodel_kfold, cp = cp)
  #在训练集上做模型评估
  predicted_train <- predict(imodel_kfold, train_set, type = "class")
  imetrics("rpart", "Train", predicted_train, train_set$wlfk)
  #在测试集上做模型评估
  predicted_test <- predict(imodel_kfold, test_set, type = "class")
  imetrics("rpart", "Test", predicted_test, test_set$wlfk)
}
ep <- Sys.time()
cat("\tFinised at:", as.character(ep), "]\n")
cat("[Time Ellapsed:\t",
    difftime(ep, sp, units = "secs"),
    " seconds]\n")


#######################################################
##随机森林
##RandomForest
#######################################################
library(randomForest)
set.seed(2012)
imodel <- randomForest(wlfk~., 
                           data = cjb[train_set_idx, ])
predicted_train <- predict(imodel,
                           newdata = cjb[train_set_idx,],
                           type = "response")
Metrics::ce(cjb$wlfk[train_set_idx], 
            predicted_train)
#[1] 0
predicted_test <- predict(imodel,
                          newdata = cjb[-train_set_idx,],
                          type = "response")
Metrics::ce(cjb$wlfk[-train_set_idx], 
            predicted_test)
#> [1] 0.2112069

#基于OOB的误分率
imodel$confusion
#>       理科 文科 class.error
#> 理科  195   71   0.2669173
#> 文科   54  222   0.1956522

#进行k-折交叉检验k-fold cross validation
library(randomForest)
sp <- Sys.time() #记录开始时间
cat("\n[Start at:", as.character(sp))
for (i in 1:length(kfolds)) {
  #获取当前折一折
  curr_fold <- kfolds[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- cjb[-curr_fold,] #训练集
  test_set <- cjb[curr_fold,] #测试集
  #训练模型
  imodel_kfold <- randomForest(wlfk~., 
                                   data = train_set)
  #在训练集上做模型评估
  predicted_train <-  predict(imodel_kfold, train_set, type = "response")
  imetrics("randomForest", "Train", predicted_train, train_set$wlfk)
  #在测试集上做模型评估
  predicted_test <- predict(imodel_kfold, test_set, type = "response")
  imetrics("randomForest", "Test", predicted_test, test_set$wlfk)
}
ep <- Sys.time()
cat("\tFinised at:", as.character(ep), "]\n")
cat("[Time Ellapsed:\t",
    difftime(ep, sp, units = "secs"),
    " seconds]\n")



#######################################################
##朴素贝叶斯
#######################################################
library(e1071)
imodel <- naiveBayes(wlfk~., 
      data = cjb[train_set_idx, ])
predicted_train <- predict(imodel,
      newdata = cjb[train_set_idx,],
      type = "class")
Metrics::ce(cjb$wlfk[train_set_idx], predicted_train)
#> [1] 0.3173432
predicted_test <- predict(imodel,
      newdata = cjb[-train_set_idx,],
      type = "class")
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#> [1] 0.2801724

#进行k-折交叉检验k-fold cross validation
library(e1071)
sp <- Sys.time() #记录开始时间
cat("\n[Start at:", as.character(sp))
for (i in 1:length(kfolds)) {
  #获取当前折一折
  curr_fold <- kfolds[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- cjb[-curr_fold,] #训练集
  test_set <- cjb[curr_fold,] #测试集
  #训练模型
  imodel_kfold <- naiveBayes(wlfk~., 
                                 data = train_set)
  #在训练集上做模型评估
  predicted_train <-  predict(imodel_kfold, train_set, type = "class")
  imetrics("naiveBayes", "Train", predicted_train, train_set$wlfk)
  #在测试集上做模型评估
  predicted_test <- predict(imodel_kfold, test_set, type = "class")
  imetrics("naiveBayes", "Test", predicted_test, test_set$wlfk)
}
ep <- Sys.time()
cat("\tFinised at:", as.character(ep), "]\n")
cat("[Time Ellapsed:\t",
    difftime(ep, sp, units = "secs"),
    " seconds]\n")




#######################################################
##逻辑斯蒂回归
#######################################################
library(ggplot2)
library(animation)
saveGIF(
  expr = {
    mov_frame <- 5*(1:30)
    for (i in mov_frame) {
      x <- seq(-i, i, len = 1000)
      y <- 1/(1 + exp(-x))
      p <- ggplot(data.frame(x, y), aes(x = x, y = y)) +
        geom_line()
      if (i == head(mov_frame, 1) || i == tail(mov_frame, 1)) { #开始和结束时多停留一会儿
        lapply(1:5, function(x) plot(p))
      }
      plot(p)
    }
  },
  movie.name = "D://desktop/animation.gif",
  convert = "gm convert",
  interval = 0.2
)
dev.off()




imodel <- glm(wlfk ~ .,
                  data = cjb[train_set_idx,],
                  family = binomial(link = "logit"))
predicted_logit <- predict(imodel,
                           newdata = cjb[train_set_idx,],
                           type = "response")
predicted_train <-
  rep(levels(cjb$wlfk)[2], length(train_set_idx))
predicted_train[predicted_logit < 0.5] <- levels(cjb$wlfk)[1]
Metrics::ce(cjb$wlfk[train_set_idx], predicted_train)
#> [1] 0.204797
predicted_logit <- predict(imodel,
                           newdata = cjb[-train_set_idx, ],
                           type = "response")
predicted_test <-
  rep(levels(cjb$wlfk)[2], nrow(cjb[-train_set_idx,]))
predicted_test[predicted_logit < 0.5] <-
  levels(cjb$wlfk)[1]
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#> [1] 0.1853448


#找到最好的分隔阈值
best_threshold <- NA
min_err <- Inf
cur_threshold <- 0.1
for (cur_threshold in seq(0.1, 0.9, by = 0.001)) {
  predicted_test <-
    rep(levels(cjb$wlfk)[2], nrow(cjb[-train_set_idx,]))
  predicted_test[predicted_logit < cur_threshold] <-
    levels(cjb$wlfk)[1]
  cur_err <- Metrics::ce(cjb$wlfk[-train_set_idx],
                         predicted_test)
  if (cur_err < min_err) {
    best_threshold <- cur_threshold
    min_err <- cur_err
  }
}


#
library(InformationValue)
optCutOff <- optimalCutoff(as.numeric(cjb$wlfk[-train_set_idx]), as.numeric(factor(predicted_test)))[1] 

#进行k-折交叉检验k-fold cross validation
sp <- Sys.time() #记录开始时间
cat("\n[Start at:", as.character(sp))
for (i in 1:length(kfolds)) {
  #获取当前折一折
  curr_fold <- kfolds[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- cjb[-curr_fold,] #训练集
  test_set <- cjb[curr_fold,] #测试集
  #训练模型
  imodel_kfold <- glm(wlfk~.,
                          data = train_set,
                          family=binomial(link="logit"))
  #在训练集上做模型评估
  predicted_logit <- predict(imodel_kfold,
                          newdata = train_set, 
                          type = "response")
  predicted_train <- rep(levels(cjb$wlfk)[2], nrow(train_set))
  predicted_train[predicted_logit < best_threshold] <- levels(cjb$wlfk)[1]
  imetrics("LogisticRegression", "Train", predicted_train, train_set$wlfk)
  #在测试集上做模型评估
  predicted_logit <- predict(imodel_kfold,
                             newdata = test_set, 
                             type = "response")
  predicted_test <- rep(levels(cjb$wlfk)[2], nrow(test_set))
  predicted_test[predicted_logit < best_threshold] <- levels(cjb$wlfk)[1]
  imetrics("LogisticRegression", "Test", predicted_test, test_set$wlfk)
}
ep <- Sys.time()
cat("\tFinised at:", as.character(ep), "]\n")
cat("[Time Ellapsed:\t",
    difftime(ep, sp, units = "secs"),
    " seconds]\n")



#######################################################
##人工神经网络
#######################################################
library(nnet)
set.seed(2012)
imodel <- nnet(wlfk~., 
      data = cjb[train_set_idx, ],
      size = 7)
predicted_train <- predict(imodel,
      newdata = cjb[train_set_idx,],
      type = "class")
Metrics::ce(cjb$wlfk[train_set_idx], predicted_train)
#> [1] 0.4907749
predicted_test <- predict(imodel,
      newdata = cjb[-train_set_idx,],
      type = "class")
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#> [1] 0.4913793
#对于神经网络而言，不调参数的话，几乎不可用的

#神经网络参数的设置相对比较复杂
#一般来讲，没有绝对的套路可循
#我们当然可以写一些循环，来进行参数的选择
#不过，类似于e1071::tune.nnet()已经替我们作了很多工作
#下面，采用的是caret包中的方法
#通过caret包中的grid搜索来进行参数选择
library(caret)
nn_grid <- expand.grid(size = c(1, 3, 7), 
                      decay = c(0.001, 0.01, 0.03, 0.1, 0.3))
imodel <- train(
  wlfk ~ .,
  data = cjb,
  method = "nnet",
  maxit = 2000,
  tuneGrid = nn_grid)
imodel$bestTune
#>    size decay
#> 9    3   0.1
#查看训练结果
plot(imodel)

predicted_train <- predict(imodel,
      newdata = cjb[train_set_idx,],
      type = "raw")
Metrics::ce(cjb$wlfk[train_set_idx], 
      predicted_train)
#> [1] 0.1697417
predicted_test <- predict(imodel,
      newdata = cjb[-train_set_idx,],
      type = "raw")
Metrics::ce(cjb$wlfk[-train_set_idx], 
      predicted_test)
#> [1] 0.1896552

#绘制神经网络
library(NeuralNetTools)
imodel2 <-  nnet(wlfk ~ .,
                          data = train_set,
                          decay = imodel$bestTune$decay,
                          size = imodel$bestTune$size,
                          maxit = 2000)
imodel2$wts
str(imodel2)
library(NeuralNetTools)
plotnet(imodel2, 
        rel_rsc = c(1.8,3),
        circle_cex = 3, 
        cex_val = 0.75,
        bord_col = "lightblue",
        max_sp = TRUE)


#进行k-折交叉检验k-fold cross validation
library(nnet)
sp <- Sys.time() #记录开始时间
cat("\n[Start at:", as.character(sp))
for (i in 1:length(kfolds)) {
  #获取当前折一折
  curr_fold <- kfolds[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- cjb[-curr_fold,] #训练集
  test_set <- cjb[curr_fold,] #测试集
  #训练模型
  imodel_kfold <-  nnet(wlfk ~ .,
                            data = train_set,
                            decay = imodel$bestTune$decay,
                            size = imodel$bestTune$size,
                            maxit = 2000)
  #在训练集上做模型评估
  predicted_train <-  predict(imodel_kfold, train_set, type = "class")
  imetrics("nnet", "Train", predicted_train, train_set$wlfk)
  #在测试集上做模型评估
  predicted_test <- predict(imodel_kfold, test_set, type = "class")
  imetrics("nnet", "Test", predicted_test, test_set$wlfk)
}
ep <- Sys.time()
cat("\tFinised at:", as.character(ep), "]\n")
cat("[Time Ellapsed:\t",
    difftime(ep, sp, units = "secs"),
    " seconds]\n")


#######################################################
##支持向量机
##Support Vector Machine
#######################################################
library(kernlab)
set.seed(2012)
imodel <- ksvm(wlfk~., 
      data = cjb[train_set_idx, ])
predicted_train <- predict(imodel,
      newdata = cjb[train_set_idx,],
      type = "response")
Metrics::ce(cjb$wlfk[train_set_idx], predicted_train)
#> [1] 0.1383764
predicted_test <- predict(imodel,
      newdata = cjb[-train_set_idx,],
      type = "response")
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#> [1] 0.2025862
imodel
#当然也可以通过caret来进行调参
library(caret)
svm_grid <- expand.grid(sigma = 2^(-10:4),
      C= -5:20)
set.seed(2012)
imodel <- train(wlfk~., 
      data = cjb[train_set_idx, ], 
      method = "svmRadial",
      preProc = c("center", "scale"),
      tuneGrid = svm_grid)
imodel$bestTune
#>   sigma C
#> 2  0.25 1

#同样也可以对train的结果进行绘制
plot(imodel)

#进行k-折交叉检验k-fold cross validation
library(kernlab)
sp <- Sys.time() #记录开始时间
cat("\n[Start at:", as.character(sp))
for (i in 1:length(kfolds)) {
  #获取当前折一折
  curr_fold <- kfolds[[i]] #当前这一折
  #划分测试集和训练集
  train_set <- cjb[-curr_fold,] #训练集
  test_set <- cjb[curr_fold,] #测试集
  #训练模型
  imodel_kfold <-  ksvm(wlfk~., 
                            C = imodel$bestTune$C,
                            gamma = imodel$bestTune$sigma,
                            data = train_set)
  #在训练集上做模型评估
  predicted_train <-  predict(imodel_kfold, train_set, type = "response")
  imetrics("ksvm", "Train", predicted_train, train_set$wlfk)
  #在测试集上做模型评估
  predicted_test <- predict(imodel_kfold, test_set, type = "response")
  imetrics("ksvm", "Test", predicted_test, test_set$wlfk)
}
ep <- Sys.time()
cat("\tFinised at:", as.character(ep), "]\n")
cat("[Time Ellapsed:\t",
    difftime(ep, sp, units = "secs"),
    " seconds]\n")






#######################################################
##变量重要性
#######################################################
#完成了模型训练、模型评估，故事基本告一段落
#再回顾一下本讲开始所讲的featurePlot
#进行完模型训练之后，咱们再通过变量重要性印证一下
#变量重要性，有很多评价方法
#既有 Model Specific Metrics，也有Model Independent Metrics
#如果是采用caret框架进行训练的话，多种指标可选
#具体请参阅
#http://topepo.github.io/caret/variable-importance.html

library(randomForest)
imodel <- randomForest(wlfk~., data = cjb)
#变量重要性的分析
randomForest::importance(imodel) %>%
  as.data.frame() %>%
  rownames_to_column(var = "variables") %>%
  arrange(desc(MeanDecreaseGini)) %>%
  mutate(variables = factor(variables, 
                            levels = variables)) %>%
  ggplot(aes(x = variables, 
             y = MeanDecreaseGini,
             fill = variables)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(y = MeanDecreaseGini * 1.02,
                label = format(MeanDecreaseGini, digits = 4)))
#变量重要性的分析
randomForest::importance(imodel) %>%
  as.data.frame() %>%
  rownames_to_column(var = "variables") %>%
  arrange(desc(MeanDecreaseGini)) %>%
  mutate(variables = forcats::fct_inorder(variables)) %>%
  ggplot(aes(x = variables, 
             y = MeanDecreaseGini,
             fill = variables)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(y = MeanDecreaseGini * 1.02,
                label = format(MeanDecreaseGini, digits = 4)))
#里边涉及到排序之后对因子水平进行重排序
#也有一个更简洁的版本
randomForest::importance(imodel) %>%
  as.data.frame() %>%
  rownames_to_column(var = "variables") %>%
  mutate(variables = forcats::fct_reorder(variables,
                                          MeanDecreaseGini,
                                          .desc = TRUE)) %>%
  ggplot(aes(x = variables, 
             y = MeanDecreaseGini,
             fill = variables)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(y = MeanDecreaseGini * 1.02,
                label = format(MeanDecreaseGini, digits = 4)))

#######################################################
##模型综合评估
#######################################################
#模型进行评估
metrics_com <- global_performance %>%
  group_by(method, type) %>%
  summarise(mean_error_rate = mean(error_rate)) %>%
  spread(key = type, value = mean_error_rate)
metrics_com$method <- factor(as.character(metrics_com$method),
                             levels = c("kknn", "rpart", "randomForest", 
                                        "naiveBayes", "LogisticRegression",
                                        "nnet", "ksvm"))
View(metrics_com[c(6, 1, 3:4, 2, 5), c(1, 3, 2)])
global_performance %>%
  group_by(method, type) %>%
  summarise(mean_error_rate = mean(error_rate)) %>%
  ggplot(aes(x = method, y = mean_error_rate, fill = type)) +
  geom_bar(stat= "identity", position = "dodge") +
  geom_text(aes(label = format(mean_error_rate, digits = 3)),
            position = position_dodge(width = 1))+
  scale_fill_manual(values=c("orange","darkgrey")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
metrics_com %>%
  gather(key = "type", value = "mean_error_rate", - method) %>%
  arrange(type, mean_error_rate)%>%
  ggplot(aes(x = fct_inorder(method), y = mean_error_rate, fill = type)) +
  geom_bar(stat= "identity", position = "dodge") +
  geom_text(aes(label = format(mean_error_rate, digits = 3)),
            position = position_dodge(width = 1))+
  scale_fill_manual(values=c("orange","darkgrey"))

#本实验中，为了减少小伙伴们熟悉问题背景本身的成本
#再次使用了学生成绩这份数据
#受数据本身的限制，也让我们错过了很多的精彩：
#比如：
#这份数据太干净，没有缺失值，也就不要通过mice::md.pattern()
#之类的函数来观察缺失模式，或是通过近邻法等方法来填补缺失值
#又如：我们面对的是类相对均衡的问题，文理科学生数大体相当
#而实际问题中，也会有很多类不平衡的问题，这个时候可能专门
#需要对数据、算法进行处理，评估指标也不能用简单的正确率来衡量
#再比如：我们的数据量相对较少，没有涉及到复杂数据处理

#分类与回归（实际上本讲只是涉及到分类，不过二者本质一致）到此结束
#代码中，算法原理等阐述较少，请小伙伴们参照PPT讲义，
#或是相应的机器学习/数据挖掘教材
#当然，几乎所有的包、函数的帮助文档中，都列举了相应的参考文献，
#小伙伴们可自行参考

#分类与回归算法，其体量应该是数以百计的，
#caret包中列举了百余种算法
#本讲中，只是列举了比较经典的集中。有很多算法并未考虑纳入，
#比如：
#线性判别分析：MASS::lda()
#逻辑斯蒂回归：stats::glm()
#装袋法：adabag::bagging()
#助推法：adabag::boosting()
#GBDT: xgboost::xgboost
#即便是演示过得算法，参数调优过程也显得比较粗糙
#更多的精彩，由小伙伴们自行探索吧


#######################################################
##The End ^-^
#######################################################
