


# 06_既是世间法、自当有分别------------------------------------------------


#这里的分别，指的是分门别类
#更具体的讲，是根据特征做出判断、作出分类

#分类与回归，几乎是有监督学习的代名词
#也是机器学习/数据挖掘最核心的内容
#旨在揭示自变量与因变量之间的映射关系
#因变量为类别变量时，称之为分类
#因变量为连续变量时，称之为回归
#本实验以分类为主

#在R语言里，caret包提供了分类与回归的统一框架
#caret包也是R里边使用最广泛的包之一
#小伙伴们可以多加留意


# Data Exploration --------------------------------------------------------

#清空内存
rm(list = ls())
library(tidyverse)
#加载数据
cjb_url <- "data/cjb.csv"
cjb <- read_csv(cjb_url,
                locale = locale(encoding = "CP936"))
cjb %<>%
    mutate(zcj = rowSums(.[, 4:12])) %>%
    mutate_at(vars(xb, bj, wlfk), factor) %>%
    filter(zcj != 0) %>%
    select(xb:wlfk)
#按照一般的数据分析流程，自然应该是先对数据进行探索性分析
#小伙伴们可以参照之前Get to Know Your Data相关代码，认识这份数据

#对于分类与回归问题，除了认识数据中的其他一些数据探索外
#通常需要观察不同自变量，相对于不同因变量取值时的数据分布
#考察其分类的潜力
#我们可以借助caret::featurePlot()和plotluck::plotluck()来进行观察
library(caret)
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
    pch = "|"
)

# library(devtools)
# devtools::install_github("stefan-schroedl/plotluck")
library(plotluck)
plotluck(cjb, wlfk ~ xb)
#绘制不同所有自变量、因变量各自分布
plotluck(cjb, . ~ 1)
#绘制自变量相对于因变量的分组分布
plotluck(cjb, wlfk ~ .,
         opts = plotluck.options(verbose = TRUE))
#上述代码出图顺序，并非变量原有顺序，
#而是conditional entropy从小打到排列的结果
plotluck(cjb, wlfk ~ .,
         opts = plotluck.options(verbose = TRUE,
                                 multi.entropy.order = FALSE))
plotluck(cjb, wlfk ~ yw + sx)




# k-fold Cross Validation -------------------------------------------------

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
train_set_idx <- sample(nrow(cjb), nrow(cjb) * 0.7)
str(train_set_idx)
#> int [1:541] 169 576 218 722 575 673 411 700 687 696 ...
length(train_set_idx) / nrow(cjb)
#> [1] 0.6989664
train_set <- cjb[train_set_idx, ]
# test_set <- ?

# #工业级
# train_set_idx <-
#   caret::createDataPartition(cjb$wlfk,
#                       p = 0.7, list = FALSE)
# str(train_set_idx)
# length(train_set_idx) / nrow(cjb)
# train_set <- cjb[train_set_idx, ]
# # test_set <- ?

#k折交叉检验
cv_kfold <- function(data, k = 10, seed = 2012) {
    n_row <- nrow(data)#计算数据的行数
    n_foldmarkers <- rep(1:k, ceiling(n_row / k))[1:n_row]
    set.seed(seed)
    n_foldmarkers <- sample(n_foldmarkers)  #打乱顺序
    kfold <- lapply(1:k, function(i) {
        (1:n_row)[n_foldmarkers == i]
    })
    return(kfold)
}
cv_kfold(cjb)
#> [[1]]
#> [1]   7  14  15  25  35  48  56  59  60  61  65  91  92
#> [14] 102 109 114 128 130 135 141 156 169 178 180 181 185
#> [27] 189 190 191 196 208 217 244 245 247 263 280 282 291
#> [40] 293 301 309 319 324 327 328 329 330 332 356 361 362
#> [53] 376 384 412 413 446 456 485 489 499 500 519 525 531
#> [66] 534 550 559 578 585 586 598 607 619 620 675 685 719
sapply(cv_kfold(cjb), length)
#>  [1] 78 78 78 78 77 77 77 77 77 77


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



# Global performance ------------------------------------------------------

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
#plotluck(cjb, .~1)
#由于类是相对均衡的，本实验仅采用分类正确率和错误率
#手工版如下：
global_performance <- NULL
imetrics <- function(method, type, predicted, actual) {
    con_table <- table(predicted, actual)
    cur_one <- data.frame(
        method = method,
        #算法模型的名称
        type = type,
        #取值为train或是test
        accuracy = sum(diag(con_table)) / sum(con_table),
        error_rate = 1 - sum(diag(con_table)) / sum(con_table)
    )
    assign("global_performance",
           rbind(get("global_performance", envir = .GlobalEnv) ,
                 cur_one),
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

# #分类回归模型，有数十种
# #各类改进的模型，更是数以百计
# available_models <- modelLookup()
# unique(available_models$model)
# #> [1] "ada"                 "AdaBag"
# #> [3] "adaboost"            "AdaBoost.M1"
# #> [235] "xgbLinear"           "xgbTree"
# #> [237] "xyf"
# length(unique(available_models$model))
# #> [1] 237
# #想穷尽所有的算法模型，几乎是不可能的
# #本实验仅涉及部分经典算法模型
# #包括决策树、近邻法、朴素贝叶斯、
# #人工神经网络、支持向量机和随机森林



# kknn --------------------------------------------------------------------

load('data/cjb.rda')
cjb <- cjb %>%
    select(4:13) %>%
    mutate(wlfk = factor(wlfk))
train_set_idx <- sample(nrow(cjb), 0.7 * nrow(cjb))
test_set_idx <- (1:nrow(cjb))[-train_set_idx]
library(kknn)
set.seed(2012)
imodel <- kknn(wlfk ~ .,
               train = cjb[train_set_idx, ],
               test = cjb[train_set_idx, ])
predicted_train <- imodel$fit
#ce: classification error
Metrics::ce(cjb$wlfk[train_set_idx], predicted_train)
#> [1] 0.1090573
#作为惰性学习法，训练和测试同时进行
imodel <- kknn(wlfk ~ .,
               train = cjb[train_set_idx, ],
               test = cjb[-train_set_idx, ])
predicted_test <- imodel$fit
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#> [1] 0.1888412

#选取最优的k和核
train_kk <- train.kknn(
    wlfk  ~ .,
    data = cjb,
    kmax = 100,
    kernel = c(
        "rectangular",
        "epanechnikov",
        "cos",
        "inv",
        "gaussian",
        "optimal"
    )
)

#查看具体结果
train_kk
#> Call:
#>   train.kknn(formula = wlfk ~ ., data = cjb, kmax = 100,
#>              kernel = c("rectangular",     "epanechnikov",
#>                         "cos", "inv", "gaussian", "optimal"))
#>
#> Type of response variable: nominal
#> Minimal misclassification: 0.2105943
#> Best kernel: gaussian
#> Best k: 49

#不同的k和核所对应的分类错误率
train_kk$MISCLASS
#     rectangular epanechnikov       cos       inv  gaussian   optimal
# 1     0.2919897    0.2919897 0.2919897 0.2919897 0.2919897 0.2919897
# 2     0.2984496    0.2919897 0.2919897 0.2919897 0.2919897 0.2919897
# 3     0.2661499    0.2739018 0.2751938 0.2661499 0.2661499 0.2919897
# 4     0.2713178    0.2661499 0.2648579 0.2519380 0.2532300 0.2919897
# 5     0.2583979    0.2571059 0.2596899 0.2571059 0.2558140 0.2558140
# 6     0.2609819    0.2558140 0.2532300 0.2441860 0.2454780 0.2532300

#显然，上述矩阵中，序号就是相应的k


#最佳的k值
best_k <- train_kk$best.parameters$k
best_k
#> [1] 49
best_kernel <- train_kk$best.parameters$kernel
best_kernel
#> [1] "gaussian"

#最小的误分率
min_ce <- train_kk$MISCLASS[best_k,
                            train_kk$best.parameters$kernel]
#下边这种方法更简单
min(train_kk$MISCLASS)

#提取不同k和核相应的分类错误率
ce_kk <- train_kk$MISCLASS
#View(ce_kk)
#最小错误率
min_ce <- min(train_kk$MISCLASS)
str(ce_kk)
#通过ggplot2进行绘制
ce_kk %>%
    as.data.frame() %>%
    mutate(k = row_number()) %>%
    gather(key = "kernel", value = "ce", -k) %>%
    ggplot(aes(x = k, y = ce, colour = kernel)) +
    geom_vline(aes(xintercept = best_k), linetype = "dashed") +
    geom_hline(aes(yintercept = min_ce), linetype = "dashed") +
    geom_line() +
    geom_point(aes(shape = kernel)) +
    theme(legend.position = c(0.9, 0.8))

#进行k-折交叉检验k-fold cross validation
library(kknn)
sp <- Sys.time() #记录开始时间
cat("\n[Start at:", as.character(sp))
for (i in 1:length(kfolds)) {
    curr_fold <- kfolds[[i]] #当前这一折
    train_set <- cjb[-curr_fold,] #训练集
    test_set <- cjb[curr_fold,] #测试集
    predicted_train <- kknn(
        wlfk ~ .,
        train = train_set,
        test = train_set,
        k = best_k,
        kernel = best_kernel
    )$fit
    imetrics("kknn", "Train", predicted_train, train_set$wlfk)
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
global_performance
#>     method  type  accuracy error_rate
#> 1    kknn Train 0.8333333  0.1666667
#> 2    kknn  Test 0.8076923  0.1923077
#> 3    kknn Train 0.8405172  0.1594828
#> 4    kknn  Test 0.8076923  0.1923077
#> 5    kknn Train 0.8333333  0.1666667
#> 6    kknn  Test 0.8461538  0.1538462
#> 7    kknn Train 0.8405172  0.1594828
#> 8    kknn  Test 0.7564103  0.2435897
#> 9    kknn Train 0.8350072  0.1649928
#> 10   kknn  Test 0.7922078  0.2077922
#> 11   kknn Train 0.8278336  0.1721664
#> 12   kknn  Test 0.7922078  0.2077922
#> 13   kknn Train 0.8378766  0.1621234
#> 14   kknn  Test 0.8051948  0.1948052
#> 15   kknn Train 0.8350072  0.1649928
#> 16   kknn  Test 0.7792208  0.2207792
#> 17   kknn Train 0.8307030  0.1692970
#> 18   kknn  Test 0.6623377  0.3376623
#> 19   kknn Train 0.8278336  0.1721664
#> 20   kknn  Test 0.7792208  0.2207792



#考虑到每种方法都要采用交叉检验的方法，
#根据事不过三法则，反反复复拷贝、更改以上代码是不合适的
#为此，将上述代码改写为相应的函数
kfold_cross_validation <-
    function(formula, data, kfolds, learner, ...) {
        sp <- Sys.time() #记录开始时间
        cat("\n[Start at:", as.character(sp))
        lapply(kfolds, function(curr_fold) {
            train_set <- data[-curr_fold,] #训练集
            test_set <- data[curr_fold,] #测试集
            predictions <- do.call(learner, args = c(
                list(
                    formula = formula,
                    train = train_set,
                    test = test_set
                ),
                list(...)
            ))
            imetrics(learner,
                     "Train",
                     predictions$predicted_train,
                     train_set$wlfk)
            imetrics(learner,
                     "Test",
                     predictions$predicted_test,
                     test_set$wlfk)
        })
        ep <- Sys.time()
        cat("\tFinised at:", as.character(ep), "]\n")
        cat("[Time Ellapsed:\t",
            difftime(ep, sp, units = "secs"),
            " seconds]\n")
    }


learn.kknn <- function(formula, train, test, ...) {
    predicted_train <-
        kknn(formula, train = train, test = train, ...)$fit
    predicted_test <-
        kknn(formula, train = train, test = test, ...)$fit
    return(list(predicted_train = predicted_train,
                predicted_test = predicted_test))
}

global_performance <- NULL
kfold_cross_validation(
    formula = wlfk ~ .,
    data = cjb,
    kfolds = kfolds,
    learner = "learn.kknn",
    k = best_k,
    kernel = best_kernel
)


# CART --------------------------------------------------------------------

#决策树的生长
#rpart.plot包会自动加载rpart包
library(rpart.plot)
imodel <- rpart(wlfk ~ .,
                data = cjb[train_set_idx,])
imodel
# n= 541
#
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
#
# 1) root 541 258 文科 (0.47689464 0.52310536)
#   2) wl>=85.5 230  70 理科 (0.69565217 0.30434783)
#     4) sx>=87.5 185  41 理科 (0.77837838 0.22162162)
#       8) yw< 91.5 132  19 理科 (0.85606061 0.14393939) *
#       9) yw>=91.5 53  22 理科 (0.58490566 0.41509434)
#         18) wl>=88.5 38  10 理科 (0.73684211 0.26315789) *
#         19) wl< 88.5 15   3 文科 (0.20000000 0.80000000) *
#     5) sx< 87.5 45  16 文科 (0.35555556 0.64444444)
#       10) xb=男 23  10 理科 (0.56521739 0.43478261) *
#       11) xb=女 22   3 文科 (0.13636364 0.86363636) *
#   3) wl< 85.5 311  98 文科 (0.31511254 0.68488746)
#     6) xb=男 127  61 文科 (0.48031496 0.51968504)
#       12) yw< 88.5 98  43 理科 (0.56122449 0.43877551)
#         24) hx>=93 21   4 理科 (0.80952381 0.19047619) *
#         25) hx< 93 77  38 文科 (0.49350649 0.50649351)
#           50) ls< 86.5 41  16 理科 (0.60975610 0.39024390)
#             100) yw>=77.5 34  10 理科 (0.70588235 0.29411765) *
#             101) yw< 77.5 7   1 文科 (0.14285714 0.85714286) *
#           51) ls>=86.5 36  13 文科 (0.36111111 0.63888889) *
#       13) yw>=88.5 29   6 文科 (0.20689655 0.79310345) *
#     7) xb=女 184  37 文科 (0.20108696 0.79891304)
#       14) sw>=81.5 110  32 文科 (0.29090909 0.70909091)
#         28) ls< 91.5 62  27 文科 (0.43548387 0.56451613)
#           56) sx>=91.5 14   2 理科 (0.85714286 0.14285714) *
#           57) sx< 91.5 48  15 文科 (0.31250000 0.68750000) *
#         29) ls>=91.5 48   5 文科 (0.10416667 0.89583333) *
#       15) sw< 81.5 74   5 文科 (0.06756757 0.93243243) *


predicted_train <-
    predict(imodel,
            newdata = cjb[train_set_idx,],
            type = "class")
Metrics::ce(cjb$wlfk[train_set_idx],
            predicted_train)
#> [1] 0.1959335

#当然，我们更关注的是测试误差
predicted_test <-
    predict(imodel,
            newdata = cjb[-train_set_idx, ],
            type = "class")
Metrics::ce(cjb$wlfk[-train_set_idx],
            predicted_test)
#> [1] 0.2575107

#决策树剪枝
printcp(imodel, digits = 2)
#> Classification tree:
#>   rpart(formula = wlfk ~ ., data = cjb[train_set_idx, ])
#>
#> Variables actually used in tree construction:
#>   [1] hx ls sw sx wl wy xb
#>
#> Root node error: 266/542 = 0.49
#>
#> n= 542
#>
#>      CP nsplit rel error xerror  xstd
#> 1 0.349      0      1.00   1.00 0.045
#> 2 0.050      1      0.65   0.69 0.042
#> 3 0.023      2      0.60   0.70 0.042
#> 4 0.019      4      0.55   0.67 0.042
#> 5 0.017      7      0.50   0.67 0.042
#> 6 0.013      9      0.46   0.67 0.042
#> 7 0.012     12      0.42   0.66 0.042
#> 8 0.010     13      0.41   0.65 0.042
plotcp(imodel)

imodel$cptable
#>            CP nsplit rel error    xerror       xstd
#> 1 0.34883721      0 1.0000000 1.0000000 0.04502822
#> 2 0.05038760      1 0.6511628 0.6627907 0.04191607
#> 3 0.02325581      2 0.6007752 0.6589147 0.04184977
#> 4 0.01937984      4 0.5542636 0.6666667 0.04198161
#> 5 0.01744186      7 0.4961240 0.6627907 0.04191607
#> 6 0.01291990      9 0.4612403 0.6434109 0.04157683
#> 7 0.01162791     12 0.4224806 0.6356589 0.04143566
#> 8 0.01000000     13 0.4108527 0.6356589 0.04143566
imodel$cptable
#剪枝的一般方法
opt <- which.min(imodel$cptable[, "xerror"])
cp <- imodel$cptable[opt, "CP"]
#> [1] 0.01
imodel_pruned <- prune(imodel, cp = cp)
print(imodel_pruned)

#剪枝前后效果对比
predicted_train <- predict(imodel_pruned,
                           newdata = cjb[train_set_idx,],
                           type = "class")
Metrics::ce(cjb$wlfk[train_set_idx],
            predicted_train)
#> [1] 0.1959335
predicted_test <- predict(imodel_pruned,
                          newdata = cjb[-train_set_idx,],
                          type = "class")
Metrics::ce(cjb$wlfk[-train_set_idx],
            predicted_test)
#> 0.2575107


#绘制决策树的基本方法
plot(imodel)
text(imodel)
#上边的效果小伙伴们肯定是不满意的
rpart.plot(
    imodel_pruned,
    type = 4,
    fallen = F,
    branch = 0.5,
    round = 0,
    leaf.round = 2,
    clip.right.labs = T,
    cex = 0.85,
    under.cex = 0.75,
    box.palette = "GnYlRd",
    branch.col = "gray",
    branch.lwd = 2,
    extra = 108,
    #extra参数的含义需留意
    under = T,
    split.cex = 1
)

#除了可视化之外，我们还希望把这个树导成规则
library(rattle)
rules <- asRules(imodel_pruned, compact = TRUE)
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
learn.rpart <- function(formula, train, test, ...) {
    imodel_kfold <- rpart(formula, train) #模型训练
    opt <- which.min(imodel_kfold$cptable[, "xerror"])
    cp <- imodel_kfold$cptable[opt, "CP"]
    imodel_kfold <- prune(imodel_kfold, cp = cp)
    predicted_train <- predict(imodel_kfold, train, type = "class")
    predicted_test <- predict(imodel_kfold, test, type = "class")
    return(list(predicted_train = predicted_train,
                predicted_test = predicted_test))
}
kfold_cross_validation(
    formula = wlfk ~ .,
    data = cjb,
    kfolds = kfolds,
    learner = "learn.rpart"
)



# RandomForest ------------------------------------------------------------


library(randomForest)
set.seed(2012)
imodel <- randomForest(wlfk ~ .,
                       ntree = 25,
                       data = cjb[train_set_idx, ])
predicted_train <- predict(imodel,
                           newdata = cjb[train_set_idx,],
                           type = "response")
Metrics::ce(cjb$wlfk[train_set_idx],
            predicted_train)
#>[1] 0.001848429
predicted_test <- predict(imodel,
                          newdata = cjb[-train_set_idx,],
                          type = "response")
Metrics::ce(cjb$wlfk[-train_set_idx],
            predicted_test)
#> [1] 0.1845494


rf_ces <- sapply(1:500, function(x) {
    set.seed(2012)
    imodel <- randomForest(wlfk ~ .,
                           ntree = x,
                           data = cjb[train_set_idx, ])
    predicted_train <- predict(imodel,
                               newdata = cjb[train_set_idx,],
                               type = "response")
    Metrics::ce(cjb$wlfk[train_set_idx],
                predicted_train)
    #>[1] 0
    predicted_test <- predict(imodel,
                              newdata = cjb[-train_set_idx,],
                              type = "response")
    Metrics::ce(cjb$wlfk[-train_set_idx],
                predicted_test)
})
which.min(rf_ces)
plot(rf_ces, type = "o")

#基于OOB的误分率
imodel$confusion
#>       理科 文科 class.error
#> 理科  195   71   0.2669173
#> 文科   54  222   0.1956522

#进行k-折交叉检验k-fold cross validation
learn.randomForest <- function(formula, train, test, ...) {
    imodel_kfold <- randomForest(formula, train, ...)
    predicted_train <-
        predict(imodel_kfold, train, type = "response")
    predicted_test <- predict(imodel_kfold, test, type = "response")
    return(list(predicted_train = predicted_train,
                predicted_test = predicted_test))
}
kfold_cross_validation(
    formula = wlfk ~ .,
    data = cjb,
    kfolds = kfolds,
    learner = "learn.randomForest",
    ntree = which.min(rf_ces)
)



# NaiveBayes --------------------------------------------------------------

library(e1071)
imodel <- naiveBayes(wlfk ~ .,
                     data = cjb[train_set_idx, ])
predicted_train <- predict(imodel,
                           newdata = cjb[train_set_idx,],
                           type = "class")
Metrics::ce(cjb$wlfk[train_set_idx], predicted_train)
#> [1] 0.2920518
predicted_test <- predict(imodel,
                          newdata = cjb[-train_set_idx,],
                          type = "class")
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#> [1] 0.27897

#进行k-折交叉检验k-fold cross validation
learn.naiveBayes <- function(formula, train, test, ...) {
    imodel_kfold <- naiveBayes(formula, train)
    predicted_train <-  predict(imodel_kfold, train, type = "class")
    predicted_test <- predict(imodel_kfold, test, type = "class")
    return(list(predicted_train = predicted_train,
                predicted_test = predicted_test))
}
kfold_cross_validation(
    formula = wlfk ~ .,
    data = cjb,
    kfolds = kfolds,
    learner = "learn.naiveBayes"
)


# Logistic Regression -----------------------------------------------------

library(ggplot2)

# 以下代码仅为复现课件中的动画，感兴趣的小伙伴可以了解一下
# library(animation)
# saveGIF(
#     expr = {
#         mov_frame <- 5 * (1:30)
#         for (i in mov_frame) {
#             x <- seq(-i, i, len = 1000)
#             y <- 1 / (1 + exp(-x))
#             p <- ggplot(data.frame(x, y), aes(x = x, y = y)) +
#                 geom_line()
#             if (i == head(mov_frame, 1) ||
#                 i == tail(mov_frame, 1)) {
#                 #开始和结束时多停留一会儿
#                 lapply(1:5, function(x)
#                     plot(p))
#             }
#             plot(p)
#         }
#     },
#     movie.name = "animation.gif",
#     convert = "gm convert",
#     interval = 0.2
# )
# dev.off()
#



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
#> [1] 0.2181146
predicted_logit <- predict(imodel,
                           newdata = cjb[-train_set_idx, ],
                           type = "response")
predicted_test <-
    rep(levels(cjb$wlfk)[2], nrow(cjb[-train_set_idx,]))
predicted_test[predicted_logit < 0.5] <-
    levels(cjb$wlfk)[1]
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#> [1] 0.1888412

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
best_threshold
#> [1] 0.592

#当然，也可以用下边这种写法
threshold_range <- seq(0.1, 0.9, by = 0.001)
ce_set <- sapply(threshold_range, function(cur_threshold) {
    predicted_test <-
        rep(levels(cjb$wlfk)[2], nrow(cjb[-train_set_idx,]))
    predicted_test[predicted_logit < cur_threshold] <-
        levels(cjb$wlfk)[1]
    cur_err <- Metrics::ce(cjb$wlfk[-train_set_idx],
                           predicted_test)
})
#最佳阈值
threshold_range[which.min(ce_set)]
#相应的分类错误率
min(ce_set)

#进行k-折交叉检验k-fold cross validation
learn.LogisticRegression <- function(formula, train, test, ...) {
    dot_args <- list(...)
    imodel_kfold <-
        glm(formula, train, family = binomial(link = "logit"))
    predicted_logit <-
        predict(imodel_kfold, train, type = "response")
    predicted_train <- rep(levels(cjb$wlfk)[2], nrow(train))
    predicted_train[predicted_logit < dot_args[["best_threshold"]]] <-
        levels(cjb$wlfk)[1]
    predicted_logit <-
        predict(imodel_kfold, test, type = "response")
    predicted_test <- rep(levels(cjb$wlfk)[2], nrow(test))
    predicted_test[predicted_logit < dot_args[["best_threshold"]]] <-
        levels(cjb$wlfk)[1]
    return(list(predicted_train = predicted_train,
                predicted_test = predicted_test))
}
kfold_cross_validation(
    formula = wlfk ~ .,
    data = cjb,
    kfolds = kfolds,
    learner = "learn.LogisticRegression",
    best_threshold = threshold_range[which.min(ce_set)]
)


# Artificial Neural Network -----------------------------------------------

library(nnet)
set.seed(2012)
imodel <- nnet(wlfk ~ .,
               data = cjb[train_set_idx, ],
               size = 7)
names(imodel)
#> [1] "n"             "nunits"        "nconn"
#> [4] "conn"          "nsunits"       "decay"
#> [7] "entropy"       "softmax"       "censored"
#> [10] "value"         "wts"           "convergence"
#> [13] "fitted.values" "residuals"     "lev"
#> [16] "call"          "terms"         "coefnames"
#> [19] "contrasts"     "xlevels"

imodel$n
#> [1] 10  7  1
imodel$wts
#> [1]   -0.394367962    0.341672486   -0.305656476
#> [4]    0.609244299    0.344983392    0.524696717
#> [7]    0.049098761    0.577261671    0.553892391
#> [79]    0.851107738    0.275935098   -0.237562349
#> [82]    0.109386068    0.637609693   -2.774100396
#> [85]    0.019783268
imodel$fitted.values
#     [,1]
# 1   0.8048857
# 2   0.2047307
# 3   0.8048857
#
# 540 0.8048857
# 541 0.2047307

predicted_train <- predict(imodel,
                           newdata = cjb[train_set_idx,],
                           type = "class")
Metrics::ce(cjb$wlfk[train_set_idx], predicted_train)
#> [1] 0.1996303
predicted_test <- predict(imodel,
                          newdata = cjb[-train_set_idx,],
                          type = "class")
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#> [1] 0.1759657

#神经网络参数的设置相对比较复杂
#一般来讲，没有绝对的套路可循
#我们当然可以写一些循环，来进行参数的选择
#不过，类似于e1071::tune.nnet()已经替我们作了很多工作
#下面，采用的是caret包中的方法
#通过caret包中的grid搜索来进行参数选择
tune_results <- e1071::tune.nnet(
    wlfk ~ .,
    data = cjb,
    decay = c(0.01, 0.03, 0.1, 0.3, 0.6, 0.9),
    size = 1:7
)

library(caret)
set.seed(2012)
nn_grid <- expand.grid(size = c(1, 3, 7, 9),
                       decay = c(0.01, 0.03, 0.1, 0.3, 0.6, 0.9))
# nn_grid <- expand.grid(.decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7),
#                        .size = c(3, 5, 10, 20))
imodel <- train(
    wlfk ~ .,
    data = cjb,
    method = "nnet",
    maxit = 10000,
    tuneGrid = nn_grid
)
imodel$bestTune
#>    size decay
#> 9    1  0.6
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
imodel2 <-  nnet(
    wlfk ~ .,
    data = train_set,
    decay = imodel$bestTune$decay,
    size = imodel$bestTune$size,
    maxit = 2000
)
imodel2$wts
str(imodel2)
library(NeuralNetTools)
plotnet(
    imodel2,
    rel_rsc = c(1.8, 3),
    circle_cex = 3,
    cex_val = 0.75,
    bord_col = "lightblue",
    max_sp = TRUE
)


#进行k-折交叉检验k-fold cross validation
learn.nnet <- function(formula, train, test, ...) {
    imodel_kfold <-  nnet(formula, train, ...)
    predicted_train <-  predict(imodel_kfold, train, type = "class")
    predicted_test <- predict(imodel_kfold, test, type = "class")
    return(list(predicted_train = predicted_train,
                predicted_test = predicted_test))
}

kfold_cross_validation(
    formula = wlfk ~ .,
    data = cjb,
    kfolds = kfolds,
    learner = "learn.nnet",
    decay = imodel$bestTune$decay,
    size = imodel$bestTune$size,
    maxit = 2000
)



# Support Vector Machine --------------------------------------------------

library(kernlab)
set.seed(2012)
imodel <- ksvm(wlfk ~ .,
               data = cjb[train_set_idx, ])
predicted_train <- predict(imodel,
                           newdata = cjb[train_set_idx,],
                           type = "response")
Metrics::ce(cjb$wlfk[train_set_idx], predicted_train)
#> [1] 0.1497227
predicted_test <- predict(imodel,
                          newdata = cjb[-train_set_idx,],
                          type = "response")
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#> [1] 0.1759657
imodel
#当然也可以通过caret来进行调参
library(caret)
svm_grid <- expand.grid(sigma = 2 ^ (-10:4),
                        C = -5:20)
set.seed(2012)
imodel <- train(
    wlfk ~ .,
    data = cjb[train_set_idx, ],
    method = "svmRadial",
    preProc = c("center", "scale"),
    tuneGrid = svm_grid
)
imodel$bestTune
#>   sigma C
#> 2  0.25 1

#同样也可以对train的结果进行绘制
plot(imodel)

#进行k-折交叉检验k-fold cross validation
learn.svm <- function(formula, train, test, ...) {
    imodel_kfold <-  ksvm(formula, train, ...)
    predicted_train <-
        predict(imodel_kfold, train, type = "response")
    predicted_test <- predict(imodel_kfold, test, type = "response")
    return(list(predicted_train = predicted_train,
                predicted_test = predicted_test))
}
kfold_cross_validation(
    formula = wlfk ~ .,
    data = cjb,
    kfolds = kfolds,
    learner = "learn.svm",
    C = imodel$bestTune$C,
    gamma = imodel$bestTune$sigma
)


# Variable Importance -----------------------------------------------------

#完成了模型训练、模型评估，故事基本告一段落
#再回顾一下本讲开始所讲的featurePlot
#进行完模型训练之后，咱们再通过变量重要性印证一下
#变量重要性，有很多评价方法
#既有 Model Specific Metrics，也有Model Independent Metrics
#如果是采用caret框架进行训练的话，多种指标可选
#具体请参阅
#http://topepo.github.io/caret/variable-importance.html

library(randomForest)
imodel <- randomForest(wlfk ~ ., data = cjb)
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
    geom_text(aes(
        y = MeanDecreaseGini * 1.02,
        label = format(MeanDecreaseGini, digits = 4)
    ))






# Model Comparison --------------------------------------------------------


#模型进行评估
global_performance %>%
    group_by(method, type) %>%
    summarise(mean_error_rate = mean(error_rate)) %>%
    arrange(type, mean_error_rate) %>%
    ggplot(aes(
        x = fct_inorder(method),
        y = mean_error_rate,
        fill = type
    )) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = format(mean_error_rate, digits = 3)),
              position = position_dodge(width = 1)) +
    scale_fill_manual(values = c("orange", "darkgrey")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))



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
#MASS::lda()
#adabag::bagging()
#adabag::boosting()
#caretEnsemble::caretStack
#xgboost::xgboost
#即便是演示过的算法，参数调优过程也显得比较粗糙
#更多的精彩，由小伙伴们自行探索吧
#毕竟，这份代码只是一个引导性的参考，
#并不是可以简单套用的标准模板



# The End ^-^ -------------------------------------------------------------
