

# 07_方以类聚、物以群分 ---------------------------------------------------


#前一实验，分类与回归，是有监督学习的代名词
#本实验，则主要是聚焦于无监督学习
#无监督学习涉及到特征降维、聚类分析等
#本实验主体内容是聚类分析

#日常生活中，我们都说“物以类聚、人以群分”
#不过考虑到区分对象的普遍性
#我们还是从《易传》中找了一个成语来概括我们的主题：
#方以类聚、物以群分

#同样，为了减少熟悉问题情境的时间和精力
#和前述其他实验一样，
#本实验数据依然是学生成绩

# Data Import -------------------------------------------------------------

#加载数据
rm(list = ls())
library(tidyverse)
library(magrittr)
library(GGally)
cjb_url <- "data/cjb.csv"
cjb <- read_csv(cjb_url,
                locale = locale(encoding = "CP936"))
cjb %<>%
    mutate(zcj = rowSums(.[4:12])) %>%
    filter(zcj != 0) %>% #剔除脏数据
    mutate_at(vars(xb, wlfk), factor) #类型转换


#对于聚类而言，主要是观察数据空间的结构
#这里的空间结构，主要就是距离结构
#拿到数据之后，同样是进行数据点散布的观测


# Distance ----------------------------------------------------------------

#聚类主要是考察数据点之间的距离关系
#在R里边，最常用的是dist()函数
#可以求取欧氏距离、明氏距离等
#但不能求取混合类型数据的距离
#先看一个简单的示例
library(cluster)
artificial_data <- data.frame(x = as.factor(c(1, 1, 4, 1)),
                              y = c(2, 3, 7, 2),
                              z = c(1, 3, 1, 1))
dist(artificial_data[, 2:3],
     method = "euclidean",
     diag = TRUE,
     upper = TRUE)
dist(artificial_data[, 2:3],
     method = "maximum",
     diag = TRUE,
     upper = TRUE)
dist(
    artificial_data[, 2:3],
    method = "minkowski",
    diag = TRUE,
    upper = TRUE,
    p = 1
)
dist(artificial_data[, 2:3],
     method = "manhattan",
     diag = TRUE,
     upper = TRUE)
#求取混合类型的距离
library(cluster)
as.matrix(daisy(artificial_data))

#若只考虑各科成绩，
#观察各数据点（一个同学一个数据点）之间的距离
cj <- cjb[, 4:12]
cj_matri <- as.matrix(cj)
cj_dist <- dist(cj_matri)
#距离可视化
library("factoextra")
fviz_dist(
    cj_dist,
    order = FALSE,
    gradient = list(low = "#00AFBB",
                    mid = "white",
                    high = "#FC4E07"),
    show_labels = FALSE
)
#也可以考虑性别
library(cluster)
cj_dist_plus <- daisy(cjb[, 3:12])
fviz_dist(
    cj_dist_plus,
    order = FALSE,
    gradient = list(low = "#00AFBB",
                    mid = "white",
                    high = "#FC4E07"),
    show_labels = FALSE
)


# MDS ---------------------------------------------------------------------

#多维标度分析
#将高维空间的距离关系，映射到二维空间
cj_mds <- cmdscale(d = cj_dist, k = 2)
library(tidyverse)
cj_mds %>%
    as.data.frame() %>%
    mutate(name = cjb$xm,
           type = cjb$wlfk) %>%
    setNames(c("x", "y", "name", "type")) %>%
    ggplot(aes(x = x, y = y)) +
    geom_text(aes(label = name, colour = type),
              size = 3,
              alpha = 0.75)
#当然也可以增加性别的因素
cj_mds_plus <- cmdscale(d = cj_dist_plus, k = 2)
library(tidyverse)
cj_mds_plus %>%
    as.data.frame() %>%
    mutate(name = cjb$xm,
           type = cjb$wlfk,
           sex = cjb$xb) %>%
    setNames(c("x", "y", "name", "type", "sex")) %>%
    ggplot(aes(x = x, y = y)) +
    geom_text(aes(label = name, colour = type),
              size = 3,
              alpha = 0.75)


# Hopkins -----------------------------------------------------------------

#数据能聚类么？
#霍普金斯统计量
#对数据进行聚类，有一个逻辑前提：
#数据不是均匀分布的
#而是呈现一定的模式
#这里的模式，就是表现为数据是倾斜的，而非均匀的
library(clustertend)
set.seed(2012)
scores <- cjb %>%
    select(yw:sw)
n <- floor(nrow(cjb) * 0.05)
hopkins_100 <- unlist(replicate(100,  hopkins(scores, n)))
mean(hopkins_100)
#> [1] 0.1577968
ggplot(data.frame(H = hopkins_100), aes(x = factor(0), y = H)) +
    geom_boxplot(width = 0.5) +
    geom_rug(position = "jitter",
             sides = "b") +
    coord_flip()
#取值偏向于0.5时，是均匀的
#偏向于0时，是倾斜的
#由此可见，目前这份数据进行聚类还是可行的


# k-means -----------------------------------------------------------------

# #以下代码，仅为复现课件中的动画，感兴趣的小伙伴可以了解
# #先直观展示一下kmeans的迭代过程
# #小伙伴们也可以如法炮制
# #写一些简单版的算法，有助于理解算法原理
# library(animation)
# library(deldir)
# saveGIF(
#     expr = {
#         data("iris")
#         set.seed(2012)
#         init_centers_idx <- sample(150, 3)
#         init_centers <-
#             iris[init_centers_idx, c("Petal.Length", "Petal.Width")]
#         voronoi <- deldir(init_centers[, 1], init_centers[, 2]) #对空间进行划分
#         cur_centers <- init_centers
#         old_centers <-
#             -cur_centers #其实可以是和cur_centers长宽相同的任意data.frame,但里边的值不能相同
#         old_centers == cur_centers
#         repeat_first <- TRUE
#         while (any(old_centers != cur_centers)) {
#             #新的中心和原来的中心是否完全一样
#             #将点分派至不同的中心
#             Species_type <-
#                 apply(iris[, c("Petal.Length", "Petal.Width")], 1, function(x) {
#                     which.min(apply(cur_centers[, c("Petal.Length", "Petal.Width")], 1, function(y) {
#                         sqrt(sum((x - y) ^ 2))
#                     }))
#                 })
#             iris$Species <-
#                 c("setosa", "versicolor", "virginica")[Species_type]
#             voronoi <-
#                 deldir(cur_centers$Petal.Length, cur_centers$Petal.Width)
#             p <- ggplot() +
#                 geom_point(data = iris,
#                            aes(
#                                x = Petal.Length,
#                                y = Petal.Width,
#                                colour = Species
#                            )) +
#                 geom_point(
#                     data = cur_centers,
#                     aes(
#                         x = Petal.Length,
#                         y = Petal.Width,
#                         colour = "red"
#                     ),
#                     size = 3
#                 ) +
#                 geom_segment(
#                     data = voronoi$dirsgs,
#                     aes(
#                         x = x1,
#                         y = y1,
#                         xend = x2,
#                         yend = y2
#                     ),
#                     size = 1.2,
#                     linetype = 1,
#                     color = "red"
#                 ) +
#                 coord_fixed() + #固定长宽比例，否则看不出垂直的效果
#                 theme(legend.position = "none")
#
#             plot(p)
#             if (repeat_first) {
#                 plot(p)
#                 plot(p)
#                 plot(p)
#                 repeat_first <- FALSE
#             }
#
#
#             old_centers <- cur_centers
#             #重新计算各组的中心
#             cur_centers <- iris %>%
#                 group_by(Species) %>%
#                 summarise(
#                     Petal.Length = mean(Petal.Length),
#                     Petal.Width = mean(Petal.Width)
#                 ) %>%
#                 mutate(Species = NULL)
#         }
#     },
#     movie.name = "animation.gif",
#     convert = "gm convert",
#     interval = 1,
#     ani.width = diff(range(iris$Petal.Length)) * 100,
#     ani.height = diff(range(iris$Petal.Width)) * 100
# )

scores <- cjb %>%
    select(yw:sw)
#stats包中的kmeans()函数
set.seed(2012)
imodel <- kmeans(scores,
                 centers = 2)
names(imodel)
#> [1] "cluster"      "centers"      "totss"
#> [4] "withinss"     "tot.withinss" "betweenss"
#> [7] "size"         "iter"         "ifault"

imodel$cluster
#> [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1
#> [24] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> [47] 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1
#> [714] 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2
#> [737] 2 2 2 2 2 2 2 2 2 1 2 2 2 1 1 1 1 1 1 1 1 1 1
#> [760] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

imodel$centers
#>    yw       sx       wy       zz       ls
#> 1 85.07616 77.06623 83.24172 90.54967 84.85430
#> 2 88.85169 92.03178 90.24153 93.46822 91.88559
#>    dl       wl       hx       sw
#> 1 90.08940 71.22185 85.64901 79.58609
#> 2 94.91949 87.58475 95.54661 90.72034

imodel$totss
#> [1] 431975.9
imodel$withinss
#> [1] 168392.4 105535.6
imodel$tot.withinss
#> [1] 273928
imodel$betweenss
#> [1] 158047.9

imodel$size
#> [1] 472 302

cluster_idx <- imodel$cluster

#手工实现一次代码
global_center <- apply(scores, 2, mean)
total_SS <- sum(apply(scores, 1, function(x) {
    sum((x - global_center) ^ 2)
}))
total_SS
#> [1] 431975.9


library(fpc)
kmeans_results <- kmeansruns(scores,
                             criterion = "asw")
kmeans_results
#> Available components:
#>
#>   [1] "cluster"      "centers"      "totss"
#> [4] "withinss"     "tot.withinss" "betweenss"
#> [7] "size"         "iter"         "ifault"
#> [10] "crit"         "bestk"

kmeans_results$crit
#> [1] 0.0000000 0.3330389 0.2490188 0.2453507
#> [5] 0.1889768 0.1652665 0.1603259 0.1579961
#> [9] 0.1552047 0.1401957

plot(kmeans_results$crit, type = "o")#> 后续有更好的展示方式，当然本质上是一样的

kmeans_results$bestk
#> [1] 2

library(factoextra)
fviz_nbclust(scores,
             kmeans,
             method = "silhouette",
             k.max = 20) +
    geom_vline(xintercept = 2, linetype = 2)

#绘制聚类效果图
library(factoextra)
fviz_cluster(imodel,
             data = scores,
             ellipse.type = "convex") +
    theme_minimal()

require(cluster)
scores_dist <- dist(scores)
imodel2 <- kmeans(scores, 2)
cluster_idx2 <- imodel2$cluster
#计算轮廓系数
kmeans_k2_silhouette <- silhouette(cluster_idx2, scores_dist)
#绘制轮廓系数
fviz_silhouette(kmeans_k2_silhouette)
imodel3 <- kmeans(scores, 3)
cluster_idx3 <- imodel3$cluster
k3_silhouette <- silhouette(cluster_idx3, scores_dist)
fviz_silhouette(k3_silhouette)


fviz_cluster(
    imodel,
    data = scores,
    palette = c("#2E9FDF", "#00AFBB"),
    ellipse.type = "euclid",
    # Concentration ellipse
    star.plot = TRUE,
    # Add segments from centroids to items
    #repel = TRUE, # Avoid label overplotting (slow)
    ggtheme = theme_minimal()
)

min(Metrics::ce(cjb$wlfk,
                c("理科", "文科")[imodel$cluster]),
    1 - Metrics::ce(cjb$wlfk,
                    c("理科", "文科")[imodel$cluster]))
#> [1] 0.3540052

#是特征越多越好么？
#未必如此
set.seed(2012)
imodel <- kmeans(scores[, c("sw", "wl", "sx")],
                 centers = 2)
table(cjb$wlfk, c("文科", "理科")[imodel$cluster])
min(Metrics::ce(cjb$wlfk,
                c("理科", "文科")[imodel$cluster]),
    1 - Metrics::ce(cjb$wlfk,
                    c("理科", "文科")[imodel$cluster]))
#> [1] 0.3294574

Metrics::ce(cjb$wlfk, c("理科", "文科")[imodel$cluster])
imodel$centers


# Hierarchical Clustering -------------------------------------------------

#Demo
selected_students <- c("伊礼贤", "鲁孟秋", "焦金音", "宁琦", "赖旺",
                       "于知平", "方顺", "谭思缘", "僪福星", "尚玉芳")
scores <- cjb %>%
    filter(xm %in% selected_students) %>%
    select(xm, yw:sw) %>%
    column_to_rownames(var = "xm")
row.names(scores)
#计算距离矩阵
demo_dist <- dist(scores)
#利用hclust进行聚类
imodel <- hclust(demo_dist)
imodel
#>
#> Call:
#>   hclust(d = demo_dist)
#>
#> Cluster method   : complete
#> Distance         : euclidean
#> Number of objects: 10
names(imodel)
#> [1] "merge"       "height"      "order"       "labels"
#> [5] "method"      "call"        "dist.method"
imodel$merge
#>       [,1] [,2]
#> [1,]   -7   -8
#> [2,]   -6   -9
#> [3,]    1    2
#> [4,]   -2  -10
#> [5,]   -3    4
#> [6,]   -5    5
#> [7,]   -1    6
#> [8,]   -4    7
#> [9,]    3    8

min(dist(scores))
imodel$height
#> [1]   4.000000
#> [2]   5.291503
#> [3]   7.937254
#> [4]  24.819347
#> [5]  29.933259
#> [6]  41.073106
#> [7]  44.068129
#> [8]  76.360985
#> [9] 134.000000
imodel$height
#> [1]   4.000000   5.291503   7.937254  24.819347
#> [5]  29.933259  41.073106  44.068129  76.360985
#> [9] 134.000000
sort(dist(scores))
#> [1]   4.000000   5.196152   5.291503   5.567764
#> [5]   7.000000   7.937254  24.819347  26.888659
#> [9]  27.092434  29.933259  32.572995  36.891733
#> [13]  38.639358  41.073106  43.347434  44.068129
#> [17]  54.194096  57.271284  63.229740  70.285134
#> [21]  76.360985  76.674637  77.485483  77.833155
#> [25]  79.517294  81.492331  82.042672  82.788888
#> [29]  83.928541  90.752410  90.901045  91.010988
#> [33]  91.021975  91.656969  92.238820  92.293012
#> [37]  93.616238 100.682670 100.935623 101.113797
#> [41] 102.815369 132.461315 133.540256 133.787144
#> [45] 134.000000
imodel$order
#> [1]  7  8  6  9  4  1  5  3  2 10
imodel$labels
#> [1] "于知平" "僪福星" "谭思缘" "赖旺"   "尚玉芳"
#> [6] "焦金音" "伊礼贤" "鲁孟秋" "宁琦"   "方顺"
imodel$method
#>[1] "complete"
imodel$call
#>hclust(d = demo_dist)
imodel$dist.method
#>[1] "euclidean"

imodel$order <- rev(imodel$order)
plot(imodel, hang = -1)

cluster_idx <- cutree(imodel, k = 2)
#> 于知平 僪福星 谭思缘
#> 1      1      1
#> 赖旺 尚玉芳 焦金音
#> 1      1      2
#> 伊礼贤 鲁孟秋   宁琦
#> 2      2      2
#> 方顺
#> 1
plot(imodel, hang = -1)
rect.hclust(imodel, k = 2)

library(factoextra)
res <- hcut(
    dist(scores),
    k = 2,
    hc_func = "hclust",
    hc_method = "complete",
    hc_metric = "euclidean",
    stand = FALSE,
    graph = FALSE
)
fviz_dend(
    res,
    rect = TRUE,
    cex = 0.75,
    horiz = TRUE,
    type = "rectangle",
    k_colors = c("#CD534CFF", "#0073C2FF")
)

require(cluster)
scores <- cjb %>%
    select(yw:sw)
scores_dist <- dist(scores)
imodel <- hclust(scores_dist, method = "ward.D")
cluster_idx <- cutree(imodel, k = 2)
#计算轮廓系数
kmeans_k2_silhouette <- silhouette(cluster_idx, scores_dist)
#绘制轮廓系数
fviz_silhouette(kmeans_k2_silhouette)
cluster_idx <- cutree(imodel, k = 3)
k3_silhouette <- silhouette(cluster_idx, scores_dist)
fviz_silhouette(k3_silhouette)


fviz_nbclust(scores,
             FUNcluster = hcut,
             method = "silhouette",
             kmax = 20) +
    geom_vline(xintercept = 2, linetype = 2)


imodel <- hclust(scores_dist, method = "ward.D")
cluster_idx <- cutree(imodel, k = 2)
(ic_metric <- min(Metrics::ce(cjb$wlfk,
                              c("理科", "文科")[cluster_idx]),
                  1 - Metrics::ce(cjb$wlfk,
                                  c("理科", "文科")[cluster_idx])))
#> [1] 0.2860892
#分类准确率挺近70%大关


# About Model Innovation --------------------------------------------------

#了解完以上的基本原理之后，
#小伙伴们也应该有算法创造者的角度，
#对其开展研究
library(DMwR)
out_rank <- outliers.ranking(scores_dist,
                             clus = list(dist = "euclidean",
                                         alg = "hclust", meth = "ward.D"))
cjb %>%
    arrange(desc(out_rank$prob.outliers)) %>%
    View()
#与箱线图异常值检测作比较
(outliers <- boxplot.stats(cjb$zcj)$out)
outliers_idx <- which(cjb$zcj %in% outliers)
View(cjb[outliers_idx,])

out_rank$prob.outliers
cjb[order(out_rank$prob.outliers, decreasing = TRUE)[1:10],] %>%
    View()
(outliers <- boxplot.stats(cjb$zcj)$out)
outliers_idx <- which(cjb$zcj %in% outliers)
View(cjb[outliers_idx,])



# The End ^-^ -------------------------------------------------------------
