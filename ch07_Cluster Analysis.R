

#######################################################
#######################################################
##
## 名称：《R语言数据分析·聚类分析》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V7
## 时间：2017年9月
##
##*****************************************************
##
## ch07_Cluster Analysis_V7
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



#前一实验，分类与回归，是有监督学习的代名词
#本实验，则主要是聚焦于无监督学习
#无监督学习涉及到特征降维、聚类分析等
#本实验主体内容是聚类分析

#同样，为了减少熟悉问题情境的时间和精力
#和前述其他实验一样，
#本实验数据依然是学生成绩

#######################################################
##观察数据
#######################################################
#加载数据
rm(list = ls())
load("cj.rda", verbose = TRUE)
View(cheng_ji_biao)

#对于聚类而言，主要是观察数据空间的结构
#这里的空间结构，主要就是距离结构
#拿到数据之后，同样是进行数据点散布的观测

#先采用平行坐标观测高维空间的数据分布
#平行坐标图
#结果与Get to Know Your Data一样
#但是采用pipe方式重写了
cheng_ji_biao %>%
  group_by(文理分科) %>%
  arrange(desc(总成绩)) %>%
  head(n = 300) %>%
  ungroup() %>%
  ggparcoord(columns = 3:12,
             groupColumn = 13) +
  geom_point()
#平行坐标图的结果，形如渔网
#但更应观测其中具有辨识能力的变量
#如生物、物理、数学等

#聚类，本质上是识别数据点在数据空间的（距离）结构
#可以先看看在生物、物理二维空间里是怎么分布的
library(ggplot2)
ggplot(cheng_ji_biao, aes(x = 物理, y = 数学)) +
  geom_point(aes(colour = 文理分科, shape = 文理分科))
#数据空间的密度
#将50~100细分为N份，看每一个有多少落入其间
ibreaks <- seq(50, 100, len = 21)
cheng_ji_biao %>%
  select(物理, 数学) %>%
  mutate(物理 = cut(物理, breaks = ibreaks),
           数学 = cut(数学, breaks = ibreaks)) %>%
  group_by(物理, 数学) %>%
  summarise(freq = n()) %>%
  complete(物理, 数学) %>%
  mutate(freq = ifelse(is.na(freq), 0, freq)) %>%
  ggplot(aes(x = 物理, y = 数学, fill = freq)) +
  geom_tile(colour="white", size = 0.5) +
  geom_text(aes(label = freq), size = 3) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle = 90))


#三维散点图
library(rgl)
plot3d(
  x = cheng_ji_biao$数学,
  y = cheng_ji_biao$物理,
  z = cheng_ji_biao$生物,
  xlab = "Mathematics", 
  ylab = "Physics",
  zlab = "Biology",
  type = "s",
  size = 0.5,
  col = c("red", "green")[cheng_ji_biao$文理分科])

#剔除其中的缺失值和异常值
na_idx <- which(cheng_ji_biao$总成绩 == 0)
cheng_ji_biao <- cheng_ji_biao[-na_idx, ]
(outliers <- boxplot.stats(cheng_ji_biao$总成绩)$out)
#离群值标签
(outliers_idx <- which(cheng_ji_biao$总成绩
                       %in% outliers))
cheng_ji_biao <- cheng_ji_biao[-outliers_idx, ]
#重新运行前述plot3d代码
library(rgl)
plot3d(
  x = cheng_ji_biao$数学,
  y = cheng_ji_biao$物理,
  z = cheng_ji_biao$生物,
  xlab = "Mathematics", 
  ylab = "Physics",
  zlab = "Biology",
  type = "s",
  radius = 0.5,
  col = c("red", "green")[cheng_ji_biao$文理分科])

#绘制其中的密度分布
selected_cols <- c("数学", "物理", "生物")
shu_wu_sheng <- cheng_ji_biao[, selected_cols]
sws_dist <- as.matrix(dist(shu_wu_sheng,
                           diag = TRUE,
                           upper = TRUE))
iseq <- seq(50, 100, len = 10)
imatrix <- expand.grid(iseq, iseq, iseq)
names(imatrix) <- selected_cols
dist_imatrix <- apply(imatrix, 1, function(x) {
  apply(shu_wu_sheng, 1, function(y) {
    sqrt(sum((y - x)^2))
  })
})
#定义半径为平均距离
epsilon <- mean(sws_dist)
#计算半径范围之内的点数作为密度
sws_density <- apply(dist_imatrix, 2, function(x) {
  sum(x < epsilon)
})
#调颜色
my_color_ramp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}
cols <- my_color_ramp(c("white", "red"), sws_density) 
#绘制图形
grd <- imatrix
grd$col <- cols
length <- width <- height <- (max(iseq) - min(iseq)) / length(iseq)
for(i in seq(nrow(grd))){
  #创建一个长方体
  icube3d <- cube3d(col = grd$col[i])
  #设定长宽高
  icube3d <- scale3d(icube3d, length, width, height)
  #将长方体移动至指定位置
  icube3d <- translate3d(icube3d, grd$数学[i], grd$物理[i], grd$生物[i])
  #绘制长方体
  shade3d(icube3d, alpha = 0.05)
}


#从三维图之中，已经能直观的看到数据的散布情况
#及其密度分布
#对数据进行聚类，有一个逻辑前提：
#数据不是均匀分布的
#而是呈现一定的模式
#这里的模式，就是表现为数据是倾斜的，而非均匀的

#数据能聚类么？
#霍普金斯统计量
clustertend::hopkins(cj_matri, n = 100)
#[1] 0.1865712
#取值偏向于0.5时，是均匀的
#偏向于0时，是倾斜的

#当然，我们也可以同时取多个值
sapply((1:10)*50, function(x) {
  clustertend::hopkins(cj_matri, x)
})
# 0.2123391 0.1990008 0.1992889 0.204042 0.1974034 
#0.1920272 0.2077448 0.191369 0.193734 0.1989523
#由此可见，目前这份数据进行聚类还是可行的


#再看看数据点之间的距离关系
#现在考虑所有维度，求欧氏距离
#在R里边，最常用的是dist()函数
#可以求取欧氏距离、明氏距离等
#但不能求取混合类型数据的距离
#先看一个简单的示例
library(cluster)
artificial_data <- data.frame(
  x = as.factor(c(1, 1, 4, 1)), 
  y = c(2, 3, 7, 2),
  z = c(1, 3, 1, 1))
dist(
  artificial_data[, 2:3],
  method = "euclidean",
  diag = TRUE,
  upper = TRUE
)
dist(
  artificial_data[, 2:3],
  method = "maximum",
  diag = TRUE,
  upper = TRUE
)
dist(
  artificial_data[, 2:3],
  method = "minkowski",
  diag = TRUE,
  upper = TRUE,
  p = 1
)
dist(
  artificial_data[, 2:3],
  method = "manhattan",
  diag = TRUE,
  upper = TRUE
)
#求取混合类型的距离
library(cluster)
as.matrix(daisy(artificial_data))

#若只考虑各科成绩，
#观察各数据点（一个同学一个数据点）
#之间的距离
cj <- cheng_ji_biao[, 4:12]
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
cj_dist_plus <- daisy(cheng_ji_biao[, 3:12])
fviz_dist(
  cj_dist_plus,
  order = FALSE,
  gradient = list(low = "#00AFBB",
                  mid = "white",
                  high = "#FC4E07"),
  show_labels = FALSE
)

#多维标度分析
#将高维空间的距离关系，映射到二维空间
cj_mds <- cmdscale(d = cj_dist, k = 2)
library(tidyverse)
cj_mds %>%
  as.data.frame() %>%
  mutate(name = cheng_ji_biao$姓名,
         type = cheng_ji_biao$文理分科) %>%
  setNames(c("x", "y", "name", "type")) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_text(aes(label = name, colour = type), 
            size = 3, alpha = 0.75)
#当然也可以增加性别的因素
cj_mds_plus <- cmdscale(d = cj_dist_plus, k = 2)
library(tidyverse)
cj_mds_plus %>%
  as.data.frame() %>%
  mutate(name = cheng_ji_biao$姓名,
         type = cheng_ji_biao$文理分科,
         sex = cheng_ji_biao$性别) %>%
  setNames(c("x", "y", "name", "type", "sex")) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_text(aes(label = name, colour = type), 
            size = 3, alpha = 0.75)

#莫非增加性别之后就把他们区别开来了？
cj_mds_plus %>%
  as.data.frame() %>%
  mutate(name = cheng_ji_biao$姓名,
         type = cheng_ji_biao$文理分科,
         sex = cheng_ji_biao$性别) %>%
  setNames(c("x", "y", "name", "type", "sex")) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_text(aes(label = name, colour = sex), 
            size = 3, alpha = 0.75)
#由此可见，数据空间，并没有很好地区分为文理科两类




#######################################################
##k-means算法
#######################################################

#注意区别是否剔除异常点的区别
# load("cj.rda")
# #剔除其中的缺失值和异常值
# na_idx <- which(cheng_ji_biao$总成绩 == 0)
# cheng_ji_biao <- cheng_ji_biao[-na_idx, ]
# (outliers <- boxplot.stats(cheng_ji_biao$总成绩)$out)
# #离群值标签
# (outliers_idx <- which(cheng_ji_biao$总成绩
#                        %in% outliers))
# cheng_ji_biao <- cheng_ji_biao[-outliers_idx, ]

cj <- cheng_ji_biao[, 4:12]
#stats包中的kmeans()函数
set.seed(2012)
imodel <- kmeans(cj, centers = 2, iter.max = 5000)
min(Metrics::ce(cheng_ji_biao$文理分科, 
                c( "理科", "文科")[imodel$cluster]),
    1- Metrics::ce(cheng_ji_biao$文理分科, 
                   c( "理科", "文科")[imodel$cluster]))
#[1] 0.36 未剔除异常点
#[1] 0.3464567 剔除异常点

#绘制聚类效果图
factoextra::fviz_cluster(imodel,
             data = cj, 
             ellipse.type = "convex") +
  theme_minimal()

#是特征越多越好么？
#未必如此
set.seed(2012)
imodel <- kmeans(cj[, c("生物", "物理", "数学")], centers = 2, iter.max = 5000)
table(cheng_ji_biao$文理分科, c("文科", "理科")[imodel$cluster])
min(Metrics::ce(cheng_ji_biao$文理分科, 
                c( "理科", "文科")[imodel$cluster]),
    1- Metrics::ce(cheng_ji_biao$文理分科, 
                   c( "理科", "文科")[imodel$cluster]))
#[1] 0.3277419未剔除异常点
#[1] 0.3228346 剔除异常点

library(flexclust)
mycont = list(iter=5000, initcent = "kmeanspp")
as(mycont, "flexclustControl")
imodel_kmeans2 <- kcca(cj[, c("生物", "物理", "数学")], 
                       k = 2, 
                       family=kccaFamily("kmeans"),
                       control = mycont)
min(Metrics::ce(cheng_ji_biao$文理分科, 
            c( "理科", "文科")[imodel_kmeans2@cluster]),
    1- Metrics::ce(cheng_ji_biao$文理分科, 
                   c( "理科", "文科")[imodel_kmeans2@cluster]))
#[1] 0.3303226 未剔除异常点
#[1] 0.312336 剔除异常点


#######################################################
##中心点算法
#######################################################

library("cluster")
imodel <- pam(cj, 2)
min(Metrics::ce(cheng_ji_biao$文理分科, 
                c( "理科", "文科")[imodel$cluster]),
    1- Metrics::ce(cheng_ji_biao$文理分科, 
                   c( "理科", "文科")[imodel$cluster]))
#[1] 0.3225806 未剔除异常点
#[1] 0.3215223 剔除异常点

imodel <- pam(cj[, c("生物", "物理", "数学")], 2)
min(Metrics::ce(cheng_ji_biao$文理分科, 
                c( "理科", "文科")[imodel$cluster]),
    1- Metrics::ce(cheng_ji_biao$文理分科, 
                   c( "理科", "文科")[imodel$cluster]))
#[1] 0.3109677 未剔除异常点
#[1] 0.3097113 剔除异常点
#将近百分之七十的正确率，已经和某些有监督学习算法相近了

#绘制聚类效果图
fviz_cluster(imodel)

#######################################################
##层次聚类法
#######################################################

imodel_hc <- hclust(cj_dist, method = "ward.D")
imodel_hc_cut2 <- cutree(imodel_hc, k = 2)
(ic_metric <- min(Metrics::ce(cheng_ji_biao$文理分科, 
                             c( "理科", "文科")[imodel_hc_cut2]),
                 1- Metrics::ce(cheng_ji_biao$文理分科, 
                                c( "理科", "文科")[imodel_hc_cut2])))
#[1] 0.2860892
#分类准确率挺近70%大关

#这当然也取决于参数的设置
#可以写以下循环来获取最优参数配置
hc_metric_com <- NULL
for (dist_method in c("euclidean",
                      "maximum",
                      "manhattan",
                      "canberra",
                      "binary",
                      "minkowski")) {
  for (hc_method in c(
    "ward.D",
    "ward.D2",
    "single",
    "complete",
    "average",
    "mcquitty",
    "median",
    "centroid"
  )) {
    cat("Processing ", hc_method, "\t", dist_method, "\n")
    imodel_hc <- hclust(stats::dist(cheng_ji_biao[,  4:12],
                                    method = dist_method),
                        method = hc_method)
    imodel_hc_cut2 <- cutree(imodel_hc, k = 2)
    hc_metric <- min(Metrics::ce(cheng_ji_biao$文理分科,
                                 c("理科", "文科")[imodel_hc_cut2]),
                     1 - Metrics::ce(cheng_ji_biao$文理分科,
                                     c("理科", "文科")[imodel_hc_cut2]))
    hc_metric_com <- rbind(
      hc_metric_com,
      data.frame(
        hc_method = hc_method,
        dist_method = dist_method,
        hc_metric = hc_metric
      )
    )
  }
}
View(hc_metric_com)


#绘制图形
imodel_hc <- hclust(cj_dist, method = "ward.D")
plot(imodel_hc, cex = 0.01,hang = -1)
rect.hclust(imodel_hc, k = 2, border = 2:3)

#层次聚类，画的更好看一点
library(dendextend)
dend_hc <- as.dendrogram(imodel_hc)
dend <- rotate(dend_hc, 1:nrow(cheng_ji_biao))
# Color the branches based on the clusters:
dend <- color_branches(dend, k = 2) 
# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  colorspace::rainbow_hcl(2)[sort_levels_values(as.numeric(cheng_ji_biao$文理分科)[order.dendrogram(dend)])]
#Hang the dendrogram a bit:
dend <- hang.dendrogram(dend, hang_height = 0.1)
dend <- set(dend, "labels_cex", 0.25)
plot(
  dend,
  main = "Clustered Student Scores",
  horiz =  FALSE,
  nodePar = list(cex = .007)
)
legend("topright",
       legend = levels(cheng_ji_biao$文理分科),
       fill = colorspace::rainbow_hcl(2))


#######################################################
##dbscan算法
#######################################################
#先看一个演示
#基于人工数据
library(factoextra)
data(multishapes)
df <- multishapes[, 1:2]
set.seed(2012)
km_res <- kmeans(df, 5)
fviz_cluster(km_res, df, ellipse = FALSE, geom = "point")
set.seed(2012)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
fviz_cluster(db, df, ellipse = FALSE, geom = "point")

dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)

#针对cheng_ji_biao数据集进行聚类
#找到相应的k和Epsilon
cj_matri <- as.matrix(cheng_ji_biao[, 4:12])
dbscan::kNNdistplot(cj_matri, k =  18)
abline(h = 10, lty = 2, col = "red")

#利用fpc::dbscan()或dbscan::dbscan()扩展包进行聚类
set.seed(2012)
imodel_db <- fpc::dbscan(cj_matri,
                         eps = 10,
                         MinPts = 18)
imodel_db$cluster
(ic_metric <- min(
  Metrics::ce(cheng_ji_biao$文理分科,
              c("理科", "文科")[(imodel_db$cluster == 1) + 1]),
  1 - Metrics::ce(cheng_ji_biao$文理分科,
                  c("理科", "文科")[(imodel_db$cluster == 1) + 1])
))
#[1] 0.3293963

#绘制聚类图形
imodel_db <- list(data = cj_matri,
                  cluster = imodel_db$cluster)
fviz_cluster(imodel_db, 
             cj_matri,
             geom = "point")


#从聚类效果可以看出，
#基于密度的聚类，针对我们目前的问题情境，
#其实并不适用，实际上，对于我们问题的情境
#很难想象具有想前述人工数据那样的形状，
#即便有，对于成绩数据而言，也很难解释。


#######################################################
##聚类评估
#######################################################
#轮廓系数
require(cluster)
fviz_silhouette(pam(cj_matri, k = 2))
fviz_silhouette(pam(cj_matri, k = 3))
fviz_silhouette(pam(cj_matri, k = 4))



#######################################################
##The End ^-^
#######################################################
