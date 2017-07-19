

#######################################################
#######################################################
##
## 名称：《R语言数据分析·聚类分析》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V6
## 时间：2017年6月
##
##*****************************************************
##
## ch07_Cluster Analysis_V6
## Data Analytics with R
## Instructed by Xinbo Ai
## @Beijing University of Posts and Telecommunications
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
##观察数据
#######################################################

#加载数据
data("iris")

#查看数据的结构
str(iris)
# 'data.frame':	150 obs. of  5 variables:
#   $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
#可以看出，iris是一个包含5个变量*150条记录的数据框
#前四列分别代表：
#Sepal.Length:萼片长
#Sepal.Width:萼片宽
#Petal.Length:花边长
#Petal.Widht:花瓣宽
#第五列代表每一朵花的类别
#Species:种类
#种类共有三种：
levels(iris$Species)
#[1] "setosa"     "versicolor" "virginica" 
#setosa:山鸢尾
#versicolor:变色鸢尾
#virginica:弗吉尼亚鸢尾


#显示iris数据集
View(iris)

#采用Hmisc中的包
#对iris数据集进行描述
library(Hmisc)
describe(iris)
#观察数据点之间的距离
#数据标准化
iris_new <- iris[, 1:4]
iris_new_scaled <- scale(iris_new)
View(iris_new_scaled)

#查看一下标准化前后的数据
describe(iris_new)
describe(iris_new_scaled)


#观察数据结构
MASS::parcoord(iris[, 1:4],
               col = iris[, 5],
               var.label = TRUE,
               lwd = 2)
#也可通过以下方式
library(MASS)
MASS::parcoord(iris[, 1:4],
               col = iris[, 5],
               var.label = TRUE,
               lwd = 2)


#聚类，本质上是识别数据点在数据空间的（距离）结构
#可以先看看在二维空间里是这么分布的
library(ggplot2)
#先看一下在花瓣长、花瓣宽二维空间中的分布
#三种类别被映射为不同颜色和形状
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(colour = Species, shape = Species))
#再看一下在萼片长、萼片宽二维空间中的分布
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(colour = Species, shape = Species))


#看看标准化之后的数据空间
library(ggplot2)
iris_new_scaled <- as.data.frame(iris_new_scaled)
iris_new_scaled$Species <- iris$Species
ggplot(iris_new_scaled, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(colour = Species, shape = Species))

library("ggplot2")
ggplot(iris_new_scaled, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(colour = Species, shape = Species)) +  # Scatter plot
  geom_density_2d() # Add 2d density estimation

#聚类，无监督学习的代名词
#重新将Species这些标签去掉
iris_new_scaled$Species <- NULL



#生成距离矩阵
library("cluster")
library("factoextra")
res_dist <- dist(iris_new)
View(as.matrix(res_dist))
library(corrplot)
corrplot(as.matrix(res_dist),
         method = "col",
         is.corr = F)
fviz_dist(
  res_dist,
  order = FALSE,
  gradient = list(low = "#00AFBB",
                  mid = "white",
                  high = "#FC4E07"))

res_dist_scaled <- dist(iris_new_scaled)
fviz_dist(
  res_dist_scaled,
  order = FALSE,
  gradient = list(low = "#00AFBB",
                  mid = "white",
                  high = "#FC4E07"))





#######################################################
##k-means算法
#######################################################

res_km <- kmeans(iris_new, centers = 3, iter.max = 1000)
fviz_cluster(res_km,
             data = iris_new, 
             ellipse.type = "convex") +
  theme_minimal()

res_km_scaled <-
  kmeans(iris_new_scaled, 
         centers = 3, 
         iter.max = 1000)
fviz_cluster(res_km_scaled, 
             data = iris_new_scaled, 
             ellipse.type = "convex") +
  theme_minimal()

table(iris$Species, res_km$cluster)
table(iris$Species, res_km$cluster)[, c(1, 3, 2)]
table(iris$Species, res_km_scaled$cluster)
table(iris$Species, res_km_scaled$cluster)[, c(1, 3, 2)]


res_km <- kmeans(iris_new[, 3:4], 
                 centers = 3, iter.max = 1000)
table(iris$Species, res_km$cluster)
table(iris$Species, res_km$cluster)[, c(1, 3, 2)]
res_km_scaled <-
  kmeans(iris_new_scaled[, 3:4], 
         centers = 3, iter.max = 1000)
table(iris$Species, res_km_scaled$cluster)[, c(3, 1, 2)]
(con_table <-
    table(iris$Species, res_km_scaled$cluster)[, c(3, 1, 2)])
sum(diag(con_table)) / sum(con_table)

#与有监督学习的比较
library(rpart)
irpart <- rpart(Species ~ ., 
                data = iris)
res_predicted <- predict(irpart, 
                         newdata = iris, 
                         type = "class")
table(iris$Species, res_predicted)





#######################################################
##中心点算法
#######################################################

library("cluster")
res_pam <- pam(iris_new, 3)
fviz_cluster(res_pam)
table(iris$Species, res_pam$clustering)

res_pam_scaled <- pam(iris_new_scaled, 3)
fviz_cluster(res_pam_scaled)
table(iris$Species, res_pam_scaled$clustering)
table(iris$Species, res_pam_scaled$clustering)[, c(1, 3, 2)]

res_pam34 <- pam(iris_new[, 3:4], 3)
fviz_cluster(res_pam34)
table(iris$Species, res_pam34$clustering)


res_pam_scaled34 <- pam(iris_new_scaled[, 3:4], 3)
fviz_cluster(res_pam_scaled34)
table(iris$Species, res_pam_scaled34$clustering)

describe(iris[, 3:4])





#######################################################
##层次聚类法
#######################################################

dist_iris <-
  dist(iris_new,
       method = "euclidean")
res_hc <- hclust(dist_iris, method = "ward.D2")
res_cluster3 <- cutree(res_hc, k = 3)
plot(res_hc, cex = 0.6)
rect.hclust(res_hc, k = 3, border = 1:3)

fviz_dend(
  res_hc,
  rect = TRUE,
  cex = 0.5,
  k_colors = c("#00AFBB", "#2E9FDF", "#E7B800"))

table(iris$Species, res_cluster3)

dist_iris <-
  dist(iris_new, method = "euclidean")
res_hc <- hclust(dist_iris, method = "ward.D2")
res_cluster3 <- cutree(res_hc, k = 3)
plot(res_hc, cex = 0.6)
rect.hclust(res_hc, k = 3, border = 1:3)
table(iris$Species, res_cluster3)

#层次聚类，画的更好看一点
library(dendextend)
dend_hc <- as.dendrogram(res_hc)
dend <- rotate(dend_hc, 1:150)
# Color the branches based on the clusters:
dend <- color_branches(dend, k = 3) 
# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  colorspace::rainbow_hcl(3)[sort_levels_values(as.numeric(iris[, 5])[order.dendrogram(dend)])]

# We shall add the flower type to the labels:
labels(dend) <-
  paste(as.character(iris[, 5])[order.dendrogram(dend)],
        "(", labels(dend), ")",
        sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend, hang_height = 0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3, 3, 3, 7))
plot(
  dend,
  main = "Clustered Iris data set
  (the labels give the true flower species)",
  horiz =  TRUE,
  nodePar = list(cex = .007)
)
legend("topleft",
       legend = levels(iris$Species),
       fill = colorspace::rainbow_hcl(3))

#变一种形式
circlize_dendrogram(dend)





#######################################################
##dbscan算法
#######################################################

#先看一个演示
library(factoextra)
data(multishapes)
df <- multishapes[, 1:2]
set.seed(123)
km_res <- kmeans(df, 5)
fviz_cluster(km_res, df, ellipse = FALSE, geom = "point")
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
fviz_cluster(db, df, ellipse = FALSE, geom = "point")

dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)

#针对iris数据集进行聚类
#找到相应的k和Epsilon
iris_mat <- as.matrix(iris[, 1:4])
dbscan::kNNdistplot(iris_mat, k =  4)
abline(h = 0.4, lty = 2)

#利用fpc或dbscan扩展包进行聚类
set.seed(123)
res_fpc <- fpc::dbscan(iris_mat, 
                       eps = 0.4, 
                       MinPts = 4)
# dbscan package
res_db <- dbscan::dbscan(iris_mat,
                         0.4, 4)
res_db$cluster
fviz_cluster(res_fpc, 
             iris_mat,
             geom = "point")





#######################################################
##聚类评估
#######################################################

#数据能聚类么？
#霍普金斯统计量
clustertend::hopkins(iris_mat, n = 100)

#聚类结果的评估
#肘方法
fviz_nbclust(iris_new, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(iris_new, pam, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(iris_new, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#轮廓系数
require(cluster)
fviz_silhouette(pam(iris_new, k = 2))
fviz_silhouette(pam(iris_new, k = 3))
fviz_silhouette(pam(iris_new, k = 4))



#k值的选择
#通过不同的指标
#来判定最好的k值
library(NbClust)
set.seed()
res.nbclust <- NbClust(
  iris_new,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 10,
  method = "complete",
  index = "all")




#######################################################
##The End
#######################################################