

# 04_观数以形 -----------------------------------------------------------


#了解一个人，可能是先看其长相，了解其言谈举止
#而后再逐渐深入
#了解一份数据，首先也是要看数据的高矮胖瘦
#然后再去深入了解内在的关系结构

#要初步了解数据，大部分教材上用的是以下两种方法：
#1、用少量数字描述数据
#2、用图形直观刻画数据

#机器学习之要义在于“关系结构”
#尤其是变量之间的关系和数据空间的结构
#认识数据：
#同样是刻画数据空间的形态和变量之间的关系


# Data Import -------------------------------------------------------------

#为保持课程的一致性，减少小伙伴们熟悉业务背景的成本
#本次课程同样采用前述的《学生文理分科》的数据
#采用真实数据的目的很简单：所得结果是鲜活的

#书接前文，在观测数据的外表之前，首先还是将数据读入
library(tidyverse)
cjb_url <- "data/cjb.csv"
cjb <- read.csv(cjb_url,
                stringsAsFactors = FALSE,
                encoding = "CP936")

cjb %<>%
    mutate(zcj = rowSums(.[, 4:12])) %>%
    mutate_at(vars(xb, bj, wlfk), factor) %>%
    dplyr::filter(zcj != 0)


#可能有小伙伴觉得，一次次从网络读取数据很麻烦
#好吧，那咱们就把它存到本地
#实际上，在大部分数据分析项目中
#我们都可以把清理好的数据存为rda格式放在本地
View(cjb)
save(cjb, file = "data/cjb.rda")
#文件存储在当前工作路径的data子目录之下
#当前工作路径可通过getwd()/setwd()来查询和设置

rm(list = ls())
load("data/cjb.rda",
     verbose = TRUE)
#也可以采用下边这种方式选取
#load(file.choose())


# 1D-univariate -----------------------------------------------------------


#机器学习的核心任务
#是揭示变量之间的关系和数据空间的结构
#变量之间的关系，自然是一个变量变化或若干变量变化之后，
#另外变量随之产生变化；
#数据空间的形态，
#则主要是数据点在数据空间的散布所呈现的结构

#要考察变量之间的依存/随动关系，
#自然首先要看单个变量的分布情况
#若某个变量取值不变，退化为常量，则几乎是不被作为特征的
#我们要考查的，恰恰就是数据本身的变化或者说分布情况
#同样，要考查数据空间的形态，
#当然也可以从单个维度的形态着手
#因此，我们要认识数据，做的第一件事情，
#往往就是单变量的分布情况的描述

#一维数据空间，毫无疑问就是一条直线


# Stem --------------------------------------------------------------------

#一维空间的数据形态，可以通过茎叶图或是Wikinson点图
#来直观表示
library(tidyverse)
cjb %>%
    dplyr::filter(bj == "1101") %>%
    pull(sx) %>%
    sort() -> sx_1101

cjb %>%
    dplyr::filter(bj == "1110") %>%
    pull(sx) %>%
    sort() -> sx_1110

stem(sx_1101, scale = 0.5)
table(cjb$bj)
set.seed(2012)
tmp_x <- 10 * round(sx_1101 + rnorm(length(sx_1101)), digits = 2)
sort(tmp_x)
# [1] 542.2 564.2 590.9 593.7 596.6 606.0 612.6 637.7 643.7 654.1 672.8 673.1 681.6 688.2 691.8 692.5 703.7 718.9 719.3
# [20] 724.3 724.4 729.8 731.4 731.5 736.3 742.4 749.4 760.3 767.9 778.6 779.8 789.4 789.9 797.7 801.4 801.8 812.5 819.5
# [39] 822.6 825.4 825.6 828.6 831.8 840.0 840.4 845.5 853.5 865.7 891.0 912.6 915.9 950.0
stem(tmp_x, scale = 0.5)

#1101班数学成绩茎叶图
cjb %>%
    dplyr::filter(bj == "1101") %>%
    select(sx) %>%
    as_vector() %>%
    stem(scale = 0.5)
#1110班数学成绩茎叶图
cjb %>%
    dplyr::filter(bj == "1110") %>%
    select(sx) %>%
    as_vector() %>%
    stem(scale = 2)

stem(sx_1101, scale = 0.5)
stem(sx_1110, scale = 2)



# Histogram ---------------------------------------------------------------

results <- hist(cjb$zz,
                breaks = "Sturges")

results <- hist(cjb$zz,
                breaks = "Sturges",
                plot = FALSE)
results$breaks

(max(cjb$zz) - min(cjb$zz)) /
    (ceiling(log2(nrow(cjb))) + 1)
nclass.Sturges(cjb$yw)
nclass.Sturges(cjb$zz)

#ggplot2的基本绘图模板template是：
# ggplot(data = <DATA>) +
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>),
#                   stat = <STAT>,
#                   position = <POSITION>) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>
#虽然绘图框架很直观明了，但是真正要精通各类图形绘制，
#仍然需要日积月累

#在初学ggplot2的过程中，除了图形语法之外，
#还有一个难点，那就是数据的转换的过程，
#比如数据的长宽变换等
#在接下来的具体代码演示过程中，
#这一点小伙伴们也许多加留意

#看一看数据分布的形状
sx_hist_results <- hist(cjb$sx,
                        plot = FALSE)
? hist
#查看sx_hist_results的类型
typeof(sx_hist_results)
#> [1] "list"
#查看列表的组成
names(sx_hist_results)
#> [1] "breaks"   "counts"   "density"  "mids"     "xname"    "equidist"
sx_hist_results$density
sx_hist_results$counts / length(cjb$sx) * 2 - sx_hist_results$density

library(tidyverse)
ggplot(data = cjb, mapping = aes(sx)) +
    geom_histogram(breaks = sx_hist_results$breaks,
                   color = "darkgray",
                   fill = "white") +
    stat_bin(breaks = sx_hist_results$breaks,
             geom = "text",
             aes(label = ..count..)) +
    coord_flip()

ggplot(data = cjb, mapping = aes(sx)) +
    geom_histogram(breaks = sx_hist_results$breaks,
                   color = "darkgray",
                   fill = "white") +
    stat_bin(breaks = sx_hist_results$breaks,
             geom = "text",
             aes(label = ..count..)) +
    coord_flip()


ggsave("histogram1.png", dpi = 600)
#dpi一般设为300，就可以达到印刷要求了
#换言之，发表论文时，dpi设置为300也就足够了
#当然，dpi值越高、质量越好

ggplot(data = cjb, mapping = aes(sx)) +
    geom_histogram(breaks = sx_hist_results$breaks,
                   color = "darkgray",
                   fill = "white") +
    stat_bin(breaks = sx_hist_results$breaks,
             geom = "text",
             aes(label = ..count..))
ggsave("histogram2.png", dpi = 600)


# Density -----------------------------------------------------------------

#获取直方图相关参数
sx_hist_results <- hist(cjb$sx,
                        plot = FALSE)
#绘制直方图
ggplot(data = cjb, mapping = aes(sx)) +
    geom_histogram(
        aes(y = ..count..),
        breaks = sx_hist_results$breaks,
        color = "darkgray",
        fill = "white"
    ) +
    #绘制概率密度曲线
    geom_density(colour = "red")
ggplot(data = cjb, mapping = aes(sx)) +
    geom_histogram(
        aes(y = ..density..),
        breaks = sx_hist_results$breaks,
        color = "darkgray",
        fill = "white"
    ) +
    #绘制概率密度曲线
    geom_density(colour = "red")
ggsave("histogram2+density.png", dpi = 600)

#概率密度图
data_points <- head(cjb$sx, n = 10)
sx_density <- density(data_points)
gaussian_kernel <- function(X, x, h) {
    u <- (X - x) / h
    return(1 / sqrt(2 * pi) * exp(-0.5 * (u ^ 2)))
}
X <- sx_density$x
h <- sx_density$bw
n <- length(data_points)
components <- lapply(data_points, function(y) {
    gaussian_kernel(X, y, h)
})
plot(sx_density)
for (i in seq_along(components)) {
    lines(X, components[[i]] / (n * h), col = "grey")
}
points(data_points, rep(0, n), col = "red")


#不同的核函数
#方窗
u <- c(-2.5, -2.5, 2.5, 2.5)
kernel_2 <- c(0, 0.5, 0.5, 0)
plot(
    u,
    kernel_2,
    type = "l",
    xaxt = "n",
    yaxt = "n",
    axes = F,
    xlim = c(-5, 5),
    ylim = c(0, 1)
)
arrows(0, 0, 0, 0.7, length = 0.1)
arrows(-5, 0, 5, 0, length = 0.1)

#三角窗
u <- c(-2.5, 0, 2.5)
kernel_2 <- c(0, 0.5, 0)
plot(
    u,
    kernel_2,
    type = "l",
    xaxt = "n",
    yaxt = "n",
    axes = F,
    xlim = c(-5, 5),
    ylim = c(0, 1)
)
arrows(0, 0, 0, 0.7, length = 0.1)
arrows(-5, 0, 5, 0, length = 0.1)

#正态窗
u <- seq(-4, 4, len = 10000)
kernel_2 <- 1 / sqrt(2 * pi) * exp(-0.5 * (u ^ 2))
plot(
    u,
    kernel_2,
    type = "l",
    xaxt = "n",
    yaxt = "n",
    axes = F,
    xlim = c(-5, 5),
    ylim = c(0, 1)
)
arrows(0, 0, 0, 0.6, length = 0.1)
arrows(-5, 0, 5, 0, length = 0.1)

#指数窗
u <- seq(-4, 4, len = 10000)
kernel_2 <- 1 / 2 * exp(-abs(u))
plot(
    u,
    kernel_2,
    type = "l",
    xaxt = "n",
    yaxt = "n",
    axes = F,
    xlim = c(-5, 5),
    ylim = c(0, 1)
)
arrows(0, 0, 0, 0.6, length = 0.1)
arrows(-5, 0, 5, 0, length = 0.1)


# Violin ------------------------------------------------------------------

#绘制小提琴图
ggplot(cjb, aes(x = factor(0), y = sx)) +
    geom_violin(fill = "orange", alpha = 0.2) +
    coord_flip()
ggsave("violin.png", dpi = 600)


# Boxplot -----------------------------------------------------------------

ggplot(cjb, aes(x = factor(0), y = sx)) +
    geom_violin(fill = "orange", alpha = 0.2) +
    geom_boxplot(width = 0.25,
                 fill = "blue",
                 alpha = 0.2) +
    coord_flip()

#箱线图
set.seed(2012)
sample_data <- rnorm(100000)
op <- par(mfrow = c(2, 1))
boxplot(sample_data,
        horizontal = TRUE)
plot(density(sample_data), main = NA)
par(op)

op <- par(mfrow = c(2, 3))
library(skewt)
set.seed(2012)
rt1 <- rskt(5000, 12, 10)
rt2 <- rskt(5000, 12, 1)
rt3 <- rskt(5000, 12, 0.2)
plot(density(rt1), col = "red", lwd = 2)
plot(density(rt2), col = "red", lwd = 2)
plot(density(rt3), col = "red", lwd = 2)
boxplot(rt1, horizontal = TRUE)
boxplot(rt2, horizontal = TRUE)
boxplot(rt3, horizontal = TRUE)
par(op)

cjb %>%
    ggplot(aes(x = factor(0), y = sx)) +
    geom_violin(fill = "#56B4E9", width = 0.75) +
    geom_boxplot(
        width = 0.25,
        fill = "#E69F00",
        outlier.colour = "red",
        outlier.shape = 1,
        outlier.size = 2
    ) +
    geom_rug(position = "jitter",
             size = 0.1,
             sides = "b") +
    coord_flip() +
    theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )


cjb %>%
    ggplot(aes(x = factor(0), y = sx)) +
    geom_violin(fill = "#56B4E9", width = 0.75) +
    geom_boxplot(
        width = 0.25,
        fill = "#E69F00",
        outlier.colour = "red",
        outlier.shape = 1,
        outlier.size = 2
    ) +
    geom_rug(position = "jitter",
             size = 0.1,
             sides = "b") +
    coord_flip()
ggsave("boxplot1.png", dpi = 600)


cjb %>%
    ggplot(aes(x = factor(0), y = sx)) +
    geom_boxplot(
        width = 0.25,
        fill = "#E69F00",
        outlier.colour = "red",
        outlier.shape = 1,
        outlier.size = 2
    ) +
    geom_rug(position = "jitter",
             size = 0.1,
             sides = "b") +
    coord_flip()
ggsave("boxplot2.png", dpi = 600)


boxplot_results <- boxplot.stats(cjb$sx)
# $`stats`
# [1]  60  81  89  95 100
#
# $n
# [1] 774
#
# $conf
# [1] 88.20491 89.79509
#
# $out
# [1] 55 59 57 59 58 51 56 55 59 26 58 46 59 59
sort(boxplot_results$out)

typeof(boxplot_results)
names(boxplot_results)
boxplot_results$stats
fivenum(cjb$sx)



# Location ----------------------------------------------------------------

#前边是数据形态的直观展示
#我们需要有一些定量指标来对数据形态进行刻画
#数据的集中趋势
#均值
cjb %>%
    group_by(wlfk) %>% #按文理分科分组统计
    summarise(
        count = n(),
        #各组人数
        sx_max = max(sx),
        #最大值
        sx_Q3 = quantile(sx, 0.75),
        #第三分位数
        sx_median = median(sx),
        #中位数
        sx_mean = mean(sx),
        #均值
        sx_Q1 = quantile(sx, 0.25),
        #第一分位数
        sx_iqr = IQR(sx),
        #四分位距
        sx_min = min(sx),
        #最小值
        sx_range = max(sx) - min(sx)#极差
    )


cjb %>%
    group_by(wlfk) %>% #按文理分科分组统计
    summarise(count = n(),
              #各组人数
              sx_median = median(sx),
              #中位数
              sx_mean = mean(sx))#均值


# Scale -------------------------------------------------------------------

cjb %>%
    group_by(wlfk) %>% #按文理分科分组统计
    summarise(
        sx_max = max(sx),
        #最大值
        sx_min = min(sx),
        #最小值
        sx_range = max(sx) - min(sx)
    )#极差
cjb %>%
    group_by(wlfk) %>% #按文理分科分组统计
    summarise(
        sx_Q3 = quantile(sx, 3 / 4),
        #第三分位数
        sx_Q1 = quantile(sx, 1 / 4),
        #第一分位数
        sx_iqr = IQR(sx)
    )#四分位距

#查看各科情况
round(apply(cjb[, 4:12], 2, function(x) {
    c(
        mean = mean(x),
        median = median(x),
        range = diff(range(x)),
        IQR = IQR(x)
    )
}))


# More Plots --------------------------------------------------------------

cjb %>%
    dplyr::filter(bj == "1110") %>%
    select(xm, sx) %>%
    mutate(sx_z = (sx - mean(sx)) / sd(sx),
           sx_type = ifelse(sx_z >= 0, "above", "below")) %>%
    arrange(sx_z) %>%
    ggplot(aes(x = fct_inorder(xm), y = sx_z, label = sx_z)) +
    geom_bar(stat = 'identity', aes(fill = sx_type), width = .5)  +
    scale_fill_manual(
        name = "Math Score",
        labels = c("Above Average", "Below Average"),
        values = c("above" = "#00ba38", "below" = "#f8766d")
    ) +
    coord_flip()

cjb %>%
    dplyr::filter(bj == "1110" & xb == "男") %>%
    select(xm, sx) %>%
    mutate(sx_z = (sx - mean(sx)) / sd(sx),
           sx_type = ifelse(sx_z >= 0, "above", "below")) %>%
    arrange(sx_z) %>%
    ggplot(aes(x = fct_inorder(xm), y = sx_z, label = sx_z)) +
    geom_bar(stat = 'identity', aes(fill = sx_type), width = 0.5)  +
    scale_fill_manual(
        name = "Math Score",
        labels = c("Above Average", "Below Average"),
        values = c("above" = "#00ba38", "below" = "#f8766d")
    ) +
    coord_flip() +
    theme_bw()


g <- ggplot(cjb, aes(sx))
sx_hist_results <- hist(cjb$sx,
                        plot = FALSE)
library(tidyverse)
g + geom_histogram(aes(fill = bj),
                   breaks = sx_hist_results$breaks,
                   col = "black",
                   size = .1) + scale_fill_brewer(palette = "Spectral")




# 2D-Two Variables --------------------------------------------------------




# Treemap-Cat vs cat ------------------------------------------------------

#离散变量vs离散变量
#形式可以有很多种，比如马赛克图
#本实验中推荐的是矩形树图

library(tidyverse)
library(treemap)
cjb_sum <- cjb %>%
    group_by(wlfk) %>%
    summarise(count = n())
cjb_sum <- cjb %>%
    group_by(wlfk, xb) %>%
    summarise(count = n())
treemap(
    as.data.frame(cjb_sum) ,
    index = c("wlfk", "xb"),
    vSize = "count",
    vColor = "xb",
    type = "categorical",
    fontsize.labels = 20,
    lowerbound.cex.labels = 0.6
)

#更改其中的字体
library(showtext)
font_add("fzqt", regular = "D://tools/fonts/FZQTJW.TTF")
showtext_begin()
treemap(
    as.data.frame(cjb_sum) ,
    index = c("wlfk", "xb"),
    vSize = "count",
    vColor = "xb",
    type = "categorical",
    fontsize.labels = 20,
    fontfamily.title = "fzqt",
    fontfamily.labels = "fzqt",
    fontfamily.legend = "fzqt",
    lowerbound.cex.labels = 0.6
)
showtext_end()


View(cjb_sum)
nrow(cjb)
library(treemap)
cjb %>%
    group_by(wlfk, bj, xb) %>%
    summarise(count = n()) %>%
    as.data.frame() %>%
    treemap(
        index = c("wlfk", "bj", "xb"),
        vSize = "count",
        vColor = "count",
        type = "value"
    )



cjb %>%
    group_by(bj, wlfk) %>%
    summarise(count = n())


# Numeric vs numeric ------------------------------------------------------

#连续变量vs连续变量
#散点图是最常见、但同时也应该是最有用的图之一
#散点图可用来观察变量之间可能存在的模式
#同时也是二位数据空间形态的最直接的体现
library(ggplot2)
ggplot(cjb,
       aes(
           x = sx,
           y = sw,
           shape = wlfk,
           colour = wlfk
       )) +
    geom_point(size = 2) +
    labs(
        x = "数学",
        y = "生物",
        colour = "文理分科",
        shape = "文理分科"
    )
#散点图矩阵
GGally::ggpairs(cjb, columns = 4:12)
ggsave("scatter_pairs.png", dpi = 600)
View(cjb)


# cor and cov -------------------------------------------------------------

#协方差以及内积的含义
#请大家进一步思考加减乘除的物理含义
#内积在某种程度上讲，也是在衡量相似性
set.seed(2012)
X <- rnorm(100)
Y <- rnorm(100)
sum(X * Y)
#> [1] 16.52361
#相关系数矩阵
sum(sort(X) * sort(Y))
#> [1] 113.6489
sum(sort(X) * rev(sort(Y)))
#> [1] -112.7025

cov(X, Y)
sum((X - mean(X)) * (Y - mean(Y))) / 99



library(animation)
saveGIF(
    expr = {
        Xs <- seq(-2, 2, len = 100)
        Ys <- seq(-2, 2, len = 100)
        area <- 4
        Wx <- seq(2, -2, len = 20)
        Wy <- sqrt(area - Wx ^ 2)
        Wx <- c(Wx, rev(Wx))
        Wy <- c(Wy, -rev(Wy))
        Wx <- rev(Wx)
        Wy <- rev(Wy)
        W <- cbind(Wx, Wy)
        W <- t(apply(W, 1, function(x) {
            x / sqrt(x[1] ^ 2 + x[2] ^ 2)
        }))


        XY <- expand.grid(Xs, Ys)
        names(XY) <- c("Xs", "Ys")
        XY_bak <- XY
        XY <- as.data.frame(XY)
        b <- 1 / 2
        i <- 5
        for (i in 1:nrow(W)) {
            w <- W[i, ]
            XY <- XY_bak
            XY$Inner_Product <- apply(XY, 1, function(x) {
                sum(x * w)
            })
            names(XY) <- c("x", "y", "Inner_Product")
            w_label <- paste0("(",
                              round(w[1], digits = 2),
                              ",",
                              round(w[2], digits = 2),
                              ")")
            library(ggplot2)
            p <- ggplot(XY, aes(
                x = x,
                y = y,
                colour = Inner_Product
            )) +
                geom_point(size = 0.5) +
                geom_segment(
                    aes(
                        x = 0,
                        y = 0,
                        xend = w[1],
                        yend = w[2]
                    ),
                    colour = "red",
                    size = 1.2,
                    arrow = arrow(length = unit(0.03, "npc"))
                ) +
                geom_text(aes(
                    x = w[1],
                    y = w[2],
                    label = w_label
                ),
                colour = "blue") +
                #scale_colour_gradient2(low="#22FF00", mid="white", high="#FF0000", midpoint=0) +
                scale_colour_gradient2(
                    low = "red",
                    mid = "white",
                    high = "blue",
                    midpoint = 0
                ) +
                coord_fixed()
            print(p)
        }
    },
    movie.name = "animation5.gif",
    convert = "gm convert",
    interval = 1
)


library(animation)
saveGIF(
    expr = {
        Xs <- seq(-2, 2, len = 100)
        Ys <- seq(-2, 2, len = 100)
        area <- 4
        Wx <- seq(2, -2, len = 20)
        Wy <- sqrt(area - Wx ^ 2)
        Wx <- c(Wx, rev(Wx))
        Wy <- c(Wy, -rev(Wy))
        Wx <- rev(Wx)
        Wy <- rev(Wy)
        W <- cbind(Wx, Wy)
        W <- t(apply(W, 1, function(x) {
            x / sqrt(x[1] ^ 2 + x[2] ^ 2)
        }))


        XY <- expand.grid(Xs, Ys)
        names(XY) <- c("Xs", "Ys")
        XY_bak <- XY
        XY <- as.data.frame(XY)
        b <- 1
        for (i in 1:nrow(W)) {
            w <- W[i, ]
            XY <- XY_bak
            XY$Inner_Product <- apply(XY, 1, function(x) {
                sum(x * w) > b
            })
            names(XY) <- c("x", "y", "Inner_Product")
            w_label <- paste0("(",
                              round(w[1], digits = 2),
                              ",",
                              round(w[2], digits = 2),
                              ")")
            library(ggplot2)
            p <- ggplot(XY, aes(
                x = x,
                y = y,
                colour = Inner_Product
            )) +
                geom_point(size = 0.5) +
                geom_segment(
                    aes(
                        x = 0,
                        y = 0,
                        xend = w[1],
                        yend = w[2]
                    ),
                    colour = "red",
                    size = 1.2,
                    arrow = arrow(length = unit(0.03, "npc"))
                ) +
                geom_text(aes(
                    x = w[1],
                    y = w[2],
                    label = w_label
                ),
                colour = "blue") +
                coord_fixed()
            print(p)
        }
    },
    movie.name = "animation6.gif",
    convert = "gm convert",
    interval = 1
)

library(animation)
saveGIF(
    expr = {
        Xs <- seq(-2, 2, len = 100)
        Ys <- seq(-2, 2, len = 100)
        area <- 4
        Wx <- seq(2, -2, len = 20)
        Wy <- sqrt(area - Wx ^ 2)
        Wx <- c(Wx, rev(Wx))
        Wy <- c(Wy, -rev(Wy))
        Wx <- rev(Wx)
        Wy <- rev(Wy)
        W <- cbind(Wx, Wy)
        W <- t(apply(W, 1, function(x) {
            x / sqrt(x[1] ^ 2 + x[2] ^ 2)
        }))


        XY <- expand.grid(Xs, Ys)
        names(XY) <- c("Xs", "Ys")
        XY_bak <- XY
        XY <- as.data.frame(XY)
        b <- 1 / 2
        for (i in 1:nrow(W)) {
            w <- W[i, ]
            XY <- XY_bak
            XY$Inner_Product <- apply(XY, 1, function(x) {
                sum(x * w) > b
            })
            names(XY) <- c("x", "y", "Inner_Product")
            w_label <- paste0("(",
                              round(w[1], digits = 2),
                              ",",
                              round(w[2], digits = 2),
                              ")")
            library(ggplot2)
            p <- ggplot(XY, aes(
                x = x,
                y = y,
                colour = Inner_Product
            )) +
                geom_point(size = 0.5) +
                geom_segment(
                    aes(
                        x = 0,
                        y = 0,
                        xend = w[1],
                        yend = w[2]
                    ),
                    colour = "red",
                    size = 1.2,
                    arrow = arrow(length = unit(0.03, "npc"))
                ) +
                geom_text(aes(
                    x = w[1],
                    y = w[2],
                    label = w_label
                ),
                colour = "blue") +
                coord_fixed()
            print(p)
        }
    },
    movie.name = "animation7.gif",
    convert = "gm convert",
    interval = 1
)

set.seed(2012)
X <- rnorm(100)
Y <- rnorm(100)
inner_prod <- sapply(1:1000000, function(x) {
    sum(sample(X) * sample(Y))
})
#上边的代码，当然也可以用replicate改写
sum(sort(X) * sort(Y))
range(inner_prod)

hist(inner_prod)

cor_coef <- cjb %>%
    select(yw:sw) %>%
    cor() %>%
    round(digits = 2)
#>    yw   sx   wy   zz   ls   dl   wl   hx   sw
#> yw 1.00 0.46 0.54 0.47 0.38 0.37 0.30 0.38 0.40
#> sx 0.46 1.00 0.55 0.39 0.37 0.45 0.57 0.59 0.60
#> wy 0.54 0.55 1.00 0.37 0.28 0.38 0.47 0.44 0.44
#> zz 0.47 0.39 0.37 1.00 0.39 0.32 0.20 0.28 0.28
#> ls 0.38 0.37 0.28 0.39 1.00 0.41 0.31 0.33 0.38
#> dl 0.37 0.45 0.38 0.32 0.41 1.00 0.39 0.44 0.46
#> wl 0.30 0.57 0.47 0.20 0.31 0.39 1.00 0.62 0.65
#> hx 0.38 0.59 0.44 0.28 0.33 0.44 0.62 1.00 0.69
#> sw 0.40 0.60 0.44 0.28 0.38 0.46 0.65 0.69 1.00

# Tile for cor ------------------------------------------------------------
#HOW TO INTERPRET A CORRELATION COEFFICIENT R By Deborah J. Rumsey
library(ggplot2)
library(tidyverse)
cor_coef %>%
    as.data.frame() %>%
    rownames_to_column(var = "km1") %>%
    gather(key = km2, value = cor_num, -km1) %>%
    mutate(cor_level = cut(
        cor_num,
        breaks = c(0, 0.3, 0.5, 0.7, 1),
        right = FALSE
    )) %>%
    ggplot(aes(
        x = fct_inorder(km1),
        y = fct_inorder(km2),
        fill = cor_level
    )) +
    geom_tile(colour = "white", size = 1.5) +
    geom_text(aes(label = format(cor_num, digits = 2))) +
    scale_fill_brewer(palette = "YlGn", name = "相关系数区间")
ggsave("cor_coef.png", dpi = 600)

#区间划分方法并不唯一：
#Hinkle DE, Wiersma W, Jurs SG (2003). Applied Statistics for the Behavioral Sciences 5th ed. Boston: Houghton Mifflin
#0~0.3negligible correlation
#0.3~0.5low correlation
#0.5~0.7moderate correlation
#0.7~0.9high correlation
#0.9~1very high correlation
# Catetorial vs Numeric ---------------------------------------------------

#离散变量vs连续变量
#主要是分组绘图
#对不同的组别进行比较


# Grouped boxplots --------------------------------------------------------

#分组绘制箱线图
#看看不同班级数学成绩的分布
library(ggplot2)
ggplot(cjb, aes(x = bj,
                y = sx,
                fill = bj)) +
    geom_boxplot(
        outlier.colour = "red",
        outlier.shape = 3,
        outlier.size = 1
    ) +
    labs(x = "班级", y = "数学成绩") +
    theme(legend.position = "none")
ggsave("grouped_boxplots.png", dpi = 600)

#其余图形如直方图、概率密度图等，请自行练习


# Grouped density plots ---------------------------------------------------

#当然，如果分组太多，显然不适合全都叠加在一起
#可以采用以下方式
library(ggridges)#绘制层峦叠嶂图
library(viridis)#采用其中的颜色
ggplot(cjb, aes(x = sx, y = bj, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2,
                                 rel_min_height = 0.01,
                                 gradient_lwd = 1) +
    scale_fill_viridis(name = "数学成绩",
                       option = "C") +
    labs(x = "数学", y = "班级")
ggsave("density_ridges.png", dpi = 600)


# featurePlot -------------------------------------------------------------

#对于分类问题而言，在进行数据描述时
#最关键的，当属因变量vs自变量了
library(caret)
featurePlot(
    x = cjb[, 4:12],
    y = cjb$wlfk,
    plot = "density",
    scales = list(
        x = list(relation = "free"),
        y = list(relation = "free")
    ),
    adjust = 1.5,
    pch = "|",
    auto.key = list(columns = 2)
)
#从上图可以看出，数学/生物最优辨识度
#而语文，几乎文理科生没有什么区别

#变量之间的依存关系
#可以通过信息增益来度量
#也就是说，当我们知道某个自变量时
#有助于因变量不确定性的减少
library(infotheo)
condentropy(cjb$wlfk) -
    condentropy(cjb$wlfk, cjb$xb)

library(FSelectorRcpp)
information_gain(x = cjb[, c(1, 3)],
                 y = cjb$wlfk)
#实际上，信息增益也是特征选择常用的方法



# More Variables ----------------------------------------------------------



# 3D-scatter --------------------------------------------------------------

#三维散点图
library(rgl)
plot3d(
    x = cjb$sx,
    y = cjb$wl,
    z = cjb$sw,
    xlab = "Mathematics",
    ylab = "Physics",
    zlab = "Biology",
    type = "s",
    size = 0.6,
    col = c("red", "green")[cjb$wlfk]
)


# Define point shapes
myshapes = c(16, 17, 18)
myshapes <- myshapes[as.numeric(cjb$wlfk)]
# Define point colors
mycols <- c("#999999", "#E69F00", "#56B4E9")
mycols <- mycols[as.numeric(cjb$wlfk)]
# Plot
library("scatterplot3d")
cjb %>%
    select(sx, wl, sw) %>%
    scatterplot3d(
        pch = myshapes,
        color = mycols,
        grid = TRUE,
        box = FALSE
    )
library(plot3D)
bty <- c("b", "b2", "f", "g", "bl", "bl2", "u", "n")
scatter3D(
    x = cjb$sx,
    y = cjb$wl,
    z = cjb$sw,
    pch = 16,
    bty = "g",
    colkey = FALSE,
    xlab = "数学",
    ylab = "物理",
    zlab = "生物",
    main = "数学、物理、生物散点图",
    #col.panel ="lightgreen",
    expand = 0.75,
    phi = 0
)


data(VADeaths)
icut <- function(x) {
    ibreaks <- c(0, seq(50, 100, len = 11))
    cut(x, breaks = ibreaks)
}

rotated_angle <- 20
cjb %>%
    select(wl, sx) %>%
    mutate_at(vars(wl, sx), icut) %>%
    group_by(wl, sx) %>%
    summarise(freq = n()) %>%
    spread(key = sx, value = freq, fill = 0) %>%
    column_to_rownames(var = "wl") %>%
    as.matrix() %>%
    hist3D(
        z = .,
        scale = FALSE,
        expand = 0.02,
        bty = "g",
        phi = rotated_angle,
        col = "#0072B2",
        border = "black",
        shade = 0.2,
        ltheta = 90,
        space = 0.3,
        ticktype = "detailed"
    )



library(animation)
saveGIF(
    expr = {
        for (rotated_angle in seq(20, 380, by = 5)) {
            cjb %>%
                select(wl, sx) %>%
                mutate_at(vars(wl, sx), icut) %>%
                group_by(wl, sx) %>%
                summarise(freq = n()) %>%
                spread(key = sx,
                       value = freq,
                       fill = 0) %>%
                column_to_rownames(var = "wl") %>%
                as.matrix() %>%
                hist3D(
                    z = .,
                    scale = FALSE,
                    expand = 0.02,
                    bty = "g",
                    phi = 20,
                    col = "#0072B2",
                    border = "black",
                    shade = 0.2,
                    ltheta = 90,
                    theta = rotated_angle,
                    space = 0.3,
                    ticktype = "detailed"
                )

        }

    },
    movie.name = "animation5.gif",
    convert = "gm convert",
    interval = 1
)
dev.off()


# faces -------------------------------------------------------------------

#除了三维之外，可以继续向多维扩展
#比如脸谱图
library(aplpack)
selected_cols <- c("wl", "hx", "sw")
selected_rows <-
    c(488, 393, 490,  440,
      287, 289,  292, 293)
View(cjb[selected_rows, ])
faces(cjb[selected_rows,
          selected_cols],
      ncol.plot = 4,
      nrow.plot = 2,
      face.type = 1)

#> effect of variables:
#>   modified item       Var
#> "height of face   " "wl"
#> "width of face    " "hx"
#> "structure of face" "sw"
#> "height of mouth  " "wl"
#> "width of mouth   " "hx"
#> "smiling          " "sw"
#> "height of eyes   " "wl"
#> "width of eyes    " "hx"
#> "height of hair   " "sw"
#> "width of hair   "  "wl"
#> "style of hair   "  "hx"
#> "height of nose  "  "sw"
#> "width of nose   "  "wl"
#> "width of ear    "  "hx"
#> "height of ear   "  "sw"


# parallel coordinate plot ------------------------------------------------

#绘制平行坐标图
cjb_top_wen <- cjb %>%
    dplyr::filter(wlfk == "文科") %>%
    arrange(zcj) %>%
    select(4:13) %>%
    mutate_at(vars(yw:sw), jitter) %>%
    head(n = 50)
cjb_top_li <- cjb %>%
    dplyr::filter(wlfk == "理科") %>%
    arrange(zcj) %>%
    select(4:13) %>%
    mutate_at(vars(yw:sw), jitter) %>%
    head(n = 50)
cjb_top <- rbind(cjb_top_wen, cjb_top_li)
require(GGally)
GGally::ggparcoord(iris, columns = 1:4, groupColumn = 5) +
    geom_point()
ggparcoord(cjb_top,
           columns = 1:9,
           groupColumn = 10) +
    geom_point()

ggsave("par2.png", dpi = 600)



# Density revisited -------------------------------------------------------

#我们当然可以对这个三维数据进行直观展示，
#但这显然是不够的，我们只有进行了量化，
#各种各样的关系、模式才能呈现出来，
#这也是数据科学最令人着迷的地方

#数据空间的密度
#数据的密度，当然与我们以前学过的物质的密度不一样
#不可能是质量与体积之比
#这里的密度，只是密集程度而已
#一个简单的方法：单位面积/体积内数据点的多少
#将50~100细分为N份，看每一个有多少落入其间
icut <- function(x) {
    ibreaks <- c(0, seq(50, 100, len = 11))
    cut(x, breaks = ibreaks)
}
range(cjb$wl)
range(cjb$sx)
cjb %>%
    select(wl, sx) %>%
    mutate_at(vars(wl, sx), icut) %>%
    group_by(wl, sx) %>%
    summarise(freq = n()) %>%
    complete(wl, sx, fill = list(freq = 0)) %>%
    mutate(freq = ifelse(is.na(freq), 0, freq)) %>% View
distinct() %>% View
ggplot(aes(x = wl, y = sx, fill = freq)) +
    geom_tile(colour = "white", size = 0.5) +
    geom_text(aes(label = freq), size = 3) +
    scale_fill_gradient(low = "white", high = "red") +
    theme(axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
    ))

cjb %>%
    select(wl, sx) %>%
    ggplot(aes(x = wl, y = sx)) +
    geom_point()
breaks <-  c(0, seq(50, 100, len = 11))
wl_sx_freq <- cjb %>%
    select(wl, sx) %>%
    mutate_at(vars(wl, sx),
              function(x) {
                  cut(x, breaks = breaks)
              }) %>%
    group_by(wl, sx) %>%
    summarise(freq = n()) %>%
    complete(wl, sx, fill = list(freq = 0))
ggplot(wl_sx_freq, aes(x = wl, y = sx, fill = freq)) +
    geom_tile(colour = "white", size = 0.5) +
    geom_text(aes(label = freq), size = 3) +
    scale_fill_gradient(low = "white",
                        high = "red") +
    theme(axis.text.x =
              element_text(
                  angle = 90,
                  hjust = 1,
                  vjust = 0.5
              )) +
    coord_fixed()
ggsave("density.png", dpi = 300)

#感兴趣的小伙伴可以用stat_density2d
#或是stat_bin2d实现类似的效果

#接下来考虑另一种计算密度的方法
#每一个点，半径为epsilon领域内点的多少
selected_cols <- c("sx", "wl", "sw")
shu_wu_sheng <- cjb[, selected_cols]
sws_dist <- as.matrix(dist(shu_wu_sheng,
                           diag = TRUE,
                           upper = TRUE))
iseq <- seq(50, 100, len = 10)
imatrix <- expand.grid(iseq, iseq, iseq)
names(imatrix) <- selected_cols
dist_imatrix <- apply(imatrix, 1, function(x) {
    apply(shu_wu_sheng, 1, function(y) {
        sqrt(sum((y - x) ^ 2))
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
    v <- (values - min(values)) / diff(range(values))
    x <- colorRamp(colors)(v)
    rgb(x[, 1], x[, 2], x[, 3], maxColorValue = 255)
}
cols <- my_color_ramp(c("white", "red"), sws_density)
#绘制图形
library(rgl)
#绘制“空”间
plot3d(
    x = imatrix,
    size = 2,
    type = "n",
    xlab = "Mathematics",
    ylab = "Physics",
    zlab = "Biology",
    col = cols
)
grd <- imatrix
grd$col <- cols
grd$alpha <-
    (sws_density - min(sws_density)) / (max(sws_density) - min(sws_density)) * 0.9
length <- width <- height <- (max(iseq) - min(iseq)) / length(iseq)
for (i in seq(nrow(grd))) {
    #创建一个长方体
    icube3d <- cube3d(col = grd$col[i])
    #设定长宽高
    icube3d <- scale3d(icube3d, length, width, height)
    #将长方体移动至指定位置
    icube3d <- translate3d(icube3d, grd$sx[i], grd$wl[i], grd$sw[i])
    #绘制长方体
    shade3d(icube3d, alpha = grd$alpha[i])
}



# Hopkins -----------------------------------------------------------------

#数据空间的均匀程度
#可以用hopkins统计量来描述
#均匀分布的话，趋近于0.5
#倾斜的话，趋近于0
clustertend::hopkins(cjb[, 4:12], n = 100)
#> $`H`
#> [1] 0.1549145



# Last words --------------------------------------------------------------

#这里展示的，只是数据可视化的一部分图形
#还有很多图形尚未涉及，请小伙伴们自行研究：
#比如：
#马赛克图mosaicplot()
#雷达图stars()
#关系图
#网络图plot.igraph
#地图ggmap
#词云wordcloud2
#等等
#以后在进行具体算法建模时，也会涉及到很多专用的图形
#比如：
#关联规则：关联网络图
#聚类分析：层次谱系图
#分类回归：分类回归树、神经网络图、变量重要性图
#……
#这些图形展示，当然已经超越了所谓的简单的数据描述了
#而是在对模型本身进行直观展示
#换言之，通过这些图形，不只是看数据的长相
#而是透过现象看本质了


# The End ^-^ -------------------------------------------------------------
