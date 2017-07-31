

#######################################################
#######################################################
##
## 名称：《R语言数据分析·认识数据》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V6
## 时间：2017年6月
##
##*****************************************************
##
## ch04_Get to Know Your Data_V6
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


#了解一个人，可能是先看其长相，了解其言谈举止
#而后再逐渐深入
#了解一份数据，首先也是要看数据的高矮胖瘦
#然后再去深入了解内在的关系结构

#要初步了解数据，一般有两种方法：
#1、用少量数字描述数据
#2、用图形直观刻画数据



#######################################################
##获取数据
#######################################################
#为保持课程的一致性，减少小伙伴们熟悉业务背景的成本
#本次课程同样采用前述的《学生成绩分析》的数据
#采用真实数据的目的很简单：所得结果是鲜活的
cheng_ji_url <-
  "https://raw.githubusercontent.com/byaxb/RDataAnalytics/master/data/cj.csv"
cheng_ji_biao <- read.csv(cheng_ji_url,
                          head = TRUE,
                          stringsAsFactors = TRUE)


#######################################################
##用少量数字描述数据
##观察数据的分布特征
#######################################################

#拿到数据之后，首先要做的事情，当然是观察数据的结构
str(cheng_ji_biao)

#现以语文、数学这两门课程为例
#对数据形态进行观察
yu_wen <- cheng_ji_biao$语文
shu_xue <- cheng_ji_biao$数学

#######################################################
#数据的集中趋势
#众数modal number
names(which.max(table(yu_wen)))
#[1] "89"
names(which.max(table(shu_xue)))
#[1] "96"
#均值
mean(yu_wen)
#[1] 87.26581
mean(shu_xue)
#[1] 86.08129
#两门课平均分相差不大

#中位数
median(yu_wen)
#[1] 88
median(shu_xue)
#[1] 89



#######################################################
#数据的分散程度
#分位数
quantile(yu_wen)
quantile(shu_xue)

#四分位距
IQR(yu_wen)
IQR(shu_xue)

#极差=全距
range(yu_wen)
diff(range(yu_wen))
range(shu_xue)
diff(range(shu_xue))
max(shu_xue) - min(shu_xue) #与上一语句同


#标准差
sd(yu_wen)
sd(shu_xue)

#绝对中位差
mad(yu_wen)
mad(shu_xue)

#变异系数
#Coefficient of Variation
cv <- function(x) {
  sd(x) / mean(x)
}
cv(yu_wen)
cv(shu_xue)

#######################################################
#数据分布的形状
library(e1071)
#skewness偏度
skewness(yu_wen) #-5.78左侧有长尾
skewness(shu_xue) #-1.62左侧有长尾，偏移程度没有语文成绩那么大
#kurtosis峰度
#通过峰度也可以看出平均数的代表性
kurtosis(yu_wen, type = 3)#尖峰分布，72.17
kurtosis(shu_xue)#尖峰分布，5.75
#注意：峰度的不同定义

plot(density(yu_wen),
     xlim = c(min(scale(yu_wen, scale = FALSE)), max(yu_wen)),
     lwd = 2)
lines(density(scale(yu_wen, scale = FALSE)), col = "blue",
      lwd = 2)
lines(density(scale(yu_wen)), col = "red",
      lwd = 2)


#######################################################
##用图形刻画数据
#######################################################

#在R里边，有基础绘图系统、lattice以及ggplot2等
#也有散见于各种包的相应绘制函数
#当然，也有echarts等外部接口
#本讲以ggplot2为主
#ggplot2：这里的gg是指Grammar of Graphics
#顾名思义，提供的是一整套图形语法的实现
#所以ggplot2与其它某些零散的绘图函数有本质的区别，
#自成体系
#在进入ggplot2的实操之前，建议对其脉络有个了解：
#请阅读《A Layered Grammar of Graphics》，
#网址：http://vita.had.co.nz/papers/layered-grammar.pdf

#ggplot2的基本绘图框架是：
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

#######################################################
#条形图、柱状图_barplot
#条形图：将数值映射为长度
#基础绘图系统
#选取前两个学生周黎、汤海明进行barplot对比
zhou_tang <- cheng_ji_biao[1:2, 4:12]
class(zhou_tang)
barplot(as.matrix(zhou_tang),
        beside = TRUE)
barplot(as.matrix(zhou_tang),
        col = rainbow(2), #重新设置颜色
        beside = TRUE)

#将文理科平均值进行柱状图对比
#文科生各科平均值
wen <- apply(cheng_ji_biao[cheng_ji_biao$文理分科  == "文科", 4:12],
             2, mean)
#理科生各科平均值
li <- apply(cheng_ji_biao[cheng_ji_biao$文理分科  == "理科", 4:12],
            2, mean)
wen_li <- rbind(wen, li)
lengend_text <- c(wen = "文科", li = "理科")
midpoints_coor <- barplot(
  wen_li,
  beside = TRUE,
  horiz = TRUE,
  #horizontal水平
  xpd = FALSE,
  #不允许条形图跑出绘图区域
  las = 2,
  #竖着写不下，横着来
  cex.names = 0.75,
  #字体太大，变小点
  xlim = c(70, 100),
  legend.text = lengend_text[row.names(wen_li)],
  xlab = "成绩",
  ylab = "科目",
  main = "文理各科平均成绩对比图"
)
text(wen_li + 1,
     #x坐标
     midpoints_coor,
     #轴坐标
     label = format(wen_li, digits = 3),
     #相应坐标的文字
     cex = 0.75) #文字缩放

#采用ggplot2进行绘图
library(ggplot2)
library(tidyr)
wen_li <- as.data.frame(wen_li)
wen_li$文理分科  <- lengend_text[row.names(wen_li)]
wen_li2 <- gather(wen_li,  文理分科)
colnames(wen_li2) <- c("文理分科", "科目", "平均成绩")
ggplot(wen_li2, aes(x =  科目, y =  平均成绩, fill =  文理分科)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim = c(75, 100)) +
  theme(legend.position = c(0.9, 0.9))
#http://ggplot2.tidyverse.org/reference/coord_cartesian.html

ggplot(wen_li2, aes(x =  科目, y =  平均成绩, fill =  文理分科)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip(ylim = c(75, 100)) + #也可以横过来显示
  theme(legend.position = c(0.9, 0.9))


View(cheng_ji_biao)

#看看周黎得分与平均分之差
#fen_cha分差
ping_jun <- apply(cheng_ji_biao[, 4:12],
                  2, mean)
fen_cha <- cheng_ji_biao[1, 4:12] - ping_jun
fen_cha <- gather(fen_cha)
barplot(as.matrix(fen_cha))
ggplot(fen_cha, aes(x = key, y = value, fill = key)) +
  geom_bar(stat = "identity", width = 0.25) +
  geom_abline(
    intercept = 0,
    slope = 0,
    size = 1.5,
    alpha = 0.5
  ) +
  #coord_flip() +
  ggtitle("周黎各科分差")


#######################################################
#Cleveland点图_dotchart
#与条形图/柱状图几乎等价：将数值映射为长度
#基础绘图系统中，可以通过dotchart()函数实现
#以下是ggplot2版本
ggplot(fen_cha, aes(x = key, y = value, fill = key)) +
  geom_segment(aes(xend = key, yend = 0), colour = "grey") +
  geom_point(size = 3, aes(colour = key)) +
  geom_abline(
    intercept = 0,
    slope = 0,
    size = 1.5,
    alpha = 0.5
  ) +
  ggtitle("周黎各科分差")

#######################################################
#饼图_pie
#主要用于占比关系的直观展示
#基础绘图系统中采用pie()来实现

#各班人数对比
#首先要将班级转换为因子
cheng_ji_biao$班级  <- factor(cheng_ji_biao$班级)
#饼图_ggplot2
ggplot(cheng_ji_biao, aes(x =  班级, fill =  班级)) +
  geom_bar() +
  ggtitle("各班人数对比")
ggplot(cheng_ji_biao, aes(x =  班级, fill =  班级)) +
  geom_bar() +
  coord_polar() +
  ggtitle("各班人数对比")
ggplot(cheng_ji_biao,  aes(x = factor(1), fill =  班级)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  ggtitle("各班人数对比")


###########################################
#面积堆积图
#查看总分排名前10、后10同学各科成绩对比图
#增加总分这一列
cheng_ji_biao$总分  <- apply(cheng_ji_biao[, 4:12], 1, sum)
cheng_ji_biao2 <-
  cheng_ji_biao[order(cheng_ji_biao$总分, decreasing = TRUE),]
topOnes_raw <- rbind(head(cheng_ji_biao2, n = 10),
                     tail(cheng_ji_biao2, n = 10))
topOnes_gather <- gather(topOnes_raw[, c(1, 4:12)], key = "姓名")
names(topOnes_gather) <- c("姓名", "科目", "成绩")
topOnes_gather$姓名  <-
  factor(topOnes_gather$姓名, levels = topOnes_raw$姓名, ordered = TRUE)
ggplot(topOnes_gather, aes(
  x =  姓名,
  y =  成绩,
  group =  科目,
  fill =  科目,
  colour =  科目
)) +
  geom_area(alpha = 0.5) +
  geom_point(position = "stack")


#######################################################
#折线图
#这些图常用来表征时序变化
#基础绘图系统中采用plot(), points(), lines()来实现

#在此，我们比较一下文理科各科成绩的平均分
#文科生各科平均成绩
wen <- apply(cheng_ji_biao[cheng_ji_biao$文理分科  == "文科", 4:12],
             2, mean)
#理科生各科平均成绩
li <- apply(cheng_ji_biao[cheng_ji_biao$文理分科  == "理科", 4:12],
            2, mean)
wen_li <- rbind(wen, li)
wen_li <- as.data.frame(wen_li)
wen_li$文理分科  <- lengend_text[row.names(wen_li)]
wen_li2 <- gather(wen_li,  文理分科)
colnames(wen_li2) <- c("文理分科", "科目", "平均成绩")
ggplot(wen_li2, aes(
  x =  科目,
  y =  平均成绩,
  group =  文理分科,
  fill =  文理分科,
  colour =  文理分科
)) +
  geom_line() +
  geom_point(shape = 22, size = 3) +
  geom_text(aes(y =  平均成绩, label = format(平均成绩, digits = 3)), vjust = 1.5) +
  ggtitle("文理科各科成绩对比图") +
  theme(legend.position = c(0.15, 0.15))


#######################################################
#直方图
#用来表示数据的分布
#基础绘图系统中采用hist(), lines(density(...))来实现

#下面看一看不同文理科数学成绩的分布
library(ggplot2)
ggplot(cheng_ji_biao, aes(
  x =  数学,
  y = ..density..,
  group =  文理分科,
  fill =  文理分科
)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  geom_density(aes(fill = NA, colour =  文理分科), alpha = 0.2, size = 1)




#######################################################
#箱线图
#同样是用来表示数据的分布
#基础绘图系统中，可用boxplot()来实现
ggplot(cheng_ji_biao, aes(x =  文理分科, y =  数学, fill =  文理分科)) +
  geom_boxplot() +
  geom_rug(position = "jitter",
           size = 0.1,
           sides = "l") +
  ggtitle("文理分科的数学成绩对比")

#######################################################
#小提琴图
#概率密度图对称绘制的效果
#可以通过vioplot::vioplot来绘制
#以下是ggplot2版本
#小提琴图
ggplot(cheng_ji_biao, aes(x =  文理分科, y =  数学, fill =  文理分科)) +
  geom_violin() +
  stat_summary(
    fun.y = fivenum,
    geom = "point",
    fill = "white",
    shape = 21,
    size = 2.5
  ) +
  coord_flip() +
  ggtitle("文理分科的数学成绩对比")
#当然，我们也可以将小提琴图与箱线图合二为一
ggplot(cheng_ji_biao, aes(x =  文理分科, y =  数学, fill =  文理分科)) +
  geom_violin() +
  geom_boxplot(width = 0.2,  outlier.colour = NA) +
  stat_summary(
    fun.y = fivenum,
    geom = "point",
    fill = "white",
    shape = 21,
    size = 2.5
  ) +
  coord_flip() +
  ggtitle("文理分科的数学成绩对比")


#######################################################
#Wikinson点图
ggplot(cheng_ji_biao, aes(x =  文理分科, y =  数学, fill =  文理分科)) +
  geom_dotplot(binaxis = "y",
               binwidth = 0.75,
               stackdir = "center") +
  ggtitle("文理分科的数学成绩对比")


#######################################################
#散点图
#应该是用得最多的图形之一了
#二维散点图一般用来表示变量两两之间的关系
ggplot(cheng_ji_biao, aes(x =  语文, y =  数学)) +
  geom_point(aes(shape =  文理分科, col =  文理分科), alpha = 0.5)
#考虑到可能有部分点会重叠
#还有所谓的向日葵散点图
#可通过sunflowerplot()函数实现

#当然，也可以绘制所有科目之间的散点图对
library(GGally)
ggpairs(cheng_ji_biao[, 4:12])

#######################################################
#二维密度图
ggplot(cheng_ji_biao, aes(
  x =  语文,
  y =  数学,
  shape =  文理分科,
  group =  文理分科
)) +
  geom_point(alpha = 0.5) +
  stat_density2d(aes(colour = ..level..))


#######################################################
#QQ图
#与聊天工具无关
#指的是分位数quantile-quantile图
#用以描述两个变量分布是否一致
#基础绘图系统中，采用qqnorm()、qqplot()实现
ggplot(cheng_ji_biao, aes(sample =  数学)) +
  stat_qq()


#######################################################
#相关系数图
library(corrplot)
corrplot(
  cor(cheng_ji_biao[, 4:12], use = "complete.obs"),
  method = "ellipse",
  is.corr = FALSE,
  diag = TRUE,
  tl.col = rgb(50, 50, 50, maxColorValue = 255)
)
#当然也可以用ggplot2来实现
library(ggplot2)
ke_mu <- as.data.frame(cor(cheng_ji_biao[, 4:12]))
ke_mu <- data.frame(row = rownames(ke_mu), ke_mu)
rownames(ke_mu) <- NULL
ke_mu_gather <- gather(ke_mu, key = row)
colnames(ke_mu_gather) <- c("科目1", "科目2", "相关系数")
ke_mu_gather$科目1 <- factor(ke_mu_gather$科目1,
                           levels = ke_mu$row)
ke_mu_gather$科目2 <- factor(ke_mu_gather$科目2,
                           levels = ke_mu$row)
View(ke_mu)
ggplot(ke_mu_gather, aes(x =  科目1, y =  科目2)) +
  geom_tile(aes(fill = 相关系数), colour = "black")


#######################################################
#三维散点图
library(rgl)
plot3d(
  x = cheng_ji_biao$语文,
  y = cheng_ji_biao$数学,
  z = cheng_ji_biao$外语,
  type = "s",
  size = 0.5,
  col = c("red", "green", "blue")[cheng_ji_biao$文理分科]
)

#######################################################
#除了三维之外，可以继续向多维扩展
#比如脸谱图
library(aplpack)
faces(cheng_ji_biao[1:30, #不适合展示太多数据点，故选取前20
                    4:12])


#######################################################
#平行坐标图
cheng_ji_biao3 <- cheng_ji_biao[sample(1:nrow(cheng_ji_biao), 150),]
library(MASS)
parcoord(
  cheng_ji_biao3[, 4:12],
  var.label = TRUE,
  col = c("red","blue")[cheng_ji_biao3$文理分科],
  cex = 3,
  main = "文理分科平行坐标图"
)

require(GGally)
ggparcoord(cheng_ji_biao3,
           columns = 4:12,
           groupColumn = 13) +
  geom_point()


#######################################################
#当然，在进行数据描述时后
#更要着眼于数据建模
#分类与回归当然是算法建模中最为重要的一个部分
#caret包提供了相应的特征绘制的方法
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
library(caret)
featurePlot(
  x = cheng_ji_biao[, 4:12],
  y = cheng_ji_biao$文理分科,
  plot = "density",
  ## Pass in options to xyplot() to
  ## make it prettier
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

featurePlot(
  x = cheng_ji_biao[, 4:12],
  y = cheng_ji_biao$文理分科,
  plot = "box",
  scales = list(y = list(relation = "free"),
                x = list(rot = 90)),
  auto.key = list(columns = 2)
)


#这里展示的，只是数据可视化的一部分图形
#还有很多图形尚未涉及，请小伙伴们自行研究：
#比如：
#茎叶图stem()
#马赛克图mosaicplot()
#雷达图stars()
#关系图
#网络图plot.igraph
#地图ggmap
#等等
#以后在进行具体算法建模时，也会涉及到很多专用的图形
#比如：
#关联规则：关联网络图
#聚类分析：层次谱系图
#分类回归：分类回归树、神经网络图、变量重要性图
#……
#这些图形展示，当然已经超越了所谓的简单的数据描述了
#而是在对模型本身进行直观展示



#######################################################
##The End ^-^
#######################################################
