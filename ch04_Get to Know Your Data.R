

#######################################################
#######################################################
##
## 名称：《R语言数据分析·认识数据》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V7
## 时间：2017年9月
##
##*****************************************************
##
## ch04_Get to Know Your Data_V7
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


#了解一个人，可能是先看其长相，了解其言谈举止
#而后再逐渐深入
#了解一份数据，首先也是要看数据的高矮胖瘦
#然后再去深入了解内在的关系结构

#要初步了解数据，大部分教材上用的是以下两种方法：
#1、用少量数字描述数据
#2、用图形直观刻画数据
#在V1~V6版中，采用的也是上述思路

#从V7版开始，本课程将更新一个思路
#子曰：吾道一以贯之
#既然本课程提出：
#机器学习之要义在于“关系结构”
#尤其是变量之间的关系和数据空间的结构
#认识数据，亦然~！

#######################################################
##获取数据
#######################################################
#为保持课程的一致性，减少小伙伴们熟悉业务背景的成本
#本次课程同样采用前述的《学生成绩分析》的数据
#采用真实数据的目的很简单：所得结果是鲜活的

#我们要做的事情，是直接将其读入
cheng_ji_url <-"https://github.com/byaxb/RDataAnalytics/raw/master/data/cj.csv"
cheng_ji_biao <- read.csv(cheng_ji_url,
                          head = TRUE,
                          stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8")
#前述fileEncoding不能少
#当然，也可以使用readr::read_csv
#会直接guess_encoding()
library(readr)
cheng_ji_biao <- read_csv(cheng_ji_url)
str(cheng_ji_biao)

#作必要的处理
cheng_ji_biao$班级 <- factor(cheng_ji_biao$班级)
cheng_ji_biao$性别 <- factor(cheng_ji_biao$性别)
cheng_ji_biao$文理分科 <- factor(cheng_ji_biao$文理分科)
cheng_ji_biao$总成绩 <- apply(cheng_ji_biao[, 4:12], 1, sum)

#在编代码时，最好不要有太多的复制粘贴行为
#一个通常的做法是写完了
#cheng_ji_biao$班级 <- factor(cheng_ji_biao$班级)
#这行代码后，复制两遍，然后将班级更改为性别和文理分科
#这种做法并不可取。因为很有可能你把左侧都改好了，而右侧
#的班级忘记修改了，而在代码执行的过程中，并不会报错
#更好的做法是写一个循环：
for(tmp_col in c("班级", "性别", "文理分科")) {
  cheng_ji_biao[, tmp_col] <- factor(cheng_ji_biao[, tmp_col])
}
#当然，还有另一种更好的选择，让你的代码干净、清爽
library(tidyverse)
cheng_ji_biao <- cheng_ji_biao %>%
  mutate(班级 = factor(班级),
           性别 = factor(性别),
           文理分科 = factor(文理分科),
           总成绩 = rowSums(.[, 4:12]))
#这里的rowSums，也可以是自定义的
my_sum <- function(x) {
  row_sum <- apply(x, 1, sum)
  return(row_sum)
}
cheng_ji_biao <- cheng_ji_biao %>%
  mutate(班级 = factor(班级),
           性别 = factor(性别),
           文理分科 = factor(文理分科),
           总成绩 = my_sum(.[, 4:12]))
View(cheng_ji_biao)
str(cheng_ji_biao)
head(cheng_ji_biao)

#可能有小伙伴觉得，一次次从网络读取数据很麻烦
#好吧，那咱们就把它存到本地
#实际上，在大部分数据分析项目中
#我们都可以把清理好的数据存为rda格式放在本地
#save(cheng_ji_biao, file = "cj.rda")
#上述语句中，默认的位置是在getwd()
#小伙伴当然也可以把它放在其他位置

load("cj.rda")
#也可以采用下边这种方式选取
#load(file.choose())

#######################################################
##一维数据空间形态
##单变量数据分布
#######################################################

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

#一维空间的数据形态，可以通过茎叶图或是Wikinson点图
#来直观表示
library(tidyverse)
shuxue_1101 <- cheng_ji_biao%>%
  filter(班级 == "1101") %>%
  select(数学)
shuxue_1110 <- cheng_ji_biao%>%
  filter(班级 == "1110") %>%
  select(数学)
stem(shuxue_1101[, 1])
# The decimal point is 1 digit(s) to the right of the |
#   
# 5 | 5799
# 6 | 0014
# 6 | 55789
# 7 | 000011122334444
# 7 | 788899
# 8 | 111222334444
# 8 | 589
# 9 | 224
stem(shuxue_1110[, 1])
# The decimal point is at the |
#   
# 88 | 0
# 90 | 000
# 92 | 00000000
# 94 | 00000000
# 96 | 000000000000000000000000
# 98 | 0000



#Wikinson点图
ggplot(cheng_ji_biao, 
       aes(x=factor(0), y = 数学, 
           fill = ..count..,
           colour = ..count..))+
  geom_dotplot(binaxis = "y",
               binwidth = 1.25,
               stackdir = "center")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

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

#前边是数据形态的直观展示
#我们也可以有一些定量指标来对散步情况进行分析
#现以数学、语文这两门课程为例
#对数据形态进行观察
#数据的集中趋势
#众数modal number
modal_number <- function(x) {
  freqs <- table(x)
  highest_freq <- which.max(freqs)
  modal_number <- names(highest_freq)
  if(is.numeric(x)) {
    modal_number <- as.numeric(modal_number)
  }
  return(modal_number)
}
modal_number(cheng_ji_biao$数学)
#[1] 96
modal_number(cheng_ji_biao$语文)
#[1] 89
modal_number(cheng_ji_biao$班级)
#[1] "1102"

#均值
mean(cheng_ji_biao$数学)
#[1] 86.08129
mean(cheng_ji_biao$语文)
#[1] 87.26581
mean(cheng_ji_biao$数学, trim = 0.01)
#[1] 86.37188
mean(cheng_ji_biao$语文, trim = 0.01)
#[1] 87.50854
#两门课平均分相差不大

#中位数
median(cheng_ji_biao$数学)
#[1] 89
median(cheng_ji_biao$语文)
#[1] 88


#数据的分散程度
#分位数
quantile(cheng_ji_biao$数学)
# 0%  25%  50%  75% 100% 
# 0   81   89   95  100 
quantile(cheng_ji_biao$语文)
# 0%  25%  50%  75% 100% 
# 0   85   88   91   96
#可能已经有小伙伴留意到：居然还有取值为0的记录
#这些异常值，接下来需要进行处理

#四分位距
IQR(cheng_ji_biao$数学)
#[1] 14
IQR(cheng_ji_biao$语文)
#[1] 6
#显然数学的四分位距要大得多
#也就是数学是比较拉分的科目
#但数学并不是分散程度最大的科目
apply(cheng_ji_biao[, 4:12], 2, IQR)
# 语文 数学 外语 政治 历史 地理 物理 化学 生物 
# 6.0 14.0  8.0  5.0  9.5  6.0 17.0 10.0 12.0
#由此可见，真正拉分的，是物理、数学、生物和化学



#极差=全距
range(cheng_ji_biao$数学)
diff(range(cheng_ji_biao$数学))
range(cheng_ji_biao$语文)
diff(range(cheng_ji_biao$语文))
max(cheng_ji_biao$数学) - min(cheng_ji_biao$数学) #与上一语句同
#当然也可以求取所有科目的极差
apply(cheng_ji_biao[, 4:12], 2, function(x) {
  diff(range(x))
  #或者
  #max(x) - min(x)
})


#标准差
sd(cheng_ji_biao$数学)
#[1] 10.89484
sd(cheng_ji_biao$语文)
#[1] 5.853646

#绝对中位差
mad(cheng_ji_biao$数学)
#[1] 8.8956
mad(cheng_ji_biao$语文)
#[1] 4.4478

#变异系数
#Coefficient of Variation
coefficient_of_ariation <- function(x) {
  sd(x) / mean(x)
}
coefficient_of_ariation(cheng_ji_biao$数学)
#[1] 0.1138789
coefficient_of_ariation(cheng_ji_biao$语文)
#[1] 0.04800896

#摄氏度
#以2017年6月15日全国各地天气为例
ce <- c(21, 21, 16, 14, 18, 17, 15, 18, 19, 19, 20,
        21, 21, 22, 22, 21, 20, 25, 25, 27, 17, 20,
        17, 19, 13, 18, 15, 12, 18, 21, 23, 27, 26)
#以下是同样的温度另外两种表示方法
#兰氏度
ra <- 1.8*ce + 32 + 459.67
#开氏度
ke <- 273.15 + ce
#标准差却不一样
sd(ra)
#[1] 6.851509
sd(ke)
#[1] 3.806394
#但变异系数相同
coefficient_of_ariation(ra)
coefficient_of_ariation(ke)
#[1] 0.01300059

#刚才已经提到，数据中存在异常点
#一个简单的办法就是通过箱线图来识别
library(ggplot2)
ggplot(cheng_ji_biao, aes(x=factor(0), y = 总成绩))+
  geom_boxplot(width = 0.5,
               fill ="#E69F00",
               outlier.colour = "red",
               outlier.shape = 3,
               outlier.size = 2)+
  geom_rug(position = "jitter",
           size = 0.1,
           sides = "l") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
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

#上图中只是直观的展示了异常点的存在
#以下通过boxplot.stats直接获取异常点的具体取值
(outliers <- boxplot.stats(cheng_ji_biao$总成绩)$out)
#异常点的标签
outliers_idx <- which(cheng_ji_biao$总成绩 %in% outliers)
#显示异常点
View(cheng_ji_biao[outliers_idx, ])
#剔除掉异常点之后的新数据
cheng_ji_biao <- cheng_ji_biao[-outliers_idx, ]
#以下的数据探索，都是针对这一份新数据

#看一看数据分布的形状
hist_results <- hist(cheng_ji_biao$数学, breaks=30)
shuxue_hist <- data.frame(counts= hist_results$counts,
                          breaks = hist_results$mids)
library(ggplot2)
ggplot(shuxue_hist, aes(x=breaks, y = counts, fill = counts)) +
  geom_bar(stat = "identity",alpha = 0.75)+
  scale_fill_gradient(low="blue", high="red")  
#当然，对于直方图和概率密度曲线
#ggplot2本身也支持得很好
ggplot(cheng_ji_biao, aes(x = 数学)) +
  geom_histogram(aes(y = ..density.., fill = ..density..)) +
  geom_density(aes(y = ..density..), 
               colour = 'red', 
               fill = 'red', 
               alpha = 0.35, 
               size = 0.75)

#可以通过小提琴图和箱线图来观看其数据分布
library(ggplot2)
ggplot(cheng_ji_biao, aes(x=factor(0), y = 数学))+
  geom_violin(fill = "#56B4E9", width = 0.75) +
  geom_boxplot(width = 0.25,
               fill ="#E69F00",
               outlier.colour = "red",
               outlier.shape = 1,
               outlier.size = 2)+
  geom_rug(position = "jitter",
           size = 0.1,
           sides = "b") +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


#以上是数据分布的直观展示
#我们也可以看一看具体的量化值
#数据分布的形状
library(e1071)
#skewness偏度
skewness(cheng_ji_biao$语文)
#[1] -0.7717088
skewness(cheng_ji_biao$数学)
#[1] -0.9768555
#kurtosis峰度
#通过峰度也可以看出平均数的代表性
kurtosis(cheng_ji_biao$语文)
#[1] 0.9025781
kurtosis(cheng_ji_biao$数学)
#[1] 0.2687023
#注意：峰度的不同定义

#前述指标中涉及到与正态分布的比较
#实际上，还有一个QQ图也可展示不同分布的比较
#QQ图
#与聊天工具无关
#指的是分位数quantile-quantile图
#用以描述两个变量分布是否一致
library(ggplot2)
ggplot(cheng_ji_biao, 
       aes(sample = 数学)) +
  stat_qq(aes(colour = ..sample..))


#数据的平移与缩放
ggplot(cheng_ji_biao, aes(x = 语文)) +
  geom_density(size = 1, 
               fill = "orange",
               colour = "blue",
               alpha = 0.75) +
  geom_density(aes(x = 语文 - mean(语文)),
               size = 1,
               fill = "orange",
               colour = "blue",
               alpha = 0.75) +
  geom_density(aes(x = (语文 - mean(语文)) / sd(语文)),
               size = 1,
               fill = "darkred",
               alpha = 0.5)
##小伙伴藉此可以看出减法、除法的数据科学含义

#以下做一个数据平移、缩放的动画
#小伙伴们正好也可以尝试一下如何直接生成GIF
library(ggplot2)
library(animation)
saveGIF(
  expr = {
    frame_count <- 100
    all_centers <- seq(from = 0,
                       to = 30,
                       length = frame_count)
    all_scales <- seq(from = 1,
                      to = 1.8,
                      length = frame_count)
    for (cur_frame in 1:frame_count) {
      cur_center <- all_centers[cur_frame]
      cur_scale <- all_scales[cur_frame]
      cur_title <- paste("Center:", format(cur_vline, digits = 4),
                         "Scale:", format(cur_scale, digits = 4))
      cur_density <- density((cheng_ji_biao$语文 - cur_center)/ cur_scale)
      cur_vline <- cur_density$x[which.max(cur_density$y)]
      cur_hline <- max(cur_density$y)
      plot(
        ggplot(cheng_ji_biao, aes(x = (语文 - cur_center)/ cur_scale)) +
          geom_density(fill = "blue", 
                       colour = "darkgrey",
                       alpha = 0.5) +
          xlim(25, 100) +
          ylim(0, 0.2) +
          geom_vline(xintercept = cur_vline, size = 1, colour = "blue") +
          geom_hline(yintercept = cur_hline, size = 1, colour = "red") +
          ggtitle(cur_title))
      
    }
  },
  movie.name = "D://desktop/animation.gif",
  convert = "gm convert",
  interval = 0.05
)


yuwen_pingjun <- cheng_ji_biao %>%
  group_by(班级) %>%
  dplyr::select(班级, 语文) %>%
  summarise(平均成绩 = mean(语文))
yuwen_pingjun
yuwen01 <- cheng_ji_biao %>%
  filter(班级 == "1101") %>%
  dplyr::select(语文)
yuwen06 <- cheng_ji_biao %>%
  filter(班级 == "1106") %>%
  dplyr::select(语文)

eq_score <- function(x, center, scale) {
  if(is.numeric(x) && is.vector(x)) {
    scale(scale(x), -center/scale, 1/scale)[, 1]
  } else {
    cat("Only numeric vectors are supported\n")
  }
}
eq_score(yuwen01[, 1], 
         mean(yuwen06[, 1]), 
         sd(yuwen06[, 1]))
#当然，标准得分并不只是应用于将一个班的学生成绩
#换算成另一个班，在聚类的过程中，要让量纲不同的
#特征发挥同等重要的作用，一般也要做标准化
#在某些算法实现方面，比如神经网络，也可能需要对
#变量进行标准化

#对于连续数据的描述，基本如此
#对于非连续数据，当然也是查看其分布情况
#计次可能是最直观的
#比如计算文理科学生的数量
ggplot(cheng_ji_biao, 
       aes(x = 文理分科, 
           fill = 文理分科)) +
  geom_bar(width = 0.25) +
  geom_text(stat="count", 
            aes(label = ..count.., y = ..count..+10))
#可见文理科学生人数表均衡
#再来看看各班男女生的数量
library(ggplot2)
ggplot(cheng_ji_biao, 
       aes(x = 班级, 
           fill = 性别)) +
  geom_bar(width = 0.5, position = "fill") +
  geom_text(stat="count", 
            aes(label = ..count..),
            position = position_fill(vjust = .5)) +
  facet_wrap(~文理分科, ncol=2, scale="free") +
  scale_fill_manual(values=c("orange","darkgrey"))
#最后这行代码，当然是手工调颜色了

#可以看出，1111和1113班还有个别文科生
#同样属于错误数据
#需要重新确认数据源
#本实验中直接进行清洗
cheng_ji_biao <- cheng_ji_biao %>%
  filter(!(文理分科 == "文科" & 班级 %in% c("1111", "1113")))
#清洗完成之后，再次执行上述代码
ggplot(cheng_ji_biao, 
       aes(x = 班级, fill = 性别)) +
  geom_bar(width = 0.5, position = "fill") +
  geom_text(stat="count", 
            aes(label = ..count..),
            position = position_fill(vjust = .5)) +
  facet_wrap(~文理分科, ncol=2, scale="free") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("orange","darkgrey"))

#从前述分析可以看出，一维数据的探索
#大部分都是关于“变化”的
#衡量变化也有其他很多指标，比如Gini指数、信息熵
library(entropy)
shannon_entropy <- function(counts) {
  frequencies <- counts / sum(counts)
  -sum(frequencies * log2(frequencies))
}
counts <- table(cheng_ji_biao$文理分科)
shannon_entropy(counts)
#[1] 0.999797
#当然，也可以直接调用已有的扩展包
library(entropy)
entropy(counts)
#[1] 0.6931472
entropy::entropy(counts, unit = "log2")
#[1] 0.999797


#######################################################
##二维数据空间形态
##变量两两之间的关系
#######################################################

#######################################################
#离散变量vs离散变量
#形式可以有很多种，比如马赛克图
#本实验中推荐的是树图
library(treemap)
library(tidyverse)
cjb_sum <- cheng_ji_biao %>% 
  group_by(文理分科, 班级, 性别) %>% 
  summarise(count = n())
View(cjb_sum)
treemap(as.data.frame(cjb_sum) ,
        index=c("文理分科", "班级", "性别"),
        vSize="count",
        vColor="count",
        type="value")

#接下来则是树图的高级版
#可交互的树图
#library(devtools)
#install_github("AntoineGuillot2/D3partitionR")
library(D3partitionR)
cjb_sum <- as.data.frame(cjb_sum)
for(i in 1:3) {
  cjb_sum[, i] <- as.character(cjb_sum[, i])
}
cjb_sum2 <- as.data.table(cjb_sum)
D3partitionR()%>%
  add_data(cjb_sum2,
           count = 'count',
           steps=c("文理分科", "班级", "性别"),
           tooltip = c('name','count'),
           color = "name")%>%
  set_chart_type('treemap')%>%
  set_tooltip_parameters(visible=T,style='background-color:lightblue;')%>%
  add_title(text='各班男女生',style='font-size:20px;')%>%
  plot()
#其他很多参数请小伙伴们自行尝试
#set_chart_type('circle_treemap')
#更多参数可见网址
#https://github.com/AntoineGuillot2/D3partitionR



#######################################################
#连续变量vs连续变量
#散点图是最常见、但同时也应该是最有用的图之一
#散点图可用来观察变量之间可能存在的模式
#同时也是二位数据空间形态的最直接的体现
library(ggplot2)
ggplot(cheng_ji_biao, aes(x = 物理,
                          y = 语文, 
                          colour = 物理)) +
  geom_point()
ggplot(cheng_ji_biao, aes(x = 生物, 
                          y = 化学, 
                          colour = 生物)) +
  geom_point()

library(ggplot2)
ggplot(cheng_ji_biao, aes(x = 数学, 
                          y = 生物, 
                          fill = 文理分科)) +
  geom_point(aes(shape = 文理分科, 
                 colour = 文理分科))

ggplot(cheng_ji_biao, aes(x = 生物, y = 化学, fill = 文理分科)) +
  geom_point(aes(shape = 文理分科, colour = 文理分科))


#散点图矩阵
library(GGally)
ggpairs(cheng_ji_biao,
        columns = 4:12)
View(cheng_ji_biao)

#相关系数矩阵
library(corrplot)
corrplot(cor(cheng_ji_biao[, 4:12]),
         method = "color",
         diag = FALSE)
View(cor(cheng_ji_biao[, 4:12]))

#相关系数
library(ggplot2)
library(tidyverse)
xgxs <- cor(cheng_ji_biao[, 4:12])
xgxs <- xgxs %>%
  as.data.frame() %>%
  mutate(ke_mu = row.names(xgxs)) %>%
  gather(key = ke_mu)
#当然，将row.names转换成一列，这种操作比较常见
#因此也有专门的函数实现
# xgxs <- xgxs %>%
#   as.data.frame() %>%
#   rownames_to_column(var = "ke_mu") %>%
#   gather(key = ke_mu)

names(xgxs) <- c("kemu1", "kemu2", "xgxs")
xgxs$xgxs_cut <- cut(xgxs$xgxs,
                 breaks= seq(0, 1, len = 11),
                 include.lowest=TRUE)
ggplot(xgxs, aes(x = kemu1, y = kemu2, fill = xgxs_cut)) +
  geom_tile(colour="white", size = 1.5) +
  geom_text(aes(label = format(xgxs, digits = 2))) +
  scale_fill_brewer(palette = "YlGn",name="相关系数")


#######################################################
#离散变量vs连续变量
#主要是分组绘图
#对不同的组别进行比较

#箱线图
library(ggplot2)
ggplot(cheng_ji_biao, aes(x =  文理分科, y =  数学, fill =  文理分科)) +
  geom_boxplot(outlier.colour = "red",
               outlier.shape = 3,
               outlier.size = 1) +
  geom_rug(aes(colour = 文理分科),
           position = "jitter",
           size = 0.1,
           sides = "l",
           alpha = 0.25)


#看看不同班级数学成绩的分布
library(ggplot2)
ggplot(cheng_ji_biao, aes(x =  班级, y =  数学, 
                          fill =  班级)) +
  geom_boxplot(outlier.colour = "red",
               outlier.shape = 3,
               outlier.size = 1) +
  theme(legend.position = "none")

#小提琴图
ggplot(cheng_ji_biao, aes(x =  文理分科, 
                          y =  数学, 
                          fill =  文理分科)) +
  geom_violin() +
  stat_summary(
    fun.y = fivenum,
    geom = "point",
    fill = "white",
    shape = 21,
    size = 2.5) +
  coord_flip()
#当然，我们也可以将小提琴图与箱线图合二为一
ggplot(cheng_ji_biao, aes(x =  文理分科, 
                          y =  数学, fill =  文理分科)) +
  geom_violin() +
  geom_boxplot(fill = "orange", width = 0.2,  outlier.colour = NA) +
  stat_summary(
    fun.y = fivenum,
    geom = "point",
    fill = "white",
    shape = 21,
    size = 2.5) +
  coord_flip()

#Wikinson点图
ggplot(cheng_ji_biao, aes(x =  文理分科, 
                          y =  数学, 
                          fill =  文理分科,
                          colour = 文理分科)) +
  geom_dotplot(binaxis = "y",
               binwidth = 0.65,
               stackdir = "center")

#直方图与概率密度图
library(ggplot2)
ggplot(cheng_ji_biao, aes(x =  数学, 
                          y = ..density.., 
                          group =  文理分科,
                          fill =  文理分科)) +
  geom_histogram(position = "identity", 
                 alpha = 0.5) +
  geom_density(aes(fill = NA, 
                   colour =  文理分科), 
               alpha = 0.2,
               size = 1)

#当然，如果分组太多，显然不适合全都叠加在一起
#可以采用以下方式
library(ggridges)
library(viridis)
ggplot(cheng_ji_biao, aes(x = `数学`, y = `班级`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "数学成绩", option = "C") +
  labs(title = '数学成绩',
       subtitle = '某高中各班数学成绩\n其中，1101~1107为文科班，1108~1115为理科班') +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank())
#顺便提一下，以上代码中用到的是反引号`backquotes，
#注意与单引号的区别
#反引号一般用于不规范变量命名，如：
`variriable name with blank` <- 1:10
`variriable name with blank`
x <- 1:10
`x`



library(tidyverse)
library(ggplot2)
cheng_ji_biao %>%
  select(4:13) %>%
  gather(文理分科) %>%
  set_names(c("文理分科", "科目", "成绩")) %>%
  group_by(文理分科, 科目) %>%
  summarise(平均成绩 = mean(成绩)) %>%
  mutate(平均成绩 = ifelse(文理分科 == "文科", 平均成绩, -1*平均成绩)) %>%
  ggplot(aes(x = 科目, y = 平均成绩, fill = 文理分科)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(abs(平均成绩), digits = 3)))+
  scale_y_continuous(labels=abs) + 
  coord_flip()

library(tidyverse)
library(ggplot2)
to_plot <- cheng_ji_biao %>%
  select(4:13) %>%
  gather(文理分科) %>%
  set_names(c("文理分科", "科目", "成绩")) %>%
  group_by(文理分科, 科目) %>%
  summarise(平均成绩 = mean(成绩)) 
ggplot(to_plot, aes(x = 科目, y = 平均成绩)) + 
  geom_bar(data=subset(to_plot, 文理分科 == "理科"),
           aes(fill = c("理科","文科")[1]), 
           stat = "identity")+
  geom_bar(data=subset(to_plot, 文理分科 == "文科"), 
           aes(fill = c("理科","文科")[2]), 
           alpha = 0.5,stat = "identity") +
  theme(legend.title = element_blank())



#对于分类问题而言，在进行数据描述时
#最关键的，当属因变量vs自变量了
library(caret)
featurePlot(
  x = cheng_ji_biao[, 4:12],
  y = cheng_ji_biao$文理分科,
  plot = "density",
  scales = list(
    x = list(relation = "free"),
    y = list(relation = "free")),
  adjust = 1.5,
  pch = "|",
  auto.key = list(columns = 2))
#从上图可以看出，数学/生物最优辨识度
#而语文，几乎文理科生没有什么区别

#变量之间的依存关系
#可以通过信息增益来度量
#也就是说，当我们知道某个自变量时
#有助于因变量不确定性的减少
library(infotheo)
condentropy(cheng_ji_biao$文理分科) -
  condentropy(cheng_ji_biao$文理分科, cheng_ji_biao$性别)

library(FSelectorRcpp)
information_gain(x = cheng_ji_biao[, c(1, 3)],
                 y = cheng_ji_biao$文理分科)
#实际上，信息增益也是特征选择常用的方法

#######################################################
##高维数据空间形态
##多变量之间的关系
#######################################################
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
ibreaks <- seq(50, 100, len = 21)
cheng_ji_biao %>%
  select(物理, 数学) %>%
  mutate(物理 = cut(物理, breaks = ibreaks),
           数学 = cut(数学, breaks = ibreaks)) %>%
  #将语文、数学离散化
  group_by(物理, 数学) %>%
  summarise(freq = n()) %>%
  #进行汇总统计
  complete(物理, 数学) %>%
  mutate(freq = ifelse(is.na(freq), 0, freq)) %>%
  #不全为0的单元格
  ggplot(aes(x = 物理, y = 数学, fill = freq)) +
  geom_tile(colour="white", size = 0.5) +
  geom_text(aes(label = freq), size = 3) +
  #scale_fill_distiller(direction = 1) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle = 90))
#感兴趣的小伙伴可以用stat_density2d
#或是stat_bin2d实现类似的效果

#接下来考虑另一种计算密度的方法
#每一个点，半径为epsilon领域内点的多少
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
library(rgl)
#绘制“空”间
plot3d(
  x = imatrix,
  size =2,
  type = "n",
  xlab = "Mathematics", 
  ylab = "Physics",
  zlab = "Biology",
  col = cols)
grd <- imatrix
grd$col <- cols
grd$alpha <- (sws_density - min(sws_density)) / (max(sws_density) - min(sws_density)) * 0.9
length <- width <- height <- (max(iseq) - min(iseq)) / length(iseq)
for(i in seq(nrow(grd))){
  #创建一个长方体
  icube3d <- cube3d(col = grd$col[i])
  #设定长宽高
  icube3d <- scale3d(icube3d, length, width, height)
  #将长方体移动至指定位置
  icube3d <- translate3d(icube3d, grd$数学[i], grd$物理[i], grd$生物[i])
  #绘制长方体
  shade3d(icube3d, alpha = grd$alpha[i])
}

#数据空间的均匀程度
#可以用hopkins统计量来描述
#均匀分布的话，趋近于0.5
#倾斜的话，趋近于0
clustertend::hopkins(cheng_ji_biao[, 4:12], n = 100)

#除了三维之外，可以继续向多维扩展
#比如脸谱图
library(aplpack)
#不适合展示太多数据点
#文理各科分别选取8个
cheng_ji_biao_a <- cheng_ji_biao[which(cheng_ji_biao$文理分科 == "文科"), ]
cheng_ji_biao_b <- cheng_ji_biao[which(cheng_ji_biao$文理分科 == "理科"), ]
top8A <- head(cheng_ji_biao_a[order(cheng_ji_biao_a$总成绩, decreasing = TRUE), 
                              4:12], n = 8)
top8B <- head(cheng_ji_biao_b[order(cheng_ji_biao_b$总成绩, decreasing = TRUE), 
                              4:12], n = 8)
faces(rbind(top8A, top8B))

View(cheng_ji_biao)

#平行坐标图
cheng_ji_biao_a <- cheng_ji_biao[which(cheng_ji_biao$文理分科 == "文科"), ]
cheng_ji_biao_b <- cheng_ji_biao[which(cheng_ji_biao$文理分科 == "理科"), ]
top150A <- head(cheng_ji_biao_a[order(cheng_ji_biao_a$总成绩, decreasing = TRUE), 
                              4:13], n = 150)
top150B <- head(cheng_ji_biao_b[order(cheng_ji_biao_b$总成绩, decreasing = TRUE), 
                              4:13], n = 150)
top300 <- rbind(top150A, top150B)
library(MASS)
parcoord(
  top300[, 1:9],
  var.label = TRUE,
  col = c("red","blue")[top300$文理分科],
  cex = 3,
  main = "文理分科平行坐标图"
)

require(GGally)
ggparcoord(top300,
           columns = 1:9,
           groupColumn = 10) +
  geom_point()


library(FSelectorRcpp)
imp <- information_gain(x = cheng_ji_biao[, 3:12],
                 y = cheng_ji_biao$文理分科, 
                 type = "gainratio")
imp$attributes <- factor(imp$attributes,
                         levels = imp$attributes)
library(ggplot2)
ggplot(imp, aes(x = attributes, 
                y = importance,
                fill = importance)) +
  geom_bar(stat = "identity")



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



#######################################################
##The End ^-^
#######################################################
