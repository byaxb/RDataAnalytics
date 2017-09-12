

#######################################################
#######################################################
##
## 名称：《R语言数据分析·Get Your Hands Dirty》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V6
## 时间：2017年6月
##
##*****************************************************
##
## ch01_Get Your Hands Dirty_V6
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



#大部分的R语言上手教程
#都先给大家介绍一些零零散散的简单操作
#本文也不能免俗，先给大家看一些简单的语句
#简单的语句之后，给大家讲一个完整的故事

#######################################################
##一些简单的数学运算
#######################################################

#创建一个变量x
x <- 2
#关于x的一些运算
sqrt(x)#熟悉的根号2
x ^ 0.5#与上一语句等价
x ^ 8#8位寄存器能存256个数
x ^ 2 #x的平方
y <- x ^ 2 #将x的平方赋值给变量y
#圆的面积
area <- pi * (x ^ 2) #pi为内置的常数，数学常数pi
area #将面积显示出来
#pi大家都已经猜到了
pi
print(pi, digits = 17) #显示小数点后
(area <- pi * y) #不仅给area赋值，还显示出来
log2(x)#底为2的对数
log10(x)#底为10的对数
log(x)#底为e的对数
exp(1)#数学常数e
exp(x)#e的二次方
log(x, base = c(2, exp(1), 10))#取不同的底

#创建一个向量
x <- seq(from = -10, to = 10, by = 0.03)#从-10开始，步长为0.03，最大值不超过10
x
2 * x
min(x)
max(x)
range(x)
diff(range(x))
y <- sin(x)
plot(x, y)
y <- abs(y)
lines(x, y, col = "blue", type = "o")
#积分
integrate(sin, 0, pi)
integrate(function(x)
  abs(sin(x)), -10, 10)
z <- sin(2 * x)
lines(x, z, col = "red", type = "o")


#创建一个向量a
a <- c(3, 4)
#向量a的长度
sqrt(sum(a2 ^ 2))
#两个向量之和
b <- c(2, 8)
a + b
#向量的
#向量的内积<a, b>
a %*% b
drop(a %*% b) #结果作为标量
sum(a * b) #与上一语句等价

#创建一个矩阵
# 1  1
# 1  -1
A <- matrix(c(1, 2,
              3, -1),
            ncol = 2,
            byrow = TRUE)
A
#在窗格中显示A
View(A)
#矩阵的转置
t(A)

#解线性代数方程
#x1 + x2 = 2
#x1 - x2 = 4
A <- matrix(c(1, 1,
              1, -1),
            ncol = 2,
            byrow = TRUE)
b <- c(2, 4)
solve(A, b)
#求解A的逆矩阵
solve(A)
#等价于
solve(A, diag(2))
#这里diag(2)是一个单位阵
diag(2)

#排列组合
choose(4,2) 
factorial(4)#4! = 4*3*2*1
combn(1:4, 2)


#######################################################
##常见的集合运算
#######################################################
A <- 1:10
B <- seq(5, 15, 2)
C <- 1:5
#求A和B的并集
union(A, B)
#求A和B的交集
intersect(A, B)
#求A-B
setdiff(A, B)
#求B-A
setdiff(B, A)
#检验集合A,B是否相同
setequal(A, B)
#检验元素12是否属于集合C
is.element(12, C)
#检验集合A是否包含C
C %in% A
all(C %in% A)
C %in% B
all(C %in% B)


#抽样
idx <- 1:100
sample(idx)
train_idx <- sample(idx, 70)
train_idx
selected_idx <- sample(idx, 100, rep = TRUE)
not_selected_idx <- setdiff(idx, selected_idx)
length(not_selected_idx) / length(idx)
unique(selected_idx)
table(selected_idx)
sort(table(selected_idx), decreasing = TRUE)


#######################################################
##一个完整的故事：《学生成绩分析》
#######################################################

#前边这些简单的脚本，显然不能满足小伙伴们关于R的各种幻想
#我们学习R语言数据分析时，显然不应该再从hello world开始
#在此，我们换一个思路：通过讲一个完整的故事，
#让大家对R语言数据分析有一个直观的认识


#故事发生在四五年前
#那时候，大家都还只是高中生
#高中会根据大家的各科成绩，进行文理分科
#咱们就从学生的各门成绩开始

#向量，用于存储对多个对象的某一属性进行观测所得到的结果
#比如现在有6个同学，对他们的姓名进行观测，得结果如下：
xing_ming <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
xing_ming
#如此便生成了一个长度为6的字符向量
#生成的方式很简单，就是调用c()这个函数
#Combine Values into a Vector or List
#R里边，函数的调用，就是用函数名紧接着相应的小括号
#c()这个函数的用法，请执行下述语句
?c

#找一找第1位、第3为同学分别叫什么
xing_ming[c(1, 3)]
#小伙伴们应该注意到了，R里边三种括号的用法
#小括号()：表示函数调用
#中括号[]：表示下标
#大括号{}：表示语句块
#下标顺序可颠倒、可重复
xing_ming[c(1, 3, 3, 2)]
#也可以反向选择
xing_ming[-c(1, 3)]
xing_ming[6:1]
rev(xing_ming) #与上一语句效果相同

#给这些同学一些学号
xue_hao <- c(201003001,
             201003002,
             201003003,
             201003004,
             201003005,
             201003006)
#因为学号是连续的，也可以通过下边的方式生成
xue_hao <- 201003001:201003006
#用冒号连接的，是指生成步长为1的等差数列
#生成等差数列更一般的方法
xue_hao_by2 <- seq(from = 201003001,
                   to = 201003006,
                   by = 2)
#显然，此时的步长为2
#生成这个序列时，也是在调用函数
#调用的是seq函数，接受了三个参数
#各参数的含义，请小伙伴自行查阅文档
?seq
#至此，我们已经学会了两种向量：字符型和数值型

xing_bie <- c("女", "男", "男", "女", "男","女")
#性别的特殊之处在于，它的取值水平是有限的
#这种情况下，我们应该将其转换为因子
xing_bie <- as.factor(xing_bie)
#统计一下男、女的数量
table(xing_bie)

#对这6个同学的语文成绩进行记录
yu_wen <- c(94, 87, 92, 91, 85, 92)
#看看平均成绩
mean(yu_wen)
#对他们进行排序
sort(yu_wen)
#将高分排在前边
sort(yu_wen, decreasing = TRUE)
#另外一个函数
order(yu_wen)
yu_wen[order(yu_wen)]
#看看同学们的排名
xing_ming[order(yu_wen)]
#可以将他们排成一个“张三 < 李四 < 王五”的队列么
paste(xing_ming[order(yu_wen)], collapse = " < ")
#paste是指将不同的字符粘在一起
#比如，创建x1~x10变量名
paste("x", 1:10, sep = "")
#再比如，将每个人的姓名与成绩粘在一起
paste(xing_ming, '语文成绩为', yu_wen)

#语文成绩加5分
yu_wen + 5#结果依旧是长度为6的一个向量
yu_wen #原向量并没有变
yu_wen <- yu_wen + 5 #换一种方式
yu_wen #这个时候变了

#找找男生的成绩
xing_bie == "男"
#[1] FALSE  TRUE  TRUE FALSE  TRUE FALSE
yu_wen[xing_bie == "男"]
#实际上就是
yu_wen[c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)]
#结果如下：
#[1] 87 92 85
#94 87 92 91 85 92
#F  T  T  F  T  F
#无非是一个等长的逻辑向量，将其中的TRUE部分取出来而已
#注意：千万不要以为R会去自动寻找什么“性别为男”的部分
#再举一个例子
#运行之前，小伙伴们猜一下结果
xing_ming[yu_wen > 90] 
#不出意外的话，你已经学会了下表里边最难掌握的内容：
#逻辑下标
#找到逻辑值为真的位置
which(yu_wen > 90)
#显然，下边的语句与前述结果一样
xing_ming[which(yu_wen > 90)]


#将百分之成绩换算成五分制成绩
yu_wen5 <- cut(yu_wen, 
               breaks = c(0, 60, 70, 80, 90, 100))
yu_wen5 #结果是变成了若干个区间
#一个简单的cut
#居然就完成了数据预处理中一个很重要的操作：
#数据离散化，或者说，数据分箱
yu_wen5 <- cut(yu_wen, 
               breaks = c(0, 60, 70, 80, 90, 100),
               labels = c("不及格", "及格", "中", "良", "优"), #每个区间打上标签
               ordered = TRUE) 
#小伙伴们应该注意到了，除了打上标签之外
#还增加了ordered这一项
#前述的xing_bie中，因为男女平等，所以是无序因子
#但是，对于成绩而言，显然是有序的
#对因子进行排序

#每一个同学都有多科成绩
#语文
yu_wen <- c(94, 87, 92, 91, 85, 92)
#数学
shu_xue <- c(82, 94, 79, 84, 92, 82)
#外语
wai_yu <- c(96, 89, 86, 96, 82, 85)
cheng_ji3 <- c(yu_wen, shu_xue)
cheng_ji3
cheng_ji3 <- c(cheng_ji3, wai_yu)
cheng_ji3
yu_shu_wai <- matrix(cheng_ji3, ncol = 3)
yu_shu_wai
View(yu_shu_wai)
#小伙伴们看到了
#矩阵，其实就是一个二维表
#每一列，是一个变量的取值
colnames(yu_shu_wai) <- c("yu_wen", "shu_xue", "wai_yu")
rownames(yu_shu_wai) <- xing_ming
View(yu_shu_wai)
#算一算各门课的平均分
apply(yu_shu_wai, 2, mean)
#看看每个人的总成绩
apply(yu_shu_wai, 1, sum)
zong_cheng_ji <- apply(yu_shu_wai, 1, sum)

nrow(yu_shu_wai) #行数
ncol(yu_shu_wai) #列数
#增加一列总成绩
yu_shu_wai <- cbind(yu_shu_wai, zong_cheng_ji)

#看看yu_shu_wai的类
class(yu_shu_wai)
mode(yu_shu_wai) #存储的模式为数值型

#看看他的第一列
yu_shu_wai[, 1]
#这里，使用的下标，依然是方括号
#但是，因为是二维的，所以方括号里边有两部分组成
#逗号之前，是表示行；逗号之后，是表示列
yu_shu_wai[1, ]
#产看某些行、某些列
yu_shu_wai[1:2, 1:3]
#不同的行，当然是可以互换的
yu_shu_wai[6:1, ]
yu_shu_wai[, c(3, 1, 2)]
#重复某些行、列当然也可以
yu_shu_wai[c(1,3, 3, 1, 2), ]


#当然，我们现在可以将姓名、学号、性别、各科成绩放一起
cheng_ji_biao <- cbind(xing_ming, xing_bie, yu_shu_wai)
View(cheng_ji_biao)
#形式上看，和矩阵差不多
#但是，不同的列，类别是不一样的
#有的是字符向量、有的是因子、有的是数值向量
#这种情况下，应该请出R里边最重要的数据对象了：
#数据框！！！！
cheng_ji_biao <- as.data.frame(cheng_ji_biao)
#可以按照总分，对成绩表进行排序
cheng_ji_biao <- cheng_ji_biao[order(cheng_ji_biao$zong_cheng_ji), ]
View(cheng_ji_biao)
#数据框更加一般的创建方法如下
cheng_ji_biao <- data.frame(
  xing_ming = c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容"),
  xing_bie = factor(c("女", "男", "男", "女", "男","女")),
  yu_wen = c(94, 87, 92, 91, 85, 92),
  shu_xue = c(82, 94, 79, 84, 92, 82),
  wai_yu = c(96, 89, 86, 96, 82, 85))
#有了这张成绩表
#我们应该简单观察一下他的结构
str(cheng_ji_biao)
#也可以对它进行简单的统计汇总
summary(cheng_ji_biao)

#当然，绝大部分时候，我们所要分析的数据
#不会在代码里边一个一个的敲入
#按照数据挖掘的方法论
#在需求确定之后，首先会采集数据
#比如，我们要分析的成绩数据，置于以下URL之中
cheng_ji_url <-
  "https://raw.githubusercontent.com/byaxb/RDataAnalytics/master/data/cj.csv"
#我们要做的事情，是直接将其读入
cheng_ji_biao <- read.csv(cheng_ji_url,
                          head = TRUE,
                          stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8")
#或者，也可以采用下边这种方式
#会自动处理编码问题
# library(readr)
# cheng_ji_biao <- read_csv(cheng_ji_url)

View(cheng_ji_biao)
#看看他的结构
str(cheng_ji_biao)
#输出如下
# 'data.frame':	775 obs. of  13 variables:                ##表示数据框，有775条记录，每条记录13个变量/属性
#   $ 姓名    : chr  "周黎" "汤海明" "舒江辉" "翁柯" ...  ##表示姓名这一列是字符型，列出部分记录
# $ 班级    : int  1101 1101 1101 1101 1101 1101 ...      ##班级，显然应该转换为因子型
# $ 性别    : chr  "女" "男" "男" "女" ...                ##同样，性别也应该转换为因子型
# $ 语文    : int  94 87 92 91 85 92 88 81 88 94 ...      ##语文成绩，为数值型
# $ 数学    : int  82 94 79 84 92 82 72 89 77 81 ...
# $ 外语    : int  96 89 86 96 82 85 86 87 95 88 ...
# $ 政治    : int  97 95 98 93 93 91 94 97 94 91 ...
# $ 历史    : int  97 94 95 97 87 90 87 94 84 85 ...
# $ 地理    : int  98 94 96 94 88 92 88 96 94 98 ...
# $ 物理    : int  95 90 89 82 95 82 89 81 87 81 ...
# $ 化学    : int  94 90 94 90 94 98 98 88 94 88 ...
# $ 生物    : int  88 89 87 83 93 90 94 83 82 88 ...
# $ 文理分科: chr  "文科" "文科" "文科" "文科" ...        ##文理分科，同样应该转换为因子型
#看完数据结构之后，可以看看具体的取值
head(cheng_ji_biao) #控制台中输出前6行
View(head(cheng_ji_biao, n = 10)) #窗口中输出前10行
View(tail(cheng_ji_biao, n = 10)) #窗口显示后10行

#看看有多少个班级
unique(cheng_ji_biao$班级)
cheng_ji_biao$班级 <- factor(cheng_ji_biao$班级)
nlevels(cheng_ji_biao$班级)
levels(cheng_ji_biao$班级)
cheng_ji_biao$性别 <- factor(cheng_ji_biao$性别)
table(cheng_ji_biao$性别)#看得出来，女生比男生多一点
cheng_ji_biao$文理分科 <- factor(cheng_ji_biao$文理分科)
table(cheng_ji_biao$文理分科)#文理分科相对均衡

#可以看看男女生，在文理分科方面有没有倾向性
table(cheng_ji_biao$性别, cheng_ji_biao$文理分科)
#     理科 文科
# 男  238  131
# 女  143  263
#可见，文理分科，性别还是很明显的

#可以增加一列总成绩
cheng_ji_biao$总成绩 <- apply(cheng_ji_biao[, 4:12], 1, sum)
View(cheng_ji_biao)

#通过五数来查看总成绩的分布
fivenum(cheng_ji_biao$总成绩)
#0 767 801 832 885
#居然有0分的，这显然是异常点，可能该生并未参加考试
#找到这个学生
cheng_ji_biao[cheng_ji_biao$总成绩 == 0, ]
#通过箱线图，可以观察数据更为详尽的分布
#由此来查找是否有更多的离群点
boxplot(cheng_ji_biao$总成绩)
boxplot.stats(cheng_ji_biao$总成绩)
outliers <- boxplot.stats(cheng_ji_biao$总成绩)$out
#看看哪些学生离群
View(cheng_ji_biao[cheng_ji_biao$总成绩 %in% outliers, ])
#离群点的序号
outliers_idx <- which(cheng_ji_biao$总成绩 %in% outliers)

#看看不同班级，学生总成绩的分布
boxplot(总成绩~班级, 
           data = cheng_ji_biao,
           col = 1:15)
#看看文理分科总成绩的分布
boxplot(总成绩~文理分科, 
           data = cheng_ji_biao)
#从图中可以看出，理科分数相对偏高

#也可以观察不同科目的数据
#语文
min(cheng_ji_biao$语文)
max(cheng_ji_biao$语文)
range(cheng_ji_biao$语文)
diff(range(cheng_ji_biao$语文))
#前十名
head(sort(cheng_ji_biao$语文, decreasing = TRUE), n = 10)
#后十名
sort(cheng_ji_biao$语文)[1:10]
mean(cheng_ji_biao$语文)
mean(cheng_ji_biao$语文, trim = 0.2)#类似于娱乐节目里边的去掉一个最低分、去掉一个最高分
median(cheng_ji_biao$语文)
sd(cheng_ji_biao$语文)

#看看哪些科目差距最大/最小
apply(cheng_ji_biao[, 4:12], 2, sd)
#由此可以看出：
#数学成绩差别最大，标准差为10.89
#政治差别最小，标准差为5.63
#意味着什么？总分方面，数学可能拉开差距？？
#那咱们来看看，究竟哪些科目，与最后总分相关性最强
names(cheng_ji_biao)
tail(cor(cheng_ji_biao[, c(4:12, 14)]), n = 1)
tail(cor(cheng_ji_biao[-outliers_idx, c(4:12, 14)]), n = 1)
#从线性相关性来看
#政治不出意外的，最不相关，仅为0.46
#数学最相关，为0.80，生物次之，为0.79
#这其实已经是一个比较有意思的结果了
#至少佐证了，哪些是容易拉分的科目

#当然，我们能够用图形来表达其相关性，那就再好不过了
cor_val <- cor(cheng_ji_biao[-outliers_idx, c(4:12, 14)])
round(cor(cheng_ji_biao[-outliers_idx, c(4:12, 14)]), digits = 2)
symnum(cor(cheng_ji_biao[-outliers_idx, c(4:12, 14)]))

#第一次运行的话
#需要安装corrplot包
#install.packages("corrplot")
library(corrplot)
corrplot(cor_val,
         method = "color",
         diag = FALSE)

#咱们来具体看看数学成绩的分布，于政治成绩分布的对比
hist(cheng_ji_biao$数学[-outliers_idx], 
     freq = FALSE,
     ylim = c(0, 0.12))
lines(density(cheng_ji_biao$数学[-outliers_idx]), 
      col = "red",
      lwd = 2)
lines(density(cheng_ji_biao$政治[-outliers_idx]),
      col = "blue",
      lwd = 2)

#当然，还可以做各种各样的数据探索
#限于篇幅，我们直接将直接进入主题
#寻找那些因素决定了文理分科
#我们采用决策树
#来对文理分科进行判定

#对于那些异常点，我们可以先剔除掉
cheng_ji_biao <- cheng_ji_biao[-outliers_idx, ]
#先将数据分为训练集和测试集两部分
train_idx <- sample(1:nrow(cheng_ji_biao), round(nrow(cheng_ji_biao) * 0.7))
train_set <- cheng_ji_biao[train_idx, ]

#第一次运行
#需要安装rpart
#install.packages("rpart")
library(rpart)
tree_model <- rpart(文理分科 ~ ., data = cheng_ji_biao[, 4:13])
plot(tree_model,
     uniform = TRUE,
     branch = 0.8,
     margin = 0.1)
text(tree_model,
     all = TRUE,
     use.n = TRUE,
     cex = 0.7)
tree_model

#可以画得好看一点
split.fun <- function(x, labs, digits, varlen, faclen) {
  labs <- paste0(substring(labs, 1, 20), "...等")
  labs
}
library(rpart.plot)
rpart.plot(
  tree_model,
  type = 4,
  fallen = T,
  branch = .5,
  round = 0,
  leaf.round = 6,
  #clip.right.labs = T,
  cex = 0.75,
  under.cex = 0.75,
  box.palette = "GnYlRd",
  #prefix = "文理分科\n",
  branch.col = "gray",
  branch.lwd = 2,
  extra = 101,
  under = T,
  lt = " < ",
  ge = " >= ",
  split.cex = 0.85,
  split.fun = split.fun)

#输出规则
library(rattle)
asRules(tree_model)

#看看规则的准确性
predicted <- predict(tree_model, test_set, type = "class")
con_table <- table(predicted, test_set$文理分科)
sum(diag(con_table)) / sum(con_table)
#77%的准确率，十之七八，是能预测对的
#仅仅根据成绩，来进行文理分科，可能这个结果已经可以接受了


#其实这份数据，小伙伴们还可以慢慢玩味
#比如：
#1、可以对不同的课程进行聚类；
#2、可以挖出诸多关联规则
#3、甚至是可以对男女姓名进行文本挖掘统计，制作一张男女生名字的对比词云
#4、……各种可能，全在乎你的想象



#######################################################
##这，只是你对数据挖掘感兴趣的开始~
##有部分代码，并没有做过多解释。
##但相信，这些令人兴奋的结果，已经引起你的兴趣了。
##兴趣，是最重要的！！！
##The End ^-^
#######################################################
