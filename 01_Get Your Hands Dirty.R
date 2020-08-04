



# 00_Get Your Hands Dirty -------------------------------------------------


#大部分的R语言上手教程
#都先给大家介绍一些零零散散的简单操作
#本课程也不能免俗，先给大家看一些简单的语句
#经过简单的语句热身之后，再给大家讲一个完整的故事

#以下代码，请逐行手敲，不要简单的拷贝粘贴
#撸起袖子开干吧


# Basics ------------------------------------------------------------------

x <- 2 #创建一个变量x

# 一些简单的数学运算
sqrt(x) #熟悉的根号2
? sqrt #打开帮助文档，会发现sq rt = square root
x ^ 0.5 #与上一语句等价
x ^ 2 #x的平方
y <- x ^ 2 #将x的平方赋值给变量y

#圆的面积
r <- x #设圆的半径为2
area <- pi * (r ^ 2)
#上式中pi的含义相信大家都已经猜到了，为内置常数
pi
area #将面积显示出来
print(pi, digits = 11)
(circumference <- 2 * pi * r) #不仅给周长赋值，还显示出来

log2(x)#底为2的对数
log10(x)#底为10的对数
log(x)#底为e的对数
exp(1)#数学常数e
exp(x)#e^x
log(x, base = c(2, exp(1), 10))#取不同的底

#创建一个向量
x <- seq(from = -10, to = 10, by = 0.5)#从-10开始，步长为0.05，最大值不超过10
#x被重新赋值了
#注意与其他一些高级语言的区别，x并不需要事先被定义
#同时，被重新赋值之后，x便是新的数据对象
x
2 * x
min(x)
max(x)
range(x) #最大值和最小值
diff(range(x)) #滞后差分项
diff(c(1, 3, 6), lag  = 2) #滞后两期
diff(c(1, 3, 6), differences  = 2) #二阶差分

x <- seq(-pi, pi, by = pi / 100)
y <- sin(x)
plot(x, y) #绘制正弦曲线
y <- abs(y)
lines(x, y, col = "blue", type = "o") # 在原图上叠加
#积分
integrate(sin, 0, pi)
integrate(function(x) {
    abs(sin(x))
    }, -10, 10)

z <- sin(2 * x)
lines(x, z, col = "red", type = "o")

#解方程，超过800按20%扣税
#求税后是x- (x - 800) * 20% = 10000
f <- function(x, AT = 10000) {
    x - (x - 800) * 0.2 - AT
}
uniroot(f, c(10000, 15000), 10000)$root

#创建向量a
a <- c(3, 4)
#向量a的长度|a|
sqrt(sum(a ^ 2))
#两个向量之和
b <- c(2, 8)
a + b
#向量的内积<a, b>
a %*% b
drop(a %*% b) #结果作为标量
sum(a * b) #与上一语句等价


#创建一个矩阵
# 1  2
# 3 -1
A <- matrix(c(1, 2,
              3, -1),
            ncol = 2,
            byrow = TRUE)
A
#在窗格中显示A
View(A)
#矩阵的转置
t(A)
#求矩阵的逆
solve(A)
#行列式
det(A)
#特征向量与特征值
eigen(A)

#排列组合
choose(4, 2)

#解线性代数方程
#Ax = b
#x1 + 2x2 = 2
#3x1 - x2 = 4
b <- c(2, 4)
solve(A, b)
#求解A的逆矩阵
solve(A)
#等价于
solve(A, diag(2))
#这里diag(2)是一个单位阵
diag(2)#单位阵
diag(1:3) #对角阵

#排列组合
choose(4, 2)
factorial(4)#4! = 4*3*2*1
combn(c('I', 'C', 'B', 'C'), 2) #从四个元素中选两个
expand.grid(c(1, 2, 3),
            c('a', 'b', 'c')) #元素组合


#常见的集合运算
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
set.seed(2012)#设定随机数种子
#在set.seed之后，执行后续相同的代码，结果相同
idx <- 1:100
sample(idx)#打乱次序
#选取70%的数据作为训练集
train_set_idx <- sample(idx, 70)
train_set_idx <- sample(idx, length(idx) * 0.7)
train_set_idx

idx_selected <- sample(idx, 100, rep = TRUE) #有放回的抽样
idx_not_selected <- setdiff(idx, idx_selected)
length(idx_not_selected) / length(idx)
unique(idx_selected)
table(idx_selected)
sort(table(idx_selected), decreasing = TRUE)


# Storytelling: Division of Arts and Sciences -----------------------------


#前边这些简单的脚本，显然不能满足小伙伴们关于R的各种幻想
#我们学习R语言数据分析时，显然不应该再从hello world开始
#在此，我们换一个思路：通过讲一个完整的故事，
#让大家对R语言数据分析有一个直观的认识
#故事的梗概是根据各门课的成绩，进行文理分科

#类似的故事应该也发生在很多小伙伴身上
#大约在五六年前（假如您修这门课时，刚上研究生的话）
#那时候，大家都还只是高中生
#高中会根据大家的各科成绩，进行文理分科
#我们希望这个数据分析的故事不需要太多专业背景知识
#同时，又是发生在咱们自己身上的事情
#自己的事情，才会比较有感触，
#也有助于我们找到数据分析的感觉

#先从最基本的向量开始
#向量，用于存储对多个对象的某一属性进行观测所得到的结果
#比如现在有6个同学，对他们的姓名进行观测，得结果如下：
xm <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
xm
#xm是变量名称。
#本课程变量命名，要么采用拼音第一个字母，要么采用相应的英文
#如此便生成了一个长度为6的字符向量
#生成的方式很简单，就是调用c()这个函数
#Combine Values into a Vector or List
#R里边，函数的调用，就是用函数名紧接着相应的小括号
#c()这个函数的用法，请执行下述语句
?  c

#找一找第1位、第3为同学分别叫什么
xm[c(1, 3)]
#小伙伴们应该注意到了，R里边三种括号的用法
#小括号()：表示函数调用
#中括号[]：表示下标
#大括号{}：表示语句块
#下标顺序可颠倒、可重复
xm[c(1, 3, 3, 2)]
#好比辅导员喊1号、3号、3号、2号，然后相应的同学报出自己的名字

#也可以反向选择
xm[-c(1, 3)] #不包含第1、3名同学
#也可以倒过来
xm[6:1]
rev(xm) #与上一语句效果相同

#给这些同学一些学号
xh <- c(201003001,
        201003002,
        201003003,
        201003004,
        201003005,
        201003006)
#因为学号是连续的，也可以通过下边的方式生成
xh <- 201003001:201003006
#用冒号连接的，是指生成步长为1的等差数列
#生成等差数列更一般的方法
xh2 <- seq(from = 201003001,
           to = 201003006,
           by = 2)
#显然，此时的步长为2
#生成这个序列时，也是在调用函数
#调用的是seq函数，接受了三个参数
#各参数的含义，请小伙伴自行查阅文档
? seq
#至此，我们已经学会了两种向量：字符型和数值型

xb <- c("女", "男", "男", "女", "男", "女")
#性别的特殊之处在于，它的取值水平是有限的
#这种情况下，我们应该将其转换为因子
xb <- as.factor(xb)
#统计一下男、女的数量
table(xb)

#对这6个同学的语文成绩进行记录
yw <- c(94, 87, 92, 91, 85, 92)
#看看平均成绩
mean(yw)
#对他们进行排序
sort(yw)
#将高分排在前边
sort(yw, decreasing = TRUE)
#另外一个函数
order(yw) #成绩从低到高的序号
yw[order(yw)] #与sort(yw)等价

#看看同学们的排名
xm[order(yw)]
#可以将他们排成一个“张三 < 李四 < 王五”的队列么
paste(xm[order(yw)], collapse = " < ")
#paste是指将不同的字符粘在一起
#比如，创建x1~x10变量名
paste("x", 1:10, sep = "")
#再比如，将每个人的姓名与成绩粘在一起
paste(xm, '语文成绩为', yw)
cat(paste(xm, '\t语文成绩为\t', yw), sep = "\n")


#语文成绩加5分
yw + 5#结果依旧是长度为6的一个向量
yw #原向量并没有变
yw <- yw + 5 #换一种方式
yw #这个时候变了

#找找男生的成绩
xb == "男"
#[1] FALSE  TRUE  TRUE FALSE  TRUE FALSE
xm[xb == "男"]
yw[xb == "男"]
#实际上就是
yw[c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)]
#无非是一个等长的逻辑向量，将其中的TRUE部分取出来而已
#注意：千万不要以为R会去自动寻找什么“性别为男”的部分
#再举一个例子
#运行之前，小伙伴们猜一下结果
xm[yw > 90]
#不出意外的话，你已经学会了下表里边最难掌握的内容：
#逻辑下标
#找到逻辑值为真的位置
which(yw > 90)
#显然，下边的语句与前述结果一样
xm[which(yw > 90)]


#将百分之成绩换算成五分制成绩
yw5 <- cut(yw, breaks = c(0, 60, 70, 80, 90, 100))
yw5 #结果是变成了若干个区间
#一个简单的cut
#居然就完成了数据预处理中一个很重要的操作：
#数据离散化，或者说，数据分箱
yw5 <- cut(
    yw,
    breaks = c(0, seq(60, 100, by = 10)),
    include.lowest = TRUE,
    right = FALSE,
    ordered_result = TRUE,
    labels = c("不及格", "及格", "中", "良", "优")
)
yw5
#小伙伴们应该注意到了，除了打上标签之外
#还增加了ordered这一项
#前述的xing_bie中，因为男女平等，所以是无序因子
#但是，对于成绩而言，显然是有序的
#对因子进行排序

#每一个同学都有多科成绩
#语文
yw <- c(94, 87, 92, 91, 85, 92)
#数学
sx <- c(82, 94, 79, 84, 92, 82)
#外语
wy <- c(96, 89, 86, 96, 82, 85)
ysw <- c(yw, sx, wy) #长度为3*6=18的向量
ysw <- matrix(ysw, ncol = 3)
ysw
View(ysw)
#小伙伴们看到了
#矩阵，其实就是一个二维表
#每一列，是一个变量的取值
colnames(ysw) <- c("yw", "sx", "wy")
rownames(ysw) <- xm
View(ysw)
#算一算各门课的平均分
apply(ysw, 2, mean)
#看看每个人的总成绩
apply(ysw, 1, sum)
zcj <- apply(ysw, 1, sum)

nrow(ysw) #行数
ncol(ysw) #列数
#增加一列总成绩
ysw <- cbind(ysw, zcj)
View(ysw)

class(ysw)#看看ysw的类
mode(ysw) #存储的模式为数值型

#看看他的第一列
ysw[, 1]
#这里，使用的下标，依然是方括号
#但是，因为是二维的，所以方括号里边有两部分组成
#逗号之前，是表示行；逗号之后，是表示列
ysw[1, ]
#产看某些行、某些列
ysw[1:2, 1:3]
#不同的行，当然是可以互换的
ysw[6:1, ]
ysw[, c(3, 1, 2)]
#重复某些行、列当然也可以
ysw[c(1, 3, 3, 1, 2), ]


#当然，我们现在可以将姓名、学号、性别、各科成绩放一起
cjb <- data.frame(
    xm = xm,
    xb = xb,
    yw = yw,
    sx = sx,
    wy = wy
)
#形式上看，和矩阵差不多
#但是，不同的列，类别是不一样的
#有的是字符向量、有的是因子、有的是数值向量
#这种情况下，应该请出R里边最重要的数据对象了：
#数据框！！！！
#可以按照总分，对成绩表进行排序
cjb$zcj <- apply(cjb[, 3:5], 1, sum)
cjb <- cjb[order(cjb$zcj, decreasing = TRUE), ]
View(cjb)

str(cjb)
#也可以对它进行简单的统计汇总
summary(cjb)

#至此，我们已经了解了R里边四种数据对象
#向量——单变量观测值（各种类型的变量，包括连续型数值变量）
#因子——单变量观测值（离散变量，取值水平有限）
#矩阵——多变量观测值（同质）
#数据框——多变量观测值（异质）

#当然，绝大部分时候，我们所要分析的数据
#不会在代码里边一个一个的敲入
#按照数据挖掘的方法论
#在需求确定之后，首先会采集数据
#比如，我们要分析的成绩数据，置于以下URL之中
cjb_url <- "data/cjb.csv"
cjb <- read.csv(cjb_url,
                stringsAsFactors = FALSE,
                encoding = "CP936")

View(cjb)
#看看他的结构
str(cjb)
#输出如下
#>'data.frame':	775 obs. of  13 variables:  ##表示数据框，有775条记录，每条记录13个变量/属性
#>   $ xm  : chr  "周黎" "汤海明" "舒江辉" "翁柯" ...
#> $ bj  : int  1101 1101 1101 1101 1101 1101 1101 1101 1101 1101 ...
#> $ xb  : chr  "女" "男" "男" "女" ...
#> $ yw  : int  94 87 92 91 85 92 88 81 88 94 ...
#> $ sx  : int  82 94 79 84 92 82 72 89 77 81 ...
#> $ wy  : int  96 89 86 96 82 85 86 87 95 88 ...
#> $ zz  : int  97 95 98 93 93 91 94 97 94 91 ...
#> $ ls  : int  97 94 95 97 87 90 87 94 84 85 ...
#> $ dl  : int  98 94 96 94 88 92 88 96 94 98 ...
#> $ wl  : int  95 90 89 82 95 82 89 81 87 81 ...
#> $ hx  : int  94 90 94 90 94 98 98 88 94 88 ...
#> $ sw  : int  88 89 87 83 93 90 94 83 82 88 ...
#> $ wlfk: chr  "文科" "文科" "文科" "文科" ...

#看完数据结构之后，可以看看具体的取值
head(cjb) #控制台中输出前6行
View(head(cjb, n = 10)) #窗口中输出前10行
View(tail(cjb, n = 10)) #窗口显示后10行

#看看有多少个班级
unique(cjb$bj)
cjb$bj <- factor(cjb$bj) #因为班级取值水平有限，转换为因子
nlevels(cjb$bj) #因子取值水平的个数，在这里也就是班级数
levels(cjb$bj) #具体的班级名
cjb$xb <- factor(cjb$xb)
table(cjb$xb)#看得出来，女生比男生多一点
cjb$wlfk <- factor(cjb$wlfk)
table(cjb$wlfk)#文理分科相对均衡

#可以看看男女生，在文理分科方面有没有倾向性
table(cjb$xb, cjb$wlfk)
#     理科 文科
# 男  238  131
# 女  143  263
#可见，文理分科，性别还是很明显的

#可以增加一列总成绩
cjb$zcj <- apply(cjb[, 4:12], 1, sum)
View(cjb)

#通过五数来查看总成绩的分布
fivenum(cjb$zcj)
#0 767 801 832 885
#居然有0分的，这显然是异常点，可能该生并未参加考试
#找到这个学生
cjb[cjb$zcj == 0, ]
#通过箱线图，可以观察数据更为详尽的分布
#由此来查找是否有更多的离群点
boxplot(cjb$zcj)
boxplot.stats(cjb$zcj)
outliers <- boxplot.stats(cjb$zcj)$out
#看看哪些学生离群
View(cjb[cjb$zcj %in% outliers, ])
#离群点的序号
outliers_idx <- which(cjb$zcj %in% outliers)

#看看不同班级，学生总成绩的分布
boxplot(zcj ~ bj,
        data = cjb,
        col = rep(2:8, len = 15))
#看看文理分科总成绩的分布
boxplot(zcj ~ wlfk,
        data = cjb,
        col = 2:3)
#从图中可以看出，理科分数相对偏高

#也可以观察不同科目的数据
#语文
min(cjb$yw) #最低分
max(cjb$yw) #最高分
range(cjb$yw) #极差
diff(range(cjb$yw)) #效果相同
#前十名
head(sort(cjb$yw, decreasing = TRUE), n = 10)
#后十名
sort(cjb$yw)[1:10]
mean(cjb$yw) #平均分
mean(cjb$yw, trim = 0.2)#类似于娱乐节目里边的去掉一个最低分、去掉一个最高分
median(cjb$yw) #中位数
sd(cjb$yw) #标准差

#看看哪些科目差距最大/最小
sort(apply(cjb[, 4:12], 2, sd), decreasing = TRUE)
#由此可以看出：
#物理成绩差别最大，标准差为12.45
#政治差别最小，标准差为5.63
#意味着什么？总分方面，数学可能拉开差距？？
#那咱们来看看，究竟哪些科目，与最后总分相关性最强
names(cjb)
tail(cor(cjb[, c(4:12, 14)]), n = 1)
tail(cor(cjb[-outliers_idx, c(4:12, 14)]), n = 1)
#从线性相关性来看
#政治不出意外的，最不相关，仅为0.46
#数学最相关，为0.80，生物次之，为0.79
#这其实已经是一个比较有意思的结果了
#至少佐证了，哪些是容易拉分的科目

#当然，我们能够用图形来表达其相关性，那就再好不过了
cor_val <- cor(cjb[-outliers_idx, c(4:12, 14)])
round(cor(cjb[-outliers_idx, c(4:12, 14)]), digits = 2)
symnum(cor(cjb[-outliers_idx, c(4:12, 14)]))

#第一次运行的话
#需要安装corrplot包
#install.packages("corrplot")
library(corrplot)
corrplot(cor_val,
         method = "color",
         diag = FALSE)

#咱们来具体看看数学成绩的分布，于政治成绩分布的对比
hist(cjb$sx[-outliers_idx],
     freq = FALSE,
     ylim = c(0, 0.12))
lines(density(cjb$sx[-outliers_idx]),
      col = "red",
      lwd = 2)
lines(density(cjb$zz[-outliers_idx]),
      col = "blue",
      lwd = 2)

#当然，还可以做各种各样的数据探索
#限于篇幅，我们直接将直接进入主题
#寻找那些因素决定了文理分科
#我们采用决策树
#来对文理分科进行判定

#对于那些异常点，我们可以先剔除掉
cjb <- cjb[-outliers_idx, ]
#先将数据分为训练集和测试集两部分
train_idx <- sample(1:nrow(cjb),
                    round(nrow(cjb) * 0.7))
train_set <- cjb[train_idx, ]

#第一次运行
#需要安装rpart
#install.packages("rpart")
library(rpart)
tree_model <- rpart(wlfk ~ ., data = cjb[, 4:13])
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
    branch.col = "gray",
    branch.lwd = 2,
    extra = 101,
    under = T,
    lt = " < ",
    ge = " >= ",
    split.cex = 0.85
)

#输出规则
library(rattle)
asRules(tree_model)

#看看规则的准确性
test_set <- cjb[-train_idx, ]
predicted <- predict(tree_model, test_set, type = "class")
con_table <- table(predicted, test_set$wlfk)
sum(diag(con_table)) / sum(con_table)
#> [1] 0.8209607
#十之七八，是能预测对的
#仅仅根据成绩，来进行文理分科，可能这个结果已经可以接受了


#以上代码的行数并不多，但是，我们已经了解到：
#（1）R里边四种数据对象
#（2）如何读取数据
#（3）进行基本的数据预处理
#（4）开展探索性数据分析
#（5）建立分类模型，并开展模型评估和结果可视化

#当然，这只是一个非常粗浅的版本
#实际上，整门课程，都将通过这一份数据进行分析
#更多的兴奋点，且听下回分解

#这，只是你对机器学习/数据挖掘感兴趣的开始~
#有部分代码，并没有做过多解释。
#但相信，这些令人兴奋的结果，已经引起你的兴趣了。
#兴趣，是最重要的！！！

# The End ^-^ -------------------------------------------------------------
