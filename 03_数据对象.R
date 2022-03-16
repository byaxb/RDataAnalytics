

# 03_数据对象 --------------------------------------------------------------

#R语言的学习 = 基础编程 + 数据对象
#本讲主要对R里边的六大类对象及其操作进行简要描述
#1、向量/因子
#2、矩阵/数组
#3、列表/数据框

#万法归宗！
#任意类型的外部数据，无论是文本、传感器信号
#还是图像、音频，或者是一般关系数据库存储的数据
#都将转换成这六种数据之一


# Constants ---------------------------------------------------------------

#查看R内置的一些常量
?Constants

#预定义的全局变量
LETTERS
letters
month.abb
month.name
pi
format(pi, digits = 17)
T #TRUE是真正的常量，而T <- FALSE
F

#数值常量
?NumericConstants
Inf <- 0
pi <- 1
rm(pi)#恢复pi为内置常量

.12
.12 == 0.12

#R中的保留字
?Reserved
#不能把一个数值赋给另外一个
6 <- 1
#也不能把TRUE赋值给FALSE
FALSE <- TRUE
#混淆是非
F <- TRUE
if (isTRUE(F)) {
    print("F is TRUE")
} else {
    print("F is FALSE")
}
if (isTRUE(F)) {
    print("F is FALSE")
}
rm(F)
if (!F) {
    print("F is FALSE")
}

#锁定某些变量，不让别人修改
fake_constant <- 1
lockBinding("fake_constant", globalenv())
fake_constant <- 2
rm(fake_constant) #清理掉伪常量


# Vector ------------------------------------------------------------------


#c()创建向量最常见的方式
#Combine Values into a Vector
#字符型向量
xm <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
xb <- c("女", "男", "男", "女", "男", "女")
#数值型向量
yw <- c(94, 87, 92, 91, 85, 92)
#逻辑型向量
xb2 <- c(F, T, TRUE, FALSE, T, F)

my_pi <- c(3, ".", 1, 4, 1, 5, 9, 2, 6) #不能有混合类型
my_pi
#> [1] "3" "." "1" "4" "1" "5" "9" "2" "6“
my_pi <- c(3, TRUE, 4, TRUE, 5, 9, 2, 6) #强制类型转换
my_pi
#[1] 3 1 4 1 5 9 2 6
c(1, 2, c(4, 3), c(1, 0)) ##不存在包含向量的向量，一律拆包
#> [1] 1 2 4 3 1 0
c(1, 2, 4, 3, 1, 0)
#> [1] 1 2 4 3 1 0



#假如事先知道长度和类型
(x1 <- vector("numeric", 8))
#> [1] 0 0 0 0 0 0 0 0
(x2 <- numeric(8))
#> [1] 0 0 0 0 0 0 0 0
(x3 <- character(8))
#>  [1] "" "" "" "" "" "" "" ""
(x4 <- vector(len = 8))
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
(x5 <- logical(8))
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE


#规则序列的产生
#等差数列
seq(from = 1, to = 20, by = 2)
#> [1]  1  3  5  7  9 11 13 15 17 19
seq(from = 20, to = 1, by = -2)
#>  [1] 20 18 16 14 12 10  8  6  4  2
seq(from = 1, to = 20, len = 10)
#> [1]  1.0  3.1  5.2  7.3  9.4 11.6 13.7 15.8 17.9 20.0
#by = (to - from) / (len - 1)

1:10#from:to,步长为1的等差数列
#> [1]  1  2  3  4  5  6  7  8  9 10
pi:1
#[1] 3.14 2.14 1.14
#注意运算符的优先级
1:10 - 1 #长度为10
# [1] 0 1 2 3 4 5 6 7 8 9
1:(10 - 1) #长度为9
#[1] 1 2 3 4 5 6 7 8 9
#不要有记忆的负担，在R里边，不要吝啬{}和()的使用

#顺便提一句，seq为泛型函数
#以本课程2012年第一次开课课程安排为例
#2012年9月3日至2013年1月6日（校历2-19周）
#9月3日（周一）第一次上课，1月6日前结束
#用下述语句可以排出一个学期的课表
seq(from = as.Date("2012-9-3"),
    to = as.Date("2013-1-6"),
    by = "weeks")
#可以采用下边的方法
#当时为18周36学时
seq(from = as.Date("2012-9-3"),
    by = "weeks",
    length = 18)

#生成重复元素的向量
rep("a", 10)
rep(c("a", "b"), c(2, 3))
rep(letters, 1:26)

#产生随机数
#产生服从某种分布的随机数
rnorm(100)#标准正态分布
rnorm(100, 2, 1) #均值为2，标准差为1
#当然，正态分布只是诸多分布的其中一种
? Distributions
#另外，请注意d/p/q/r四个前缀各自的含义

#随机抽样
sample(10)
#> [1]  6  5  8  7  2  3  4  1 10  9
sample(c("b", "u", "p", "t", "a", "x", "b"))
#> [1] "u" "x" "t" "b" "a" "p" "b"
set.seed(2012)
sample(10)
#[1]  3  7 10  9  5  6  8  4  2  1
(train_idx <- sample(1:10, 7))
#> [1]  3  6 10  5  4  1  8
(test_idx <- setdiff(1:10, train_idx))
#> [1] 2 7 9

#有放回的抽样
re_sample <- sample(1:100,
                    100,
                    replace = TRUE)
unique_re_sample <- unique(re_sample)
length(unique_re_sample)
#> [1] 62

#请小伙伴们验证一下
#从N个对象中，有放回抽取N个对象
#大约有多少是抽取不到的？
times <- 1000
num <- 10000
1 - mean(unlist(replicate(times, {
    re_sample <- sample(num, num, replace = TRUE)
    unique_re_sample <- unique(re_sample)
    length(unique_re_sample)
}))) / num
#> [1] 0.3677274
#这里的replicate函数用于重复执行某些语句
#当然也可以采用sapply来实现类似的效果，
#只不过replicate更加便捷而已


#一旦涉及到随机，每次结果都不一样
#要复现结果，尤其是在开展可重复性学术研究时，
#需要设置随机数种子，确保之后的随机动作所得结果都是一样的
set.seed(2012)
#这里的随机数种子没有什么特殊含义
#可以随便设置，这里取2012，
#只是因为该门课从2012年第一次开
sample(20)
sample(20)
sample(20)
#请注意，每次执行上述四条语句时，
#三个sample(20)，前后结果不一样，
#但每次的结果是一样的

#向量的增删改查
#以“查”最为重要
(x <- letters[1:10])
x <- c(x[1:7], x[10])
x
#追加某些元素
append(x, c("h", "i"), after = 7)
x
x <- append(x, c("h", "i"), after = 7)

#访问向量的子集
#子集的访问，应该说是数据对象里边最需要掌握的内容
#向量的子集通过[]来指定
#第1种方法：采用1~N的正整数来指定，N为向量的长度
yw <- c(94, 87, 92, 91, 85, 92)
yw[c(2, 5)]
#[1] 87 85
yw[c(2, 5)] - 90
#[1] -3 -5
yw[c(2, 5)] <- yw[c(2, 5)] + 6
yw
#[1] 94 93 92 91 91 92
yw[] <- mean(yw)
yw
#[1] 92.17 92.17 92.17 92.17 92.17 92.17
(yw <- mean(yw))
##[1] 92.17

xm <- c("周黎", "汤海明", "舒江辉")
xm[c(1, 3, 2, 3)]

#方法二：采用负整数，反向选择——排除某些元素
yw <- c(94, 87, 92, 91, 85, 92)
yw[-c(2, 5)]
#[1] 94 92 91 92
which(yw < 90)
#> [1] 2 5
idx <- which(yw < 90)
yw[-which(yw < 90)] #避免了硬代码
#[1] 94 92 91 92

#注意：which()并不是一个值得推荐的函数
#有时候可能会出一些很诡异的结果
yw[-which(yw > 100)]
#慎用which()尤其是当可能返回结果不包含任何下标时

xm <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
xm[-which(yw < 90)]
#[1] "周黎"   "舒江辉" "翁柯"   "湛容"

(yw <- yw[-which(yw < 90)])
xm <- xm[-idx]
#xm <- xm[-which(yw < 90)]
names(yw) <- xm
yw
# 周黎 舒江辉   翁柯   湛容
# 94     92     91     92

#方法三：逻辑下标
xm <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
yw <- c(94, 87, 92, 91, 85, 92)
yw < 90
#> [1] FALSE  TRUE FALSE FALSE  TRUE FALSE
yw[yw < 90]
#> [1] 87 85
xm[yw < 90]
#> [1] "汤海明" "祁强"

#小伙伴们思考一下
#为什么R会很“智能”地识别出：
#哪些同学语文成绩小于90呢？
#注意！！！！！！！！！
#其实没有那种识别过程
#yw < 90无非是一个与xm等长的逻辑向量而已
#这个逻辑向量为TRUE，对应位置的xm的元素取出来而已
#方法四：通过元素名访问相应的子集
xm <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
yw <- c(94, 87, 92, 91, 85, 92)
names(yw) <- xm
yw
# 周黎 汤海明 舒江辉   翁柯   祁强   湛容
# 94     87     92     91     85     92
yw[c("汤海明", "祁强")]
#> 汤海明   祁强
#> 87     85

#向量排序
fen_shu_xian2016 <- c(
    中国科学院大学    = 671,
    中央民族大学    = 625,
    北京大学    = 678,
    中国人民大学    = 670,
    清华大学    = 680,
    北京交通大学    = 640,
    北京科技大学    = 635,
    北京化工大学    = 620,
    北京邮电大学    = 646,
    中国农业大学    = 634,
    北京林业大学    = 621
)
sort(fen_shu_xian2016)
# 北京化工大学   北京林业大学   中央民族大学   中国农业大学
# 620            621            625            634
# 北京科技大学   北京交通大学   北京邮电大学   中国人民大学
# 635            640            646            670
# 中国科学院大学       北京大学       清华大学
# 671            678            680

print(fen_shu_xian2016,
      trim = TRUE,
      width = 3,
      justify = "right")

sort(fen_shu_xian2016, decreasing = TRUE)
order(fen_shu_xian2016, decreasing = TRUE)
#> [1]  5  3  1  4  9  6  7 10  2 11  8
fen_shu_xian2016[order(fen_shu_xian2016, decreasing = TRUE)]
#> 清华大学       北京大学 中国科学院大学   中国人民大学
#> 680            678            671            670
#> 北京邮电大学   北京交通大学   北京科技大学   中国农业大学
#> 646            640            635            634
#> 中央民族大学   北京林业大学   北京化工大学
#> 625            621            620


#倒序
yw <- c(94, 87, 92, 91, 85, 92)
sort(yw)
#> [1] 85 87 91 92 92 94
rev(yw)
#> [1] 92 85 91 92 87 94
yw[6]
#> [1] 92
yw[length(yw)]
#> [1] 92
tail(yw, n = 1)
#> [1] 92
rev(tail(yw, n = 3))
#> [1] 92 85 91
head(rev(yw), n = 3)

#向量化运算
#设张三、李四、王五合伙开店
#分别投入3200、1500和900
#现获利530，按照投入比进行分成
cheng_ben <- c(张三    = 3200,    李四    = 1500,    王五    = 900)
li_run <- cheng_ben / sum(cheng_ben) * 530
names(li_run) <- names(cheng_ben)
li_run


#以上均是向量作为一个存储容器的基本操作
#接下来看一下向量的数学运算

#原点
p0 <- c(x = 0, y = 0)
#向量1
p1 <- c(x = 1, y = 2)
#向量2
p2 <- c(x = 2, y = 1)

#求和
p3 <- p1 + p2

#数乘
p4 <- 1.5 * p3

library(ggplot2)
my_ggplot <- ggplot() +
    xlim(0, 5) +
    ylim(0, 5) +
    coord_fixed()
plot(my_ggplot)
my_ggplot <- my_ggplot +
    geom_point(aes(x = p1["x"], y = p1["y"])) +
    geom_segment(
        aes(
            x = p0["x"],
            y = p0["y"],
            xend = p1["x"],
            yend = p1["y"]
        ),
        arrow = arrow(length = unit(0.3, "cm")),
        colour = 'black'
    ) +
    geom_text(aes(x = p1["x"], y = p1["y"], label = "p1"),
              size = 4,
              vjust = -1) +
    xlab("x") +
    ylab("y")
plot(my_ggplot)

my_ggplot <- my_ggplot +
    geom_point(aes(x = p2["x"], y = p2["y"])) +
    geom_segment(
        aes(
            x = p0["x"],
            y = p0["y"],
            xend = p2["x"],
            yend = p2["y"]
        ),
        arrow = arrow(length = unit(0.3, "cm")),
        colour = 'black'
    ) +
    geom_text(aes(x = p2["x"], y = p2["y"], label = "p2"),
              size = 4,
              vjust = -1)
plot(my_ggplot)
my_ggplot <- my_ggplot +
    geom_segment(aes(
        x = p2["x"],
        y = p2["y"],
        xend = p3["x"],
        yend = p3["y"]
    ),
    linetype = 2,
    colour = 'grey') +
    geom_segment(aes(
        x = p1["x"],
        y = p1["y"],
        xend = p3["x"],
        yend = p3["y"]
    ),
    linetype = 2,
    colour = 'grey')
plot(my_ggplot)
my_ggplot <- my_ggplot +
    geom_point(aes(x = p3["x"], y = p3["y"]), colour = 'red', size = 3) +
    geom_segment(
        aes(
            x = p0["x"],
            y = p0["y"],
            xend = p3["x"],
            yend = p3["y"]
        ),
        arrow = arrow(length = unit(0.3, "cm")),
        colour = 'red'
    ) +
    geom_text(aes(x = p3["x"], y = p3["y"], label = "p3"),
              size = 4,
              vjust = -1)
plot(my_ggplot)

my_ggplot <- my_ggplot +
    geom_point(aes(x = p4["x"], y = p4["y"]), size = 3, colour = 'blue') +
    geom_segment(
        aes(
            x = p3["x"],
            y = p3["y"],
            xend = p4["x"],
            yend = p4["y"]
        ),
        linetype = 2,
        arrow = arrow(length = unit(0.4, "cm")),
        colour = 'blue'
    ) +
    geom_text(aes(x = p4["x"], y = p4["y"], label = "p4"),
              size = 4,
              vjust = -1)
plot(my_ggplot)

ggsave("p1p2p3.png", dpi = 600)
#注意：
#图片存储在"getwd()的结果/p1p2p3.png"

#投影
#向量1
p1 <- c(x = 1, y = 2)
#向量2
p2 <- c(x = 2, y = 1)
#投影
p1_on_p2 <-
    sum(p1 * p2) /
    sum(p2 * p2) * p2


p2 / sum(p2 * p2)

sqrt(sum(p2 * p2)) * sqrt(sum(p1_on_p2 * p1_on_p2))

#容易看出，求投影，完全不需要用到cos之类的
library(ggplot2)
ggplot() +
    xlim(0, 3) +
    ylim(0, 3) +
    coord_fixed() + #求投影时，必须fixed，否则垂直效果容易失真
    geom_point(aes(x = p1["x"], y = p1["y"])) +
    geom_segment(
        aes(
            x = p0["x"],
            y = p0["y"],
            xend = p1["x"],
            yend = p1["y"]
        ),
        arrow = arrow(length = unit(0.3, "cm")),
        colour = 'black'
    ) +
    geom_text(aes(x = p1["x"], y = p1["y"], label = "p1"),
              size = 4,
              vjust = -1) +
    geom_point(aes(x = p2["x"], y = p2["y"])) +
    geom_segment(
        aes(
            x = p0["x"],
            y = p0["y"],
            xend = p2["x"],
            yend = p2["y"]
        ),
        arrow = arrow(length = unit(0.3, "cm")),
        colour = 'black'
    ) +
    geom_text(aes(x = p2["x"], y = p2["y"], label = "p2"),
              size = 4,
              vjust = -1) +
    geom_point(aes(x = p1_on_p2["x"], y = p1_on_p2["y"]),
               size = 2,
               colour = "red") +
    geom_segment(aes(
        x = p0["x"],
        y = p0["y"],
        xend = p1_on_p2["x"],
        yend = p1_on_p2["y"]
    ),
    colour = 'red') +
    geom_segment(
        aes(
            x = p1["x"],
            y = p1["y"],
            xend = p1_on_p2["x"],
            yend = p1_on_p2["y"]
        ),
        linetype = 2,
        colour = 'red'
    ) +
    geom_text(
        aes(x = p1_on_p2["x"], y = p1_on_p2["y"], label = "p1_on_p2"),
        size = 4,
        vjust = 1,
        hjust = -0.2
    )
ggsave("prj.png", dpi = 600)

#向量相乘
petal_raw <- iris[, c("Petal.Length", "Petal.Width")]
petal_raw$type <- "raw"
w <- c(1, 2)
petal_multiply <- t(apply(petal_raw[, 1:2], 1, function(x) {
    w * x
}))
petal_multiply <- petal_multiply %>%
    as.data.frame() %>%
    mutate(type = "multiply")
petal <- rbind(petal_raw, petal_multiply)
ggplot(petal, aes(x = Petal.Length, y = Petal.Width)) +
    geom_point() +
    facet_wrap( ~ type)
#由此可见，向量相乘，只不过是在不同维度上缩放而已

#向量的内积
set.seed(2012)
x <- rnorm(100)
y <- rnorm(100)
sum(x * y)
#> [1] -11.1336
sum(sort(x) * sort(y))
#> [1] 128.3501
sum(sort(x) * sort(y, decreasing = T))
#> [1] -127.108
x <- sort(x)
y <- sort(y)
inner_products <- NULL
for (i in 2:99) {
    same_part_len <- rep(i, 500)
    inner_product <- replicate(500,
                               sum(x * y[c(sample(i), (i + 1):100)]))

    inner_products <- rbind(inner_products,
                            cbind(same_part_len, inner_product))
}
y <- rev(y)
for (i in 99:2) {
    same_part_len <- rep(i, 500)
    inner_product <- replicate(500,
                               sum(x * y[c(sample(i), (i + 1):100)]))

    inner_products <- rbind(inner_products,
                            cbind(same_part_len, inner_product))
}



ggplot(
    as.data.frame(inner_products),
    aes(
        x = same_part_len,
        y = inner_product,
        group = same_part_len,
        fill = same_part_len
    )
) +
    geom_boxplot() +
    xlab("顺序不相同的长度") +
    ylab("内积大小") +
    theme(legend.position = "none")


#向量是逐个相乘之后再相加
#相乘之后再相减
#x1*x2 - y1*y2
p0 <- c(0, 0)
p1 <- c(4, 3)
p2 <- c(1, 3)
p3 <- p1 + p2
diff(rev(p1) * p2)
#其实，行列式代表的都是面积或是体积
demo_points <- rbind(p0, p1, p3, p2)
point_label <- c("c(0, 0)",
                 "c(4, 3)",
                 "c(4, 6)",
                 "c(1, 3)")
ggplot(as.data.frame(demo_points),
       aes(x = demo_points[, 1], y = demo_points[, 2])) +
    geom_point() +
    geom_polygon(fill = "red",
                 alpha = 0.25,
                 colour = "black") +
    geom_label(
        aes(label = point_label),
        hjust = c(0, 0, 1, 1),
        vjust = c(1, 1, 0, 0),
        fill = "yellow"
    ) +
    xlim(0, 5.5) +
    ylim(0, 6.5) +
    coord_fixed()
diff(rev(p1) * p2)
#其实，行列式代表的都是面积或是体积


# Factor ------------------------------------------------------------------

#从连续变量和离散变量的角度看
#向量主要用来存储连续取值变量
#（向量当然可以存储任意取值的集合，包括字符、逻辑值等）
#而离散取值的变量，则用因子来存储

#比如性别：
xb <- c("女", "男", "男", "女", "男", "女")
is.vector(xb)
#[1] TRUE
typeof(xb)
#[1] "character"
xb <- factor(xb)
is.vector(xb)
#[1] FALSE
is.factor(xb)
#[1] TRUE
xb
typeof(xb)
#[1] "integer"
as.numeric(xb)
#> [1] 2 1 1 2 1 2

#取值水平
levels(xb)
#? [1] "男" "女"
#结果与下属语句相同
sort(unique(as.character(xb)))
#> [1] "男" "女"
#取值水平的个数
nlevels(xb)
#> [1] 2
table(xb)
#> xb
#> 男 女
#> 3  3

xb
#> [1] 女 男 男 女 男 女
#> Levels: 男 女

as.integer(xb)
#> [1] 2 1 1 2 1 2
#以上顺序为字符顺序，可参阅?Comparison的结果

as.character(xb)
#> [1] "女" "男" "男" "女" "男" "女"

xb == "男"
#> [1] FALSE  TRUE  TRUE FALSE  TRUE FALSE

xb == 1
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE
as.integer(xb) == 1
#> [1] FALSE  TRUE  TRUE FALSE  TRUE FALSE

levels(xb)[as.integer(xb)]
levels(xb)[xb]
#> [1] "女" "男" "男" "女" "男" "女"

xb[c(1, 4:5)]
#[1] 女 女 男
# Levels: 男 女
xb[-c(2:3, 6)]
# [1] 女 女 男
# Levels: 男 女
xm <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
yw <- c(94, 87, 92, 91, 85, 92)
xb <- c("女", "男", "男", "女", "男", "女")
xb <- factor(xb)
xb[yw > 90]
#> [1] 女 男 女 女
#> Levels: 男 女

xb[1] <- "男"
xb
# [1] 男 男 男 女 男 女
# Levels: 男 女
xb[1] <- "未知"
#> Warning message:
#>   In `[<-.factor`(`*tmp*`, 1, value = "未知") :
#>   invalid factor level, NA generated
xb <- c("女", "男", "男", "女", "男", "女")
xb <- factor(xb,
             levels = c("男", "女", "未知"))
xb
# [1] 女 男 男 女 男 女
# Levels: 男 女 未知
table(xb)
# xb
# 男   女 未知
# 3    3    0
xb[1] <- "未知" #此时可以赋值了
xb
# [1] 未知 男   男   女   男   女
# Levels: 男 女 未知

number_factors <- factor(c(10, 20, 20, 20, 10))
mean(number_factors)
#[1] NA
mean(as.numeric(number_factors))
#[1] 1.6
as.numeric(number_factors)
#[1] 1 2 2 2 1
mean(as.numeric(as.character(number_factors)))
#[1] 16
levels(number_factors)
#[1] "10" "20"
mean(as.numeric(levels(number_factors)[number_factors]))

#男女平等，xb为无序因子
#因而下述逻辑运算符没有意义
xb[1] > xb[2]
#> [1] NA
#> Warning message:
#>   In Ops.factor(xb[1], xb[2]) : ‘>’ not meaningful for factors

score <- factor(c("优", "良", "优", "优", "良", "优"),
                ordered = TRUE)
score[1] > score[2]
#> [1] TRUE

days <- factor(c("周一", "周三", "周二", "周二"),
               ordered = TRUE)
days[3] < days[2]
#> [1] TRUE
days[1] < days[3]
#> [1] FALSE
days
#> [1] 周一 周三 周二 周二
#> Levels: 周二 < 周三 < 周一

days <- factor(c("周一", "周三", "周二", "周二"),
               ordered = TRUE,
               levels = c("周一", "周二", "周三"))
days
#> [1] 周一 周三 周二 周二
#> Levels: 周一 < 周二 < 周三

days[3] < days[2]
#> [1] TRUE
days[1] < days[3]
#> [1] TRUE

#百分制成绩变为五分制成绩
yw <-  c(94, 87, 92, 91, 85, 92)
#数据分箱
yw5 <-  cut(yw,
            breaks = c(0, (6:10) * 10))
yw5
#> [1] (90,100] (80,90]  (90,100] (90,100] (80,90]  (90,100]
#> Levels: (0,60] (60,70] (70,80] (80,90] (90,100]

#百分制成绩变为五分制成绩
yw <-  c(94, 87, 92, 91, 85, 92)
#数据分箱+闭区间
yw5 <-  cut(yw,
            breaks = c(0, (6:10) * 10),
            include.lowest = TRUE)
yw5
#> [1] (90,100] (80,90]  (90,100] (90,100] (80,90]  (90,100]
#> Levels: [0,60] (60,70] (70,80] (80,90] (90,100]

#百分制成绩变为五分制成绩
yw <-  c(94, 87, 92, 91, 85, 92)
#数据分箱+闭区间+左开右闭
yw5 <-  cut(
    yw,
    breaks = c(0, (6:10) * 10),
    include.lowest = TRUE,
    right = FALSE
)
yw5
#> [1] [90,100] [80,90)  [90,100] [90,100] [80,90)  [90,100]
#> Levels: [0,60) [60,70) [70,80) [80,90) [90,100]


#百分制成绩变为五分制成绩
yw <-  c(94, 87, 92, 91, 85, 92)
#数据分箱+闭区间+左开右闭+有序因子
yw5 <-  cut(
    yw,
    breaks = c(0, (6:10) * 10),
    include.lowest = TRUE,
    right = FALSE,
    ordered_result = TRUE
)
yw5
#> [1] [90,100] [80,90)  [90,100] [90,100] [80,90)  [90,100]
#> Levels: [0,60) < [60,70) < [70,80) < [80,90) < [90,100]

#百分制成绩变为五分制成绩
yw <-  c(94, 87, 92, 91, 85, 92)
#数据分箱+闭区间+左开右闭+有序因子+标签
yw5 <-  cut(
    yw,
    breaks = c(0, (6:10) * 10),
    include.lowest = TRUE,
    right = FALSE,
    ordered_result = TRUE,
    labels = c("不及格", "及格", "中", "良", "优")
)
yw5
#> [1] 优 良 优 优 良 优
#> Levels: 不及格 < 及格 < 中 < 良 < 优


# Matrix and Array --------------------------------------------------------

#一维数据可以用向量或因子存储
#假如对多个观测对象的多个属性同时进行记录
#若这些数据是同质的，宜采用矩阵进行存储
#依然以学生成绩这份数据为例
xm <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
yw <- c(94, 87, 92, 91, 85, 92)
sx <- c(82, 94, 79, 84, 92, 82)
wy <- c(96, 89, 86, 96, 82, 85)

#语文、数学、外语三科成绩最好放一起
ysw <- matrix(c(94, 87, 92, 91, 85, 92,
                82, 94, 79, 84, 92, 82,
                96, 89, 86, 96, 82, 85),
              ncol = 3)
colnames(ysw) <- c("yw", "sx", "wy")
row.names(ysw) <- xm
View(ysw)


#假如数据本身就是“站”着的
#要注意其中byrow = 参数的设置
ysw <- matrix(
    c(94, 82, 96,
      87, 94, 89,
      92, 79, 86,
      91, 84, 96,
      85, 92, 82,
      92, 82, 85),
    byrow = TRUE,
    ncol = 3
)
colnames(ysw) <- c("yw", "sx", "wy")
row.names(ysw) <- xm


example_vector <- 1:18
example_matrix <- matrix(example_vector, ncol = 3)
View(example_matrix)

example_vector <- 1:18
example_matrix <- matrix(example_vector, ncol = 3, byrow = TRUE)
View(example_matrix)


#矩阵的基本性质
colnames(ysw)
#[1] "yw" "sx" "wy"
row.names(ysw)
#[1] "周黎"   "汤海明" "舒江辉" "翁柯"   "祁强"   "湛容"
nrow(ysw) #行数
#[1] 6
ncol(ysw) #列数
#[1] 3
dim(ysw) #行数和列数
#[1] 6 3
dimnames(ysw) #行列名称
# [[1]]
# [1] "周黎"   "汤海明" "舒江辉" "翁柯"   "祁强"   "湛容"
#
# [[2]]
# [1] "yw" "sx" "wy"

#访问矩阵的子集
#子集的访问依然是通过[]
#由于矩阵是二维的，需要','来分别指定行和列
ysw[1,] #第一个同学语文、数学、外语得分
ysw["周黎",] #同上
# yw sx wy
# 94 82 96

ysw[, 1] #语文成绩
ysw[, "yw"] #同上
# 周黎 汤海明 舒江辉   翁柯   祁强   湛容
# 94     87     92     91     85     92

ysw[1, 1] #第一个同学的第一门课得分
ysw["周黎", "yw"] #第一个同学的第一门课得分
#[1] 94

ysw["周黎", 2:3]
ysw[1, c("sx", "wy")]
# sx wy
# 82 96
ysw[1, -1]
# sx wy
# 82 96

#列重新排序
ysw[, c("sx", "yw", "wy")]
ysw[, c(2, 1, 3)]
#         sx yw  wy
#周黎     82  94  96
#汤海明   94 87 89
# 舒江辉 79 92 86
# 翁柯   84 91 96
# 祁强   92 85 82
# 湛容   82 92 85
#行进行排序
#比如，按照数学成绩进行排序
(order_sx <- order(ysw[, "sx"],
                   decreasing = TRUE))
#[1] 2 5 4 1 6 3
ysw[order_sx,]
ysw[order(ysw[, "sx"], ysw[, "wy"], decreasing = c(FALSE, TRUE)),]
# yw sx wy
# 汤海明 87 94 89
# 祁强   85 92 82
# 翁柯   91 84 96
# 周黎   94 82 96
# 湛容   92 82 85
# 舒江辉 92 79 86



#将两个矩阵摞起来，像叠罗汉一样
ysw1 <- matrix(
    c(94, 87, 92, 91, 85, 92,
      82, 94, 79, 84, 92, 82,
      96, 89, 86, 96, 82, 85),
    ncol = 3,
    dimnames = list(c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容"),
                    c("yw", "sx", "wy"))
)
ysw2 <- matrix(c(88, 81,
                 72, 89,
                 86, 87),
               ncol = 3,
               dimnames = list(c("穆伶俐", "韦永杰"),
                               c("yw", "sx", "wy")))
ysw <- rbind(ysw1, ysw2)
cjb$zz[1:8]
cjb$ls[1:8]
yu_shu_wai <- matrix()
#政治zz和历史ls成绩
zzls <- matrix(
    c(97, 97,
      95, 94,
      98, 95,
      93, 97,
      93, 87,
      91, 90,
      94, 87,
      97, 94),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
        c("周黎", "汤海明", "舒江辉", "翁柯",
          "祁强", "湛容", "穆伶俐", "韦永杰"),
        c("zz", "ls")
    )
)
#将个矩阵并列合并，像书架上的书一样
#得到成绩表cjb如下
cjb <- cbind(ysw, zzls)


#对矩阵进行操作
rowSums(cjb) #每个同学的总成绩
# 周黎 汤海明 舒江辉   翁柯   祁强   湛容 穆伶俐 韦永杰
# 466    459    450    461    439    440    427    448
colMeans(cjb) #各门课的平均分
# yw     sx     wy     zz     ls
# 88.750 84.250 88.375 94.750 92.625
#更一般的方法
apply(cjb, 1, sum)
# 周黎 汤海明 舒江辉   翁柯   祁强   湛容 穆伶俐 韦永杰
# 466    459    450    461    439    440    427    448
apply(cjb, 2, mean)
# yw    sx    wy    zz    ls
# 88.75 84.25 88.38 94.75 92.62
round(apply(cjb, 2, sd), digits = 2)
# yw   sx   wy   zz   ls
# 4.33 7.23 5.10 2.43 4.10

#可以自定义函数
coefficient_of_variation <- function(x) {
    sd(x) / mean(x)
}
apply(cjb, 2, coefficient_of_variation)
#当然，也可以采用匿名函数
round(apply(cjb, 2, function(x) {
    sd(x) / mean(x)
}), digits = 3)
# yw    sx    wy    zz    ls
# 0.049 0.086 0.058 0.026 0.044

#矩阵的妙用
#设有向量
x <- c(12, 23, 17, 48, 35, 23, 14, 39, 101)
# （1）求三个相邻元素的乘积，不滚动
# 即：12*23*17；48*35*23
# （2）求三个相邻元素的乘积，滚动
# 即：12*23*17；23*17*48
x_matrix1 <- matrix(x,
                    ncol = 3)
apply(x_matrix1, 2, prod)
x_len <- length(x)
x_matrix2 <- rbind(x[1:(x_len - 2)],
                   x[2:(x_len - 1)],
                   x[3:x_len])
apply(x_matrix2, 2, prod)


A <- matrix(c(1, 2, 3,
              2, 2, 5,
              3, 5, 1),
            ncol = 3,
            byrow = TRUE)
b <- 1:3
solve(A, b)
#[1] 1 0 0

#是否可以利用solve函数求逆矩阵
diag(3)
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1
solve(A, diag(3))
round(solve(A), digits = 2)
# [,1]  [,2]  [,3]
# [1,] -1.53  0.87  0.27
# [2,]  0.87 -0.53  0.07
# [3,]  0.27  0.07 -0.13

solve(A) %*% A
sqrt(2) ^ 2 == 2
#[1] FALSE
dplyr::near(sqrt(2) ^ 2, 2)
#[1] TRUE
all(near(solve(A) %*% A, diag(3)))
#[1] TRUE


#数组是矩阵的扩展
#矩阵是二维的，数组则可以是高维的
#比如，我们读入一个JPEG文件
#就是一个三维的数组
#download the presidents.jpg from:
jpg_url <-
    "https://raw.githubusercontent.com/byaxb/RDataAnalytics/master/data/presidents.jpg"
download.file(jpg_url,
              "presidents.jpg", mode = "wb")
library(imager)
presidents <- load.image("presidents.jpg")
str(presidents)
#> 'cimg' num [1:482, 1:345, 1, 1:3] 1 0.976 0.929 0.914 0.914

presidents <- load.image("presidents.jpg")
plot(presidents)
typeof(presidents)

presidents <- load.image("presidents.jpg")
presidents[, , 2] <- 0
presidents[, , 3] <- 0
plot(presidents)

presidents <- load.image("presidents.jpg")
presidents[, , 1] <- 0
presidents[, , 3] <- 0
plot(presidents)

presidents <- load.image("presidents.jpg")
presidents[, , 1] <- 0
presidents[, , 2] <- 0
plot(presidents)

#黄色#FFFF00
presidents <- load.image("presidents.jpg")
presidents[, , 3] <- 0
plot(presidents)

presidents <- load.image("presidents.jpg")
area_coor_x <- 350:449 #100
area_coor_y <- 110:259 #150
degree <- 0.6
array_dim <- c(length(area_coor_x),
               length(area_coor_y),
               3)
array_data <- runif(prod(array_dim))
random_noise <- array(dim = array_dim,
                      data = array_data)
presidents[area_coor_x, area_coor_y, ] <-
    (1 - degree) * presidents[area_coor_x, area_coor_y, ] +
    degree * random_noise
plot(presidents)

? imager
imager::save.image(president,
                   file = "president2.jpg")

#Windows里边，还可以使用以下函数
shell.exec("president2.jpg") # 效果一样


#感兴趣的小伙伴，可以再去探索一下其他的一些图像计算
#实现对图像平移、旋转、放大和缩小、灰度差值、加噪等


# List --------------------------------------------------------------------

#矩阵已经可以存储高维数据了
#但是，矩阵只能存储“同质”的数据
#假如要存储非同质的数据，或者是类型、长度都不一样的数据
#则需要用到列表list这种结构

#北京邮电大学下设以下学院
xue_yuan <- c(
    "信息与通信工程学院",
    "电子工程学院",
    "计算机学院",
    "自动化学院",
    "软件学院",
    "数字媒体与设计艺术学院",
    "现代邮政学院",
    "网络空间安全学院",
    "光电信息学院",
    "理学院",
    "经济管理学院",
    "人文学院",
    "马克思主义学院",
    "国际学院",
    "网络教育学院",
    "继续教育学院",
    "民族教育学院"
)
length(xue_yuan)
#拥有以下基地
ji_di <- c(国家重点实验室    = 2,
                  国家工程实验室    = 5,
                  部级重点实验室    = 9)
#校区分布
xiao_qu <- c("西土城路校区", "沙河校区", "宏福校区")
#学生数量
xue_sheng <- c(全日制     = 23000,     非全日制     = 55000)
#集合在一起
bupt <- list(
    xue_yuan = xue_yuan,
    xiao_qu = xiao_qu,
    ji_di = ji_di,
    xue_sheng = xue_sheng
)
length(bupt)
#[1] 4
names(bupt)
#[1] "xue_yuan"  "xiao_qu"   "ji_di"     "xue_sheng"
typeof(bupt)
#[1] "list"

bupt$A_xueke <- c("信息与通信工程", "计算机科学与技术", "电子科学与技术")
length(bupt)
#[1] 5
names(bupt)
#> [1] "xue_yuan"  "xiao_qu"   "ji_di"     "xue_sheng"
#> [5] "A_xueke"
bupt$A_xueke <- NULL
names(bupt)
#>[1] "xue_yuan"  "xiao_qu"   "ji_di"     "xue_sheng"

#访问列表
#通过美元$符号访问
bupt$xue_yuan[1:2]
#[1] "信息与通信工程学院" "电子工程学院"
bupt$xiao_qu
#[1] "西土城路校区" "沙河校区"     "宏福校区"
sum(bupt$ji_di)
#[1] 16

bupt$xue_sheng
#> 全日制 非全日制
#> 23000    55000

bupt$xue_sheng["全日制"]
# 全日制
# 30000
sum(bupt$xue_sheng)
#[1] 75000

#以下三种方式效果相同
bupt$xue_sheng
bupt[[4]]
bupt[["xue_sheng"]]
# 全日制 非全日制
# 30000    45000

#也可以通过[]来访问
bupt[4]
#> $`xue_sheng`
#> 全日制 非全日制
#> 30000    45000
typeof(bupt[4]) #单个的[]，看到的依然是包装箱
#> [1] "list"
bupt[[4]]
#> 全日制 非全日制
#> 30000    45000
typeof(bupt[[4]])
#> [1] "double"

sum(bupt[4])
#> Error in sum(bupt[4]) : invalid 'type' (list) of argument
#这才是正确的打开方式
sum(bupt[[4]])
#[1] 75000
#两个方括号，
#相当于进入包装箱内部了
#能看到包装箱内部了
bupt["xue_sheng"]
# $`xue_sheng`
# 全日制 非全日制
# 30000    45000
bupt[["xue_sheng"]]
# 全日制 非全日制
# 30000    45000

unlist(bupt[4])

bupt$xue_sheng
# 全日制 非全日制
# 30000    45000
bupt$xue_sheng <- c(全日制     = 23000,
                       非全日制    = 55000)
bupt$xue_sheng
# 全日制 非全日制
# 23000    55000

#可以同时获取两个部分
#也就是锁定两个仓库里的两个箱子
bupt[3:4]
#下边这种方式显然是不被允许的
bupt[[1:3]]

#对列表的每一个组成部分，执行某种操作
(component_length <- lapply(bupt, length))
# $`xue_yuan`
# [1] 17
#
# $xiao_qu
# [1] 3
#
# $ji_di
# [1] 3
#
# $xue_sheng
# [1] 2
unlist(component_length)
# xue_yuan   xiao_qu     ji_di xue_sheng
# 17         3         3         2
#可以返回一个可读性更强的结果
sapply(bupt, length)
# xue_yuan   xiao_qu     ji_di xue_sheng
# 17         3         3         2

sapply(bupt, typeof)
#> xue_yuan     xiao_qu       ji_di   xue_sheng
#> "character" "character"    "double"    "double"

# Data Frame --------------------------------------------------------------

#毫无疑问，数据框是数据分析领域最美妙的结构
#数据框本质上是一个列表，所以不同的列类别可以不一样
#但形式上，又像是矩阵，以一个二维关系表的方式呈现

xm <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
xb <- factor(c("女", "男", "男", "女", "男", "女"))
yw <- c(94, 87, 92, 91, 85, 92)
sx <- c(82, 94, 79, 84, 92, 82)
wy <- c(96, 89, 86, 96, 82, 85)
cjb <- data.frame(
    xm = xm,
    xb = xb,
    yw = yw,
    sx = sx,
    wy = wy
)
#注意比较与下述语句的区别
#cheng_ji_biao <- cbind(xing_ming, xing_bie, yu_wen, shu_xue, wai_yu)
#class(cheng_ji_biao)

str(cjb[1,])
typeof(cjb[1,])
class(cjb[1,])
str(cjb[, 1])

View(cjb) #打开成绩表
#由于数据框本质上是列表，可以通过以下三种方式访问其中的列
cjb$xm
cjb[[1]]
cjb[["xm"]]
#[1] "周黎"   "汤海明" "舒江辉" "翁柯"   "祁强"   "湛容"
#但一般来讲[[]]的用法较少，要么用$，要么像矩阵一样访问
cjb[, 1]
cjb[, 'xm']
#[1] "周黎"   "汤海明" "舒江辉" "翁柯"   "祁强"   "湛容"

cjb[1,]
# xm xb yw sx wy
# 1 周黎 女 94 82 96
cjb[c(1, 3), c("xm", "sx")]
# xm sx
# 1   周黎 82
# 3 舒江辉 79
cjb[1:3, -1]
#   xb yw sx wy
# 1 女 94 82 96
# 2 男 87 94 89
# 3 男 92 79 86

#作为列表，通过美元符号增加一列政治zz
cjb$zz <- c(97, 95, 98, 93, 93, 91)
View(cjb)
#像矩阵，cbind也可以
cjb <- cbind(cjb,
             ls = c(97, 94, 95, 97, 87, 90))

str(cjb)
#当然，绝大部分情况下
#数据不会在代码里逐字敲入
#也不会通过控制台输入
#毕竟采集数据和分析数据的过程是分开的
cjb_url <- "data/cjb.csv"
cjb <- read.csv(cjb_url,
                head = TRUE,
                stringsAsFactors = FALSE)
#注意：
#在前边的章节中，已经读取了cjb数据
#在读取完数据之后，可以直接通过以下代码将cjb存在本地以备后用
#存储：
#save(cjb, file = 'cjb.rda')
#加载：
#load('cjb.rda')
#当然，本课程为保证各章节代码的相对独立性，会多次从Github中加载数据


View(cjb)
head(cjb)
#>       xm   bj xb yw sx wy zz ls dl wl hx sw wlfk
#> 1   周黎 1101 女 94 82 96 97 97 98 95 94 88 文科
#> 2 汤海明 1101 男 87 94 89 95 94 94 90 90 89 文科
#> 3 舒江辉 1101 男 92 79 86 98 95 96 89 94 87 文科
#> 4   翁柯 1101 女 91 84 96 93 97 94 82 90 83 文科
#> 5   祁强 1101 男 85 92 82 93 87 88 95 94 93 文科
#> 6   湛容 1101 女 92 82 85 91 90 92 82 98 90 文科
tail(cjb, n = 3)
#>         xm   bj xb yw sx wy zz ls dl wl hx sw wlfk
#> 773 徐宏平 1115 男 85 59 89 80 85 82 61 64 75 理科
#> 774 昌肖峰 1115 男 81 62 76 89 76 91 49 68 74 理科
#> 775 郑慕海 1115 男 72 59 82 92 85 82 59 58 55 理科

#Compactly Display the Structure
str(cjb) #查看数据的结构
#> 'data.frame':	775 obs. of  13 variables:
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
summary(cjb) #对数据进行统计描述
# xm                  bj            xb
# Length:775         Min.   :1101   Length:775
# Class :character   1st Qu.:1104   Class :character
# Mode  :character   Median :1107   Mode  :character
#                    Mean   :1108
#                    3rd Qu.:1111
#                    Max.   :1115
# yw              sx               wy
# Min.   : 0.00   Min.   :  0.00   Min.   : 0.0
# 1st Qu.:85.00   1st Qu.: 81.00   1st Qu.:84.0
# Median :88.00   Median : 89.00   Median :88.0
# Mean   :87.27   Mean   : 86.08   Mean   :87.4
# 3rd Qu.:91.00   3rd Qu.: 95.00   3rd Qu.:92.0
# Max.   :96.00   Max.   :100.00   Max.   :99.0
# zz               ls               dl
# Min.   :  0.00   Min.   :  0.00   Min.   :  0.00
# 1st Qu.: 90.00   1st Qu.: 85.00   1st Qu.: 90.00
# Median : 93.00   Median : 90.00   Median : 94.00
# Mean   : 92.21   Mean   : 89.03   Mean   : 92.91
# 3rd Qu.: 95.00   3rd Qu.: 94.50   3rd Qu.: 96.00
# Max.   :100.00   Max.   :100.00   Max.   :100.00
# wl              hx               sw
# Min.   :  0.0   Min.   :  0.00   Min.   :  0.00
# 1st Qu.: 74.0   1st Qu.: 88.00   1st Qu.: 81.00
# Median : 83.0   Median : 94.00   Median : 88.00
# Mean   : 81.1   Mean   : 91.57   Mean   : 86.26
# 3rd Qu.: 91.0   3rd Qu.: 98.00   3rd Qu.: 93.00
# Max.   :100.0   Max.   :100.00   Max.   :100.00
# wlfk
# Length:775
# Class :character
# Mode  :character

length(cjb)
#> [1] 13
names(cjb)
#> [1] "xm"   "bj"   "xb"   "yw"   "sx"   "wy"   "zz"
#> [8] "ls"   "dl"   "wl"   "hx"   "sw"   "wlfk"
colnames(cjb) #结果同上
nrow(cjb)
#> [1] 775
ncol(cjb)
#> [1] 13
rownames(cjb)
row.names(cjb)

library(Hmisc)
describe(cjb) #对数据进行描述


#作必要的类型转换
cjb$bj <- factor(cjb$bj)
cjb$xb <- factor(cjb$xb)
cjb$wlfk <- factor(cjb$wlfk)
str(cjb)
# 'data.frame':	775 obs. of  14 variables:
#   $ xm  : chr  "周黎" "汤海明" "舒江辉" "翁柯" ...
# $ bj  : Factor w/ 15 levels "1101","1102",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ xb  : Factor w/ 2 levels "男","女": 2 1 1 2 1 2 2 1 2 2 ...
# $ yw  : int  94 87 92 91 85 92 88 81 88 94 ...
summary(cjb)
#> xm                  bj       xb
#> Length:775         1102   : 58   男:369
#> Class :character   1103   : 57   女:406
#> Mode  :character   1105   : 57
#>                    1104   : 56
#>                    1106   : 56
#>                    1107   : 55
#>                    (Other):436

cjb$zcj <- apply(cjb[, 4:12], 1, sum)
order(cjb$zcj, decreasing = TRUE)[1:5]
#> [1] 488 392 438 393 489 337
cjb_sorted <- cjb[order(cjb$zcj, decreasing = TRUE),]
View(cjb_sorted)

(top5 <- order(cjb$zcj, decreasing = TRUE)[1:5])
#[1] 488 392 438 393 489
cjb$xm[top5]
#[1] "宁琦"   "焦金音" "鲁孟秋" "伊礼贤" "傅世鸿"
cjb[top5, "xm"]
#[1] "宁琦"   "焦金音" "鲁孟秋" "伊礼贤" "傅世鸿"
cjb$zcj[top5]
#[1] 885 879 878 876 872
sort(cjb$zcj, decreasing = TRUE)[1:5]
#[1] 885 879 878 876 872

#数据集分为训练集和测试集
set.seed(2012)
n_record <- nrow(cjb)
train_idx <- sample(1:n_record, floor(n_record * 0.7))
train_idx <- sample(n_record, n_record * 0.7)
length(train_idx)
#[1] 542
test_idx <- (1:n_record)[-train_idx]
test_idx <- setdiff(1:n_record, train_idx)
all((1:n_record)[-train_idx] == setdiff(1:n_record, train_idx))
#[1] TRUE
length(test_idx)
#[1] 233

#得到测试集和训练集
train_set <- cjb[train_idx,]
test_set <- cjb[-train_idx,]
#或者
test_set <- cjb[-test_idx,]
#显然，下面这种方式是错的
train_set <- cjb[sample(n_record, n_record * 0.7),]
test_set <- cjb[sample(n_record, n_record * 0.3),]


#当然，像训练集、测试集划分这么常见的任务
#一些成熟的包里边早就实现了，
#如分类与回归的框架包caret
library(caret)
train_idx <- createDataPartition(cjb$文理分科,
                                 p = 0.7,
                                 list = FALSE)
train_set <- cjb[train_idx,]
test_set <- cjb[-train_idx,]
#在工业级/商业级代码中，我们建议不要重复造轮子
#但是在入门时，多做一些手工活还是有必要的


#以上只是阐述了六种数据对象的基本操作
#对于文件的读取、数据库操作、字符操作、日期操作、网络文件的解析等，
#均未涉及，留待小伙伴自行前去探索


# tidyverse ---------------------------------------------------------------

#在结束数据对象的探索之前
#tidyverse这个扩展包套装是强烈推荐的
#https://www.tidyverse.org/
#这个网站里边的dplyr/tidyr都是数据转换所必须掌握的包
#文件读取readr和readxl，也极大增加了文件读取的便利，避免字符编码等问题
#当然，其他的包如ggplot2等，早就改变了R的生态
#一句话：tidyverse，让R变得更美好
#以下简单演示一下tidyverse的使用

#初次使用，当然是要安装
#install.packages("tidyverse")

library(tidyverse)
cjb_url <- "data/cjb.csv"
cjb <- read.csv(cjb_url,
                stringsAsFactors = FALSE,
                encoding = "CP936")
#从https://github.com/byaxb/RDataAnalytics下载之后读取
#cjb <- readxl::read_excel("data/cjb.xlsx")

#x %>% f
#相当于f(x)
cjb %>%
    head
#即：
head(cjb)

cjb %>%
    head(n = 4) #cjb默认为第一个参数
#相当于
head(cjb, n = 4)

#选择某些列
cjb %>%
    select(xm, yw, sx) %>% #注意xm, yw, sx无需用引号括起来
    head(n = 3)

cjb %>%
    select("xm", "yw", "sx") %>% #用引号括起来当然也可以
    head(n = 3)

#选择某些列之后，对列进行重命名
#一般来讲，在编代码时不建议用中文做变量名或列名
#即将显示的时候，可以改为中文
cjb %>%
    select(xm, yw, sx) %>%
    set_names(c("姓名", "语文", "数学")) %>%
    head(n = 3)

#通过正整数来选择
cjb %>%
    select(1, 4:12) %>%
    head(n = 3)

#连续选择某些列
cjb %>%
    select(xm, yw:sw) %>% #注意：这里是列名yw:sw
    head(n = 3)

#修改某些列
#修改或是新增，都是直接赋值
cjb %>%
    mutate_at(vars(bj, xb, wlfk), factor) %>%
    mutate(zcj = rowSums(.[4:12])) %>%
    arrange(desc(zcj)) %>%
    tail(n = 2)

#上述操作只是产生临时对象，cjb本身的值未动
#采用%<>%操作符
cjb %<>%
    mutate_at(vars(bj, xb, wlfk), factor) %>%
    mutate(zcj = rowSums(.[4:12])) %>%
    arrange(desc(zcj))
cjb <- cjb %>% #和上述语句等价
    mutate_at(vars(bj, xb, wlfk), factor) %>%
    mutate(zcj = rowSums(.[4:12])) %>%
    arrange(desc(zcj))


#以下再来看看行操作
#找出语文成绩不及格的同学
cjb %>%
    dplyr::filter(yw < 60)

#找出有不及格科目的同学
cjb %>%
    filter_at(vars(4:12), any_vars(. < 60))


#按照性别进行分组统计
cjb %>%
    dplyr::filter(zcj != 0) %>%
    group_by(xb) %>%
    summarise(
        count = n(),
        max = max(zcj),
        mean = mean(zcj),
        min = min(zcj)
    )

#数据的长宽变换
#宽数据变成长数据：
#-->多列变两列
#-->一行变多行
cjb %>%
    gather(key = ke_mu, value = cheng_ji, yw:sw) %>%
    arrange(xm)


#按科目进行汇总统计
cjb %>%
    dplyr::filter(zcj != 0) %>%
    gather(key = ke_mu, value = cheng_ji, yw:sw) %>%
    group_by(ke_mu) %>%
    summarise(
        max = max(cheng_ji),
        mean = mean(cheng_ji),
        median = median(cheng_ji),
        min = min(cheng_ji)
    ) %>%
    arrange(desc(mean))



# The End ^-^ -------------------------------------------------------------
