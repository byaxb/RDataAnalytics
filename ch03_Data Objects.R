

#######################################################
#######################################################
##
## 名称：《R语言数据分析·数据对象》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V7
## 时间：2017年9月
##
##*****************************************************
##
## ch03_Data Objects_V7
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


#R语言的学习 = 基础编程 + 数据对象
#本讲主要对R里边的六大类对象及其操作进行简要描述
#1、向量/因子
#2、矩阵/数组
#3、列表/数据框
#万法归宗！
#你所接触到的任意数据，无论是文本、传感器信号
#还是图像，或者是一般关系数据库存储的数据
#都将转换成这六种数据之一

#######################################################
##常量
#######################################################
#查看R内置的一些常量
?Constants
LETTERS
letters
month.abb
month.name
pi
format(pi, digits = 17)

#数值常量
?NumericConstants
Inf <- 0
pi <- 1
.12


#R中的保留字
?Reserved
#不能把一个数值赋给另外一个
6 <- 1
#也不能把TRUE赋值给FALSE
FALSE <- TRUE
if(!F) {
  print("F is FALSE")
}
F <- TRUE
if(!F) {
  print("F is FALSE")
}
rm(list = ls())
if(!F) {
  print("F is FALSE")
}

#锁定某些变量，不让别人修改
fakeConstant <- 1
lockBinding("fakeConstant", globalenv())
fakeConstant <- 2
rm(fakeConstant) #清理掉伪常量


#######################################################
##向量
#######################################################
#数值向量
x <- c(3, 1, 4, 1, 5, 9, 2, 6)
#不要用下边的这种语句
numeric
numeric<-c(1, 3, 6, 7, 3, 8, 6, 4)
#建议在定义某个变量之前
#先看看这个变量是否存在

#字符向量
cn_provins <- c("Beijing", "Shanghai", "Jiangxi", "Shanxi")
xing_ming <- c("周黎", "汤海明", "舒江辉", 
               "翁柯", "祁强", "湛容", "穆伶俐", 
               "韦永杰", "龚兰秀", "舒亚")


#逻辑向量
is_female <- c(T, F, TRUE, FALSE, TRUE, TRUE)

#向量只能存储单一类型
#涉及多种类型时，会强制类型转换
x <- c(3, ".", 1, 4, 1, 5, 9, 2, 6)
x
#非要让他们共存的话，必须用后边讲到的list
x <- list(3, ".", 1, 4, 1, 5, 9, 2, 6)

#不存在包含向量的向量
c(c(1, 2), 1, 2, c(2, 1), c(T, F))


#规则序列的产生
#等差数列
seq(from = 1, to = 10, by = 3)#从1开始，不超过10， 步长为3
seq(from = 1, to = 10, length = 100)#从1开始，到10结束， 长度为100
#当然也可以从大到小，生成等差数列
seq(from = 5, to = -5, length = 100)#从5开始，到-5结束， 长度为100
#若步长为1，可以简写为
1:10
pi:6
6:pi
#注意运算符的优先级
1:10 - 1 #长度为10
1:(10 - 1) #长度为9
#顺便提一句，seq为泛型函数
#譬如，知道了第一次上课的时间是2017年9月13日
#17年年底前结课
#每周三上课
#用下述语句可以排出一个学期的课表
seq(from = as.Date("2017-9-13"),
    to = as.Date("2017-12-31"),
    by = "weeks")
#更准确的做法
#课程总共32学时
#也就是上16次课
#可以采用下边的方法
seq(from = as.Date("2017-9-13"),
    by = "weeks", 
    length = 16)

#生成重复元素的向量
rep("a", 10)
rep(c("a", "b"), c(2, 3))
rep(letters, 1:26)

#产生随机数
sample(20)
sample(1:100, 70)
#有放回的抽样
sample(1:100, 100, replace = TRUE)
#请小伙伴们验证一下
#从N个对象中，有放回抽取N个对象
#大约有多少是抽取不到的？

#一旦涉及到随机
#每次结果都不一样
#要复现结果，则可以设置随机数种子
set.seed(1)
sample(20)
set.seed(1)
sample(20)#与前边的结果一样
#注意，随机数种子只对下一次随机产生作用
set.seed(1)
sample(20)#与前边的结果一样
sample(20)#与前边的结果不一样

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
x[1:2] <- 0
x
x[c(1,3)] <- c(144, 169)#长度必须一致
letters[c(1,3,2,1)]
#下标可重复、顺序可变
c("a", "b", "c")[rep(c(2,1,3), 3)]
(c(1, 3, 5) + 5)[2]
x[] <- 0
x
x <- 0 #试比较与x[] <- 0的异同
#方法二：采用负整数，反向选择
letters[-(1:3)]
#方法三：逻辑下标
x <- seq(-pi, pi, by = 0.1)
y <- sin(x)
plot(x, y, type = "h")
y[y < 0] <- -y[y < 0]
lines(x, y, col = "blue", type = "h")
y[x < 0] <- -y[x < 0]
lines(x, y, col = "red", type = "h")
#小伙伴们思考一下
#为什么R会很“智能”的识别出：
#当x<0时，y取相应的值呢？
#注意！！！！！！！！！
#其实没有那种识别过程
#x < 0无非是一个与y等长的逻辑向量而已
#这个逻辑向量为TRUE，对应位置的y的元素取出来而已
z <- rnorm(50, 0, 1)
z <- append(z, NA, after = sample(50, 1))
plot(z, type = "h")
na_idx <- which(is.na(z))
z[is.na(z)] <- mean(z[!is.na(z)])
lines(na_idx, z[na_idx], 
      type = "h", 
      col = "red", 
      lwd = 2)
#方法四：通过元素名访问相应的子集
hua_wei <- c(p1 = 2012, 
             p9 = 2016,
             p10 = 2017)
hua_wei["p9"]


#向量排序
fen_shu_xian2016 <- c(
  中国科学院大学 = 671,
  中央民族大学 = 625,
  北京大学 = 678,
  中国人民大学 = 670,
  清华大学 = 680,
  北京交通大学 = 640,
  北京科技大学 = 635,
  北京化工大学 = 620,
  北京邮电大学 = 646,
  中国农业大学 = 634,
  北京林业大学 = 621)
sort(fen_shu_xian2016)
sort(fen_shu_xian2016, decreasing = TRUE)
order(fen_shu_xian2016)
fen_shu_xian2016[order(fen_shu_xian2016)]
fen_shu_xian2016[order(fen_shu_xian2016, 
                       decreasing = TRUE)]
#倒序
rev(fen_shu_xian2016)
rev(hua_wei)

#取向量的最后一个元素
#当然可以采用这种方法
fen_shu_xian2016[length(fen_shu_xian2016)]
#更好的办法是
tail(fen_shu_xian2016, n = 1)

#向量化运算
#设张三、李四、王五合伙开店
#分别投入3200、1500和900
#现获利530，按照投入比进行分成
tou_ru <-c(张三 = 3200, 李四 = 1500, 王五 = 900)
fen_cheng <- tou_ru /sum(tou_ru)*530
names(fen_cheng) <- names(tou_ru)
fen_cheng



#######################################################
##因子
#######################################################
#从连续变量和离散变量的角度看
#向量主要用来存储连续取值变量
#（向量当然可以存储任意取值的集合，包括字符、逻辑值等）
#而离散取值的变量，则用因子来存储

#比如性别：
xing_bie <- c("女", "男", "男", "女", "男",
              "女", "女", "男", "女", "女")
class(xing_bie)
mode(xing_bie)
xing_bie <- factor(xing_bie)
class(xing_bie)
mode(xing_bie)#所以，内存中的本质，是numberic
as.integer(xing_bie)
levels(xing_bie)#取值水平
nlevels(xing_bie)#取值水平的个数
table(xing_bie)
xing_bie[1]
xing_bie[c(1, 4:5, 7)]
xing_bie[-c(2:3, 6)]
xing_bie[xing_bie == "Male"] #逻辑下标
xing_bie[as.integer(xing_bie) == 2] #逻辑下标，将因子转换为整型
xing_bie[as.character(xing_bie) == "Male"] #将因子转换为字符向量
xing_bie[1] <- "男"
xing_bie[1] <- "中性" #由于不在取值范围之内，赋值有误，产生NA
#提中性Middlesex，并无其它含义
xing_bie <- factor(c("女", "男", "男", "女", "男",
                       "女", "女", "男", "女", "女"),
                   levels = c("男", "女", "中性"))
xing_bie[1] <- "中性" #此时可以赋值了


#男女平等，xing_bie为无序因子
#因而下述逻辑运算符没有意义
xing_bie[1] > xing_bie[2]

#若是五分制成绩，则应存储为有序因子
#百分之成绩
yu_wen100 <-  c(94, 87, 92, 91, 85, 92, 88, 81, 88, 94)
#转换为五分制成绩
yu_wen5 <-  cut(yu_wen100,
                breaks = c(0, (6:10)*10),
                include.lowest = TRUE, #否则，若有人得0分，则产生NA
                ordered_result = TRUE)
yu_wen5
#为不同的取值区间贴上标签
yu_wen5 <-  cut(yu_wen100,
                breaks = c(0, (6:10)*10),
                labels = c("不及格", "及格", "中", "良", "优"),
                include.lowest = TRUE, #否则，若有人得0分，则产生NA
                ordered_result = TRUE)
yu_wen5
table(yu_wen5) #注意，此时不及格/及格/中统计为0
yu_wen5[1] > yu_wen5[2]

weekdays <-factor(c('周一', "周三", "周二","周二"))
#比较运算符不支持无序因子
weekdays[3] < weekdays[1]
#变成有序因子
weekdays <-factor(c("周一", "周三", "周二","周二"),ordered=T)
weekdays[1] < weekdays[2]
weekdays[2] < weekdays[3]
weekdays <-factor(c("周一", "周三", "周二","周二"),
                  levels = c("周一", "周二", "周三"),
                  ordered=T)
weekdays[1] < weekdays[2]
weekdays[2] < weekdays[3]



#######################################################
##矩阵和数组
#######################################################
#一维数据可以用向量或因子存储
#假如对多个属性同时进行观测
#比如，通知获取语文、数学、外语成绩
yu_wen <- c(94, 87, 92, 91, 85, 92, 88, 81, 88, 94)
shu_xue <- c(82, 94, 79, 84, 92, 82, 72, 89, 77, 81)
wai_yu <- c(96, 89, 86, 96, 82, 85, 86, 87, 95, 88)
#此时适合用矩阵来进行存储
yu_shu_wai <- cbind(yu_wen, shu_xue, wai_yu)
class(yu_shu_wai)
View(yu_shu_wai)

#当然，也有可能是下边这种方式创建
yu_shu_wai <- matrix(c(94, 87, 92, 91, 85, 92, 88, 81, 88, 94,
                       82, 94, 79, 84, 92, 82, 72, 89, 77, 81,
                       96, 89, 86, 96, 82, 85, 86, 87, 95, 88),
                     ncol = 3)
colnames(yu_shu_wai) <- c("yu_wen", "shu_xue", "wai_yu")
#假如你的数据本身就是“站”着的
#要注意其中byrow = 参数的设置
yu_shu_wai <- matrix(c(94, 82, 96,
                       87, 94, 89,
                       92, 79, 86,
                       91, 84, 96,
                       85, 92, 82,
                       92, 82, 85,
                       88, 72, 86,
                       81, 89, 87,
                       88, 77, 95,
                       94, 81, 88),
                     byrow = TRUE,
                     ncol = 3)
colnames(yu_shu_wai) <- c("yu_wen", "shu_xue", "wai_yu")
#再看看另外一种创建方式
yu_shu_wai <- c(
  94, 87, 92, 91, 85, 92, 88, 81, 88, 94,
  82, 94, 79, 84, 92, 82, 72, 89, 77, 81,
  96, 89, 86, 96, 82, 85, 86, 87, 95, 88)
dim(yu_shu_wai)
dim(yu_shu_wai) <- c(10, 3)
yu_shu_wai
class(yu_shu_wai)
colnames(yu_shu_wai) <- c("yu_wen", "shu_xue", "wai_yu")
row.names(yu_shu_wai) <- c("周黎", "汤海明", "舒江辉",
                           "翁柯", "祁强", "湛容", "穆伶俐",
                           "韦永杰", "龚兰秀", "舒亚")

#访问矩阵的子集
#由于矩阵是二维的
#访问自己时，虽然依然是通过[]来指定
#但是需要通过,来分别指定行和列
yu_shu_wai[1, ] #第一个同学语文、数学、外语得分
yu_shu_wai[1, 3] #第一个同学外语得分
yu_shu_wai[1, 2:3] #第一个同学数学、外语得分
yu_shu_wai["周黎", ] #周黎同学语文、数学、外语得分
yu_shu_wai["周黎", 3] #周黎同学外语得分
yu_shu_wai["周黎", 2:3] #周黎同学数学、外语得分
yu_shu_wai[-1, ] #其他同学语文、数学、外语得分
yu_shu_wai[-1, 3] #其他同学外语得分
yu_shu_wai[-1, 2:3] #其他同学数学、外语得分
yu_shu_wai[, 1] #所有同学语文得分
yu_shu_wai[, "yu_wen"] #所有同学语文得分
yu_shu_wai[, 1:2] #所有同学语文、数学得分
yu_shu_wai[, c("yu_wen", "shu_xue")] #所有同学语文、数学得分
#当然也可以重新排序
#比如列重新排序
yu_shu_wai[, c("shu_xue", "yu_wen", "wai_yu")] #所有同学数学、语文、外语得分
yu_shu_wai[, c(2, 1, 3)] #所有同学数学、语文、外语得分
#当然也可以对行进行排序
#比如，按照数学成绩进行排序
shu_xue_pai_ming <- order(yu_shu_wai[, "shu_xue"], decreasing = TRUE)
yu_shu_wai[shu_xue_pai_ming, ] #所有按数学成绩从高到低排名

#矩阵的基本性质
nrow(yu_shu_wai) #行数
ncol(yu_shu_wai) #列数
dim(yu_shu_wai) #行数和列数
dimnames(yu_shu_wai) #行列名称

#将两个矩阵摞起来，像叠罗汉一样
yu_shu_wai <- rbind(yu_shu_wai[1:5, ], yu_shu_wai[6:10, ])
#将个矩阵并列合并，像书架上的书一样
#增加政治zheng和历史shi
zheng_shi <- matrix(c(97, 95, 98, 93, 93, 91, 94, 97, 94, 91,
                      97, 94, 95, 97, 87, 90, 87, 94, 84, 85),
                    ncol = 2)
colnames(zheng_shi) <- c("zheng_zhi", "li_shi")
#五科wu_ke成绩为:
wu_ke <- cbind(yu_shu_wai, zheng_shi)
View(wu_ke)

#对矩阵进行转置
t(wu_ke)
View(wu_ke)
View(t(wu_ke))

#对矩阵进行操作
rowSums(wu_ke) #每个同学的总分
colMeans(wu_ke) #各门课的平均分
#更一般的方法
apply(wu_ke, 1, sum)
apply(wu_ke, 1, max)
apply(wu_ke, 2, mean)
apply(wu_ke, 2, sd)

#可以自定义函数
#比如自定义变异系数Coefficient of Variation
cv <- function(x) {
  sd(x) / mean(x)
}
apply(wu_ke, 2, cv)

#当然，也可以采用匿名函数
apply(wu_ke, 2, function(x) { #求极差
  max(x) - min(x) #说白了，这里的x，就是每一列，或者说每一个列向量
})


#数组是矩阵的扩展
#矩阵是二维的，数组则可以是高维的
#比如，我们读入一个JPEG文件
#就是一个三维的数组
library(jpeg)
#download the presidents.jpg from:
#https://raw.githubusercontent.com/byaxb/RDataAnalytics/master/data/presidents.jpg
jpg_url <- "https://raw.githubusercontent.com/byaxb/RDataAnalytics/master/data/presidents.jpg"
download.file(jpg_url, "presidents.jpg", mode="wb")
shell("presidents.jpg")
president3 <- readJPEG("presidents.jpg")
president3[ , , 3] <- 0
writeJPEG(president3, target = "presidents3_changed.jpg")
shell("presidents3_changed.jpg") #照片已泛黄
#当然，你也可以不用shell()函数，直接在在资源管理器中
#打开getwd()所得到的路径，然后查看相应的jpeg文件
#Windows里边，还可以使用以下函数
shell.exec("presidents3_changed.jpg") # 效果一样


#######################################################
##列表
#######################################################
#矩阵已经可以存储高维数据了
#但是，矩阵只能存储“同质”的数据
#假如要存储非同质的数据，或者是类型、长度都不一样的数据
#则需要用到列表list这种结构

#比如：
#北京邮电大学下设以下学院
xue_yuan <- c("信息与通信工程学院",
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
              "民族教育学院")
ji_di <- c(国家重点实验室 = 2,
                  国家工程实验室 = 5,
                  部级重点实验室 = 9)
xiao_qu <- c("西土城路校区",
             "沙河校区",
             "宏福校区")
xue_sheng <- c(全日制 = 30000, 非全日制 = 45000)
#将上述这些零零散散的东东
#全都变成一个整体
#就是一个list了
bupt <- list(xue_yuan = xue_yuan,
             xiao_qu = xiao_qu,
             ji_di = ji_di,
             xue_sheng = xue_sheng)

#访问列表
#通过美元$符号访问
bupt$xue_yuan
bupt$xue_yuan[1:2]
bupt$xiao_qu
bupt$ji_di
bupt$xue_sheng
bupt$xue_sheng["全日制"]

#也可以通过[]来访问
#注意，列表的组成部分，依旧是列表
#也就是说，进入这个仓库，不管之前是什么样子，
#总得在加一个包装箱
#一个[]，看到的依然是包装箱
bupt[1] 
class(bupt[1])
#因此，下边的语句会出错
sum(bupt[4])
#这才是正确的打开方式
sum(bupt[[4]])
#两个方括号，
#相当于进入包装箱内部了
#能看到包装箱内部了
bupt[[1]] #等价于bupt$xue_yuan
class(bupt[[1]]) #此时才是
bupt[[1]][1]
#通过名称访问，
#过程与前述一般不二
bupt["xue_yuan"]
bupt[["xue_yuan"]][1]

#可以同时获取两个部分
#也就是锁定两个仓库里的两个箱子
bupt[1:3]
#下边这种方式显然是不被允许的
bupt[[1:3]]

#对列表的每一个组成部分，执行某种操作
#比如：
lapply(bupt, length)
#可以返回一个可读性更强的结果
sapply(bupt, length)



#######################################################
##数据框
#######################################################
#毫无疑问，数据框是数据分析领域最美妙的结构
#数据框本质上是一个列表，所以不同的列类别可以不一样
#但形式上，又像是矩阵，以一个二维关系表的方式呈现

xing_ming <- c("周黎", "汤海明", "舒江辉", 
               "翁柯", "祁强", "湛容", "穆伶俐", 
               "韦永杰", "龚兰秀", "舒亚")
xing_bie <- factor(c("女", "男", "男", "女", "男",
                     "女", "女", "男", "女", "女"))
yu_wen <- c(94, 87, 92, 91, 85, 92, 88, 81, 88, 94)
shu_xue <- c(82, 94, 79, 84, 92, 82, 72, 89, 77, 81)
wai_yu <- c(96, 89, 86, 96, 82, 85, 86, 87, 95, 88)
cheng_ji_biao <- data.frame(xing_ming = xing_ming,
                            xing_bie = xing_bie,
                            yu_wen = yu_wen,
                            shu_xue = shu_xue,
                            wai_yu = wai_yu)
#注意比较与下述语句的区别
#cheng_ji_biao <- cbind(xing_ming, xing_bie, yu_wen, shu_xue, wai_yu)
#class(cheng_ji_biao)

str(cheng_ji_biao) #查看数据的结构
View(cheng_ji_biao) #打开成绩表
summary(cheng_ji_biao) #对数据进行统计描述
library(Hmisc)
describe(cheng_ji_biao) #对数据进行描述


#当然，绝大部分情况下
#数据不会在代码里逐字敲入
#也不会通过控制台输入
#毕竟采集数据和分析数据的过程是分开的

cheng_ji_url <-
  "https://raw.githubusercontent.com/byaxb/RDataAnalytics/master/data/cj.csv"
cheng_ji_biao <- read.csv(cheng_ji_url,
                          head = TRUE,
                          stringsAsFactors = FALSE)
str(cheng_ji_biao)
#作必要的类型转换
cheng_ji_biao$班级 <- factor(cheng_ji_biao$班级)
cheng_ji_biao$性别 <- factor(cheng_ji_biao$性别)
cheng_ji_biao$文理分科 <- factor(cheng_ji_biao$文理分科)

#由于数据框本质上是列表，形式上是矩阵
#访问数据框子集的方式，也可以是以下方式
#像列表一样，通过$来访问
cheng_ji_biao$姓名 #字符向量
cheng_ji_biao$性别 #因子
cheng_ji_biao$语文 #数值向量

#也可以像matrix一样，通过[]来访问
cheng_ji_biao[, "语文"]
cheng_ji_biao[, c("语文", "数学", "外语")]
cheng_ji_biao[, c(1, 6:4)]
cheng_ji_biao[, -(2:3)]
cheng_ji_biao[, c(1, 13)]

#查看数据框的前n行
head(cheng_ji_biao, n = 10)
View(head(cheng_ji_biao, n = 10))
#查看数据框的后n行
tail(cheng_ji_biao, n = 10)
View(tail(cheng_ji_biao, n = 10))

#给数据框增加一列
cheng_ji_biao$总分 <- apply(cheng_ji_biao[, 4:12], 
                          1, sum)

#删除数据框的某一列
xing_bie_bak <- cheng_ji_biao$性别
cheng_ji_biao$性别 <- NULL
View(cheng_ji_biao)
cheng_ji_biao$性别 <- xing_bie_bak
ncol(cheng_ji_biao)
cheng_ji_biao <- cheng_ji_biao[, c(1:2, 14, 3:13)]
View(cheng_ji_biao)


#数据集的排序
#根据总分，进行排序
zong_fen_pai_xu <- order(cheng_ji_biao$总分, decreasing = TRUE)
cheng_ji_biao_ordered <- cheng_ji_biao[zong_fen_pai_xu, ]

#数据集分为训练集和测试集
train_idx <- sample(1:nrow(cheng_ji_biao), nrow(cheng_ji_biao) * 0.7)
train_set <- cheng_ji_biao[train_idx, ]
test_set <- cheng_ji_biao[-train_idx, ]
#当然，假如采用caret等包的话，也有专门的分割函数
library(caret)
train_idx <- createDataPartition(cheng_ji_biao$文理分科, p = 0.7)
train_set <- cheng_ji_biao[train_idx, ]
test_set <- cheng_ji_biao[-train_idx, ]


#以上只是阐述了六种数据对象的基本操作
#对于文件的读取、数据库操作、字符操作、日期操作、网络文件的解析等，
#均未涉及，留待小伙伴自行前去探索

#关于数据这一块，有几个网站是强烈推荐的

#首先推荐的是：http://tidyverse.org/
#这个网站里边的dplyr/tidyr都是数据转换所必须掌握的包
#文件读取readr和readxl，也极大增加了文件读取的便利，避免字符编码等问题
#当然，其他的包如ggplot2等，早就改变了R的生态
#一句话：http://tidyverse.org/让R变得更美好

#其次推荐的是https://github.com/Rdatatable/data.table/wiki
#说到数据处理，data.table这个包也是要强烈推荐的
#尤其涉及到大规模的data.frame时，data.table的速度优势尤为明显
#这里说的大规模，比如记录数过千万
#或者说，体量为10-100 Gb类似的数据
#当然data.table本身就像SQL查询一样，很多操作较之data.frame更为便利


#还是回到之前的那句话：这只是一个开始，广阔的天地，大有可为~！



#######################################################
##The End ^-^
#######################################################
