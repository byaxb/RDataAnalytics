

#######################################################
#######################################################
##
## 名称：《R语言数据分析·基础编程》
## 作者：艾新波
## 学校：北京邮电大学
## 版本：V6
## 时间：2017年6月
##
##*****************************************************
##
## ch02_Programming Fundamentals_V6
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




#######################################################
##开场小段
##用R快速给学生提分
#######################################################
#某火星课程，同学们得分特别低
scores_low <- c(27, 36, 41, 53, 33, 34, 78, 90, 39)
#接下来的语句便开始提分了
#小伙伴们应该看出了下边这个提分公式了
#据说是钱学森钱老的大手笔哦^-^
scores_high <- floor(10*sqrt(scores_low))

scores_high <- floor(10*sqrt(scores_low))
mean(scores_low)
mean(scores_high)
scores_comparison <- data.frame(low = scores_low, high = scores_high)
View(scores_comparison)
library(ggplot2)
library(tidyr)
scores_comparison_melt <- gather(scores_comparison)
#反复提分
scores_high <- floor(10*sqrt(scores_high))
ggplot(scores_comparison_melt, 
       aes(x = key, y = value, fill = key)) +
  geom_boxplot()


#######################################################
##扩展包及其函数的查找
#######################################################
#时至今日，编程几乎是必备技能
#编程，并不只是程序员的事情
#无论你身处学术界、产业界
#无论你搞科研，还是做工程
#在绝大部分的高技术里，都需要编程
#不会编程，几乎是难以想像的
#R语言的编程，是自己的逻辑（1%）+巨人的肩膀（99%）
#所以，先应该学会安装包、查找包

#绝大部分的包都可以通过以下语句来安装
install.packages("nnet")
#有些包，没有放在CRAN
#需要通过特定渠道下载
#主要是bioconductor和github
#比如minet包
install.packages("minet")
# Warning in install.packages :
#   package ‘minet’ is not available (for R version 3.4.1)
source("http://bioconductor.org/biocLite.R")
biocLite("minet")
#从github上下载安装一些最新版本的包
# install.packages("devtools")
devtools::install_github("hadley/tidyverse")

#R不提供升级的功能
#重新安装就好
#把低版本中的library文件夹和新安装版本中的library合并
#新版本中已有文件不要覆盖
#然后运行以下代码即可
update.packages()

#打开R的帮助页面
help.start()
#打开函数c()的帮助页面
?c
?plot

#操作符也是函数
#也可以打开相应的帮助文档
?'+'
?"if"
#模糊查找包含某些字符的函数
#比如，查找一下R所支持的假设检验
apropos("test")
#当然，这里边所列出的，并不包含其他扩展包的内容
apropos("test", where = TRUE)
search()


#下边这个问答网站是强烈推荐的
#无论你学的是R还是Python
#"https://stackoverflow.com/questions/tagged/r"
#当然，你也可以在R里边，执行下边的语句直接打开浏览器
browseURL("https://stackoverflow.com/questions/tagged/r")

#推荐使用sos包
#初次使用，需要安装
#install.packages("sos")
library(sos)
#比如，查找R语言里边深度学习的相关包和函数
findFn("deep learning")


#######################################################
##一切都是对象
#######################################################
#objectName <- value

#变量命名
#不要太短的名字
#可能过了几个月，你自己都完全忘了它是什么意思
#可以由多个单词，表达完整的意思
#切记，变量命名，一定要有意义meaningful
#比如，用下划线连接
i_use_snake_case <- 1:10
#用.连接
other.people.use.periods <- seq(1, 10)
#或者是驼峰命名法
evenOthersUseCamelCase <- c(1,2,3,4,5,6,7,8,9,10)

#其它编码规范，请参阅Google's R Style Guide
browseURL("https://google.github.io/styleguide/Rguide.xml")

2 + 2     #直接显示结果
a <- 2    #赋值，但不显示结果
(a <- 2)  #既赋值，又显示结果

#既可以向左赋值
b <- 211
#也可以向右赋值
985 -> c
#
d <- "信息黄埔"  -> e
d
e
ls()


#######################################################
##控制流
#######################################################
#顺序、分支、循环
#是一切结构化编程的基本逻辑

#先看一个简单的for循环
for(me in c("Song Jiang", "Wu Yong", "Lu Junyi")) {
  cat("My name is", me, "\n")
}

#以下通过斐波那契数列
#查看不同结构的使用方法
#斐波那契数列
#for循环
nFn <- 16
Fn <- numeric(nFn)
Fn[1:2] <- c(1, 1)
for(i in 3:nFn) {
  Fn[i] <- Fn[i-2] + Fn[i-1]
}
Fn



#斐波那契数列
#while来实现
Fn <- c(1, 1)
while(sum(tail(Fn, 2)) < 1000) {
  Fn <- c(Fn, sum(tail(Fn, 2)))
}
Fn
#另外一种实现，相对繁琐
F_n_minus1 <- 1
F_n <- 1
F_n_series <- c(F_n_minus1, F_n)
F_n_plus1 <- F_n_minus1 + F_n
while(F_n_plus1 < 1000) {
  F_n_series <- c(F_n_series, F_n_plus1)
  F_n_minus1 <- F_n
  F_n <- F_n_plus1
  F_n_plus1 <- F_n_minus1 + F_n
}
F_n_series


#斐波那契数列
#repeat来实现
Fn <- c(1, 1)
repeat {
  if(sum(tail(Fn, 2)) >= 1000) {
    break
  }
  Fn <- c(Fn, sum(tail(Fn, 2)))
}
Fn
#当然，上述语句的逻辑
#与下述的while结构更加吻合
Fn <- c(1, 1)
while(TRUE) {
  if(sum(tail(Fn, 2)) >= 1000) {
    break
  }
  Fn <- c(Fn, sum(tail(Fn, 2)))
}
Fn

#小伙伴们需要注意一点：
#在上述实现过程中，用到了tail函数
#指的是倒数的某些元素，具体用法请看帮助文档
?tail

#斐波那契数列，也可以采用递归的方式实现
Fn <- function(n) {
  if(n >= 3) {
    return(c(Fn(n-1), sum(tail(Fn(n-1),2))))
  } else if(n == 2) {
    return(c(Fn(n-1),1))
  } else if(n == 1) {
    return(1)
  }
}
Fn(16)


#还有一个更简单的做法
#并且是并行的方式哦^-^
#公式的具体含义，请小伙伴自行百度
sapply(1:nFn, function(x) {
  1 / sqrt(5) * (((1 + sqrt(5)) / 2) ^ x - ((1 - sqrt(5)) / 2) ^ x)
})



#当然，在R里边
#尽量不要使用显式循环
#能向量化运算的，尽量向量化
x <- 1:10000000
y <- 2:10000001
z <- numeric(10000000)
system.time(z <- x + y, gcFirst = TRUE)
system.time({
  for(i in 1:10000000) {
    z[i] <- x[i] + y[i]
  }
},  gcFirst = TRUE)


#能并行计算的，尽量并行
#比如，要计算某复杂网络
#删除其中任意节点，对SCC size的影响
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl, cores = detectCores())
library(igraph)
#生成一个网络
ig <- sample_gnp(200, 1/200)
clusterExport(cl, 
              varlist = c("ig"),
              envir=environment()) 
#对每个节点进行某个操作
scc_mxs <- foreach(
  i = 1:vcount(ig),
  .combine = "c",
  .packages = c("foreach", "doParallel", "igraph")) %dopar% {
    tmpGraph <- delete.vertices(ig, i)
    SCC <- components(tmpGraph, mode = "strong")
    max(SCC$csize)
  }
#stop clusters
stopCluster(cl)
max(scc_mxs)


#注意分支语句的写法
p = 0.03
if(p<=0.05) {
  print("p <= 0.05!") 
  
} else {
  print("p > 0.05!")
}
#显然，下边这种写法是错的
if(p<=0.05)
  print("p <= 0.05!") 
else
  print("p > 0.05!")

#需要注意的是，若小数点后有无限多位，计算机是没法存储的
if(sqrt(2) ^ 2 == 2) {
  cat("Equal, to do something")
} else {
  cat("Unequal, to do something else")
}

#此时，可以采用下边的方式
if(all.equal(sqrt(2) ^ 2 ,2)) {
  cat("Equal, to do something")
} else {
  cat("Unequal, to do something else")
}
#all.equal函数中，可以通过设置参数tolerance
#Differences smaller than tolerance are not reported
#该参数的默认值is close to 1.5e-8



#分段函数的绘制
x <- seq(from = -10, to = 10, by = 0.01)
y <- numeric(length(x))
y[x<0] <- -1
y[x == 0] <- 0
y[x > 0] <- 1
plot(x, y, type = "l", col = "blue", lwd = 2)



#######################################################
##编写函数
#######################################################

#函数就是一个输入、处理、到输出的过程
#输入的是参数

#摄氏度（Celsius）到华氏度（Fahrenheit）的转换
ce2fa <- function(ce) { #参数ce为输入
  fa <- 1.8 * ce + 32 #对输入进行处理
  return(fa) #输出相应的值
}
ce2fa(0)#0℃相当于32℉
ce2fa(0:10)#将0~10℃转换为相应的℉
#当然，我们可以写得更加复杂一点
ce2all <- function(ce) { #参数ce为输入
  #华氏度
  #巴哈马、伯利兹、英属开曼群岛、帕劳
  #美利坚合众国及其他附属领土
  fa <- 1.8 * ce + 32
  #列氏度，德国还在使用列氏温度
  re <- 0.8 * ce
  #兰氏度
  ra <- 1.8*ce + 32 + 459.67
  #开氏度
  ke <- 273.15 + ce
  return(c(C = ce, F = fa, Re = re, Ra = ra, K = ke))
}
ce2all(0)


#位置参数和名义参数
frm <- function(names, frm = "BUPT") {
  cat(names," is frm ", frm)
}
#下边这种方式会出错
#因为有参数没有缺失值
frm()
frm("axb")#参数的缺省值
frm(frm = "BJTU", names = "AXB")

#特殊参数...
dotDemo <- function(...) {
  #捕捉到...
  dotArgs <- list(...)
  #装进list之后的...，便可以随意使用了
  #比如，将他们输出至控制台
  for(i in 1:length(dotArgs)) {
    cat("Arg", i, ": \n\t", dotArgs[[i]], "\n")
  }
}
y <- 1:9
dotDemo(y, x = 1, 2, "ok")
dotDemo(seq(1, 9, by = 3), y = letters[1:10])

#需要指出的是
#+、-、*、/binary operators
#其实都是函数

#可以自行定义一些二元操作符
#勾股定理
#a、b为直角边，c为斜边
"%ab2c%" <- function(a, b) {
  sqrt(sum(a^2, b^2))
}
3 %ab2c% 4
# [1] 5

#当我们看完了上边的%my_binary_operator%之后
#对下边的符号，也就不怕了
x <- c(17, 28, 17, 12, 15, 12, 49)
x %>% 
  unique() %>%
  sort()


#泛型函数
whoami <- function(x, ...) UseMethod("whoami")
whoami.boy <- function(x) {
  print("I am a boy")
}
whoami.girl <- function(x) {
  print("I am a girl")
}
whoami.default <- function(x) {
  print("I don't know who I am")
}
a <- 1:10
whoami(a)
class(a) <- "boy"
whoami(a)
class(a) <- "girl"
whoami(a)
attr(a, "class") <- "boy"
whoami(a)


#其实，+也是一个泛型函数
#你当然可以对它进行修改
#以下操作纯属娱（e）乐（gao）
a <- 10:100
a + 11:101
"+.onlyFirst" <- function(a, b) {
  return(a[1] + b[1])
}
class(a) <- "onlyFirst"
a + 11:101
#当你理解了上边这个泛型的+
#后续看到ggplot2中的加号
#就不会陌生了
library(ggplot2)
ggplot(data = iris, 
       aes(x = Petal.Length, 
           y = Petal.Width, 
           colour = Species)) + 
  geom_point()


#究竟有多少个+的函数
methods("+")
detach(package:ggplot2)
methods("+")
rm(list = "+.onlyFirst")
methods("+")


#代码调试
findRuns <- function(x, k) {
  n <- length(x)
  runs <- NULL
  for(i in 1:(n-k)) {
    if(all(x[i:i+k-1] == 1)) runs <- c(runs, i)
  }
  return(runs)
}
x <- c(1, 0, 0, 1, 1, 1, 0, 1, 1)
#期望的是4,5,8
findRuns(x = x, k = 2)
debugonce(findRuns)
debug(findRuns)

for(i in letters[1:3]) {
  for(j in 1:9) {
    if(j %% 2 == 0) {
      next
    } else if(j == 7) {
      break
    }
    cat("Inner loop with \tj=", j, " and i=", i, "\n")
  }
  cat("Outer loop with \ti=", i, "\n")
}

#异常处理
X <- list(1,2,"3", 4, 5)
#注意理解为何不能用c()替换list()
for(cur_x in X) {
  reciprocal <- 1/cur_x
  cat("\nThe reciprocal of", cur_x, "is", reciprocal)
}
#改用下边的方式
#也就是把可能出问题的语句，
#全都交给tryCatch()函数
for(cur_x in X) {
  tryCatch({
    reciprocal <- 1/cur_x
    cat("\nThe reciprocal of", cur_x, "is", reciprocal)
  }, #显然，函数的第一个参数就是表达式
  #表达式可能有很多，建议都用{}括起来
  error = function(e) {
    cat("\nSomething wrong while processing ", cur_x)
  })
}


#######################################################
##The End ^-^
#######################################################
