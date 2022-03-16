

# 02_基础编程 --------------------------------------------------------------

#学习R语言数据分析，或是其它数据科学编程
#往往学习曲线都比较陡
#因为既要学习语言本身，还需要掌握机器学习/数据挖掘的模型和算法

#本课程的策略是分而治之：
#先讲R语言，
#再以R语言为工具，讲授机器学习/数据挖掘的模型和算法

#单就R语言的学习来说，无非掌握两个方面：
#R语言 = 基础编程 + 数据对象
#这也体现了作为数据分析语言的特殊之处
#在掌握一些基本的语法、逻辑控制之后
#R语言的核心在于数据对象及其操作
#或者说，R语言会比一般的编程语言更多关注数据对象本身
#02_基础编程
#03_数据对象


#先看R的基础编程
#考虑到大家已经Get Hands Dirty了
#接下来，我们从R语言本身的特点讲起
#力图用最简单语言，把R基础编程需要掌握的核心要点
#给大家做一个交待

#时至今日，编程几乎是必备技能
#编程，并不只是程序员的事情
#无论你身处学术界、产业界
#无论你搞科研，还是做工程
#在绝大部分的高技术里，都需要编程
#不会编程，几乎是难以想像的

#R编程最大的特点，就是：
#用别人的包和函数，讲述你自己的故事！
#当然，你有新的idea，可以写新的算法或是改进已有算法
#即便如此，你也需要大量使用既有函数
#作为一个数据分析人员，编写代码，早就不应该是从零开始了
#而是站在巨人的肩膀上
#尽管，很多小伙伴觉得从零开始更能满足自己的控制欲

#所以，先应该学会安装包、查找包
#查找包的方法，请参照讲义内容


# Add-on Packages ---------------------------------------------------------

#对于Windows用户，推荐将扩展包单独安装在D:盘
#这样不会污染R系统本身的文件
#此时注意设置好环境变量R_LIBS_USER，
#会直接将我们私有的包安装在指定位置
#并且每次重装R之后，自己用到的扩展包也无需重新安装
#即便重装系统，也不受影响
#（如果是R大版本升级时，比如从R 3.X.X升级到R 4.X.X，可能有不少扩展包需要重装）

#绝大部分的包都可以通过以下语句来安装
#比如安装神经网络的包nnet
install.packages("nnet")
#有些包，没有放在CRAN
#需要通过特定渠道下载
#比如github、bioconductor等

#从github上下载安装一些最新版本的包
#install.packages("devtools")
devtools::install_github("hadley/tidyverse")


#有些情况下，也需要将包文件download到本地，然后再安装
#比如你有一台机子数据保密很严格，不能上网
#此时能做的事情就是先下载到本地，然后拷贝文件安装
#方法如下：
# install.packages("maptree_1.4-7.tar.gz",
#                  repos = NULL,
#                  type = "source")
#这里边涉及到包之间的依存关系，感兴趣的小伙伴自行思考解决


#安装完包之后，就是加载使用了
#这和一次性安装完word/excel/powerpoint，
#然后可以反反复复使用，是一个道理
library(tidyverse)
#或者
require(tidyverse)
#以上两个语句几乎完全相同，只不过是后者有一个返回值TRUE/FALSE
#代表是否加载成功
#这也就不难理解有些代码通过require来判断能否加载包，
#若不能加载，则通过install.packages()语句进行安装

#若有多个包，可以通过以下方式一次性加载
my_libs <- c("igraph", "infotheo")
sapply(my_libs, require, character.only = TRUE)

#也可以通过pacman扩展包进行包的管理和加载
library(pacman)
p_load(igraph, infotheo)

#当然，可能也有小伙伴希望自己管理一个迷你CRAN
#library(miniCRAN)
#这样可以在离线状态下，比如物理隔离的某些工作站上，安装扩展包


# Help --------------------------------------------------------------------

# StackOverflow当然是值得关注的


#打开R的帮助页面
help.start()
#进入该页面之后，点击其中的packages，可以查看已安装所有包的帮助文档

#打开函数c()的帮助页面
?c
?plot

#操作符也是函数
#也可以打开相应的帮助文档
?'+'
?"if"
?`if`
#注意以上单引号、双引号、反单引号的用法
#反单引号也别称为重音符，和波浪号~同一个键位
?'plot' #对于一般的函数，通过引号引起来，当然也可以
?'c'

#模糊查找包含某些字符的函数
#比如，查找一下R所支持的假设检验
apropos("test")
#当然，这里边所列出的，并不包含其他扩展包的内容
apropos("test", where = TRUE)
#以下这个函数列出当前搜索的范围
search()


#stackoverflow网站是强烈推荐的
#无论你学的是R还是Python
#"https://stackoverflow.com/questions/tagged/r"
#当然，你也可以在R里边，执行下边的语句直接打开浏览器
browseURL("https://stackoverflow.com/questions/tagged/r")
#如果自行打开stackoverflow搜索，注意加上[r]
#当然，如果是搜索其他相关主题，如ggplot2，也可以直接加上[ggplot2]之类的

#推荐使用sos包
#初次使用，需要安装
#install.packages("sos")
library(sos)
#比如，查找R语言里边深度学习的相关包和函数
findFn("deep learning")


# Task Views --------------------------------------------------------------

#毫无疑问，TASK VIEWS是最正统的
#机器学习相关主题
browseURL("https://CRAN.R-project.org/view=MachineLearning")
#聚类分析相关主题
browseURL("https://cran.r-project.org/web/views/Cluster.html")
#自然语言处理
browseURL("https://cran.r-project.org/web/views/NaturalLanguageProcessing.html")
#高性能计算相关主题
browseURL("https://cran.r-project.org/web/views/HighPerformanceComputing.html")
#模型部署相关主题
browseURL("https://cran.r-project.org/web/views/ModelDeployment.html")
#互联网相关主题
browseURL("https://cran.r-project.org/web/views/WebTechnologies.html")


# More Packages for ML and DM ---------------------------------------------

#机器学习/数据挖掘相关的一些扩展包
browseURL("https://github.com/thedataincubator/data-science-blogs/blob/master/top-r-packages.md")
browseURL("https://www.r-pkg.org/starred")


# Updates -----------------------------------------------------------------


#R的升级及包的更新
#R不支持升级，多个版本可以共存
#在更新之后，RStudio一般会自动关联
#当然，也可以手动在Tools >> Global Options中手动设置

# R Versions --------------------------------------------------------------

#有些小伙伴对于不同版本R更新周期比较感兴趣
#为了满足大家的好奇心，以下给出具体的实现代码
#这些代码属于插播的内容，初次阅读会有一定难度
#R不同版本的网址
r_htmls <- paste0("https://cran.r-project.org/src/base/R-", 0:3)
#加载爬虫工具
library(rvest)
r_version_info <- NULL
for (cur_r_html in r_htmls) {
    html_content <- read_html (cur_r_html)
    target_name <- "table , th:nth-child(1), th a, th:nth-child(2)"
    #这个target_name，是通过SelectorGadget获取的
    r_version_info <- rbind(r_version_info,
                            html_content %>% html_node(target_name) %>% html_table)
}
View(r_version_info)
library(tidyverse)
library(ggplot2)
r_version_info %>%
    as.data.frame() %>%
    select(2:4) %>%
    set_names(c("name", "last_modified", "size")) %>%
    filter(gregexpr("R-", name) != -1) %>%
    mutate(type = substring(name, 1, 3),
           last_modified = as.POSIXct(last_modified)) %>%
    mutate(days_ellpased = c(NA, round(
        difftime(last_modified[2:(length(last_modified))],
                 last_modified[1:(length(last_modified) - 1)],
                 units = "days"),
        digits = 2
    ))) %>%
    ggplot(aes(x = type, y = days_ellpased, fill = type)) +
    geom_boxplot()
#通过这个图，可以看出R.0/R.1/R.2/R.3不同版本的更新周期
#当然，我们也可以计算一下具体的数值
r_version_info %>%
    select(2:4) %>%
    set_names(c("name", "last_modified", "size")) %>%
    filter(gregexpr("R-", name) != -1) %>%
    mutate(type = substring(name, 1, 3),
           last_modified = as.POSIXct(last_modified)) %>%
    mutate(days_ellpased = c(NA, round(
        difftime(last_modified[2:(length(last_modified))],
                 last_modified[1:(length(last_modified) - 1)],
                 units = "days"),
        digits = 2
    ))) %>%
    group_by(type) %>%
    summarise(
        mean = mean(days_ellpased, na.rm = TRUE),
        median = median(days_ellpased, na.rm = TRUE)
    )
#具体数值如下
# # A tibble: 4 x 3
# type   mean median
# <chr> <dbl>  <dbl>
# 1 R-0    39.3   41.5
# 2 R-1    55.0   57
# 3 R-2    77.4   70.2
# 4 R-3    77.2   70.0
#换句话说，大概每过2个月，基本上就应该更新一下你的R



# Objects -----------------------------------------------------------------

#object_name <- value

#变量命名
#不要太短的名字
#可能过了几个月，你自己都完全忘了它是什么意思
#可以由多个单词，表达完整的意思
#切记，变量命名，一定要有意义meaningful
#比如，用下划线连接
use_snake_case <- 1:10
#用.连接
use.periods <- seq(1, 10)
#或者是驼峰命名法
useCamelCase <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

#推荐用第一种命名方法
#另外，在R中，点号.一般情况下可以当做普通字符对待，以下两种情况是例外：
#（1）以.开头的变量，一般是隐藏变量
#（2）泛型函数，通过.来匹配、分发具体类型的函数

#其它编码规范，请参阅Google's R Style Guide
browseURL("https://google.github.io/styleguide/Rguide.xml")

2 + 2     #直接显示结果
a <- 2    #赋值，但不显示结果
(a <- 2)  #既赋值，又显示结果

#既可以向左赋值
b <- 211
#也可以向右赋值
985 -> c
#这种赋值情况看似比较少见，不过下边这种情况下也比较常见：
#x %>% g() %>% f() -> new_x
#相当于new_x <- f(g(x))
#也即是经过一系列操作之后，形成一个新的对象

d <- "信息黄埔"  -> e
d
e #注意，R中并没有常量e，不过可以用函数exp(1)来代替

.x <- "not to show"
ls() #不包括隐藏对象
ls(all.names = TRUE) #包括隐藏对象

# Control Flow ------------------------------------------------------------


#从最基本的程序结构说起
#顺序、分支、循环
#是一切结构化编程的基本逻辑

# Sequence Structure ------------------------------------------------------

#顺序结构
#定义3个向量
yw <- c(94, 87, 92, 91, 85, 92) #6个同学的语文成绩
sx <- c(82, 94, 79, 84, 92, 82) #数学成绩
wy <- c(96, 89, 86, 96, 82, 85) #外语成绩
ysw <- yw + sx + wy #向量化操作：三科成绩相加
ysw #显示三科成绩
#> [1] 274 272 259 273 261 261
(yw <- yw + 2) #向量化操作：每个同学语文成绩加2分
#> [1] 96 89 94 93 87 94


(mean_score <- mean(yw)) #求语文平均分
#> [1] 92.16667
sd(yw) #求语文成绩标准差
(sd_score <- (1 / (6 - 1) * sum((yw - mean_score) ^ 2)) ^ 0.5)
#> [1] 3.430258
c(sd(yw), sd(sx), sd(wy))
#> [1] 3.430258 6.058052 5.865151
(z_score_yw <- (yw - mean_score) / sd_score) #求标准得分
#> [1]  1.12 -0.92  0.53  0.24 -1.51  0.53

show(yw) #显示语文成绩
#> [1] 96 89 94 93 87 94
show(sx) #显示数学成绩
#> [1] 82 94 79 84 92 82
yw >= 90 #向量化操作：逻辑判断
#> [1]  TRUE FALSE  TRUE  TRUE FALSE  TRUE
yw >= 85 & sx >= 85 #向量化操作：逻辑判断
#> [1] FALSE  TRUE FALSE FALSE  TRUE FALSE
yw >= 95 | sx >= 95 #向量化操作：逻辑判断
#> [1]  TRUE FALSE FALSE FALSE FALSE FALSE

round(z_score_yw, digits = 3) #小数点后三位
#> [1]  1.118 -0.923  0.534  0.243 -1.506  0.534
sqrt(c(1, 4, 9)) #开根号
#> [1] 1 2 3
">="(yw, 90) #逻辑判断
#[1]  TRUE FALSE  TRUE  TRUE FALSE  TRUE
yw + 5 #求和
#> [1] 101  94  99  98  92  99
"+"(yw, 5) #运算符作为特殊的函数
#> [1] 101  94  99  98  92  99




# Decision Structures -----------------------------------------------------

#分支结构
min_score <- min(yw)
if (min_score >= 90) {
    message("语文成绩全部为优")
} else if (min_score >= 80) {
    message("语文成绩至少为良")
} else {
    message("并非所有同学语文成绩均为优良")
}


#all()与any()
yw >= 90
#> [1]  TRUE FALSE  TRUE  TRUE FALSE  TRUE
all(yw >= 90) #逻辑向量每一个值均为TRUE时，返回TRUE；否则返回FALSE
#> [1] FALSE
if (all(yw >= 90)) {
    message("语文成绩全部为优")
} else if (all(yw >= 80)) {
    message("语文成绩至少为良")
} else {
    message("并非所有同学语文成绩均为优良")
}


show(yw)
#> [1] 96 89 94 93 87 94
any(yw <  88)
#> [1] TRUE
any(c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
if (any(yw < 60)) {
    message("有同学语文成绩挂科")
} else {
    message("所有同学语文考试顺利通过")
}
#> 所有同学语文考试顺利通过

#下边这种写法是错误的
# if(all(yw >= 90)) {
#   message("语文成绩全部为优")
# }
# else if(all(yw >= 80)) { #else不能单起一行
#   message("语文成绩至少为良")
# }
# else { #else不能单起一行
#   message("并非所有同学语文成绩均为优良")
# }

#else不能另起一行


#注意：if里边是标量
yw > 90
#[1]  TRUE FALSE  TRUE  TRUE FALSE  TRUE
if (yw > 90) {
    message("所有同学语文成绩均为优")
}
#> 所有同学语文成绩均为优
#> Warning message:
#>   In if (yw > 90) { :
#>       the condition has length > 1 and only the first element will be used
#相当于只取了第一个元素
if ((yw > 90)[1]) {
    message("所有同学语文成绩均为优")
}

yw
#[1] 96 89 94 93 87 94
ifelse(yw >= 90, "优", "非优")
#[1] "优"   "非优" "优"   "优"   "非优" "优"
ifelse(yw >= 90, "优",
       ifelse(yw >= 88, "较好", "一般"))
#[1] "优" "良" "优" "优" "良" "优"
ifelse(yw >= 90,
       #T F T T F T
       c("1", "2", "3", "4", "5", "6"),
       c("I", "II", "III", "IV", "V", "VI"))
#> [1] "1"  "II" "3"  "4"  "V"  "6"



# Loop Structures ---------------------------------------------------------


#for循环
#求斐波那契数列的前16个数
n_fib <- 16
fib <- numeric(n_fib)
fib[1:2] <- c(1, 1)
for (i in 3:n_fib) {
    fib[i] <- fib[i - 2] + fib[i - 1]
    show(fib[i])
}
fib
#> [1]   1   1   2   3   5   8  13  21  34  55  89 144 233 377 610 987

#求1000以内的斐波那契数列
#不知道循环多少次
#while来实现
fib <- c(1, 1)
while (sum(tail(fib, 2)) < 1000) {
    fib <- c(fib, sum(tail(fib, 2)))
}
fib
#[1]   1   1   2   3   5   8  13  21  34  55  89 144 233 377 610 987

#或者通过
#repeat来实现
fib <- c(1, 1)
repeat {
    if (sum(tail(fib, 2)) >= 1000) {
        break
    }
    fib <- c(fib, sum(tail(fib, 2)))
}
fib

#当然，上述语句的逻辑
#与下述的while结构更加吻合
fib <- c(1, 1)
while (TRUE) {
    if (sum(tail(fib, 2)) >= 1000) {
        break
    }
    fib <- c(fib, sum(tail(fib, 2)))
}
fib

#再尝试一个例子
#从1~100随机抽取一个数
#若是52，则停止；否则，继续抽取
time_count <- 0
repeat {
    my_number <- sample(1:100, 1)
    time_count <- time_count + 1
    if (my_number == 52) {
        message("Haha~, I finally got '52' after ",
                time_count, " attempts")
        break
    } else {
        message(time_count,
                ": Not lucky enough [", my_number, "]")
    }
}
#> 1: Not lucky enough [42]
#> 2: Not lucky enough [4]
#> 3: Not lucky enough [7]
#> 4: Not lucky enough [2]
#> 5: Not lucky enough [98]
#> 6: Not lucky enough [43]
#> 7: Not lucky enough [44]
#> 8: Not lucky enough [15]
#> 9: Not lucky enough [70]
#> 10: Not lucky enough [54]
#> 11: Not lucky enough [27]
#> 12: Not lucky enough [65]
#> 13: Not lucky enough [62]
#> 14: Not lucky enough [43]
#> 15: Not lucky enough [54]
#> 16: Not lucky enough [9]
#> 17: Not lucky enough [71]
#> 18: Not lucky enough [4]
#> 19: Not lucky enough [66]
#> 20: Not lucky enough [5]
#> 21: Not lucky enough [92]
#> Haha~, I finally got '52' after 22 attempts
#小伙伴们需要注意一点：
#在上述实现过程中，用到了tail函数
#指的是倒数的某些元素，具体用法请看帮助文档
? tail



#当然，在R里边
#尽量不要使用显式循环
#能向量化运算的，尽量向量化
x <- 1:1e8 #一亿
y <- 2:(1e8 + 1) #一亿
z <- integer(1e8)
system.time(z <- x + y, gcFirst = TRUE)
#> user  system elapsed
#> 0.36    0.09    0.45
# The ‘user time’ is the CPU time charged for
#       the execution of user instructions of the calling process.
# The ‘system time’ is the CPU time charged for
#        execution by the system on behalf of the calling process.

system.time({
    for (i in 1:1e8) {
        z[i] <- x[i] + y[i]
    }
},  gcFirst = TRUE)
#> user  system elapsed
#> 11.51    0.06   11.70


#其实，连斐波那契数列
#也可以采用并行的方式
n_fib <- 16
sapply(1:n_fib, function(x) {
    1 / sqrt(5) *
        (((1 + sqrt(5)) / 2) ^ x -
             ((1 - sqrt(5)) / 2) ^ x)
})
#> [1]   1   1   2   3   5   8  13  21  34  55  89 144 233 377 610 987


#注意：apply函数族并非真正的并行！！

#关于apply，补充一下两点：
#(1)带进度条的apply
#for循环的进度条
pbfor::pb_for()
for(i in 1:100) {
    Sys.sleep(0.5)
}
#(2)并行计算
#
library(foreach)
library(doSNOW)

cl <- makeCluster(parallel::detectCores() - 1)
registerDoSNOW(cl)

I_REPEAT_TIMES <- 20
J_REPEAT_TIMES <- 10
results <-
    foreach(
        i = seq(0, 1, length.out = I_REPEAT_TIMES)
    ) %:%
    foreach(
        j = 1:J_REPEAT_TIMES
    ) %dopar% {
        cur_ij <- paste0('i = ', i,
                           'j = ', j)
        # to do
        cur_ij
    }
stopCluster(cl)



# Function ----------------------------------------------------------------


#编代码的过程中，一定要注意避免硬代码
#千万不要一次次Ctr C之后Ctr V
#那样你的代码会变得很难维护
#如果一套逻辑需要多次重复出现
#最好的办法是编写一个函数


#函数就是一个输入、处理、到输出的过程
#输入的是参数

#一切都是对象
#所以，函数，也是通过赋值来创建的
#比如：
#摄氏度（Celsius）到华氏度（Fahrenheit）的转换
ce2fa <- function(ce) {
    #参数ce为输入
    fa <- 1.8 * ce + 32 #对输入进行处理
    return(fa) #输出相应的值
}
ce2fa(0)#0℃相当于32℉
#> [1] 32
ce2fa(0:10)#将0~10℃转换为相应的℉
#> [1] 32.0 33.8 35.6 37.4 39.2 41.0 42.8 44.6 46.4 48.2 50.0
ce2fa
#> function(ce) { #参数ce为输入
#>   fa <- 1.8 * ce + 32 #对输入进行处理
#>   return(fa) #输出相应的值
#> }
#> <bytecode: 0x00000000144b5d28>

#多种温度计量
ce2all <- function(ce) {
    if (!is.numeric(ce) || length(ce) >= 2) {
        stop("Invalid arguments!")
    }
    fa <- 1.8 * ce + 32 #华氏度，巴哈马等
    re <- 0.8 * ce #列氏度，德国
    ra <- 1.8 * ce + 32 + 459.67 #兰氏度
    ke <- 273.15 + ce #开氏度
    return(c(
        C = ce,
        F = fa,
        Re = re,
        Ra = ra,
        K = ke
    ))
}
ce2all(0)
ce2all("0")
ce2all(0:10)


#位置参数和名义参数
frm <- function(name, frm = "BUPT") {
    cat(name, " is frm ", frm)
}
frm()#出错
#> Error in cat(name, " is frm ", frm) :
#>   argument "name" is missing, with no default
frm("axb")#参数的缺省值
#> axb  is frm  BUPT
frm(name = "AXB", frm = "BJTU")
#> AXB  is frm  BJTU
frm(frm = "BJTU", name = "AXB")
#> AXB  is frm  BJTU


#看几行我们比较熟悉的代码
xm <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
yw <- c(94, 87, 92, 91, 85, 92)
xb <- c(FALSE, TRUE, TRUE)
#再看看sum函数
sum(94, 87, 92, 91, 85, 92)
sum(1, 3, 5, 7)
? c
? sum

my_func <- function(...) {
    cat("The second arg is ", ..2)
    dot_args <- list(...)
    message("\nThe sum is ", sum(dot_args[[1]], dot_args[[5]]))
}
my_func(1, 'arg2', 3, 4, 5, 6, 7, 8)
#> The second arg is  arg2
#> The sum is 6

#+、-、*、/binary operators
#其实都是函数
1 + 2
"+"(1, 2)
'+'(1, 2)
#[1] 3
'/'(2, 3)
#[1] 0.6666667
'^'(10, 2)
#[1] 100
#连赋值符号<-都可以变成函数的形式
">"(2, 1)
#[1] TRUE
'<-'(new_var, 3)
new_var
#> [1] 3
#:，本质上也是一个函数
':'(1, 10)
#> [1]  1  2  3  4  5  6  7  8  9 10
'['(1:10, 2)
#> [1] 2
#连if都是
'if'(2 > 1, {
    cat("好吧，连if都是函数")
})
#> 好吧，连if都是函数


#%in%运算符
#可以简单的理解为：
#左侧的集合是否在右侧的集合之中
c(1, 3, 9) %in% 1:3
'%in%'(c(1, 3, 9), 1:3)
#[1]  TRUE  TRUE FALSE

#自己定义二元操作符函数
#a、b为直角边，c为斜边
"%ab2c%" <- function(a, b) {
    sqrt(sum(a ^ 2, b ^ 2))
}
3 %ab2c% 4
# [1] 5

#看完%ab2c%之后，对下边的符号，也就觉得不过如此了
library(purrr)
x <- c(17, 28, 17, 12, 15, 12, 49)
x %>%
    unique() %>%
    sort()
#等价于下边的代码，不过是更加简洁优雅
x <- c(17, 28, 17, 12, 15, 12, 49)
x2 <- unique(x)
x3 <- sort(x2)
x3
#[1] 12 15 17 28 49

#来点恶作剧
"+" <- function(x, y) {
    x * y
}
5 + 2
#[1] 10
rm("+")
5 + 2
#[1] 7

#当我们看完了上边的%my_binary_operator%之后
#对下边的符号，也就不怕了
library(tidyverse)
x <- c(17, 28, 17, 12, 15, 12, 49)
#%>%管道操作符
x %>%
    unique() %>%
    sort()

#特殊函数的帮助文档
? round
? "+" #双引号
? '+' #单引号
? `+` #反单引号
? '%in%'
? 'round'

isGeneric("plot")
plot
plot(1:10)

x <- seq(1, 100, by = 10)
y <- 2 * x + 10
xy <- cbind(x, y)
class(xy)
#> [1] "matrix"
plot(
    xy,
    xlim = c(1, 100),
    ylim = c(0, 230),
    type = "o",
    col = "red"
)
x <- seq(1, 100, by = 10)
y <- 2 * x + 10
xy <- lm(y ~ x)
class(xy)
#> [1] "lm"
op <- par(mfrow = c(2, 2))
plot(xy)
par(op)

#泛型函数
interface <- function(x, y) {
    message("Single interface")
    UseMethod("particular", y)
}
particular.classA <- function(x, y) {
    message("Different behavior: classA")
}
particular.classB <- function(x, y) {
    message("Different behavior: classB")
}
particular.default <- function(x, y) {
    message("Different behavior: default")
}
x <- 1:10
y <- 1:20
class(y) <- "classA"
interface(x, y)
#> Single interface
#> Different behavior: classA
class(y) <- "classB"
interface(x, y)
#> Single interface
#> Different behavior: classB
class(y) <- "classC"
interface(x, y)
#> Single interface
#> Different behavior: default
class(y) <- NULL
interface(x, y)
#> Single interface
#> Different behavior: default


#其实，+也是一个泛型函数
methods("+")
#> [1] +.Date   +.POSIXt
#> see '?methods' for accessing help and source code
library(ggplot2)
methods("+")
# [1] +.Date   +.gg*    +.POSIXt
# see '?methods' for accessing help and source code

z <- rnorm(1000)
ggplot(data = data.frame(z), aes(z)) +
    geom_density()

#你当然可以对它进行修改
#以下操作纯属娱（e）乐（gao）
"+.onlyFirst" <- function(a, b) {
    return(a[1] + b[1])
}
`+.onlyFirst` <- function(a, b) {
    return(a[1] + b[1])
}
a <- 1:5
a + 6:10
#> [1]  7  9 11 13 15
class(a) <- "onlyFirst" #给a贴上一个类标签onlyFirst
a + 6:10
#> [1] 7

#当你理解了上边这个泛型的+
#后续看到ggplot2中的加号
#就不会陌生了
library(ggplot2)
ggplot(data = iris,
       aes(
           x = Petal.Length,
           y = Petal.Width,
           colour = Species,
           shape = Species
       )) +
    geom_point()


#究竟有多少个+的函数
methods("+")
detach(package:ggplot2, force = TRUE)
methods("+")
rm(list = "+.onlyFirst")
methods("+")


#系统方法也可以扩展哦
a <- 1:10
print(a)
print.MyClass <- function(x, ...) {
    cat("This is my print:\n")
    print.default(x, ...)
}
attr(a, 'class') <- 'MyClass'
print(a)


# Recursion ---------------------------------------------------------------

old_monk_story <- function(depth = 1) {
    message(
        rep("  ", depth),
        "400 years ago(",
        2012 - 400 * depth,
        "), monk[",
        depth,
        "] is telling the story:"
    )
    if (2012 - 400 * (depth + 1) >= 66) {
        #据说佛教公元66年传入我国
        old_monk_story(depth + 1)
    }
    message(rep("  ", depth),
            "monk [", depth, "] finished his story")
}
old_monk_story()

#400 years ago(1612), monk[1] is telling the story:
#    400 years ago(1212), monk[2] is telling the story:
#        400 years ago(812), monk[3] is telling the story:
#            400 years ago(412), monk[4] is telling the story:
#            monk [4] finished his story
#        monk [3] finished his story
#    monk [2] finished his story
#monk [1] finished his story


#斐波那契数列，也可以采用递归的方式实现
fib <- function(n) {
    if (n == 1) {
        return(1)
    } else {
        return(c(fib(n - 1), sum(tail(
            fib(n - 1), n = 2
        ))))
    }
}
fib(16)



fib(1)
fib(2)
fib(3)
fib(10)

c(c(c(c(c(
    c(1, 1), 2
), 3), 5), 8), 13)


# Debug -------------------------------------------------------------------

findRuns <- function(x, k) {
    n <- length(x)
    runs <- NULL
    for (i in 1:(n - k)) {
        if (all(x[i:i + k - 1] == 1))
            runs <- c(runs, i)
    }
    return(runs)
}
x <- c(1, 0, 0, 1, 1, 1, 0, 1, 1)
#期望的是4,5,8
findRuns(x = x, k = 2)
debugonce(findRuns)
#debug(findRuns)
findRuns(x = x, k = 2)


# Exception ---------------------------------------------------------------

#不要让个别循环出现的异常
#影响我们的程序运行
#尤其是运行时间长的代码，期望第二天能出结果，结果头天晚上23:00就出问题了
#比如在for循环中，增加异常处理，若本轮循环出问题，则直接进入下一轮
X <- list(1, 2, "3", 4, 5)
#注意理解为何不能用c()替换list()
for (cur_x in X) {
    reciprocal <- 1 / cur_x
    cat("\nThe reciprocal of", cur_x, "is", reciprocal)
}
#改用下边的方式
#也就是把可能出问题的语句，
#全都交给tryCatch()函数
for (cur_x in X) {
    tryCatch({
        reciprocal <- 1 / cur_x
        cat("\nThe reciprocal of", cur_x, "is", reciprocal)
    }, #显然，函数的第一个参数就是表达式
    #表达式可能有很多，建议都用{}括起来
    error = function(e) {
        cat("\nSomething wrong while processing ", cur_x)
    })
}


# The End ^-^ -------------------------------------------------------------
