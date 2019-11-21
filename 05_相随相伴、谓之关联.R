

# 05_相随相伴、谓之关联----------------------------------------------------

#在观察完数据的长相之后，便开始深入其内在的关系结构了
#本次实验聚焦的是关联规则
#关联规则所表达的联系，本质上是伴随关系
#因此，本章节名称为《相随相伴、谓之关联》

#教材上的名称频繁项集、关联规则
#关联规则可能是机器学习/数据挖掘领域最为知名的算法了
#啤酒和尿不湿的故事，提供了“发现数据背后意想不到的模式”的范本，
#也让关联规则成为数据挖掘最好的科（guang）普（gao）

# Data Import -------------------------------------------------------------

#清空内存
rm(list = ls())
#蛮力搜索可能的规则数
n_item <- c(2:5, 10, 20, 50, 100)
n_rules <- 3^n_item - 2^(n_item + 1) + 1
View(data.frame(n_item, n_rules))

library(tidyverse)
library(readr)
cjb_url <- "https://github.com/byaxb/RDataAnalytics/raw/master/data/cjb.csv"
cjb <- read_csv(cjb_url,
                locale = locale(encoding = "CP936"))

# Discretization ----------------------------------------------------------

#数据离散化
#arules包只能对离散数据进行关联规则挖掘
#离散化有专用的包discretization
#当然，对于大部分的任务而言，
#cut()函数已经够用了
#定义一个百分制转成五分制成绩的函数
as_five_grade_scores <- function(x) {
  cut(x,
      breaks = c(0, seq(60, 100, by = 10)),
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE,
      labels = c("不及格", "及格", "中", "良", "优"))
}

cjb %<>%
  mutate_at(vars(xb, wlfk), factor) %>% #类型转换
  mutate_at(vars(yw:sw), as_five_grade_scores) %>%#数据分箱
  select(-c(1:2, ncol(cjb)))#姓名、班级两列不参与规则挖掘
#> Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	775 obs. of  11 variables:
#> $ xb  : Factor w/ 2 levels "男","女": 2 1 1 2 1 2 2 1 2 2 ...
#> $ yw  : Ord.factor w/ 5 levels "不及格"<"及格"<..: 5 4 5 5 4 5 4 4 4 5 ...
#> $ sx  : Ord.factor w/ 5 levels "不及格"<"及格"<..: 4 5 3 4 5 4 3 4 3 4 ...
#> $ wy  : Ord.factor w/ 5 levels "不及格"<"及格"<..: 5 4 4 5 4 4 4 4 5 4 ...
#> $ zz  : Ord.factor w/ 5 levels "不及格"<"及格"<..: 5 5 5 5 5 5 5 5 5 5 ...
#> $ ls  : Ord.factor w/ 5 levels "不及格"<"及格"<..: 5 5 5 5 4 4 4 5 4 4 ...
#> $ dl  : Ord.factor w/ 5 levels "不及格"<"及格"<..: 5 5 5 5 4 5 4 5 5 5 ...
#> $ wl  : Ord.factor w/ 5 levels "不及格"<"及格"<..: 5 4 4 4 5 4 4 4 4 4 ...
#> $ hx  : Ord.factor w/ 5 levels "不及格"<"及格"<..: 5 4 5 4 5 5 5 4 5 4 ...
#> $ sw  : Ord.factor w/ 5 levels "不及格"<"及格"<..: 4 4 4 4 5 4 5 4 4 4 ...
#> $ wlfk: Factor w/ 2 levels "理科","文科": 2 2 2 2 2 2 2 2 2 2 ...


# Types of data -----------------------------------------------------------

library(arules)
#转换为transaction
cjb_trans <- as(cjb, "transactions")
#查看数据
cjb_trans
#> transactions in sparse format with
#> 775 transactions (rows) and
#> 49 items (columns)

inspect(cjb_trans[1:5])
inspect(head(cjb_trans))
#> items                                                            transactionID
#> [1] {xb=女,yw=优,sx=良,wy=优,zz=优,ls=优,dl=优,wl=优,hx=优,sw=良,wlfk=文科} 1
#> [2] {xb=男,yw=良,sx=优,wy=良,zz=优,ls=优,dl=优,wl=良,hx=良,sw=良,wlfk=文科} 2
#> [3] {xb=男,yw=优,sx=中,wy=良,zz=优,ls=优,dl=优,wl=良,hx=优,sw=良,wlfk=文科} 3
#> [4] {xb=女,yw=优,sx=良,wy=优,zz=优,ls=优,dl=优,wl=良,hx=良,sw=良,wlfk=文科} 4
#> [5] {xb=男,yw=良,sx=优,wy=良,zz=优,ls=良,dl=良,wl=优,hx=优,sw=优,wlfk=文科} 5
#> [6] {xb=女,yw=优,sx=良,wy=良,zz=优,ls=良,dl=优,wl=良,hx=优,sw=良,wlfk=文科} 6

#> items       transactionID
#> [1] {xb=女,
#> yw=优,
#> sx=良,
#> wy=优,
#> zz=优,
#> ls=优,
#> dl=优,
#> wl=优,
#> hx=优,
#> sw=良,
#> wlfk=文科}             1
#> [2] {xb=男,
#> yw=良,
#> sx=优,
#> wy=良,
#> zz=优,
#> ls=优,
#> dl=优,
#> wl=良,
#> hx=良,
#> sw=良,
#> wlfk=文科}             2

#转换为数据框
cjb_trans %>%
  as("data.frame") %>%
  View()
#转换为矩阵
cjb_trans %>%
  as("matrix") %>%
  View()
#转换为列表
cjb_trans %>%
  as("list") %>%
  head(n = 2)
#> $`1`
#> [1] "xb=女"     "yw=优"     "sx=良"     "wy=优"
#> [5] "zz=优"     "ls=优"     "dl=优"     "wl=优"
#> [9] "hx=优"     "sw=良"     "wlfk=文科"
#>
#> $`2`
#> [1] "xb=男"     "yw=良"     "sx=优"     "wy=良"
#> [5] "zz=优"     "ls=优"     "dl=优"     "wl=良"
#> [9] "hx=良"     "sw=良"     "wlfk=文科"


#无论是列表、矩阵、数据框
#还是最直接的事务记录transactions
#都可以直接用来挖掘


# Model with default args -------------------------------------------------
#关于Apriori算法的原理，请参阅课程讲义
#R中的具体实现，则简单得超乎人们的想象
#首先是加载包
#对于关联规则的挖掘和可视化
#主要用arules和arulesViz两个包
#加载后者时，前者自动加载

library(arulesViz)
#调用apriori()函数进行挖掘
#算法实现，只是一句话的事儿
irules_args_default <- apriori(cjb_trans)
irules_args_default <- apriori(cjb)

#> Apriori
#>
#> Parameter specification:
#>   confidence minval smax arem  aval originalSupport
#> 0.8    0.1    1 none FALSE            TRUE
#> maxtime support minlen maxlen target   ext
#> 5     0.1      1     10  rules FALSE
#>
#> Algorithmic control:
#>   filter tree heap memopt load sort verbose
#> 0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#>
#> Absolute minimum support count: 77
#>
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[49 item(s), 775 transaction(s)] done [0.00s].
#> sorting and recoding items ... [27 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 5 6 7 8 done [0.00s].
#> writing ... [2097 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].

#看一看挖出来的规则
irules_args_default
#> set of 2097 rules

#查看具体的规则
inspect(head(irules_args_default))
#> lhs        rhs         support confidence lift count
#> [1] {wl=优} => {sx=优}     0.21    0.83       1.8  166
#> [2] {wl=优} => {wlfk=理科} 0.21    0.81       1.6  162
#> [3] {wl=优} => {hx=优}     0.24    0.94       1.5  188
#> [4] {wl=优} => {dl=优}     0.24    0.92       1.3  185
#> [5] {wl=优} => {zz=优}     0.21    0.81       1.1  162
#> [6] {yw=优} => {dl=优}     0.24    0.89       1.2  183


# Rules information -------------------------------------------------------

#关于规则的一些基本信息
irules_args_default@info
#> $`data`
#> cjb_trans
#>
#> $ntransactions
#> [1] 775
#>
#> $support
#> [1] 0.1
#>
#> $confidence
#> [1] 0.8


# Parameters --------------------------------------------------------------

#定制其中的参数
#设置支持度、置信度、最小长度等
irules <- apriori(
  cjb_trans,
  parameter = list(
    minlen = 2,
    supp = 50 / length(cjb_trans), #最小支持度，减少偶然性
    conf = 0.8 #最小置信度，推断能力
  ))
length(irules)
inspectDT(irules)
#> [1] 5584

# #计算一下不同的支持度、置信度的结果
# sup_series <- nrow(cjb):1
# conf_series <- seq(1,0.01, by = -0.05)
# len_matrix <- matrix(ncol = length(sup_series),
#                      nrow = length(conf_series))
# sup_conf <- expand.grid(sup_series, conf_series) %>%
#   set_names(c("support", "confidence"))
# nrow(sup_conf)
# head(sup_conf)
# #根据配置不同，以下代码可能需要运行几分钟
# sup_conf$nrules <- apply(sup_conf, 1, function(cur_sup_conf) {
#   irules <- apriori(
#     cjb_trans,
#     parameter = list(
#       minlen = 2,
#       supp = cur_sup_conf["support"] / length(cjb_trans), #最小支持度，减少偶然性
#       conf = cur_sup_conf["confidence"] #最小置信度，推断能力
#     ),
#     control =  list(verbose = FALSE))
#   return(length(irules))
# })
#
# library(tidyverse)
# sup_conf %>%
#   filter(support %% 20 == 0) %>%
#   ggplot(aes(x = confidence, y = support, fill = log10(nrules))) +
#   geom_tile()

#也可以进一步设定前项和后项
irules <- apriori(
  cjb_trans,
  parameter = list(
    minlen = 2,
    supp = 50 / length(cjb_trans),
    conf = 0.8
  ),
  appearance = list(rhs = paste0("wlfk=", c("文科", "理科")),
                    default = "lhs"))
inspectDT(irules)
#对规则进行排序
irules_sorted <- sort(irules, by = "lift")
inspectDT(irules_sorted)



# Pruned Rules ------------------------------------------------------------

subset.matrix <-
  is.subset(irules_sorted, irules_sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA

View(subset.matrix)
redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
as.integer(which(redundant))
#> [1]   3   5   6   7   8  13  14  15  19  22  23  25
#> [13]  26  27  29  30  31  34  38  39  40  41  44  45
#> [25]  46  47  48  49  50  52  53  54  56  58  61  62
#> [37]  63  64  65  66  67  68  69  70  71  75  77  78
#> [49]  81  83  84  86  87  89  90  91  94  95  96  97
#> [61]  99 101 103 104 107 109 110 111 113 115 116 117
#> [73] 119 121 123 124 125 126 127 128 131 132 133 135
#> [85] 136 140 142 144 145 146 147 148 149 150 152 154
#> [97] 156 157 159 161 162 163 164 165 166 167 168 169
#> [109] 170 171 173 174 175 179 180 181 183 186 188 189
#> [121] 190 191 193 194 195 196 197 198 199 200 201 202
#> [133] 203 204 205 207 208 211 212 214 215 217 218 219
#> [145] 220 222 223 224 225 226 227 228 229 230 231 232
#> [157] 234 235 236 237 238 239 240 241 242 243 244 245
#> [169] 246 247 248 249 250 251 252 253 255 256 258 259
#> [181] 260 261 263 264 265 266 267 268 270 272 273 274
#> [193] 275 276 277 278 279 282 283 284 286 287 288 289
#> [205] 290 291 293 294 296 297 298 299 300 301 304 305
#> [217] 306 307 308 309 310 312 313 316 319 321 322 323
#> [229] 324 326 327 328 331 333 335 336 337 338 339 340
#> [241] 341 343 344 345 348 351 352 353 354 356 357 360
#> [253] 361 362 363 364 366 367 368 369 370 372 373 374
#> [265] 375 376 377 378 379 380 381 383 385 388 389 390
#> [277] 391 392 393 394

(irules_pruned <- irules_sorted[!redundant])
#> set of 57 rules
inspect(irules_pruned)
inspectDT(irules_pruned)
#当然，很多时候，我们只想查看其中部分规则
inspect(head(irules_pruned))
inspect(tail(irules_pruned))


# Model Evaluation --------------------------------------------------------

#查看评估指标
quality(irules_pruned)
#> support confidence lift count
#> 331   0.074       0.93  1.9    57
#> 334   0.090       0.93  1.9    70
#> 229   0.099       0.93  1.9    77
#> 212   0.092       0.92  1.9    71
#> 213   0.090       0.92  1.9    70
#> support confidence     lift count
#> 331 0.07354839  0.9344262 1.900736    57
#> 334 0.09032258  0.9333333 1.898513    70
#> 229 0.09935484  0.9277108 1.887076    77
#> 212 0.09161290  0.9220779 1.875618    71
#> 213 0.09032258  0.9210526 1.873532    70
#> 256 0.10451613  0.9204545 1.872316    81
#> 211 0.07354839  0.9193548 1.870079    57
#> 21  0.06967742  0.9473684 1.863478    54
#> 242 0.06838710  0.9137931 1.858765    53
#> 258 0.06838710  0.9137931 1.858765    53
#> 124 0.10838710  0.9130435 1.857241    84
#> 225 0.08129032  0.9130435 1.857241    63
#> 126 0.10580645  0.9111111 1.853310    82
#> 91  0.09161290  0.9102564 1.851571    71
#> 98  0.10064516  0.9069767 1.844900    78
#> 100 0.10064516  0.9069767 1.844900    78
#> 8   0.07483871  0.9062500 1.843422    58
#> 40  0.10967742  0.9042553 1.839364    85
#> 276 0.07225806  0.9032258 1.837270    56
#> 119 0.06967742  0.9000000 1.830709    54
#> 143 0.11612903  0.9000000 1.830709    90
#> 155 0.07870968  0.8970588 1.824726    61
#> 216 0.06709677  0.8965517 1.823694    52
#> 294 0.11096774  0.8958333 1.822233    86
#> 181 0.06580645  0.8947368 1.820003    51
#> 200 0.06580645  0.8947368 1.820003    51
#> 351 0.07354839  0.8906250 1.811639    57
#> 222 0.08258065  0.8888889 1.808107    64
#> 238 0.09290323  0.8888889 1.808107    72
#> 30  0.10193548  0.8876404 1.805568    79
#> 47  0.12129032  0.8867925 1.803843    94
#> 49  0.12000000  0.8857143 1.801650    93
#> 206 0.06967742  0.8852459 1.800697    54
#> 136 0.09806452  0.8837209 1.797595    76
#> 94  0.08774194  0.8831169 1.796366    68
#> 43  0.10709677  0.8829787 1.796085    83
#> 163 0.11612903  0.8823529 1.794812    90
#> 233 0.07612903  0.8805970 1.791241    59
#> 165 0.11354839  0.8800000 1.790026    88
#> 6   0.09032258  0.9090909 1.788186    70
#> 220 0.14838710  0.8778626 1.785679   115
#> 11  0.12903226  0.8771930 1.784316   100
#> 236 0.07354839  0.8769231 1.783767    57
#> 93  0.15354839  0.8750000 1.779856   119
#> 153 0.07225806  0.8750000 1.779856    56
#> 107 0.09806452  0.8735632 1.776933    76
#> 108 0.16000000  0.8732394 1.776274   124
#> 307 0.08000000  0.8732394 1.776274    62
#> 95  0.14838710  0.8712121 1.772151   115
#> 88  0.06967742  0.8709677 1.771654    54
#> 314 0.09548387  0.8705882 1.770882    74
#> 70  0.08645161  0.8701299 1.769949    67
#> 110 0.09419355  0.8690476 1.767748    73
#> 29  0.15354839  0.8686131 1.766864   119
#> 63  0.11870968  0.8679245 1.765463    92
#> 103 0.07612903  0.8676471 1.764899    59
#> 32  0.16516129  0.8648649 1.759240   128
#> 42  0.18064516  0.8641975 1.757882   140
#> 279 0.09032258  0.8641975 1.757882    70
#> 174 0.08129032  0.8630137 1.755474    63
#> 178 0.08903226  0.8625000 1.754429    69
#> 59  0.07870968  0.8591549 1.747625    61
#> 34  0.16258065  0.8571429 1.743532   126
#> 72  0.07741935  0.8571429 1.743532    60
#> 73  0.09290323  0.8571429 1.743532    72
#> 33  0.09935484  0.8555556 1.740303    77
#> 159 0.09935484  0.8555556 1.740303    77
#> 10  0.18322581  0.8554217 1.740031   142
#> 61  0.06838710  0.8548387 1.738845    53
#> 154 0.09032258  0.8536585 1.736445    70
#> 187 0.13935484  0.8503937 1.729803   108
#> 57  0.07483871  0.8787879 1.728580    58
#> 9   0.16774194  0.8496732 1.728338   130
#> 186 0.10838710  0.8484848 1.725921    84
#> 171 0.13677419  0.8480000 1.724934   106
#> 177 0.06451613  0.8474576 1.723831    50
#> 167 0.12645161  0.8448276 1.718481    98
#> 175 0.08129032  0.8400000 1.708661    63
#> 60  0.10064516  0.8387097 1.706037    78
#> 56  0.06709677  0.8666667 1.704738    52
#> 68  0.09161290  0.8352941 1.699089    71
#> 1   0.06451613  0.8620690 1.695694    50
#> 4   0.07225806  0.8615385 1.694651    56
#> 75  0.15354839  0.8321678 1.692730   119
#> 71  0.07612903  0.8309859 1.690326    59
#> 168 0.11354839  0.8301887 1.688704    88
#> 12  0.06838710  0.8281250 1.684506    53
#> 66  0.14193548  0.8270677 1.682355   110
#> 64  0.12903226  0.8264463 1.681092   100
#> 150 0.10967742  0.8252427 1.678643    85
#> 24  0.07225806  0.8235294 1.675158    56
#> 65  0.14451613  0.8235294 1.675158   112
#> 67  0.08387097  0.8227848 1.673644    65
#> 7   0.09548387  0.8505747 1.673085    74
#> 121 0.07741935  0.8219178 1.671880    60
#> 62  0.06451613  0.8196721 1.667312    50
#> 58  0.09290323  0.8470588 1.666169    72
#> 53  0.18580645  0.8181818 1.664281   144
#> 54  0.11483871  0.8165138 1.660888    89
#> 39  0.08000000  0.8157895 1.659414    62
#> 27  0.07354839  0.8142857 1.656355    57
#> 52  0.11870968  0.8141593 1.656098    92
#> 13  0.19741935  0.8138298 1.655428   153
#> 18  0.07870968  0.8133333 1.654418    61
#> 20  0.10064516  0.8125000 1.652723    78
#> 14  0.12774194  0.8114754 1.650639    99
#> 180 0.07741935  0.8108108 1.649287    60
#> 23  0.08258065  0.8101266 1.647895    64
#> 69  0.09290323  0.8089888 1.645581    72
#> 2   0.20903226  0.8059701 1.639441   162
#> 188 0.10580645  0.8039216 1.635274    82
#> 189 0.06838710  0.8030303 1.633461    53
#> 19  0.15096774  0.8013699 1.630083   117
#> 3   0.06838710  0.8281250 1.628926    53
#> 5   0.07870968  0.8133333 1.599831    61
#> 55  0.07741935  0.8108108 1.594869    60
#> 17  0.12516129  0.8083333 1.589996    97
#> 312 0.06451613  0.8064516 1.586294    50
#> 184 0.06838710  0.8030303 1.579565    53
#> 16  0.08258065  0.8000000 1.573604    64
#> 74  0.08258065  0.8000000 1.573604    64

str(quality(irules_pruned))
#> 'data.frame':	121 obs. of  4 variables:
#> $ support   : num  0.0735 0.0903 0.0994 0.0916 0.0903 ...
#> $ confidence: num  0.934 0.933 0.928 0.922 0.921 ...
#> $ lift      : num  1.9 1.9 1.89 1.88 1.87 ...
#> $ count     : num  57 70 77 71 70 81 57 54 53 53 ...

#更多评估指标
(more_measures <- interestMeasure(
  irules_pruned,
  measure = c("support", "confidence", "lift","casualConfidence"),
  transactions = cjb_trans))
#      support confidence   lift casualConfidence
# 1   0.073548    0.93443 1.9007          0.99990
# 2   0.090323    0.93333 1.8985          0.99990
# 3   0.099355    0.92771 1.8871          0.99989
# 4   0.091613    0.92208 1.8756          0.99988
# 5   0.090323    0.92105 1.8735          0.99988
# 6   0.104516    0.92045 1.8723          0.99987
# 7   0.073548    0.91935 1.8701          0.99988

quality(irules_pruned) <- more_measures %>%
  mutate_at(
    vars(1:3),
    funs(round(., digits = 2)))



# Rules Filtering ---------------------------------------------------------

#比如仅关心文科相关的规则
irules_sub <- subset(irules_pruned,
                      items %in% c("wlfk=文科"))
inspect(irules_sub)
inspectDT(irules_sub)

irules_sub <- subset(irules_pruned,
                      items %pin% c("文科"))
inspectDT(irules_sub)
#当然也可以同时满足多种搜索条件
#比如性别和确信度
irules_sub <- subset(irules_pruned,
                      lhs %pin% c("sw") &
                        lift > 1.8)
inspectDT(irules_sub)
inspect(irules_sub)
#> lhs                                      rhs         support confidence
#> [1]  {xb=男,sx=优,ls=优,wl=优,hx=优,sw=优} => {wlfk=理科} 0.074   0.93
#> [2]  {xb=男,sx=优,dl=优,wl=优,hx=优,sw=优} => {wlfk=理科} 0.090   0.93
#> [3]  {xb=男,dl=优,wl=优,hx=优,sw=优}       => {wlfk=理科} 0.099   0.93
#> [4]  {xb=男,sx=优,wl=优,hx=优,sw=优}       => {wlfk=理科} 0.092   0.92
#> [5]  {xb=男,sx=优,dl=优,wl=优,sw=优}       => {wlfk=理科} 0.090   0.92
#> [6]  {xb=男,sx=优,ls=优,wl=优,sw=优}       => {wlfk=理科} 0.074   0.92
#> [7]  {xb=女,zz=优,sw=中}                   => {wlfk=文科} 0.070   0.95


# Frequent Itemsets -------------------------------------------------------

#从规则中提取频繁项集
itemsets <- unique(generatingItemsets(irules_pruned))
itemsets
#> set of 121 itemsets
itemsets_df <- as(itemsets, "data.frame")
View(itemsets_df)
inspect(itemsets)

#反过来，先挖掘频繁项集
#再导出关联规则
#生成频繁项集，而不是规则
itemsets <- apriori(cjb_trans,
                    parameter = list(
                      minlen = 2,
                      supp = 50 / length(cjb_trans),
                      target = "frequent itemsets"
                    ))
inspect(itemsets)
irules_induced <- ruleInduction(itemsets,
                                cjb_trans,
                                confidence = 0.8)
irules_induced
#> set of 5584 rules

#显然，只要参数是一样的
#得到规则条数也是一样的

#1-项集的频繁程度
itemFrequency(cjb_trans, type = "relative")
itemFrequencyPlot(cjb_trans)
#当然我们更愿意统一成ggplot2的风格
item_freq <- itemFrequency(cjb_trans, type = "relative")
library(tidyverse)
item_freq %>%
  as.data.frame %>%
  rownames_to_column(var = "item") %>%
  mutate(item = factor(item, levels = item)) %>%
  ggplot(aes(x = item, y = item_freq, fill = item_freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x  = element_text(angle=60, vjust=1, hjust = 1))
#保留现有的因子水平，也有下述方法
item_freq %>%
  as.data.frame %>%
  rownames_to_column(var = "item") %>%
  mutate(item = forcats::fct_inorder(item)) %>%
  ggplot(aes(x = item, y = item_freq, fill = item_freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x  = element_text(angle=60, vjust=1, hjust = 1))


# Rules Viz ---------------------------------------------------------------

library(arulesViz)
plot(irules_pruned[1:10],
     method = "graph")#最常用的一种方式
plot(irules_pruned, method = "grouped")
plot(irules_pruned, method = "paracoord")

#交互式的规则可视化
library(tcltk2)
plot(irules_pruned,
            method="graph",
            interactive=TRUE)


# Rules Export ------------------------------------------------------------

#这些规则怎么保存呢？
#当然可以console输出之后复制、或是截图，
#但效果并不好
#稍微好一点的办法是直接将console的结果捕获
out <- capture.output(inspect(irules_pruned))
out
writeLines(out, con = "Rules.txt")

save(irules_pruned,
     file = "rules.rda")

#更好的办法，应该是将规则转换成数据框
#然后另存为csv文件
irules_pruned_in_df <-
  as(irules_pruned, "data.frame")
View(irules_pruned_in_df)
#考虑到规则中也包含逗号,
#在另存为csv文件时，一般需要设置参数quote=TRUE
write.csv(irules_pruned_in_df,
          file = "Rules.csv",
          quote = TRUE,
          row.names = FALSE)
#当然，在另存为csv之前，也可以对规则进行必要的处理
irules_pruned_in_df %<>%
  separate(
    rules,
    sep = "=>",
    into = c("LHS", "RHS")) %>%
  mutate_at(
    vars("LHS", "RHS"),
    funs(gsub("[\\{\\} ]", "", .)))
View(irules_pruned_in_df)

#转换成data.frame之后
#自然可以随意处置了
#比如可以通过正则表达式任意抽取自己想要的规则
#请小伙伴们自行练习
#当然，arules包中write()函数也可以将规则直接写到本地
write.csv(irules_pruned_in_df,
      file="Rules2.csv",
      quote = TRUE,
      row.names=FALSE)

#以上是R中关于关联规则的基本实现
#感兴趣的同学，可以进一步阅读：
#序列模式arulesSequences等主题
#当然，即便是关联规则，arules当然使用最多
#但也并非是唯一的选择，比如RKEEL等均可尝试


# The End ^-^ -------------------------------------------------------------
