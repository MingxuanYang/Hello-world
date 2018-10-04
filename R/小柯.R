#--------------- 数据挖掘工作室    ---------------------------------
#--------------- writen by marvin  ---------------------------------
setwd("D:\\Rwork\\@网易")
list.files()

#### 19.k聚类     ####
dat <- read.csv('data.txt',header = FALSE)
dat
dat<- t(dat)
dat
dim(dat)  #这里的数据是2维的，x，y确定一个坐标。

kc <- kmeans(dat,3)
summary(kc)  #一般拟合模型，我们这样查看
kc$centers   #用kc带有的参数查看聚类的中心
kc$cluster   #聚类的具体情况

library(ggplot2)
qplot(dat[,1],dat[,2],colour=kc$cluster)  #qplot(x,y,colour=类别)
kc
#作业：随机数生成40对点，每一个点2维，聚成4类，并且画出聚类图形！

#### 19. 协同过滤-推荐算法    ####
#物品A与物品B都被用户A和用户C所喜爱，将物品相对于用户表示为向量A（1，0，1），B（1，1，1），C（1，0，0），可以用余弦相似度或者欧式距离来度量，我们更有理由认为物品A与B更相像，我们就把A没有入选而与A更相似的B入选了的用户B推荐给物品A
#作业：计算余弦距离，学习一下

#### 19.决策树 ####
# 1.数据准备 2.决策树生长 3.决策树修剪 4.规则提取
# 作业：信息熵；递归


library(MASS)
library(rpart)
#绑定数据集，便于操作
attach(Pima.tr)  #加载数据到内存里
 
dim(Pima.tr)
names(Pima.tr)  #具体看一下数据的内容有哪些，我们以type为决策做-
summary(Pima.tr) #适用于模型、数据集的摘要

#CART决策树分析

# 1. 未剪枝
cart <- rpart(type~.,data = Pima.tr,control = rpart.control(cp = 0))   
#type决策变量；~规定的写法；.指对前面的7列构建决策模型；control复杂系数，表示在剪枝时用的，这里=0不剪枝

summary(cart)
par(xpd = TRUE)
plot(cart,main = '未剪枝的决策树')  #只是出个图
text(cart)  #标记一下字

# 2. 剪枝---这里cp值为复杂系数
#cp = 0.1叶节点为3，留三个枝
cart1 <- prune(cart,cp = 0.1)
#cp = 0.03叶节点为5，留五个枝
cart2 <- prune(cart,cp = 0.03)
par(mfrow = c(1,2),xpd = TRUE)
plot(cart1,main = '留3个叶节点')
text(cart1)
plot(cart2,main = '留5个叶节点')
text(cart2)

# 3.检验  [检验的数据集和构建时用的是不同的，这里Pima.te，之前用的是Pima.tr]
# 检验
#测试数据集检验
#未剪枝
pre <- predict(cart,Pima.te,type = 'class')
#建立预测交叉矩阵
m <- table(type = Pima.te$type,predict = pre)
m
#预测正确率,diag(m)函数用于求矩阵对角线和，这里用于计算预测准确的数据数量之和。
sum(diag(m))/sum(m)
#叶节点为3
pre1 <- predict(cart1,Pima.te,type = 'class')
#建立预测交叉矩阵
m1 <- table(type = Pima.te$type,predict = pre1)
m1
#预测正确率
sum(diag(m1))/sum(m1)  #正确率就是预测正确的数占总数的多少；[184+58]/[总数]

#叶节点为5 正确率最高
pre2 <- predict(cart2,Pima.te,type = 'class')
#建立预测交叉矩阵
m2 <- table(type = Pima.te$type,predict = pre2)
m2
#预测正确率
sum(diag(m2))/sum(m2)
dim(Pima.te)
# 节点为5的正确率最高，模型最优 OK





#### 19.关联规则   ####
#关联规则利用支持度、置信度和增益三个衡量指标来分别表示其显著性、正确性和价值。通过最小支持度和置信度作为对应门槛，然后才能进行下一步的增益价值的评估。
#支持度可以设置阈值，低于阈值就可以不再继续分析置信度、增益；Apriori算法

library("arules")
library("arulesViz")
data("IncomeESL")
# 拿到一份数据，首先检查缺失值，顺便去掉缺失值；complete.cases()对于某些行缺失返回FALSE，不缺失返回TURE；这里不仅检查，还可以保留完整的数据，删掉缺失数据。
IncomeESL <- IncomeESL[complete.cases(IncomeESL),]
# 转换成可以进行关联规则分析的数据
dat <- as(IncomeESL,"transactions")
View(IncomeESL)
# 进行关联规则分析，支持度0.1，置信度是0.6
rules <- apriori(dat,parameter = list(support=0.1,confidence=0.6))
# 可以是单一项目集，或者多项目集；所以一共2360条规则
summary(rules)
plot(rules,method='scatterplot')

# 筛选出有自己房子的，并且增益大于1的，关联规则 [lhs是项目集，条件，rhs是结果]
rulesOwn <- subset(rules,subset=rhs %in% "householder status=own" & lift>1)
# 根据支持度排序，取前5个。
inspect(head(sort(rulesOwn,by="support"),n=5)) #这个包比较特殊，必须用这个函数查看前5条
# income <- subset(rules,subset=rhs %in% "income=[0,10)" & lift>1 )

# 筛选出income大于40，小于40的两部分数据
IncomeESL[['income']] <- factor((as.numeric(IncomeESL[['income']])>6)+1,levels = 1:2,labels = c('$40-','$40+'))
# 转化为可以进行关联规则分析的transactions型数据
Income <- as(IncomeESL,"transactions")
# 支持度0.2，置信度0.6
rules <- apriori(Income,parameter = list(support=0.2,confidence=0.6))
# income>40，增益>1的规则   %in% 是否包含关系的判断
rulesIncome <- subset(rules,subset=rhs %in% "income=$40+" & lift>1)
# 按置信度对规则排序展示
inspect(sort(rulesIncome,by="confidence"))

#### 18.神经网络 ####
#输入层、隐含层、输出层；逻辑与运算、逻辑异或运算；实际是调参的过程，误差的反向传播，调整学习速率，是误差减小；
#bp神经网络：信号的前向传播；误差的反向传播；

library(nnet)  #bp神经网络
?nnet #一个类可以有多个构造函数，函数名可以一样，只是两个函数的参数不同
data("iris")
names(iris)
library(nnet)
# ~. 表示对其他4个参数的拟合，直接用它替代啦。10个隐含层，decay学习速率，
model.nnet <-nnet(data = iris,Species ~ .,linout = F,size = 10, decay = 0.01,maxit = 1000,trace = F)
pre.forest=predict(model.nnet, iris,type='class')
pre.forest
iris[,5]  
table(pre.forest,iris$Species)  #生成一个表格，用来比较预测和真实之间的比较


#### 17. 文本挖掘 ####

dat <- readLines("匆匆.txt")

install.packages('jiebaR')
library(jiebaRD)
library(jiebaR)
test<-'我是十堰人'
# 建立分词引擎
seg <- worker()
#“<=”分词运算符 segment(string,seg)
seg<=test
# 下面的 效果一样
segment(test,seg)

seg2 <- worker('tag')  #加个参数tag 可以标记词性
test<-'我是十堰人'
segment(test,seg2)

test<-'我是中国人'  #提取关键词
seg3<-worker('keywords',topn = 1)
seg3 <= test


text <- readLines("匆匆.txt")
seg4<-worker('keywords',topn = 5)

for(i in text){print(i)}  #打印一下，发现有好多段，不是一个字符串，故不能直接分词
#  提取每一段的关键词
length(text)
for(i in text)
{
  a <- seg4 <=i
  print(a)
}
#  提取整个文章的关键词
s <- ''
for(i in text)
{
  s <- paste0(s,i)
}
s
length(s)
print(seg4 <= s)
seg4 <= s

#### 16. 爬虫   ####
library(rvest)
read_html()#读取html文档
html_nodes()#获得指定元素
html_node()
html_attrs()#提取属性名称与值
html_attr()
html_text()#获取指定元素的文本

web <- read_html(Url,encoding = 'utf-8')
# title <- html_text( html_nodes(web,'div.news-item h2'))
# 用上管道函数  %>% 后，就变成下面的啦
f <- function(x,y){return(x+y)};  f(5,4);   5%>%f(4)

title <- web %>% html_node('div.news-item h2') %>% html_text()

ur1 <- 'http://news.sina.com.cn/'
web <- read_html(ur1,encoding = 'utf-8')
attrs <- web %>% html_node('div.ct_t_01 h1 a') %>% html_attrs()  #获取属性
attrs

library(rvest)
# 网址 检查元素可以查看具体的 html
url0 <- 'http://news.sina.com.cn/china/'
# 读取网页
web <- read_html(url0,encoding = 'utf-8')
# 新闻标题（class用.  id用#）
title <- web %>% html_nodes('div.news-item h2') %>% html_text()
# 新闻链接
link_ <- web %>% html_nodes('div.news-item h2 a') %>% html_attrs()
link <- c(1:length(link_))
for(i in c(1:length(link_))){link[i] <- link_[[i]][1]}
link
length(title);length(link)
# 新闻时间
time <- web %>% html_nodes('div.news-item .time') %>% html_text()
time
length(time)
#保存到本地
#write.csv(data.frame(title,link,time),'D:\\data.csv',row.names = FALSE)
write.csv(data.frame(title,link,time),'新浪新闻.csv',row.names = FALSE)

##  实战： 爬取“慕课网”的课程信息   ###
# 1.获取所有大类网址并解析；2.分析链接：https://www.imooc.com/ 和单项/course/list?c=fe 的组合  3. 对每一类每一页进行解析与元素获取；4.递归函数
library(rvest)
# 1.获取7大类的链接---
ur10 <- 'https://www.imooc.com/'
s <- 'https://www.imooc.com'  #设置一个s，用于后面paste0 链接2个字符串
web <- read_html(ur10,encoding = 'utf-8')
course.link <- web %>% html_nodes('div.item a') %>% html_attrs()
for(i in c(1:length(course.link)))
{
  course.link[i] <- paste0(s,course.link[[i]][1]) 
}
course.link <- unlist(course.link)
course.link
# 2. 爬取第一页的内容
web <- read_html(course.link[1])  #注意 取div的时候，选择离他最近的一个div
title <- web %>% html_nodes('div.course-card-content h3') %>% html_text()
info <- web  %>% html_nodes('div.course-card-content div.course-card-info span') %>% html_text()
rank <- info[seq(1,length(info),2)]
num <- info[seq(2,length(info),2)]
length(rank);length(num)

# 2. 爬取第一类 所有内容，翻页
#--
page.link <- web %>% html_nodes('div.page a')
page.link
page.link <- web %>% html_nodes('div.page a') %>% html_attrs()
next.page <- paste0(s,page.link[length(page.link)-1])
next.page

# 3. 开始尝试
dat <- data.frame(title='课程名',rank='等级',num='人数')
download <- function(dat,Url,i)   #使用递归函数
{
  web <- read_html(Url)  # 
  title <- web %>% html_nodes('div.course-card-content h3') %>% html_text()
  info <- web  %>% html_nodes('div.course-card-content div.course-card-info span') %>% html_text()
  rank <- info[seq(1,length(info),2)]
  num <- info[seq(2,length(info),2)]
  #Sys.sleep(3)  让它睡3秒
  dat <- rbind(dat,data.frame(title,rank,num))
  write.csv(dat,file = paste0(i,'.csv'),row.names = FALSE,col.names = FALSE)
  
  page.link <- web %>% html_nodes('div.page a') %>% html_attrs()   #获取下一页
  
  if(length(page.link)==11)
  {
    next.page <- paste0(s,page.link[length(page.link)-1])
    download(dat,next.page,i)
  }
  else if(length(page.link)==9)
  {
    try(
      {
        next.page <- paste0(s,page.link[length(page.link)-1])
        download(dat,next.page,i) 
      }
    )
  }
}
for(i in c(1:length(course.link)))   #  利用一个for循环，下载6大类
{
  dat <- data.frame(title='课程名',rank='等级',num='人数')
  download(dat,course.link[i],i)  #使用递归函数，为了获取下一页网址，需要递归。
  print(paste0('success download page',i))
}

# dat2 <-data.frame(title=c(1:3),rank=c(1:3),num=c(1:3))
# dat <- rbind(dat,dat2)
# dat <- rbind(dat,data.frame(title,rank,num))

## 可以把所有的链接放在一个向量里，link，写一个函数for循环就可以啦，就不用递归啦---

dat <- data.frame(title='课程名',rank='等级',num='人数')
download <- function(dat,Url,i)   #使用递归函数
{
  web <- read_html(Url)  # 
  title <- web %>% html_nodes('div.course-card-content h3') %>% html_text()
  info <- web  %>% html_nodes('div.course-card-content div.course-card-info span') %>% html_text()
  rank <- info[seq(1,length(info),2)]
  num <- info[seq(2,length(info),2)]
  #Sys.sleep(3)  让它睡3秒
  dat <- rbind(dat,data.frame(title,rank,num))
}
## 再用个for循环就行啦---

#### 15. 正则表达式    ####
s <- c('123abc\\456123','abc\\12378942def','')
grep('abc',s)
grep('def',s)
grepl('abc',s)
grepl('def',s)

sub('123','$$$',s)
gsub('123','$$$',s)
#
grepl('\\d',s)
grepl('\\d+',s)
grepl('\\D',s)
grepl('\\[0-9]',s)
grepl('\\[^0-9]',s)
grepl('\\w',s)
grepl('\\W',s)
grepl('\\\\',s)
grepl('.',s)
grepl('^6',s)
grepl('^1',s)
grepl('3$',s)
grepl('123(.+)456',s) 
grepl('[123]{2}',s)   #123出现的次数，最大2次
grepl('.*',s)
grepl('.+',s) #至少有一个
grepl('[456]?',s)

# 2.stringr
install.packages('stringr')
library(stringr)
s <- '123abc456'
str_extract(s,'[0-9]+')
str_extract_all(s,'[0-9]+')
unlist(str_extract_all(s,'[0-9]+'))
str_locate(s,'[0-9]+')
str_locate_all(s,'[0-9]+')
str_replace(s,'123','$$$')
s <- '123abc456123'
str_replace_all(s,'123','$$$')
str_split(s,'abc')
str_split_fixed(s,'abc',3)
str_count(s,'123')
s2 <- '工资4.5千/月'
str_extract(s2,'\\d+\\.\\d+')  #学会灵活应用\\d+  多个[4.5/12.3]  \\. [.]  \\d+[4.5/8.78]
str_sub(s,4,8)
str_dup(s,2)
s <- '  123abc456123  '
str_trim(s)
str_c('123','abc')
paste0('123','456')


#### 14. 线性回归  多元回归  主成分分析    ####
# 一元线性回归 ---
data("iris")
head(iris)
# 1.标准化scale(){消除量纲的影响}  2.拟合lm(pw~sl) #采用第1和4列 
sl <- scale(iris[,1])
pw <- scale(iris[,4])
sl
pw
model <- lm(pw~sl) #pw自变量  sl因变量
summary(model)#  Residuals:残差的分布 (Intercept)常数项  sl系数
model <- lm(pw~sl-1)  #减去1，去除常数项[因为发现常数项的p值为1]
summary(model)
plot(pw,sl,col='red')
# pw=0.81794*sl

# 多元线性回归 ---
# 1.标准化；2.模型拟合lm；3.残差图；4.得出结果
rm(list = ls())
s.l <- scale(iris[,1])
s.w <- scale(iris[,2])
p.l <- scale(iris[,3])
p.w <- scale(iris[,4])
model <- lm(s.l~s.w+p.l+p.w)  # lm(formula = s.l ~ s.w + p.l + p.w) 不确定参数的位置时用=，确定位置的时候可以直接写上去。
summary(model)
model2 <- lm(s.l~s.w+p.l+p.w-1)
summary(model2)  # Multiple R-squared:R方
plot(model2,1,col='red')
plot(model2,2,col='green')  
#  s.l=0.34258*s.w+1.51175*p.l-0.51224*p.w

# 主成分分析 ---
#构建原始数据矩阵；消除量纲——数据标准化；建立协方差矩阵（就是相关系数矩阵）；求出特征值、特征向量；根据方差、累积方差贡献率确定主成分个数；求出综合得分，给出现实意义的解释
data(swiss)
head(swiss)
# 标准化
sc <- scale(swiss)
head(sc)
cor(swiss)
# 构建模型
pri <- princomp(sc,cor = TRUE)
summary(pri)
# 确定主成分个数 {画碎石图}
screeplot(pri,type = 'line')

# 计算每一个城市在每一主成分上的得分
# summary(pri,loadings = TRUE)
pre <- predict(pri)
summary(pre)

# 特征值
y <- eigen(cor(sc))
y$values
# 最终得分
scores <- (y$values[1]*pre[,1]+y$values[2]*pre[,2]+y$values[3]*pre[,3]+y$values[4]*pre[,4])/sum(y$values[1:4])
scores
dim(swiss);length(scores)
plot(scores)

#### 13. 数据探索 缺失值处理 标准化    ####
# 数据探索 ---
dat <-iris[,1:4]

# 集中程度
mean(dat[,1])
mean(dat[,2])
median(dat[,1])
median(dat[,2])
quantile(dat[,1])
quantile(dat[,2])
head(dat)
# 离散程度
range(dat[,1])
range(dat[,2])
var(dat)
sd(dat[,1])
sd(dat[,2])
sd(dat[,1])/sqrt(length(dat[,1]))

summary(dat)
boxplot(dat[,3])
boxplot(dat,col = rainbow(4))

# 缺失值 ---
# 发现缺失值，删除
a <- matrix(c(1:6,NA,8,9),nrow = 3)
a
complete.cases(a)   #返回缺失值所在的行
b <- c(1,2,NA)
complete.cases(b)
complete.cases(a,b)    #查看ab同时缺失的位置
a[complete.cases(a,b)]  #返回a的第二行
a[complete.cases(a)]   #直接去除a里面的缺失值所在行，以向量表示
a[complete.cases(a),]  #以 矩阵显示
na.omit(a)   #直接返回去除含有缺失值所在行后的数据

# 缺失值填充：1、计算相关性；2、构建回归模型；(lm(y~x))3、带入回归模型预测。

# 标准化 ---
#### 12.R技巧集锦     ####
# 用一句代码找出“众数” ---
a <-c(1,4,6,8,3,4,6,6,6,8,6,1)
b <- table(a)
b
b==max(b)  #找出那个位置
names(b)[b==max(b)]  #找出那个位置
as.numeric(names(b)[b==max(b)])  #找出来，变为数值
as.numeric(names(table(a))[table(a)==max(table(a))])   #替换b
# 借用函数，存起来
max.num <- function(x)
{
  return(as.numeric(names(table(x))[table(x)==max(table(x))]))
}
max.num(c(1,2,2,2,3))

# source函数  ---直接放一个函数，下次用source调用
getwd()
setwd("d:/Rwork/R code")
source("众数.r")
max.num(c(1,2,2,2,3))

# 数据转换
as.character(123)
as.character(c(1,2,3))
as.complex(3.4)
as.complex(c(1,2,3))
as.numeric('123')
as.numeric(c('123','456'))
as.double('12')
as.integer(3.4)
as.logical(1)
as.logical(-1)
as.logical(0)
# 调试 debug() browser()
cou <- function(count)
{
  s <- 0
  i <- 1
  while(i<count+1)   # 等价于(i <= count)
  {
    s <- s+i
    #browser()
    i <- i+1
  } 
  return(s)
}
debug(cou)

#### 11. 随机数    ####
rnorm(10)
rnorm(20,mean=3,sd=4)

runif(20,0,1)
sum(runif(20,0,1))/20
runif(10)
runif(30,2,5)

#### 10.     ####
#
?sample

x<- c(1:100)
sample(x,20)
sample(x,20)
sample(x,20)
sample(x,20,replace = TRUE)
sample(x,20,replace = TRUE)
sample(10,3)
sample(1000,30)
y <- data.frame(name =c('kg','kv','ls'),age=c(34,23,55),source=c(89,78,99))
y [sample(dim(y)[1],2),]
y [sample(dim(y)[1],2),]
y [sample(dim(y)[1],2),]
y [,sample(dim(y)[2],2)]
y [,sample(dim(y)[2],2)]
#
a <-c(4,2,1,3)
sort(a)
sort(a,decreasing = TRUE)
order(a)
order(a,decreasing = TRUE)
a[order(a)]
a[order(a,decreasing = TRUE)]
order(-a)
y <- data.frame(name =c('kg','kv','ls'),score=c(89,78,99))
y
y[order(y$score,decreasing = TRUE),]

#apply()
x<-cbind(3,c(1:5,4:1))
apply(x,1,mean)
apply(x,2,mean)
x<-array(c(1:24),dim = c(2,3,4))
apply(x,3,mean)
apply(x,3,sum)

jishu <- function(x)
{
  # 返回true是奇数
  return(x %% 2 ==1)
}
jishu(2)
jishu(3)
a <- c(2,4,11,13,14,15,18,19)
apply(data.frame(dat=a),1,jishu)
a[apply(data.frame(dat=a),1,jishu)]

x<-list(a1 = c(1:8),a2 = c(TRUE,FALSE,FALSE,TRUE,TRUE))
x
lapply(x,sum)
lapply(x,mean)
quantile(c(1:5))
lapply(x,quantile)
lapply(x,quantile)$a1
lapply(x,quantile)$a1[2]

sapply(x,quantile)
a<-as.factor(c(1,1,2,3,3))
a
tapply(a,a,length)  #统计个数

#### 9. txt  excel  ####
#
dat1 <- read.table('1.txt',sep = ' ',header = TRUE)
dat1
dat2 <- read.table('2.txt',sep = ',',header = FALSE)
dat2
dat3 <- read.table('3.txt',sep = '\t',header = FALSE)
dat3
write.table(dat1,'11.txt',sep = ' ')
write.table(dat2,'22.txt',sep = ',',col.names = FALSE)
write.table(dat3,'33.txt',sep = '\t',col.names = FALSE)
write.table(dat3,'33.txt',sep = '\t',col.names = FALSE,row.names = FALSE)
class(dat1)
write.table(dat1,'44.txt',sep = '\\___\\____\\')
#
library(rJava)
library(xlsx)
dat <- read.xlsx('df.xlsx',sheetIndex = 1)
write.xlsx(dat,'text.xlsx',row.names = FALSE)

#### 8. 可视化  ####
x <- c(1:3)
y <- x+2.5
plot(x,y)
plot(x,y,type = 'l')
?plot
cars
plot(cars)
plot(cars,main='测试图')  #如果多于2个数据，就要设定x y
names(cars)
plot(cars,main='测试图',xlab='速度')
barplot(c(1,2,3))
barplot(c(88,79,99))  # 基本的参数是  高度
barplot(c(88,79,99),names.arg = c('肖明','肖红','骁龙'),ylim = c(0,100))
barplot(c(88,79,99),legend.text = c('肖明','肖红','骁龙'),ylim = c(0,100),col = rainbow(3))
barplot(c(88,79,99),legend.text = c('肖明','肖红','骁龙'),ylim = c(0,100),col = rainbow(3),horiz = TRUE)
barplot(c(88,79,99),legend.text = c('肖明','肖红','骁龙'),col = rainbow(3),horiz = TRUE)
iris
hist(iris[,1])
iris[,1]
hist(iris[,1],freq = TRUE)  #频率直方图
hist(iris[,1],freq = FALSE) #不是频率了
hist(iris[,1],freq = FALSE,col = rainbow(8))
pie(c(1,2,3))
pie(c(1,2,3),labels = c('中国','俄罗斯','美国'))
pie(c(1,2,3),labels = c('中国','俄罗斯','美国'),radius=0.5)
boxplot(iris[,1])
boxplot(iris[,1],notch = TRUE)
boxplot(iris[,c(1:4)],notch = TRUE,col = rainbow(4),names = c('sl','sw','pl','pw'))
par(mfrow=c(2,2))
barplot(c(1:3))
pie(c(1,2,3))
plot(c(1:3))
boxplot(iris[,1])
# qplot()
library(ggplot2)
attach(diamonds)
names(diamonds)
dat <- diamonds[sample(nrow(diamonds),200),]
dim(dat)
plot(dat$carat,dat$price)
qplot(dat$carat,dat$price)
qplot(log(dat$carat),log(dat$price))  #原来是指数关系，现在取对数
unique(color)
qplot(carat,price,data = dat,colour=color)
dat$color
unique(dat$cut)
qplot(carat,price,data = dat,shape=cut)
qplot(carat,price,data = dat,colour=color,alpha=I(1/10))
qplot(carat,price,data = dat,colour=color,alpha=I(1/5))
qplot(carat,price,data = dat,geom = 'point')
qplot(cut,price,data = dat,geom = 'boxplot')
qplot(cut,price,data = dat,geom = 'boxplot',colour=cut)
qplot(carat,price,data = dat,geom = 'line')
qplot(color,data = dat,geom = 'bar',colour=color)
# 批量生成图片，保存
setwd("D:\\Rwork\\@网易\\图片")
jpeg('1.jpg')
plot(c(1,2,3,4,5))
dev.off()
jpeg('2.jpg')
barplot(c(1:5))
dev.off()

d <- c(1,2,3,2)
for(i in c(1:10))
{
  fileName <- paste0(i,'.jpg')
  jpeg(fileName)
  plot(d)
  dev.off()
}
#### 7.6. help  ####
help(package=ggplot2)
??ggplot2
example("persp")
par(mfrow=c(2,2))
example("persp")
#### 5. 函数  ####
printTest <- function(x){print(x)}
printTest
printTest('hello world!')

ret <- function(x,y){return(x+y)}
ret(2,3)
#-矩阵的乘积  左边矩阵的每一行分别与右边矩阵的列相乘，返回一个矩阵
mat1 <-matrix(c(1:12),nrow = 3,ncol = 4)
mat2 <- matrix(c(1:24),nrow = 4,ncol = 6)

f<- function(x,y)
{
  xcol <-dim(x)[2]
  yrow <-dim(y)[1]
  m <- dim(x)[1]
  n <- dim(y)[2]
  if(xcol !=yrow)
  {
    print('the two matrix diamonds is not equal')
    return(0)
  }
  mat <-matrix(0,nrow = dim(x)[1],ncol = dim(y)[2])
  for(i in c(1:m))
    for(j in c(1:n))
      mat[i,j] <- sum(x[i,]*y[,j])
  return(mat)
}
print(f(mat1,mat2))
mat1 %*% mat2
mat3 <- matrix(c(1:14),nrow = 2,ncol = 7)
f(mat1,mat3)

#  管道
library(dplyr)
f <-function(x,y){return(x*y)}
f(2,3)
2 %>% f(3)
f2 <- function(x,y,z){return(x*y + z)}
f2(2,3,4)
3 %>% f2(2,.,4)
# f(x)=sin((x+1)^2)在x=4的值
f <- function(x){return(x+1)}
f2 <- function(x){return(x^2)}
f3 <- function(x){return(sin(x))}
4 %>% f %>% f2 %>% f3

Sys.time()
library(tidyr)
date <- as.Date('2017-6-22')+0:14
hour <- sample(1:24, 15)
min <- sample(1:60, 15)
second <- sample(1:60, 15)
dat <- data.frame(date,hour,min,second)
dat
dat1 <- dat %>% unite(datehour,date,hour,sep = ' ') %>% unite(datetime,datehour,min,second,sep = ':')
#--
a <- data.frame(a1=c(1:3),a2=c(4:6))
a
unite(a,a12,a1,a2,sep = '|_||')
a %>% unite(a12,a1,a2,sep = '|_||')
a

# 递归函数  -打印斐波那契数列
fib <- function(a=1,b=1)   #函数进口
{
  print(a)
  d <- a+b
  a <- b
  b <- d
  if(d >1000)
  {
    return()
    #break
  }
  fib(a,b)   #函数出口
}

#### 4. 循环  ####
# for
a <- c('a','b','c','d')
for(i in c(1:length(a))){print(i)}
for(i in c(1:length(a))){print(a[i])}  #下标访问
for(i in a){print(i)} #元素访问
a <- matrix(c(1:24),nrow = 4)
a  #循环嵌套
for(i in c(1:nrow(a)))
  for(j in c(1:ncol(a)))
  {
    print(a[i,j])
  }
#  作业  打印9*9 乘法表

# while
s <- 0
i <- 1
while(i <= 100)
{
  s <- s + i
  i <- i + 1
}
s

e <- 1
i <- 1
while((1/prod(1:i)-1/prod(1:(i+1)))>0.001)
{
  print(i)
  e <- e +1/prod(1:i)  # 3的阶乘 prod(1:3)
  i <- i+1
}
e

# repeat
s <- 0
i <- 1
repeat
{
  if(i > 100){break()}
  s <- s+i
  i <- i+1
}
s

#### 3. 运算 三角 方程 ####
which(); unique(); 2%in%a;
a <- seq(1,10,0.1)
plot(a,sin(a),type = 'libe')
plot(a,sin(a),col=rainbow(length(a)))
  
f <- function(x,a,b){return(a*x + b )}
root2 <- uniroot(f,c(-10,10),5,10,tol=0.0001) 
root$root

f2 <- function(x,a,b,d){return(a*x^2 + b*x+d )}
root2 <- uniroot(f2,c(-4,-2.55),a=1,b=5,d=6,tol=0.0001) 
root2$root


f<-matrix(c(3,5,1,2),nrow=2,byrow=TRUE)  
rf<-matrix(c(4,1),nrow=2) 
result<-solve(f,rf) 
#### 2.基本知识 ####
# 向量----
x <- c(1,2,3)
b <- seq(2,100,3)
e <-rep(0,5)

a <- c(1,2,3,4)
append(a,10)
append(a,6,after = 3)
a[-3]

sort(a)
rev(sort(a))
# 字符串----
s <-'123 abc'
strsplit(s,' ')
unlist(strsplit(s,' '))
strsplit(s,'a')
s
s2 <- '456 def'
paste(s,s2)
paste(s,s2,sep = '') #指定链接sep
paste0(s,s2) 
nchar(s) 
substr(s,2,4)   #开始、结束位置
substring(s,2)  #默认到结尾
# 矩阵----
mat <- matrix(c(1:12),nrow = 3,ncol = 4)
dim(mat)
mat2 <- matrix(c(1:16),nrow = 4,ncol = 4)
mat %*% mat2
colnames(mat) <-c('序号','语文','英语','数学')
mat
mat[2,]
mat[c(2:3),c(1:2)]
mat[,2] >=5
mat[mat[,2] >= 5,] #语文成绩大于等于5的
which(mat[,4]>=12)
mat[which(mat[,4]>=12),] #数学成绩大于等于5的
?apply
mat
mean(mat[,2])
apply(mat,1,mean)
apply(mat,2,mean)
apply(mat,2,sum)
# 数组----
arr <- array(c(1:24),dim = c(2,3,4))
arr
arr[1,,]
arr[,2,]
# 列表----
a <- 2
b <-'hello 小柯'
d <- c(1:4)
l <- list(a,b,d)
l
l[[2]]
l
l <- list(a=a,b=b,d=d)
l$a
attach(l)
a
b
d
l <- unlist(l)
l
l[6]
class(l)
is.vector(l)
b <-c(2:8)
l <- list(a=a,b=b,d=d)
lapply(l,sum)
lapply(l,sum)[3]
sapply(l,sum)
sapply(l,sum)[2]
# 数据框----
name <- c('kg','ky','kq')
sex <- c('m','m','f')
age <- c(22,21,24)
mat <- data.frame(name,sex,age)
class(mat)
mat$name
mat$score <- c(80:82)
mat
mat[which(mat$score==82),]
mat2 <-matrix(c(1:12),nrow = 3)
mat3 <- as.data.frame(mat2)
mat3
mat3$V4
mat0 <- data.frame(name=name,weight=c(60:62))
mat0
mat0$age <-c(20,23,24)
mat0
mat
merge(mat,mat0,by.x = 'age',by.y = 'age')
attach(mat)
name
cbind(mat,mat0)
mat <-mat[-c(1,2)]
mat
lapply(mat,sum)  #返回一个列表
sapply(mat,sum)  #返回一个向量
# 因子----
a <- factor(c('a','b','c','a','b'))
a
a <- factor(c('a','b','c','a','b'),levels = c('a','b'))
a
a <- factor(c('a','b','c','a','b'),labels = c('优','良','差'))
a
a <- factor(c('a','b','c','a','b'),exclude = 'a')
a
colour <- c('G', 'G', 'R', 'Y', 'G', 'Y', 'Y', 'R', 'Y')
class(colour)
col <- factor(colour)
class(col)
col
col1 <- factor(colour, levels = c('G', 'R', 'Y'), labels = c('Green', 'Red', 'Yellow'))	        # labels的内容替换colour相应位置对应levels的内容[可以减少输入工作量，替代] 
col2 <- factor(colour, levels = c('G', 'R', 'Y'), labels = c('1', '2', '3'))
as.vector(col2)  #转化为向量

score <- c('A', 'B', 'A', 'C', 'B')
score1 <- ordered(score, levels = c('C', 'B', 'A'))
score1

exam <- c(98, 97, 52, 88, 85, 75, 97, 92, 77, 74, 70, 63, 97, 71, 98, 65, 79, 74, 58, 59, 60, 63, 87, 82, 95, 75, 79, 96, 50, 88)
exam
exam1 <- cut(exam, breaks = 3) #[cut 分组breaks组数]切分成3组区间，区间步长这样计算(max(exam)-min(exam))/3
exam1
gender <- c('f','m','m','m','f')
age <- c(12,35,25,12,25)
tapply(age,gender,mean)  #根据gender分组求平均值
# 逻辑----
a <- c(TRUE,FALSE,TRUE)
b <- c(TRUE,FALSE,FALSE)
a & b
a | b
a && b
a || b
a <- c(3,2,2)
all(a == 2)
any(a == 2)  
1 %in% c(1,2,3)
a <- c(1,2,3,1,2,3)
which(a==1)
which(1 %in% c(1,2,3))
which(1 %in% c(1,2,3,1,2,3)) #只是第一个位置
which(1 == c(1,2,3,1,2,3))
# 表----
age <- c(12,35,25,12,25)
gender <- c('f','m','m','m','f')
table(age,gender)  # table 的对象可以是向量，数据库

#(完成于 2018-04-20)
