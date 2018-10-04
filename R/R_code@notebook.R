# -- -- -- -- -- -- -- -- -- --  -- -- -- -- -- -- -- -- -- -- --
#               《R语言》         
#                     常用语句 2018/10/02,by marvin
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---- --
# 1.常用操作 ####
 # 1.1安装bioconductor
source("https://bioconductor.org/biocLite.R")  #下载到本地
biocLite()   #安装bioconductor和基本包

BiocInstaller::biocLite('packages name')
devtools::install_github("GuangchuangYu/yyplot")
 # 1.2 安装bioconductor的其他包
source("http://bioconductor.org/biocLite.R")
biocLite(c("affy","Simpleaffy","arrayQualityMetrics"))

 # 1.3升级更新bioconductor
source("http://bioconductor.org/biocLite.R")
update.packages(repos = biocinstallRepos(),ask = FALSE)

library(limma)
limmaUsersGuide()
# packages
install.packages("ggplot2")
# 查看已经安装的包，已经加载的包,移除、删除包
library("ggplot2") 
(.packages()) #查看已经加载的包
detach("package:ggplot2") #暂时移除已经加载的包
remove.packages("ggplot2") #删除包

data(package="maps") # 查看包里的数据集
 pheatmap(d) -> x #这样既可以画图，又可以赋值给变量x
# 清除所有变量 
rm(list = ls())
ls()

# 2.1设置工作目录
getwd()   #用于知道当前工作目录

setwd('D:\\Rworkspace\\Rcode@YMX')     #符号“/”引用途径时，用一个就好
getwd()
setwd('d://R语言的学习//R study')    #符号“/”引用途径时，用二个也不会出错
getwd()
setwd('d:\\Rwork')        #但是，符号“\”引用途径时，必须用二个哦！！！
getwd()
# 2.2 打开“正态分布.R”的函数包
source("d://Rwork/正态分布.R")  #文件必须是一个函数包，或者是编写好的函数！



# 3.1读取/写入文件.csv或.txt  ####
a<-read.csv(file.choose(),header = T)   # ***

#保证要操作的文件是在工作目录之下的！
vv=read.table("D:\\Rworkspace\\R_data\\abc.txt",header = F)     
vv=read.table("D:/Rworkspace/R_data/abc.txt")   #有路径时用一个/或者用\\
vv
class(vv)
mode(vv)
typeof(vv)
# 3.2通过剪切板,来读取文件
#通过剪切板来操作，先把数据复制，再输入以下命令，header=F是不读列头，把第一行当作数据来读。
#注意啦：原来表里有列头时，用header=T;如果没有列头，第一行也是数据，用header=F
vc<-read.table('clipboard',header=F)  #不读列头是指：把第一行当作“数据”读进去！！
vc                                    #剪切板方法  也可以用来读Excel文件
head(vc)
tail(vc)
v1<-read.table('clipboard',header=T)  #若是header=T 是把第一行当作 列名 读进去！
v1
head(v1)         #注意了，是从第二行开始读的哦！

# 3.3读取Excel文件
#读Excel，先把它另存为prn格式或csv格式，然后读
v1<-read.table('abc1.prn',header=T)   ##读Excel，先把它另存为prn格式，然后读
v1
v2<-read.table('abc2.prn',header=T)
v2
v3<-read.csv('abc3.csv',header=T)     ##另一种读Excel的方法
v3

# 3.4写入文档
write.table(t,file='d:\\Rwork\\mark.txt',col.names=F,row.names=F,sep=' ')  #sep=' '里面加空格，输出的数据才有间隙
write.table(t,file='d:\\Rwork\\mark1.txt',col.names=F,row.names=F,sep='')  #sep=''不加空格是不对的。
#    sep=' '   sep=','  sep='\t'
#4.编译检查
dim()
summary()
class()
attributes()
str()
head()

#5 help 查看函数包的说明
help(package="ggplot2")

#6 查看函数的例子操作 
example("plot")
#7.查看数据集
data()

# *******************分界线**********************************

# 2018/10/02    从网易TCGA课程学的   notes ####
# 1.
rownames(rt)=rt[,1]  #设置行名！为“原来的第一列”
# 2.
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]  #设置行名[[1]]，如果是列名换成[[2]]即可
rownames(x)<- letters[1:8]        #设置行名，==等价于上面
rowSums(x); colSums(x); rowMeans(x); colMeans(x); #求和、平均值

# 3. 把data.frame变为matrix，出现""在数据上，去除引号的过程
rt=read.table("mRNAsymboll.txt",sep="\t",header=T,check.names=F)  #！！改成自己的文件名
#check.names 是指要不要检查变量名；sep 是分隔符；header 是要不要读表头；quote="\" 表示引用*
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp),colnames(exp)) 
data=matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames) #读取矩阵数据 

write.table(normalizeExp,file="normalizeExp.txt",sep="\t",quote=F,col.names=F)   
#输出所有miRNA校正后的表达值（normalizeExp.txt）
#这里的header=f是因为想添加id这个表头，所以第一行数据就是样本名字
#col.names=F 用的好，因为列头是样本名，第一行也是样本名，所以不能重复！！！

# 2018/10/02 整理笔记   note2 ####
# 1.增维
x1<-c(1,2,3,4,5,6)
dim(x1)<-c(2,3) 
# 2.查看某列某项有多少个值，或统计计数
summary(z$high==170)
table(z[,2]==170)
# 3.for语句(需要用哪个变量时，先赋值为0.如 w<-0)
w<-0         
for(i in 1:59){w[i]=i*2+3}
w
# 4.while语句
d<-0
d[1]=5
i=1
while(d[i]<121){i=i+1;d[i]=d[i-1]+2}
d
# 5.
t3[which(t3>100)]=100 
# 6.使用apply函数
apply(t,2,mean)   ##中间为2，求每列的均数
apply(t,1,mean)   ##中间为1，求每行的均数
apply(t,2,max)    ##求每列中的 最大值（每科成绩的最高分）
apply(t,2,min)    ##求每列中的  最小值（每科成绩的最低分）
apply(t[c('t1','t2','t3')],1,sum)  #求每个人的总分
which.max(apply(t[c('t1','t2','t3')],1,sum)) #哪一个人的总分最高
t$num[which.max(apply(t[c('t1','t2','t3')],1,sum))]  #求3门课总分最高的那个人学号
# 7. 散点图
plot(t$t1,t$t2,
     main='数学分析与线性代数成绩的关系',
     xlab='数学分析',
     ylab='线性代数',
     xlim=c(0,100),#根据图形的分布可以适当调节数轴的范围
     ylim=c(0,100),
     xaxs='i',#set x axis style as internal数轴的风格
     yaxs='i',#set y axis style as internal
     col='red',#set the color of plotting symbol to red
     pch=19)#set the plotting symbol to filled dots

# 8.使用颜色 ####
# 8.1. 使用颜色RColorBrewer包
plot(women,col="red")
plot(women,col=554)
plot(women,col="#FF0000")
Hex <- rgb(red = 255,green = 0,blue = 0,max=255)
plot(women,col=Hex)
#RColorBrewer包 提供了3套配色方案(连续、极端、离散)
library(RColorBrewer)
display.brewer.all(type = "seq")  #展示连续型的配色,每组分9种渐变颜色
barplot(rep(1,6),col=brewer.pal(9,"YlOrRd")[3:8])

display.brewer.all(type = "div")   #展示极端型
barplot(rep(1,6),col=brewer.pal(11,"BrBG")[3:8])

display.brewer.all(type = "qual")
barplot(rep(1,6),col=brewer.pal(12,"Set3")[3:8])

display.brewer.all(type="all") #查看所有的，选择自己需要的配色方案
#    
plot(1:10)
points(2,4,col="red")
text(2,2,"杨明轩",pos=4,col="black")   #在图上标记“想要显示的内容，比如想看那个基因就表上去
#  猜想：text(allDiff$logFC[brca1], -log10(allDiff$FDR[brca1],"brca1",pos=4)
plot(women)
fit <- lm(weight~height,data = women)

plot(women)
abline(fit,col="red",lty=2)
#
par(mfrow=c(1,2))
plot(x=rnorm(10))
plot(women)
par(mfrow=c(1,1))  #完事了，要回复一个窗口的设置

# 8.2.rainbow()
pie(rep(1, 12), col = rainbow(12))
barplot(rep(1,12),col=rainbow(12))
# 8.3.颜色收藏


# 2017-10-10 学习日志 ####
df <- data.frame(x = 1:3, y = 5:7)
## Error:
try<-(as.vector(data.frame(x = 1:3, y = 5:7), mode = "numeric"))

x <- c(a = 1, b = 2)

is.vector(x)
y<-as.vector(x)
all.equal(x, as.vector(x)) ## FALSE
# -- All the following are TRUE:
! !is.list(df)
! is.vector(df)
! is.vector(df, mode = "list")
is.vector(list(), mode = "list")

# 2018/10/02   "@jimmy :sort 在平时的应用"  notes ####
#1.引进jimmy_basic_heat.R 中的数据准备部分，解说  sort 在平时的应用
# ******************   引入部分   ***********
library(CLL)
library(ggplot2)
library(reshape2)
library(gpairs)
library(corrplot)

data(sCLLex)
sCLLex=sCLLex[,1:8] ## 样本太多，我就取前面8个
group_list=sCLLex$Disease
exprSet=exprs(sCLLex)

head(exprSet)
#1
choose_gene=names(sort(apply(exprSet, 1, mad),decreasing = T)[1:50])  #mad是中位数
choose_matrix=exprSet[choose_gene,]
choose_matrix=scale(choose_matrix)
heatmap(choose_matrix)
#**************************   end   ***********************************

# 2018/10/03  整理：筛选数据 ####

a<-as.list(read.csv("D:\\Rworkspace\\R_data\\LNC_bmk\\mRNAup.csv",sep = "\t", header = T,stringsAsFactors = FALSE))
# names(a)<-c("one","three" ,"four" ,"five"  ,"six","seven", "eight")

str(a)
a$one<-a$one[duplicated(a$one)==FALSE]
a$three<-a$three[duplicated(a$three)==FALSE]
a$four<-a$four[duplicated(a$four)==FALSE]
a$five<-a$five[duplicated(a$five)==FALSE]
a$six<-a$six[duplicated(a$six)==FALSE]
a$seven<-a$seven[duplicated(a$seven)==FALSE]
a$eight<-a$eight[duplicated(a$eight)==FALSE]
str(a)

s<-table(unlist(a))
write.csv(s,file = "file.csv")

# volin plot-- ####
#方法1 --vioplot
#install.packages("vioplot")
library("vioplot")
data<-read.table("D:\\Rworkspace\\R_data\\violin1.txt",header=T)
x1<-data$temp
x2<-data$RH
x3<-data$wind
vioplot(x1,x2,x3,names=c("Temp","RH","Wind"),ylim =c(0,90))
#vioplot(x1,x2,x3,names=c("Temp","RH","Wind"),col=c("gold"),ylim =c(0,90))
vioplot(x1, col="red", at=1,add=TRUE)
vioplot(x2, col="cyan", at=2,add=TRUE)
vioplot(x3, col="gold", at=3,add=TRUE)
title(main="my volin",xlab = "Factor",ylab ="Value")

# 方法2 ggplot2
 # 数据整理： 把一个 n*3 数据转换成长矩阵 3n*2 
q1<-matrix(data[,1],nrow = nrow(data))
q2<-matrix(data[,2],nrow = nrow(data))
q3<-matrix(data[,3],nrow = nrow(data))
colnames(q1)<-colnames(data)[1]
colnames(q2)<-colnames(data)[2]
colnames(q3)<-colnames(data)[3]
idq1<-matrix(colnames(q1),nrow = nrow(q1))
idq2<-matrix(colnames(q2),nrow = nrow(q2))
idq3<-matrix(colnames(q3),nrow = nrow(q3))
qq<-rbind(q1,q2,q3);colnames(qq)=NULL
id<-rbind(idq1,idq2,idq3)
#qqq<-data.frame(factor=id,value=qq)
#但是，qqq里面的第一列变成了factor形式，不是我想要的chr,加个参数stringsAsFactors = FALSE,就好啦，oh yeah
qqq<-data.frame(factor=id,value=qq,stringsAsFactors = FALSE)

library(ggplot2)
p1<-ggplot(qqq,aes(x=factor,y=value))+geom_violin(aes(x=factor,y=value,fill=factor),alpha=0.7)
p1+geom_boxplot(width=0.05)  #这里面的width可以调整，到最佳位置
 
 #方法2--延伸
library("ggplot2")
#载入ggplot2包
data1<-read.table("D:\\Rworkspace\\R_data\\violin2.txt",header=T)
#导入数据
p1<-ggplot(data1,aes(x=Factor,y=value))+geom_violin(aes(x=Factor,y=value,fill=Factor),alpha=0.7)
p1+geom_boxplot(width=0.05)

mytheme<-theme_bw()+theme(panel.grid.major= element_line(color = "white"),panel.grid.minor =element_line(color= "white"))
p1+geom_boxplot(width=0.05)+mytheme+  #去掉网格
  labs(title = "New plot title", subtitle = "A subtitle",x = "New x label",y="y label")+
  theme(legend.position="none")
#p1 + theme(plot.title = element_text(size = rel(2)))  #rel(2)指的是原来的字体大小的2倍
#p1 + theme(panel.background = element_rect(fill = "white", colour = "grey50")) #面板背景设为空白
#p2 + theme(legend.box.background = element_rect(), legend.box.margin = margin(6, 6, 6, 6))

#绘制小提琴图，这里给不同的Factor分别着色
#添加抖动散点（Jitteredpoints）
p2<-p1+geom_jitter(alpha=0.3,col="red",show.legend=FALSE)
p2
mytheme<-theme_bw()+theme(panel.grid.major= element_line(color = "white"),panel.grid.minor =element_line(color= "white"),legend.title = element_blank())
mytheme
#更改画布背景主题，“去掉”网格线等
p1+mytheme
p2+mytheme

# 热图-- “y shu”  2018/10/03整理 ####
d = data.frame(matrix(rnorm(100), ncol=10))
colnames(d) = paste0('t', 1:10)
rownames(d) = paste0('g', 1:10)
library(pheatmap)
pheatmap(d) -> x

require(cowplot)
p  = ggplot(d, aes(t1, t2)) + geom_point(size=5)
plot_grid(x$gtable, p, labels=c('A', 'B'))
# 有了这个simplot函数，一键出图，然后再用ggplot2改细节，再也不要说不会用ggplot2画热图了

# dplyr程序包---应用实例数据：飞机航班数据 2018/10/03####
library(dplyr)
#install.packages("hflights")
library(hflights)
View(hflights)
#    本文试图对该dplyr包的一些基础且常用的功能做简要介绍。主要包括：
#    1 变量筛选函数 select
#    2 记录筛选函数 filter
#    3 排序函数 arrange
#    4 变形（计算）函数 mutate
#    5 汇总函数 summarize
#    6 分组函数 group_by
#    7 多步操作连接符 %>%
#    8 随机抽样函数 sample_n,sample_frac
hflights_df <- hflights[1:10,];row.names(hflights_df)<-c(1:10)
select(hflights_df,Year,Month,DayOfWeek) # A tibble: 227,496 x 3
select(hflights_df,Year:ArrTime) # A tibble: 227,496 x 6

select(hflights_df,-Year,-DayOfWeek,-DayofMonth,-TailNum)
select(hflights_df,-(Year:AirTime)) #除了选择变量，也可以删除指定的变量
filter(hflights_df,Year==2011,Month==1,DepTime==1400)
filter(hflights_df,Year==2011 & Month==1 & DepTime==1400)

filter(hflights_df,Year==2011 & Month==1 & (DepTime==1400 | DepTime==1430) & UniqueCarrier=="AA") #或'的关系用|符号表示。选择起飞时间为1400或者1430的航班,且UniqueCarrier为'AA'

y<-select(hflights_df,Year,DepTime,FlightNum) 
arrange(y,DepTime)# arrange 按照某列进行排序，默认为升序，可以使用desc（）进行降序排列。
arrange(y,desc(DepTime))  #使用降序 

mutate(hflights_df,   gain = ArrDelay - DepDelay,   speed = Distance / AirTime * 60) #mutate 对已有列进行数据运算并添加为新列:

summarise(hflights_df,   delay = mean(DepDelay, na.rm = TRUE))#对数据框调用其它函数进行汇总操作, 返回一维的结果:
#group_by函数实现对数据进行分组，结合summarize函数，可以对分组数据进行汇总统计。
#按照航空公司分组进行汇总
summarise(group_by(tbl_hflights, UniqueCarrier),          
          m = mean(AirTime,na.rm = TRUE),          
          sd = sd(AirTime,na.rm = TRUE),          
          cnt = n(),          
          me = median(AirTime,na.rm = TRUE))
#%>%，多步骤连接符  dplyr包里还新引进了一个操作符,%>%, 使用时把数据集名作为开头, 然后依次对此数据进行多步操作
hflights_df%>%      
  group_by(UniqueCarrier)%>%     
  summarise(me=mean(AirTime))%>%arrange(me)%>%head(10)
#挑选随机样本sample_n, sample_frac  sample_n随机选出指定个数（样本容量）的样本数；sample_frac随机选出指定百分比（占整个数据集总体百分比）的样本数。

# new ####
  
  
  
