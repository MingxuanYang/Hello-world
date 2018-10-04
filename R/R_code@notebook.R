# -- -- -- -- -- -- -- -- -- --  -- -- -- -- -- -- -- -- -- -- --
#               ��R���ԡ�         
#                     ������� 2018/10/02,by marvin
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---- --
# 1.���ò��� ####
 # 1.1��װbioconductor
source("https://bioconductor.org/biocLite.R")  #���ص�����
biocLite()   #��װbioconductor�ͻ�����

BiocInstaller::biocLite('packages name')
devtools::install_github("GuangchuangYu/yyplot")
 # 1.2 ��װbioconductor��������
source("http://bioconductor.org/biocLite.R")
biocLite(c("affy","Simpleaffy","arrayQualityMetrics"))

 # 1.3��������bioconductor
source("http://bioconductor.org/biocLite.R")
update.packages(repos = biocinstallRepos(),ask = FALSE)

library(limma)
limmaUsersGuide()
# packages
install.packages("ggplot2")
# �鿴�Ѿ���װ�İ����Ѿ����صİ�,�Ƴ���ɾ����
library("ggplot2") 
(.packages()) #�鿴�Ѿ����صİ�
detach("package:ggplot2") #��ʱ�Ƴ��Ѿ����صİ�
remove.packages("ggplot2") #ɾ����

data(package="maps") # �鿴��������ݼ�
 pheatmap(d) -> x #�����ȿ��Ի�ͼ���ֿ��Ը�ֵ������x
# ������б��� 
rm(list = ls())
ls()

# 2.1���ù���Ŀ¼
getwd()   #����֪����ǰ����Ŀ¼

setwd('D:\\Rworkspace\\Rcode@YMX')     #���š�/������;��ʱ����һ���ͺ�
getwd()
setwd('d://R���Ե�ѧϰ//R study')    #���š�/������;��ʱ���ö���Ҳ�������
getwd()
setwd('d:\\Rwork')        #���ǣ����š�\������;��ʱ�������ö���Ŷ������
getwd()
# 2.2 �򿪡���̬�ֲ�.R���ĺ�����
source("d://Rwork/��̬�ֲ�.R")  #�ļ�������һ���������������Ǳ�д�õĺ�����



# 3.1��ȡ/д���ļ�.csv��.txt  ####
a<-read.csv(file.choose(),header = T)   # ***

#��֤Ҫ�������ļ����ڹ���Ŀ¼֮�µģ�
vv=read.table("D:\\Rworkspace\\R_data\\abc.txt",header = F)     
vv=read.table("D:/Rworkspace/R_data/abc.txt")   #��·��ʱ��һ��/������\\
vv
class(vv)
mode(vv)
typeof(vv)
# 3.2ͨ�����а�,����ȡ�ļ�
#ͨ�����а����������Ȱ����ݸ��ƣ��������������header=F�ǲ�����ͷ���ѵ�һ�е�������������
#ע������ԭ����������ͷʱ����header=T;���û����ͷ����һ��Ҳ�����ݣ���header=F
vc<-read.table('clipboard',header=F)  #������ͷ��ָ���ѵ�һ�е��������ݡ�����ȥ����
vc                                    #���а巽��  Ҳ����������Excel�ļ�
head(vc)
tail(vc)
v1<-read.table('clipboard',header=T)  #����header=T �ǰѵ�һ�е��� ���� ����ȥ��
v1
head(v1)         #ע���ˣ��Ǵӵڶ��п�ʼ����Ŷ��

# 3.3��ȡExcel�ļ�
#��Excel���Ȱ�������Ϊprn��ʽ��csv��ʽ��Ȼ���
v1<-read.table('abc1.prn',header=T)   ##��Excel���Ȱ�������Ϊprn��ʽ��Ȼ���
v1
v2<-read.table('abc2.prn',header=T)
v2
v3<-read.csv('abc3.csv',header=T)     ##��һ�ֶ�Excel�ķ���
v3

# 3.4д���ĵ�
write.table(t,file='d:\\Rwork\\mark.txt',col.names=F,row.names=F,sep=' ')  #sep=' '����ӿո���������ݲ��м�϶
write.table(t,file='d:\\Rwork\\mark1.txt',col.names=F,row.names=F,sep='')  #sep=''���ӿո��ǲ��Եġ�
#    sep=' '   sep=','  sep='\t'
#4.������
dim()
summary()
class()
attributes()
str()
head()

#5 help �鿴��������˵��
help(package="ggplot2")

#6 �鿴���������Ӳ��� 
example("plot")
#7.�鿴���ݼ�
data()

# *******************�ֽ���**********************************

# 2018/10/02    ������TCGA�γ�ѧ��   notes ####
# 1.
rownames(rt)=rt[,1]  #����������Ϊ��ԭ���ĵ�һ�С�
# 2.
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]  #��������[[1]]���������������[[2]]����
rownames(x)<- letters[1:8]        #����������==�ȼ�������
rowSums(x); colSums(x); rowMeans(x); colMeans(x); #��͡�ƽ��ֵ

# 3. ��data.frame��Ϊmatrix������""�������ϣ�ȥ�����ŵĹ���
rt=read.table("mRNAsymboll.txt",sep="\t",header=T,check.names=F)  #�����ĳ��Լ����ļ���
#check.names ��ָҪ��Ҫ����������sep �Ƿָ�����header ��Ҫ��Ҫ����ͷ��quote="\" ��ʾ����*
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp),colnames(exp)) 
data=matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames) #��ȡ�������� 

write.table(normalizeExp,file="normalizeExp.txt",sep="\t",quote=F,col.names=F)   
#�������miRNAУ����ı���ֵ��normalizeExp.txt��
#�����header=f����Ϊ������id�����ͷ�����Ե�һ�����ݾ�����������
#col.names=F �õĺã���Ϊ��ͷ������������һ��Ҳ�������������Բ����ظ�������

# 2018/10/02 �����ʼ�   note2 ####
# 1.��ά
x1<-c(1,2,3,4,5,6)
dim(x1)<-c(2,3) 
# 2.�鿴ĳ��ĳ���ж��ٸ�ֵ����ͳ�Ƽ���
summary(z$high==170)
table(z[,2]==170)
# 3.for���(��Ҫ���ĸ�����ʱ���ȸ�ֵΪ0.�� w<-0)
w<-0         
for(i in 1:59){w[i]=i*2+3}
w
# 4.while���
d<-0
d[1]=5
i=1
while(d[i]<121){i=i+1;d[i]=d[i-1]+2}
d
# 5.
t3[which(t3>100)]=100 
# 6.ʹ��apply����
apply(t,2,mean)   ##�м�Ϊ2����ÿ�еľ���
apply(t,1,mean)   ##�м�Ϊ1����ÿ�еľ���
apply(t,2,max)    ##��ÿ���е� ���ֵ��ÿ�Ƴɼ�����߷֣�
apply(t,2,min)    ##��ÿ���е�  ��Сֵ��ÿ�Ƴɼ�����ͷ֣�
apply(t[c('t1','t2','t3')],1,sum)  #��ÿ���˵��ܷ�
which.max(apply(t[c('t1','t2','t3')],1,sum)) #��һ���˵��ܷ����
t$num[which.max(apply(t[c('t1','t2','t3')],1,sum))]  #��3�ſ��ܷ���ߵ��Ǹ���ѧ��
# 7. ɢ��ͼ
plot(t$t1,t$t2,
     main='��ѧ���������Դ����ɼ��Ĺ�ϵ',
     xlab='��ѧ����',
     ylab='���Դ���',
     xlim=c(0,100),#����ͼ�εķֲ������ʵ���������ķ�Χ
     ylim=c(0,100),
     xaxs='i',#set x axis style as internal����ķ��
     yaxs='i',#set y axis style as internal
     col='red',#set the color of plotting symbol to red
     pch=19)#set the plotting symbol to filled dots

# 8.ʹ����ɫ ####
# 8.1. ʹ����ɫRColorBrewer��
plot(women,col="red")
plot(women,col=554)
plot(women,col="#FF0000")
Hex <- rgb(red = 255,green = 0,blue = 0,max=255)
plot(women,col=Hex)
#RColorBrewer�� �ṩ��3����ɫ����(���������ˡ���ɢ)
library(RColorBrewer)
display.brewer.all(type = "seq")  #չʾ�����͵���ɫ,ÿ���9�ֽ�����ɫ
barplot(rep(1,6),col=brewer.pal(9,"YlOrRd")[3:8])

display.brewer.all(type = "div")   #չʾ������
barplot(rep(1,6),col=brewer.pal(11,"BrBG")[3:8])

display.brewer.all(type = "qual")
barplot(rep(1,6),col=brewer.pal(12,"Set3")[3:8])

display.brewer.all(type="all") #�鿴���еģ�ѡ���Լ���Ҫ����ɫ����
#    
plot(1:10)
points(2,4,col="red")
text(2,2,"������",pos=4,col="black")   #��ͼ�ϱ�ǡ���Ҫ��ʾ�����ݣ������뿴�Ǹ�����ͱ���ȥ
#  ���룺text(allDiff$logFC[brca1], -log10(allDiff$FDR[brca1],"brca1",pos=4)
plot(women)
fit <- lm(weight~height,data = women)

plot(women)
abline(fit,col="red",lty=2)
#
par(mfrow=c(1,2))
plot(x=rnorm(10))
plot(women)
par(mfrow=c(1,1))  #�����ˣ�Ҫ�ظ�һ�����ڵ�����

# 8.2.rainbow()
pie(rep(1, 12), col = rainbow(12))
barplot(rep(1,12),col=rainbow(12))
# 8.3.��ɫ�ղ�


# 2017-10-10 ѧϰ��־ ####
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

# 2018/10/02   "@jimmy :sort ��ƽʱ��Ӧ��"  notes ####
#1.����jimmy_basic_heat.R �е�����׼�����֣���˵  sort ��ƽʱ��Ӧ��
# ******************   ���벿��   ***********
library(CLL)
library(ggplot2)
library(reshape2)
library(gpairs)
library(corrplot)

data(sCLLex)
sCLLex=sCLLex[,1:8] ## ����̫�࣬�Ҿ�ȡǰ��8��
group_list=sCLLex$Disease
exprSet=exprs(sCLLex)

head(exprSet)
#1
choose_gene=names(sort(apply(exprSet, 1, mad),decreasing = T)[1:50])  #mad����λ��
choose_matrix=exprSet[choose_gene,]
choose_matrix=scale(choose_matrix)
heatmap(choose_matrix)
#**************************   end   ***********************************

# 2018/10/03  ������ɸѡ���� ####

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
#����1 --vioplot
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

# ����2 ggplot2
 # ���������� ��һ�� n*3 ����ת���ɳ����� 3n*2 
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
#���ǣ�qqq����ĵ�һ�б����factor��ʽ����������Ҫ��chr,�Ӹ�����stringsAsFactors = FALSE,�ͺ�����oh yeah
qqq<-data.frame(factor=id,value=qq,stringsAsFactors = FALSE)

library(ggplot2)
p1<-ggplot(qqq,aes(x=factor,y=value))+geom_violin(aes(x=factor,y=value,fill=factor),alpha=0.7)
p1+geom_boxplot(width=0.05)  #�������width���Ե����������λ��
 
 #����2--����
library("ggplot2")
#����ggplot2��
data1<-read.table("D:\\Rworkspace\\R_data\\violin2.txt",header=T)
#��������
p1<-ggplot(data1,aes(x=Factor,y=value))+geom_violin(aes(x=Factor,y=value,fill=Factor),alpha=0.7)
p1+geom_boxplot(width=0.05)

mytheme<-theme_bw()+theme(panel.grid.major= element_line(color = "white"),panel.grid.minor =element_line(color= "white"))
p1+geom_boxplot(width=0.05)+mytheme+  #ȥ������
  labs(title = "New plot title", subtitle = "A subtitle",x = "New x label",y="y label")+
  theme(legend.position="none")
#p1 + theme(plot.title = element_text(size = rel(2)))  #rel(2)ָ����ԭ���������С��2��
#p1 + theme(panel.background = element_rect(fill = "white", colour = "grey50")) #��屳����Ϊ�հ�
#p2 + theme(legend.box.background = element_rect(), legend.box.margin = margin(6, 6, 6, 6))

#����С����ͼ���������ͬ��Factor�ֱ���ɫ
#���Ӷ���ɢ�㣨Jitteredpoints��
p2<-p1+geom_jitter(alpha=0.3,col="red",show.legend=FALSE)
p2
mytheme<-theme_bw()+theme(panel.grid.major= element_line(color = "white"),panel.grid.minor =element_line(color= "white"),legend.title = element_blank())
mytheme
#���Ļ����������⣬��ȥ���������ߵ�
p1+mytheme
p2+mytheme

# ��ͼ-- ��y shu��  2018/10/03���� ####
d = data.frame(matrix(rnorm(100), ncol=10))
colnames(d) = paste0('t', 1:10)
rownames(d) = paste0('g', 1:10)
library(pheatmap)
pheatmap(d) -> x

require(cowplot)
p  = ggplot(d, aes(t1, t2)) + geom_point(size=5)
plot_grid(x$gtable, p, labels=c('A', 'B'))
# �������simplot������һ����ͼ��Ȼ������ggplot2��ϸ�ڣ���Ҳ��Ҫ˵������ggplot2����ͼ��

# dplyr�����---Ӧ��ʵ�����ݣ��ɻ��������� 2018/10/03####
library(dplyr)
#install.packages("hflights")
library(hflights)
View(hflights)
#    ������ͼ�Ը�dplyr����һЩ�����ҳ��õĹ�������Ҫ���ܡ���Ҫ������
#    1 ����ɸѡ���� select
#    2 ��¼ɸѡ���� filter
#    3 ������ arrange
#    4 ���Σ����㣩���� mutate
#    5 ���ܺ��� summarize
#    6 ���麯�� group_by
#    7 �ಽ�������ӷ� %>%
#    8 ����������� sample_n,sample_frac
hflights_df <- hflights[1:10,];row.names(hflights_df)<-c(1:10)
select(hflights_df,Year,Month,DayOfWeek) # A tibble: 227,496 x 3
select(hflights_df,Year:ArrTime) # A tibble: 227,496 x 6

select(hflights_df,-Year,-DayOfWeek,-DayofMonth,-TailNum)
select(hflights_df,-(Year:AirTime)) #����ѡ�������Ҳ����ɾ��ָ���ı���
filter(hflights_df,Year==2011,Month==1,DepTime==1400)
filter(hflights_df,Year==2011 & Month==1 & DepTime==1400)

filter(hflights_df,Year==2011 & Month==1 & (DepTime==1400 | DepTime==1430) & UniqueCarrier=="AA") #��'�Ĺ�ϵ��|���ű�ʾ��ѡ�����ʱ��Ϊ1400����1430�ĺ���,��UniqueCarrierΪ'AA'

y<-select(hflights_df,Year,DepTime,FlightNum) 
arrange(y,DepTime)# arrange ����ĳ�н�������Ĭ��Ϊ���򣬿���ʹ��desc�������н������С�
arrange(y,desc(DepTime))  #ʹ�ý��� 

mutate(hflights_df,   gain = ArrDelay - DepDelay,   speed = Distance / AirTime * 60) #mutate �������н����������㲢����Ϊ����:

summarise(hflights_df,   delay = mean(DepDelay, na.rm = TRUE))#�����ݿ���������������л��ܲ���, ����һά�Ľ��:
#group_by����ʵ�ֶ����ݽ��з��飬���summarize���������ԶԷ������ݽ��л���ͳ�ơ�
#���պ��չ�˾������л���
summarise(group_by(tbl_hflights, UniqueCarrier),          
          m = mean(AirTime,na.rm = TRUE),          
          sd = sd(AirTime,na.rm = TRUE),          
          cnt = n(),          
          me = median(AirTime,na.rm = TRUE))
#%>%���ಽ�����ӷ�  dplyr���ﻹ��������һ��������,%>%, ʹ��ʱ�����ݼ�����Ϊ��ͷ, Ȼ�����ζԴ����ݽ��жಽ����
hflights_df%>%      
  group_by(UniqueCarrier)%>%     
  summarise(me=mean(AirTime))%>%arrange(me)%>%head(10)
#��ѡ�������sample_n, sample_frac  sample_n���ѡ��ָ����������������������������sample_frac���ѡ��ָ���ٷֱȣ�ռ�������ݼ�����ٷֱȣ�����������

# new ####
  
  
  