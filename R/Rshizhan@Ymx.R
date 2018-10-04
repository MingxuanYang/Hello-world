# -- -- -- -- -- -- -- -- -- --  -- -- -- -- -- -- -- -- -- -- --
#           ***     R��ͳ�Ʒ����е�Ӧ��    ***                 --
#                                                              -- 
#          2018/10/01, by Yang Mingxuan, based on:��R����ʵս��--
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---- --
setwd("D:\\Rworkspace\\Rcode@YMX\\R_files")
rm(list = ls())
opar <- par(no.readonly=TRUE)
# 7.1 ��������  #### 
  rm(list = ls())
  vars <-c('mpg','hp','wt')
  head(mtcars[vars])
 #ʹ��ԭ���ĺ���3�ַ���
# 1a ����ͳ��ָ��
  library(pastecs)
  stat.desc(mtcars[vars]) 
# 1b ����鿴
  aggregate(mtcars[vars],by=list(am=mtcars$am),mean)
  aggregate(mtcars[vars],by=list(am=mtcars$am),sd)
  aggregate(mtcars[vars],by=list(am=mtcars$am,cyl=mtcars$cyl),sd)
# 1c
  library(psych)
  myvars <- c("mpg", "hp", "wt")
  describe(mtcars[myvars])
  library(psych)
  myvars <- c("mpg", "hp", "wt")
  describeBy(mtcars[myvars], list(am=mtcars$am))
 #�Զ��庯����3�ַ���
# 2a 
  mystats <- function(x, na.omit=FALSE){
    if (na.omit)
      x <- x[!is.na(x)]
    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    skew <- sum((x-m)^3/s^3)/n
    kurt <- sum((x-m)^4/s^4)/n - 3
    return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
  }
  
  myvars <- c("mpg", "hp", "wt")
  sapply(mtcars[myvars], mystats)
# 2b
  dstats <- function(x)sapply(x, mystats)
  myvars <- c("mpg", "hp", "wt")
  by(mtcars[myvars], mtcars$am, dstats)
# 2c  
  library(reshape)
  dstats <- function(x)(c(n=length(x), mean=mean(x), sd=sd(x)))
  dfm <- melt(mtcars, measure.vars=c("mpg", "hp", "wt"), 
              id.vars=c("am", "cyl"))
  head(dfm)
  cast(dfm, am + cyl + variable ~ ., dstats)
# cast(dfm, am  + variable ~ ., mystats)
# measure.vars=ָ����Ҫ��������ֵ�ͱ�����id.vars������1�������2�����飬������amҲ�С�variable����ȡ���溬�壬����ǰ����Ƿ��飬
  
# 7.2 �����  ####
  rm(list = ls())
 #7.2.1 R*C������
  library(vcd)
  mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
  mytable  
  addmargins(mytable)
  
  library(gmodels)
  CrossTable(Arthritis$Treatment, Arthritis$Improved)  #�Ƽ�ʹ��
 #7.2.2 ��ά
  mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
  mytable
  ftable(mytable) 
#  margin.table(mytable, 1)
#  margin.table(mytable, 2)
#  margin.table(mytable, c(1,3))
#  ftable(prop.table(mytable, c(1,2)))
#  ftable(addmargins(mytable, c(1, 2)))
#  ftable(addmargins(mytable, c(1, 3)))
#  ftable(addmargins(prop.table(mytable, c(1, 2)),3))*100
# ~ Treatment+Sex+Improved������3���ֱ���1-3������������ϣ��б�����Ȼ����ʹ�ö�ά���ĺ�������ʾ�����
  
 # 7.2.3  ��������-�����Լ��� ####
  library(vcd)
  mytable <- xtabs(~Treatment+Improved, data=Arthritis)
  chisq.test(mytable)
  mytable <- xtabs(~Improved+Sex, data=Arthritis)
  chisq.test(mytable)
  #
  my<- read.table(file = "D:\\R���Ե�ѧϰ\\@@@R ��ϰ��ʵ��С�ף�\\my.txt",header=T)
  chi_test_data=table(my$gender,my$cascon)
  chi_test_data
  
  chisq.test(chi_test_data)
  
#�ڶ��������˾��棬˵���и���С��5�����������Fisher���顣
  # 7.2.3*  ���2*2��McNemar����
  x<-c(49,21,25,107);dim(x)<-c(2,2)
  mcnemar.test(x,correct = F)   
 # 7.2.4  fisher����   
  mytable <- xtabs(~Improved+Sex, data=Arthritis)
  fisher.test(mytable)
#ע��:Fisher����������ڶ�ά3*2��2*3�ȣ�������������ͬ�����ﲻ������2*2����������
  
 # 7.2.5 Chochran-Mantel-Haenszel test��CMH,ԭ�����ǣ�������������ڵ�����������ÿһ���ж������������ġ���
  mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
  mantelhaen.test(mytable)  
# ftable(mytable)
#��֮ǰ��chisq.test������ó���Treatment��Improved�й�����������CMH���������Treatment��Improved��sex��ÿһˮƽ���Ƿ������
#�������p<0.05,˵�����߽��ܵ�������õ��ĸ������Ա��ÿһˮƽ�²����������������Ա���������ҩ���ƵĻ��߽Ͻ��ܰ�ο���Ļ������˸���ĸ��ƣ���   
  
 # 7.2.6 ���ϵ���Ķ��� [2*2��������ó������󣬼������ϵ��]  
  library(vcd)
  mytable <- xtabs(~Treatment+Improved, data=Arthritis)
  assocstats(mytable)
#�������������ά��������phiϵ����pearson����ϵ����Cramer��s Vϵ��  
  kappa(mytable)
#kappa()���������Լ�����������Cohen��s kappaֵ�Լ���Ȩ��kappaֵ����������˵������������Ա�ʾ��λ�����߶���һϵ�ж�����з������ý����һ�³̶ȡ���  
  
# 7.3 ���--������� ����� ƫ��� ####
  rm(list = ls())
 # 7.3.1 �����Լ���[pearson��غ�spearman��kendall���]
  library(psych)
  states<- state.x77[,1:6]
  corr.test(states, use="complete")
#�����޸� method=c("pearson","spearman","kendall")�������������������ؾ���������������Pֵ��

 # 7.3.1 pearson��غ�spearman��kendall���
  # �÷�1
  states<- state.x77[,1:6]
  cov(states)
  cor(states)
  cor(states, method="spearman")
  cor(states,method = "kendall")
  #�÷�2--[���������ֱ���֮������]
  x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
  y <- states[,c("Life Exp", "Murder")]
  cor(x,y)
 # 7.3.2 ƫ���  
  library(ggm)
  pcor(c(1,5,2,3,6), cov(states))
#pcor(u,s)u��һ����ֵ������ǰ������ֵ1 5��ʾҪ�������ϵ���ı����±꣬�������ֵ2 3 6Ϊ������������Ҫ�ų�Ӱ��ı��������±ꡣSΪ������Э������  
#�ڿ��������롢��ä�ʺ͸��б�ҵ�ʵ�Ӱ��ʱ���˿ں�ıɱ��֮������ϵ��Ϊ0.346��  

# 7.4 T����  ####
  rm(list = ls())
 #7.4.1 ������t���� --�������ֵ�ıȽ�
  x <- c(20.99,20.41,20.10,20.00,20.91,22.60,20.99,20.42,20.90,22.99,23.12,20.89)
  t.test(x, alternative = "greater", mu = 20.7 )#��Ŀ��֪mu
  # ���⣺���ݹ�ʽ���tֵ
  x <- 74.2; mu <- 72; thita <- 6.5;  n <- 25
  (t <- (x-mu) / (thita/sqrt(n)))   #������n-1����n,thita�����������ı�׼�mu�Ǿ���
  #��pt()����������tֵ�����ɶ�df��n-1�����õ�pֵ
  p <- pt(t,df=24); p
  
 #7.4.2 ���[��ֵd]��t����--  [���� ��ֵd������̬�ֲ�]
  sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x),sd=sd(x))))
  with(UScrime, t.test(U1, U2, paired=TRUE))
  # t.test(UScrime$U1, UScrime$U2, paired=TRUE)
#����ľ�ֵ��61.5���㹻�󣬿��Ա�֤�ܾ��곤���������Ե�ƽ��ʧҵ����ͬ�ļ��衣�������Ե�ʧҵ�ʸ��ߡ�  

 #7.4.3  2������������t����
  # ��̬�Լ���ͷ������Լ���
  library(MASS)
  shapiro.test(UScrime$Prob)
  qqPlot(lm(Prob ~ So, data=UScrime), 
         simulate=TRUE, main="Q-Q Plot", labels=FALSE)
  var.test(Prob ~ So, data=UScrime)
  bartlett.test(Prob ~ So, data=UScrime)
  #
  t.test(Prob ~ So, data=UScrime,var.equal=TRUE) # var.equal=TRUE �ڷ�����ȵ�������
# t.test(y~x,data)��t.test(y1,y2)  # y1,y2���������ֵ�ͱ���
#���ּ��飬Ĭ���Ƿ����ȡ������Լ�����var.equal=TRUE 

# 7.5 �Ⱥͼ���  ####
  rm(list = ls())
 #7.5.1 ��������������Ⱥͼ���
  library(MASS)
  with(UScrime, by(Prob, So, median))
  wilcox.test(Prob ~ So, data=UScrime)
 #7.5.2 �����Ⱥͼ��飬��Ե�����
  sapply(UScrime[c("U1", "U2")], median)
  with(UScrime, wilcox.test(U1, U2, paired=TRUE)) 
 #7.5.3 Kruskal��Wallis����
  states <- data.frame(state.region, state.x77)
  kruskal.test(Illiteracy ~ state.region, data=states)
# kruskal.test(y~A,data)
 #7.5.4 Friedman����
# friedman.test(Y~A|B,data)  #y����ֵ�ͽ������,A��һ���������,��B��һ�������϶�ƥ��۲���������.
 #7.5.5 ���رȽ�
  source("http://www.statmethods.net/RiA/wmc.txt")              
  states <- data.frame(state.region, state.x77)
  wmc(Illiteracy ~ state.region, data=states, method="holm")
  
# 8. �ع�--[�����Իع顢����ʽ�ع顢��Ԫ���Իع�]  ####
  rm(list = ls())
 #8.1 ��ɢ��ͼ���۲�
  library(car)
  scatterplot(weight ~ height, data=women,
              spread=FALSE, smoother.args=list(lty=2), pch=19,
              main="Women Age 30-39",
              xlab="Height (inches)",
              ylab="Weight (lbs.)")
 #8.2 �����Իع�
  fit <- lm(weight ~ height, data=women)
  summary(fit)
#  women$weight
  fitted(fit)  #���ֵ
  residuals(fit)  #�в�ֵ
  plot(women$height,women$weight,
       main="Women Age 30-39", 
       xlab="Height (in inches)", 
       ylab="Weight (in pounds)")
  abline(fit)
 # 8.3 ����ʽ�ع�
  fit2 <- lm(weight ~ height + I(height^2), data=women)
  summary(fit2)
  plot(women$height,women$weight,
       main="Women Age 30-39",
       xlab="Height (in inches)",
       ylab="Weight (in lbs)")
  lines(women$height,fitted(fit2))
 #8.4 ��Ԫ���Իع�
  #step1.�鿴�������
  states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                       "Illiteracy", "Income", "Frost")])
  cor(states)
  library(car)
  scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2),
                    main="Scatter Plot Matrix")
  #step2.����ģ��
  states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                       "Illiteracy", "Income", "Frost")])
  fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
  summary(fit)
 #8.5 �н�����Ķ�Ԫ���Իع�
  fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
  summary(fit)
  
  library(effects)
  plot(effect("hp:wt", fit,, list(wt=c(2.2, 3.2, 4.2))), multiline=TRUE)
 #8.6 �ع����  #### 
 #8.6.1. ����1-�����ֱ����̬�ԡ����������ԡ�����ۺ�5������
 #8.6.1.1 ��̬�Լ���1--QQͼ
  library(car)
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
  qqPlot(fit, labels=row.names(states), id.method="identify",
         simulate=TRUE, main="Q-Q Plot")
#  states["Nevada",]
#  fitted(fit)["Nevada"]
#  residuals(fit)["Nevada"]
#  rstudent(fit)["Nevada"]
 #8.6.1.1 ��̬�Լ���2--ѧ�����в�-��״ͼ
  residplot <- function(fit, nbreaks=10) {
    z <- rstudent(fit)
    hist(z, breaks=nbreaks, freq=FALSE,
         xlab="Studentized Residual",
         main="Distribution of Errors")
    rug(jitter(z), col="brown")
    curve(dnorm(x, mean=mean(z), sd=sd(z)),
          add=TRUE, col="blue", lwd=2)
    lines(density(z)$x, density(z)$y,
          col="red", lwd=2, lty=2)
    legend("topright",
           legend = c( "Normal Curve", "Kernel Density Curve"),
           lty=1:2, col=c("blue","red"), cex=.7)
  }
  residplot(fit) 
#����һ�������Ե���Ⱥ�㣬���ܺõط�������̬�ֲ���
 #8.6.1.2 �в�����Լ��� #���Ķ�����
  durbinWatsonTest(fit)   # simulate=FALSE������������Pֵ����ÿ�����в���ʱ��õĽ���������в�ͬ��
#pֵ��������p=0.282��˵����������ԣ������֮��������ͺ��lag=1���������ݼ���
#ÿ�����ݶ��������һ�����ݽ��бȽϵġ��ü���������ʱ����������ݣ����ڷǾۼ��͵����ݲ������á�
 
 #8.6.1.3 ����--�ɷֲв�ͼ..��������Ա���֮���Ƿ�ʷ����Թ�ϵ
  library(car)
  crPlots(fit)
#��ͼ�δ��ڷ����ԣ���˵������ܶ�Ԥ������ĺ�����ʽ��ģ������֣���ô����Ҫ����һЩ���߳ɷ֣��������ʽ����һ�������������б任������log(X)  

 #8.6.1.4 ͬ����
  library(car)
  ncvTest(fit)
  spreadLevelPlot(fit)
#�Ʒּ��鲻������p=0.19����˵�����㷽�����裻ͨ���ֲ�ˮƽͼ������һ�㣬���еĵ���ˮƽ��������������Χ��ˮƽ����ֲ���
#��ͼ����ʾ���˷�ˮƽ���ƣ������ݴ�ת��Ϊ0.5���ڻع��ʽ���� Y ����Y�����ܻ�ʹģ������ͬ�����ԡ��������ݴ�Ϊ0����ʹ�ö����任��
# ncvTest()��������һ���Ʒּ��飬�����Ϊ����䣬�������Ϊ�����������ֵˮƽ�ı仯���仯����������������˵�������췽����  
# spreadLevelPlot()��������һ�����������������ߵ�ɢ��ͼ��չʾ��׼���в����ֵ�����ֵ�Ĺ�ϵ��        
  
 #8.6.1.5 �ۺ��Եļ���--
  library(gvlma)
  gvmodel <- gvlma(fit) 
  summary(gvmodel) #����һ�е�Global StatΪ�ۺϼ����Pֵ�������л���ƫб�ȡ���ȡ��췽��ļ���
  
 #8.6.2 ����2.һ��Ļع����
  opar <- par(no.readonly=TRUE)
  fit <- lm(weight ~ height, data=women)
  par(mfrow=c(2,2))
  plot(fit)
  par(opar)
#  fit2 <- lm(weight ~ height + I(height^2), data=women)
#  opar <- par(no.readonly=TRUE)
#  par(mfrow=c(2,2))
#  plot(fit2)
#  par(opar)
  
 #8.6.3 ���ع����� ���--VIF��Variance Inflation Factor�������������ӣ�
  library(car)
  vif(fit) 
  sqrt(vif(fit)) > 2 # problem?
#VIF��ƽ������ʾ�����ع��������������������Ϊ��ģ���޹ص�Ԥ������ĳ̶ȣ���˶�������  
# һ��ԭ���£� sqrt(vif) >2�ͱ������ڶ��ع��������⡣ 
  
 #8.6.4 �쳣�۲�ֵ--��Ⱥ�㡢�߸ܸ�ֵ�㡢ǿӰ���  ***
 #8.6.4.1 -- ��Ⱥ��
  library(car)
  outlierTest(fit)
#  ע�⣬�ú���ֻ�Ǹ��ݵ�����󣨻����򸺣��в�ֵ�����������ж��Ƿ�����Ⱥ�㡣������������˵�����ݼ���û����Ⱥ�㣻���������������ɾ������Ⱥ�㣬Ȼ���ټ����Ƿ���������Ⱥ����ڡ�  
# ���Կ���Nevada���ж�Ϊ��Ⱥ�㣨p=0.048��  
  
 #8.6.4.2 -- �߸ܸ�ֵ��
  hat.plot <- function(fit) {
    p <- length(coefficients(fit))
    n <- length(fitted(fit))
    plot(hatvalues(fit), main="Index Plot of Hat Values")
    abline(h=c(2,3)*p/n, col="red", lty=2)
    identify(1:n, hatvalues(fit), names(hatvalues(fit)))
  }
  hat.plot(fit)
#�߸ܸ�ֵ�Ĺ۲���ͨ��ñ��ͳ������hat statistic���жϣ�һ����˵�����۲���ñ��ֵ����ñ�Ӿ�ֵ��2��3�����������϶�Ϊ�߸ܸ�ֵ�㡣  
 #8.6.4.3 -- ǿӰ���
  #����1 --  Cook���룬���Dͳ����
  cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
  plot(fit, which=4, cook.levels=cutoff)
  abline(h=cutoff, lty=2, col="red")  #ͼ�ο����ж�Alaska�� Hawaii��Nevada��ǿӰ��㡣��ɾ����Щ�㣬���ᵼ�»ع�ģ�ͽؾ����б�ʷ��������仯��
  #����2 -- ��������ͼ
  library(car)
  avPlots(fit, ask=FALSE, id.method="identify") #ͼ�м����AlaskaΪǿӰ���
#��ɾ����Alaska��ֱ�߽��������ƶ�����ʵ�ϣ�ɾ��Alaska�� Income�Ļع�ϵ�������0.000 06��Ϊ???0.000 85��  
 #8.6.4.4 �ۺϵ��жϣ���Ⱥ�㡢�߸ܸ�ֵ�㡢ǿӰ��㡣
  library(car)
  influencePlot(fit, id.method="identify", main="Influence Plot", 
                sub="Circle size is proportial to Cook's Distance" )
#��ӳ��Nevada��Rhode Island����Ⱥ�㣬 New York�� California�� Hawaii��Washington�и߸ܸ�ֵ�� Nevada�� Alaska��HawaiiΪǿӰ��㡣
#�����곬��+2��С��???2���ݿɱ���Ϊ����Ⱥ�㣬ˮƽ�ᳬ��0.2��0.3�����и߸ܸ�ֵ��ͨ��ΪԤ��ֵ����ϣ���ԲȦ��С��Ӱ��ɱ�����ԲȦ�ܴ�ĵ�����Ƕ�ģ�Ͳ����Ĺ�����ɵĲ��ɱ���Ӱ���ǿӰ���  
  
 #8.7 ����ת����-- ̽��
  library(car)
  summary(powerTransform(states$Murder)) #��ģ��Υ������̬����ʱ��ͨ�����Զ���Ӧ��������ĳ�ֱ任�� 
  # Box-Tidwell Transformations to linearity
  library(car)
  boxTidwell(Murder~Population+Illiteracy,data=states) #��Υ�������Լ���ʱ����Ԥ��������б任������Ƚ����á� 

 # 8.8 ѡ������ģ��  ####
 # 8.8.1 �Ƚ�ģ�� --  ����1
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
             data=states)
  fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
  anova(fit2, fit1)
# ���ڼ��鲻������p=0.994����������ǿ��Եó����ۣ�����Ҫ���������������ӵ�����ģ���У����Խ����Ǵ�ģ����ɾ����  
  
 # 8.8.2 �Ƚ�ģ�� -- ����2
  fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
             data=states)
  fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
  AIC(fit1,fit2)  #AIC��С��Ϊ����ģ��
 # 8.8.3 ����𲽻ع鷨 Backward stepwise
  library(MASS)
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
            data=states)
  stepAIC(fit, direction="backward")
#��һ���� Frost��ɾ���� AIC��97.75���͵�95.75���ڶ����� Income��ɾ�� AIC�����½�����Ϊ93.76��Ȼ����ɾ��������������AIC�������ֹѡ����̡�  
  
  # 8.8.4 ȫ�Ӽ��ع� ѡ�� All subsets regression
  library(leaps)
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +
                       Frost, data=states, nbest=4)
  plot(leaps, scale="adjr2")
#��һ���У�ͼ�ײ���ʼ�������Կ�����intercept���ؾ����Income��ģ�͵���Rƽ��Ϊ0.33����intercept��Population��ģ�͵���Rƽ��Ϊ0.1��
  library(car)
  subsets(leaps, statistic="cp",main="Cp Plot for All Subsets Regression") # ��Ҫ����
  abline(1,1,lty=2,col="red") 
#��ῴ�����ڲ�ͬ�Ӽ���С������Mallows Cpͳ�������ĸ����ģ�͡�Խ�õ�ģ����ؾ����б�ʾ�Ϊ1��ֱ��Խ����  
  
 # 8.9 ���η���
  #8.9.1 k�ؽ�����֤ --R������֤
  #��k �ؽ�����֤�У���������Ϊk����������������k???1�������������Ϊѵ����������1����������Ϊ����������������k ��Ԥ�ⷽ�̣���¼k ������������Ԥ����ֽ����Ȼ������ƽ��ֵ��
  shrinkage <- function(fit,k=10){
    require(bootstrap)
    
    # define functions 
    theta.fit <- function(x,y){lsfit(x,y)}
    theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
    
    # matrix of predictors
    x <- fit$model[,2:ncol(fit$model)]
    # vector of predicted values
    y <- fit$model[,1]
    
    results <- crossval(x,y,theta.fit,theta.predict,ngroup=k)
    r2 <- cor(y, fit$fitted.values)**2 # raw R2 
    r2cv <- cor(y,results$cv.fit)**2 # cross-validated R2
    cat("Original R-square =", r2, "\n")
    cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
    cat("Change =", r2-r2cv, "\n")
  }
  # using it
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  fit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=states)
  shrinkage(fit)  #���Կ��������ڳ�ʼ������Rƽ����0.567�������ֹ��ˡ��������ݸ��õķ�������ʹ����ǽ�����֤���Rƽ����0.448����
  fit2 <- lm(Murder~Population+Illiteracy,data=states)
  shrinkage(fit2)  #�������ý�����֤����ѡ���������磬������Ԥ�������Population��Illiteracy����ģ�ͣ���ȫ����ģ��Rƽ�����ٵø���
  
 #8.9.2 Ԥ���������Ҫ�� --�Ƚ�
 #8.9.2.1 ����1 -- ��׼���ع�ϵ��
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  zstates <- as.data.frame(scale(states))
  zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
  coef(zfit)  # ����ΪIlliteracy������Ҫ��Ԥ���������Frost�����Ҫ�ġ�
  
 # 8.9.2.2 ���Ȩ�ط�  #�������в�ͬ�ĵط�
  relweights <- function(fit,...){
    R <- cor(fit$model)
    nvar <- ncol(R)
    rxx <- R[2:nvar, 2:nvar]
    rxy <- R[2:nvar, 1]
    svd <- eigen(rxx)
    evec <- svd$vectors
    ev <- svd$values
    delta <- diag(sqrt(ev))
    lambda <- evec %*% delta %*% t(evec)
    lambdasq <- lambda ^ 2
    beta <- solve(lambda) %*% rxy
    rsquare <- colSums(beta ^ 2)
    rawwgt <- lambdasq %*% beta ^ 2
    import <- (rawwgt / rsquare) * 100
    import <- as.data.frame(import)
    row.names(import) <- names(fit$model[2:nvar])
    names(import) <- "Weights"
    import <- import[order(import),1, drop=FALSE]
    dotchart(import$Weights, labels=row.names(import),
             xlab="% of R-Square", pch=19,
             main="Relative Importance of Predictor Variables",
             sub=paste("Total R-Square=", round(rsquare, digits=3)),
             ...)
    return(import)
  }
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
  relweights(fit, col="blue")
# 9 �������  ####
  setwd("D:/Rworkspace/Rshizhan_code_@liurui/work_dir@mine")
  rm(list = ls())
  opar <- par(no.readonly=TRUE)
 #9.1 �����ط������ ANOVA ####
 # 9.1.1 ��̬�Լ���  Q-Qͼ
  library(car)
  qqPlot(lm(response ~ trt, data=cholesterol), 
         simulate=TRUE, main="Q-Q Plot", labels=FALSE)
 # 9.1.2 �������Լ���
  bartlett.test(response ~ trt, data=cholesterol)
 # 9.1.3 - �����ط������ ANOVA  
  library(multcomp)
  attach(cholesterol)
  table(trt)     
  aggregate(response, by=list(trt), FUN=mean) 
  aggregate(response, by=list(trt), FUN=sd) 
  fit <- aov(response ~ trt)                                  
  summary(fit)
  library(gplots)
  plotmeans(response ~ trt, xlab="Treatment", ylab="Response", 
            main="Mean Plot\nwith 95% CI")
  detach(cholesterol)
 # 9.1.3-1 ����1�� ���رȽ� �����Ƚ�
  TukeyHSD(fit)
# detach("package::HH") #���TukeyHSD()�ܵ���ͻ  
  par(las=2)
  par(mar=c(5,8,4,2)) 
  plot(TukeyHSD(fit))
  par(opar)
 # 9.1.3-2 ����2�� ���رȽ� �����Ƚ�Multiple comparisons
  library(multcomp)  #multcomp���е�glht()�����ṩ�˶��ؾ�ֵ�Ƚϸ�Ϊȫ��ķ�����������������ģ�ͣ��籾�¸�������Ҳ�����ڹ�������ģ�ͣ�����13�£�
  par(mar=c(5,4,6,2))
  tuk <- glht(fit, linfct=mcp(trt="Tukey"))
  plot(cld(tuk, level=.05),col="lightgrey")
  par(opar)  #����ͬ��ĸ���飨������ͼ��ʾ��˵����ֵ���첻������
 # 9.1.4 ��Ⱥ��
  library(car)
  outlierTest(fit)
  
 #9.2 -������Э������� One-way ANCOVA ####
  #9.2.1 ����
  data(litter, package="multcomp")
  attach(litter)
  table(dose) 
  aggregate(weight, by=list(dose), FUN=mean)
  fit <- aov(weight ~ gesttime + dose)   #����ʱ��ΪЭ������д��ǰ��                          
  summary(fit)
  # 
  library(effects)
  effect("dose", fit) #��Ҫ��ȡ���������ֵ������ȥ��Э����ЧӦ������ֵ
  # 9.2.2 ���رȽϣ��û�����[����c(3, -1, -1, -1)�趨��һ�����������ľ�ֵ���бȽϡ�]
  library(multcomp)
  contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1))
  summary(glht(fit, linfct=mcp(dose=contrast)))  #��ˣ����Եó�δ��ҩ���������ҩ�����µĳ������ظߵĽ��ۡ�
  # 9.2.3 ����ع�б�ʵ�ͬ���� - 
  library(multcomp)
  fit2 <- aov(weight ~ gesttime*dose, data=litter)
  summary(fit2) #���Կ�������ЧӦ��������֧����б����ȵļ��衣�����費���������Գ��Ա任Э���������������ʹ���ܶ�ÿ��б�ʶ������͵�ģ�ͣ�
  # 9.2.4 ������ӻ�[���Ի����������Э����������֮��Ĺ�ϵͼ��]
  library(HH)
  ancova(weight ~ gesttime + dose, data=litter)
 #9.3 ˫���ط������ ####
  # - Two way ANOVA
  attach(ToothGrowth)
  table(supp,dose)
  aggregate(len, by=list(supp=supp,dose=dose), FUN=mean)
  aggregate(len, by=list(supp=supp,dose=dose), FUN=sd)
  dose <- factor(dose)
  fit <- aov(len ~ supp*dose)
  summary(fit)
  # ���ӻ�
  library(HH)
  interaction2wt(len~supp*dose)
 #9.4 �ظ������������  ####
  CO2$conc <- factor(CO2$conc)
  w1b1 <- subset(CO2, Treatment=='chilled')
  fit <- aov(uptake ~ (conc*Type) + Error(Plant/(conc)), w1b1)
  summary(fit)
  # ���ӻ�
  par(las=2)
  par(mar=c(10,4,4,2))
  with(w1b1, 
       interaction.plot(conc,Type,uptake, 
                        type="b", col=c("red","blue"), pch=c(16,18),
                        main="Interaction Plot for Plant Type and Concentration"))
  boxplot(uptake ~ Type*conc, data=w1b1, col=(c("gold","green")),
          main="Chilled Quebec and Mississippi Plants", 
          ylab="Carbon dioxide uptake rate (umol/m^2 sec)")
  par(opar)
  # ��ͼ3
  attach(w1b1)
  interaction2wt(uptake ~ Type*conc)
  detach(w1b1)
 #9.5 ��Ԫ�������  ####
 #9.5.1 �����ض�Ԫ������� 
  library(MASS)
  attach(UScereal)
  shelf <- factor(shelf)
  y <- cbind(calories, fat, sugars)
  aggregate(y, by=list(shelf), FUN=mean)
  cov(y)
  fit <- manova(y ~ shelf)
  summary(fit)
  summary.aov(fit)
  # ���� ��Ԫ��̬��
  center <- colMeans(y)
  n <- nrow(y)
  p <- ncol(y)
  cov <- cov(y)
  d <- mahalanobis(y,center,cov)
  coord <- qqplot(qchisq(ppoints(n),df=p),
                  d, main="QQ Plot Assessing Multivariate Normality",
                  ylab="Mahalanobis D2")
  abline(a=0,b=1)
  identify(coord$x, coord$y, labels=row.names(UScereal))
 # 9.5.2 �Ƚ���Ԫ�������[���������Ԫ��̬�ͷ�������ʱ]
  library(rrcov)
  Wilks.test(y,shelf, method="mcd")  # this can take a while
#�ӽ���������Ƚ��������Ⱥ���Υ��MANOVA�������������У�������һ����֤�˴洢�ڻ��ܶ������в��͵ײ��Ĺ���Ӫ���ɷֺ�����ͬ��  
 # 9.6 #  ����������  ====
  block <- c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)) ##��������
  treat <- c(rep(c("A", "B", "C"), 4)) ##�������
  alt <- c(76, 86, 115, 12, 38, 85, 40, 81, 103, 12, 33, 57) 
  alt <- data.frame(block= block, treat= treat, alt= alt) ##�������ݿ�
  alt$block <- as.factor(alt$block) ##���ӻ�
  alt$treat <- as.factor(alt$treat) ##���ӻ�
  model <- aov(alt ~ treat+ block, data= alt) ##������鷽�����
  summary(model)             ##�鿴ģ��
  
  TukeyHSD(model)  ##�����Ƚ� 
  res <- residuals(model) ##���ɲв�
  shapiro.test(res) ##�����в���̬��
  par(mfrow=c(2,2))
  plot(model)
  par(mfrow=c(1,1))
  
#10. logistics�ع� ####
 #10.1 get summary statistics
  data(Affairs, package="AER")
  summary(Affairs)
  table(Affairs$affairs)
 #10.2 create binary outcome variable
  Affairs$ynaffair[Affairs$affairs > 0] <- 1
  Affairs$ynaffair[Affairs$affairs == 0] <- 0
  Affairs$ynaffair <- factor(Affairs$ynaffair, 
                             levels=c(0,1),
                             labels=c("No","Yes"))
  table(Affairs$ynaffair)
  
 #10.3����ģ��1-- fit full model
  fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                    religiousness + education + occupation +rating,
                  data=Affairs,family=binomial())
  summary(fit.full)
  
 #10.4 ���������󣬽���ģ��2 fit reduced model
  fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + 
                       rating, data=Affairs, family=binomial())
  summary(fit.reduced)
 #10.5 compare models
  anova(fit.reduced, fit.full, test="Chisq")
 #10.6 ���� ��ֵ�� interpret coefficients
  coef(fit.reduced)
  exp(coef(fit.reduced))  #��ֵ��
  exp(confint(fit.reduced)) #ϵ����95%��������
 #10.7[���⣬�������ݣ��Զ�������������ģ��]calculate probability of extramariatal affair by marital ratings
  testdata <- data.frame(rating = c(1, 2, 3, 4, 5),
                         age = mean(Affairs$age),
                         yearsmarried = mean(Affairs$yearsmarried),
                         religiousness = mean(Affairs$religiousness))
  testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
  testdata
  
 #10.7 calculate probabilites of extramariatal affair by age
  testdata <- data.frame(rating = mean(Affairs$rating),
                         age = seq(17, 57, 10), 
                         yearsmarried = mean(Affairs$yearsmarried),
                         religiousness = mean(Affairs$religiousness))
  testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
  testdata
  
 #10.8 �������� evaluate overdispersion
  fit <- glm(ynaffair ~ age + yearsmarried + religiousness +
               rating, family = binomial(), data = Affairs)
  fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness +
                  rating, family = quasibinomial(), data = Affairs)
  pchisq(summary(fit.od)$dispersion * fit$df.residual,  
         fit$df.residual, lower = F)
# ��2��ģ�� ʹ��family = quasibinomial()���˴�pֵ��0.34����Ȼ��������p > 0.05���������ǿ��������Ϊ�����ڹ������Ƶ����ġ�
#11. ���ɻع� ####
 #11.1 ����
  data(breslow.dat, package="robust")
  names(breslow.dat)
  summary(breslow.dat[c(6, 7, 8, 10)])
 # plot distribution of post-treatment seizure counts
  opar <- par(no.readonly=TRUE)
  par(mfrow=c(1, 2))
  attach(breslow.dat)
  hist(sumY, breaks=20, xlab="Seizure Count", 
       main="Distribution of Seizures")
  boxplot(sumY ~ Trt, xlab="Treatment", main="Group Comparisons")
  par(opar)
 #11.2 fit regression
  fit <- glm(sumY ~ Base + Age + Trt, data=breslow.dat, family=poisson())
  summary(fit)
 #11.3 ������� interpret model parameters
  coef(fit)
  exp(coef(fit))
 #11.4 �������� evaluate overdispersion
  deviance(fit)/df.residual(fit)
  library(qcc)
  qcc.overdispersion.test(breslow.dat$sumY, type="poisson")
#�����Լ����pֵ��ȻС��0.05����һ������ȷʵ���ڹ������ơ�
 # fit model with quasipoisson
  fit.od <- glm(sumY ~ Base + Age + Trt, data=breslow.dat,
                family=quasipoisson())
  summary(fit.od) #ͨ����family = "quasipoisson"�滻family = "poisson"�������������ơ�
  
  
  
  
  
#12. ʱ�����з��� ####
 # 12.1 ��������
  sales <- c(18, 33, 41,  7, 34, 35, 24, 25, 24, 21, 25, 20, 
             22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
  tsales <- ts(sales, start=c(2003, 1), frequency=12) 
  tsales
  plot(tsales)
  
  start(tsales) 
  end(tsales)
  frequency(tsales)
  
  tsales.subset <- window(tsales, start=c(2003, 5), end=c(2004, 6))
  tsales.subset
 # 12.2 ���ƶ�ƽ�� ƽ������
  library(forecast)
  opar <- par(no.readonly=TRUE)
  par(mfrow=c(2,2))
  ylim <- c(min(Nile), max(Nile))
  plot(Nile, main="Raw time series")
  plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
  plot(ma(Nile, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
  plot(ma(Nile, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
  par(opar)
 # 12.3 �����Էֽ⣬�˽����� using slt()
  plot(AirPassengers)                                               
  lAirPassengers <- log(AirPassengers)
  plot(lAirPassengers, ylab="log(AirPassengers)")
  fit <- stl(lAirPassengers, s.window="period")           
  plot(fit)
  fit$time.series                                 
  exp(fit$time.series)
  
  par(mfrow=c(2,1))
  library(forecast)
  monthplot(AirPassengers, xlab="",  ylab="")  
  seasonplot(AirPassengers, year.labels="TRUE", main="")
  par(opar)
 # 12.4 ָ��ƽ��ģ��
  #��ָ��ƽ��[����ˮƽ���ʱ���i�������]
  library(forecast)
  fit <- ets(nhtemp,model = "ANN")
  fit
  forecast(fit,1)
  accuracy(fit)
  #˫ָ��ƽ����Holtָ��ƽ��[ˮƽ���������]
  library(forecast)
  fit <- ets(nhtemp,model = "AAN")
  fit
  forecast(fit,1)
  accuracy(fit)
  #��ָ��ƽ����Holt-Wintersָ��ƽ��[ˮƽ����������ЧӦ]
  fit <- ets(log(AirPassengers),model="AAA")      
  fit #�����⻬��������ˮƽ��0.82��������0.0004��������0.012
  accuracy(fit)
  
  pred <- forecast(fit, 5)                                  
  pred
  plot(pred, main="Forecast for Air Travel", 
       ylab="Log(AirPassengers)", xlab="Time")       
  pred$mean <- exp(pred$mean)
  pred$lower <- exp(pred$lower)
  pred$upper <- exp(pred$upper)
  p <- cbind(pred$mean, pred$lower, pred$upper)
  dimnames(p)[[2]] <- c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
  p
  # ets()�Զ�ѡ��Ԥ��
  library(forecast)
  fit <- ets(JohnsonJohnson)
  fit #��ѡ�е�ģ��ͬʱ�пɳ��������������������
  plot(forecast(fit), main="Johnson and Johnson Forecasts", 
       ylab="Quarterly Earnings (Dollars)", xlab="Time")
 #12.5. ARIMAģ�� & SARIMAģ��
  # 12.5.1 ��������
  library(forecast)
  library(tseries)
  plot(Nile)  
#�жϣ���ͼ�Ͽ���������ȣ�������ȣ���log(y)������һ������
#���е�һ�β�ֿ����Ƴ������е��������ƣ����β���Ƴ����������ƣ����β���Ƴ����������ơ�
  ndiffs(Nile) #�ҳ����ʺϵ�dֵ��������1
  dNile <- diff(Nile)                                              
  plot(dNile)
  adf.test(dNile)  # �����log=?,�����Զ���adf.test(dNile,k=12)
  
  par(mfrow=c(2,1))
  Acf(dNile)  #����ʹ�õ���forecast()��ĺ�����Ҳ�����û�����acf()pacf()
  Pacf(dNile)
  par(mfrow=c(1,1))
 # 12.5.2 �������ģ��
  fit <- arima(Nile, order=c(0,1,1)) #����acfͼ/pacfͼ��ȷ��p/q                                 
  fit
  accuracy(fit)
 # 12.5.3 ģ�ͼ���
  qqnorm(fit$residuals)     
  qqline(fit$residuals)
  Box.test(fit$residuals, type="Ljung-Box") #Pֵ����0.05��˵��ģ��ok
#ģ�͵Ĳв�û��ͨ�����������飬�����ǿ�����Ϊ�в�������ϵ��Ϊ�㡣 ARIMAģ���ܽϺõ���ϱ����ݡ� 
  
 # 12.5.4 ģ��Ԥ��
  forecast(fit, 3)
  plot(forecast(fit, 3), xlab="Year", ylab="Annual Flow")

 # 12.5.6 �Զ�Ԥ�� - Automated ARIMA forecasting
  library(forecast)
  fit <- auto.arima(Nile,trace = T)
  fit
  forecast(fit, 3)
  accuracy(fit)
  plot(forecast(fit, 3), xlab="Year", ylab="Annual Flow")
 # 12.5.7 SARIMAģ�� ####
  y<-read.csv(file.choose(),header = T)
  ty<-ts(y,start = c(2005,1),frequency = 12)
  ty
  plot(ty,ylab='incidence/10???',xlab='Time')
  #
  ndiffs(ty) #�ҳ����ʺϵ�dֵ��������1
#  dty <- diff(ty) #һ�ײ��  
  sdty<-diff(diff(ty),lag=12)  # 1�ײ�� + 1�׼��ڲ��
  plot(sdty)
  adf.test(sdty)  # �����log=?,�����Զ���
  #
  par(mfrow=c(2,1))
  Acf(sdty)  #����ʹ�õ���forecast()��ĺ�����Ҳ�����û�����acf()pacf()
  Pacf(sdty)
  par(mfrow=c(1,1))  #��ͼ���ж�p1 q1 P1 Q1
  #
  auto.arima(ty,trace=T)  #������ģ�� ARIMA(2,0,2)(0,1,1)[12] 
  fit0 <- auto.arima(ty,trace=T)
  accuracy(fit0)
  #
  fit<-arima(ty,order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12),method = 'ML')
  fit
  accuracy(fit)
  
  # fit<-fit0  #�ڱȽ�fit0��fit��ѡ���ŵ�ģ��
  qqnorm(fit$residuals)
  qqline(fit$residuals)
  Box.test(fit$residuals,type = "Ljung-Box")
  
  forecast(fit,12)
  plot(forecast(fit,12),xlab='Year',ylab='prevalence/10��')
  plot(forecast(fit,12),xlab='Year',type='o',ylab='prevalence/10��')
  
#### plot  ####

  
#12R
  options(digits = 2)
  cor(mtcars)
  library(corrgram)
  corrgram(mtcars,order = TRUE,lower.panel = panel.shade,upper.panel = panel.pie,text.panel = panel.txt,main="Correlogram of mtcars intercorrelations")
  
  
  
  
  
  
  
  
  