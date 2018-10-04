# -- -- -- -- -- -- -- -- -- --  -- -- -- -- -- -- -- -- -- -- --
#           ***     R在统计分析中的应用    ***                 --
#                                                              -- 
#          2018/10/01, by Yang Mingxuan, based on:《R语言实战》--
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---- --
setwd("D:\\Rworkspace\\Rcode@YMX\\R_files")
rm(list = ls())
opar <- par(no.readonly=TRUE)
# 7.1 基本描述  #### 
  rm(list = ls())
  vars <-c('mpg','hp','wt')
  head(mtcars[vars])
 #使用原本的函数3种方法
# 1a 基本统计指标
  library(pastecs)
  stat.desc(mtcars[vars]) 
# 1b 分组查看
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
 #自定义函数的3种方法
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
# measure.vars=指明了要分析的数值型变量，id.vars表明了1个分组或2个分组，单独用am也行。variable本身取字面含义，它的前面的是分组，
  
# 7.2 交叉表  ####
  rm(list = ls())
 #7.2.1 R*C列联表
  library(vcd)
  mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
  mytable  
  addmargins(mytable)
  
  library(gmodels)
  CrossTable(Arthritis$Treatment, Arthritis$Improved)  #推荐使用
 #7.2.2 多维
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
# ~ Treatment+Sex+Improved把他们3个分别当作1-3，可以两两组合，列表。依然可以使用二维表的函数来表示结果。
  
 # 7.2.3  卡方检验-独立性检验 ####
  library(vcd)
  mytable <- xtabs(~Treatment+Improved, data=Arthritis)
  chisq.test(mytable)
  mytable <- xtabs(~Improved+Sex, data=Arthritis)
  chisq.test(mytable)
  #
  my<- read.table(file = "D:\\R语言的学习\\@@@R 练习（实验小白）\\my.txt",header=T)
  chi_test_data=table(my$gender,my$cascon)
  chi_test_data
  
  chisq.test(chi_test_data)
  
#第二个出现了警告，说明有格子小于5，所以最好用Fisher检验。
  # 7.2.3*  配对2*2的McNemar检验
  x<-c(49,21,25,107);dim(x)<-c(2,2)
  mcnemar.test(x,correct = F)   
 # 7.2.4  fisher检验   
  mytable <- xtabs(~Improved+Sex, data=Arthritis)
  fisher.test(mytable)
#注意:Fisher检验可以用于多维3*2或2*3等，跟其他软件不同，这里不能用于2*2的列联表！
  
 # 7.2.5 Chochran-Mantel-Haenszel test（CMH,原假设是，两个名义变量在第三个变量的每一层中都是条件独立的。）
  mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
  mantelhaen.test(mytable)  
# ftable(mytable)
#在之前的chisq.test检验里得出，Treatment和Improved有关联，继续用CMH检验分析，Treatment和Improved在sex的每一水平下是否独立？
#结果表明p<0.05,说明患者接受的治疗与得到的改善在性别的每一水平下并不独立（即，分性别来看，用药治疗的患者较接受安慰剂的患者有了更多的改善）。   
  
 # 7.2.6 相关系数的度量 [2*2交叉表，得出关联后，计算关联系数]  
  library(vcd)
  mytable <- xtabs(~Treatment+Improved, data=Arthritis)
  assocstats(mytable)
#可以用来计算二维列联表的phi系数、pearson列联系数和Cramer’s V系数  
  kappa(mytable)
#kappa()函数，可以计算混淆矩阵的Cohen’s kappa值以及加权的kappa值。（举例来说，混淆矩阵可以表示两位评判者对于一系列对象进行分类所得结果的一致程度。）  
  
# 7.3 相关--线性相关 秩相关 偏相关 ####
  rm(list = ls())
 # 7.3.1 显著性检验[pearson相关和spearman，kendall相关]
  library(psych)
  states<- state.x77[,1:6]
  corr.test(states, use="complete")
#可以修改 method=c("pearson","spearman","kendall")来更换；结果上面是相关矩阵，下面是显著性P值。

 # 7.3.1 pearson相关和spearman，kendall相关
  # 用法1
  states<- state.x77[,1:6]
  cov(states)
  cor(states)
  cor(states, method="spearman")
  cor(states,method = "kendall")
  #用法2--[单独看部分变量之间的相关]
  x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
  y <- states[,c("Life Exp", "Murder")]
  cor(x,y)
 # 7.3.2 偏相关  
  library(ggm)
  pcor(c(1,5,2,3,6), cov(states))
#pcor(u,s)u是一个数值向量，前两个数值1 5表示要计算相关系数的变量下标，其余的数值2 3 6为条件变量（即要排除影响的变量）的下标。S为变量的协方差阵。  
#在控制了收入、文盲率和高中毕业率的影响时，人口和谋杀率之间的相关系数为0.346。  

# 7.4 T检验  ####
  rm(list = ls())
 #7.4.1 单样本t检验 --与总体均值的比较
  x <- c(20.99,20.41,20.10,20.00,20.91,22.60,20.99,20.42,20.90,22.99,23.12,20.89)
  t.test(x, alternative = "greater", mu = 20.7 )#题目已知mu
  # 番外：根据公式算出t值
  x <- 74.2; mu <- 72; thita <- 6.5;  n <- 25
  (t <- (x-mu) / (thita/sqrt(n)))   #或者用n-1代替n,thita是样本均数的标准差，mu是均数
  #用pt()函数，输入t值和自由度df（n-1），得到p值
  p <- pt(t,df=24); p
  
 #7.4.2 配对[差值d]的t检验--  [假设 差值d满足正态分布]
  sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x),sd=sd(x))))
  with(UScrime, t.test(U1, U2, paired=TRUE))
  # t.test(UScrime$U1, UScrime$U2, paired=TRUE)
#差异的均值（61.5）足够大，可以保证拒绝年长和年轻男性的平均失业率相同的假设。年轻男性的失业率更高。  

 #7.4.3  2个独立样本的t检验
  # 正态性检验和方差齐性检验
  library(MASS)
  shapiro.test(UScrime$Prob)
  qqPlot(lm(Prob ~ So, data=UScrime), 
         simulate=TRUE, main="Q-Q Plot", labels=FALSE)
  var.test(Prob ~ So, data=UScrime)
  bartlett.test(Prob ~ So, data=UScrime)
  #
  t.test(Prob ~ So, data=UScrime,var.equal=TRUE) # var.equal=TRUE 在方差相等的条件下
# t.test(y~x,data)和t.test(y1,y2)  # y1,y2是两组的数值型变量
#这种检验，默认是方差不相等。可以自己设置var.equal=TRUE 

# 7.5 秩和检验  ####
  rm(list = ls())
 #7.5.1 两组独立样本的秩和检验
  library(MASS)
  with(UScrime, by(Prob, So, median))
  wilcox.test(Prob ~ So, data=UScrime)
 #7.5.2 符号秩和检验，配对的两组
  sapply(UScrime[c("U1", "U2")], median)
  with(UScrime, wilcox.test(U1, U2, paired=TRUE)) 
 #7.5.3 Kruskal—Wallis检验
  states <- data.frame(state.region, state.x77)
  kruskal.test(Illiteracy ~ state.region, data=states)
# kruskal.test(y~A,data)
 #7.5.4 Friedman检验
# friedman.test(Y~A|B,data)  #y是数值型结果变量,A是一个分组变量,而B是一个用以认定匹配观测的区组变量.
 #7.5.5 多重比较
  source("http://www.statmethods.net/RiA/wmc.txt")              
  states <- data.frame(state.region, state.x77)
  wmc(Illiteracy ~ state.region, data=states, method="holm")
  
# 8. 回归--[简单线性回归、多项式回归、多元线性回归]  ####
  rm(list = ls())
 #8.1 画散点图，观察
  library(car)
  scatterplot(weight ~ height, data=women,
              spread=FALSE, smoother.args=list(lty=2), pch=19,
              main="Women Age 30-39",
              xlab="Height (inches)",
              ylab="Weight (lbs.)")
 #8.2 简单线性回归
  fit <- lm(weight ~ height, data=women)
  summary(fit)
#  women$weight
  fitted(fit)  #拟合值
  residuals(fit)  #残差值
  plot(women$height,women$weight,
       main="Women Age 30-39", 
       xlab="Height (in inches)", 
       ylab="Weight (in pounds)")
  abline(fit)
 # 8.3 多项式回归
  fit2 <- lm(weight ~ height + I(height^2), data=women)
  summary(fit2)
  plot(women$height,women$weight,
       main="Women Age 30-39",
       xlab="Height (in inches)",
       ylab="Weight (in lbs)")
  lines(women$height,fitted(fit2))
 #8.4 多元线性回归
  #step1.查看数据情况
  states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                       "Illiteracy", "Income", "Frost")])
  cor(states)
  library(car)
  scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2),
                    main="Scatter Plot Matrix")
  #step2.建立模型
  states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                       "Illiteracy", "Income", "Frost")])
  fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
  summary(fit)
 #8.5 有交互项的多元线性回归
  fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
  summary(fit)
  
  library(effects)
  plot(effect("hp:wt", fit,, list(wt=c(2.2, 3.2, 4.2))), multiline=TRUE)
 #8.6 回归诊断  #### 
 #8.6.1. 方法1-包括分别对正态性、独立、线性、方差、综合5个方面
 #8.6.1.1 正态性检验1--QQ图
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
 #8.6.1.1 正态性检验2--学生化残差-柱状图
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
#除了一个很明显的离群点，误差很好地服从了正态分布。
 #8.6.1.2 残差独立性检验 #误差的独立性
  durbinWatsonTest(fit)   # simulate=FALSE，自助法导出P值，则每次运行测试时获得的结果都将略有不同。
#p值不显著（p=0.282）说明无自相关性，误差项之间独立。滞后项（lag=1）表明数据集中
#每个数据都是与其后一个数据进行比较的。该检验适用于时间独立的数据，对于非聚集型的数据并不适用。
 
 #8.6.1.3 线性--成分残差图..因变量与自变量之间是否呈非线性关系
  library(car)
  crPlots(fit)
#若图形存在非线性，则说明你可能对预测变量的函数形式建模不够充分，那么就需要添加一些曲线成分，比如多项式项，或对一个或多个变量进行变换（如用log(X)  

 #8.6.1.4 同方差
  library(car)
  ncvTest(fit)
  spreadLevelPlot(fit)
#计分检验不显著（p=0.19），说明满足方差不变假设；通过分布水平图看到这一点，其中的点在水平的最佳拟合曲线周围呈水平随机分布。
#若图形显示出了非水平趋势，建议幂次转换为0.5，在回归等式中用 Y 代替Y，可能会使模型满足同方差性。若建议幂次为0，则使用对数变换。
# ncvTest()函数生成一个计分检验，零假设为误差方差不变，备择假设为误差方差随着拟合值水平的变化而变化。若检验显著，则说明存在异方差性  
# spreadLevelPlot()函数创建一个添加了最佳拟合曲线的散点图，展示标准化残差绝对值与拟合值的关系。        
  
 #8.6.1.5 综合性的检验--
  library(gvlma)
  gvmodel <- gvlma(fit) 
  summary(gvmodel) #看第一行的Global Stat为综合检验的P值，其余行还有偏斜度、峰度、异方差的检验
  
 #8.6.2 方法2.一般的回归诊断
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
  
 #8.6.3 多重共线性 诊断--VIF（Variance Inflation Factor，方差膨胀因子）
  library(car)
  vif(fit) 
  sqrt(vif(fit)) > 2 # problem?
#VIF的平方根表示变量回归参数的置信区间能膨胀为与模型无关的预测变量的程度（因此而得名）  
# 一般原则下， sqrt(vif) >2就表明存在多重共线性问题。 
  
 #8.6.4 异常观测值--离群点、高杠杆值点、强影响点  ***
 #8.6.4.1 -- 离群点
  library(car)
  outlierTest(fit)
#  注意，该函数只是根据单个最大（或正或负）残差值的显著性来判断是否有离群点。若不显著，则说明数据集中没有离群点；若显著，则你必须删除该离群点，然后再检验是否还有其他离群点存在。  
# 可以看到Nevada被判定为离群点（p=0.048）  
  
 #8.6.4.2 -- 高杠杆值点
  hat.plot <- function(fit) {
    p <- length(coefficients(fit))
    n <- length(fitted(fit))
    plot(hatvalues(fit), main="Index Plot of Hat Values")
    abline(h=c(2,3)*p/n, col="red", lty=2)
    identify(1:n, hatvalues(fit), names(hatvalues(fit)))
  }
  hat.plot(fit)
#高杠杆值的观测点可通过帽子统计量（hat statistic）判断：一般来说，若观测点的帽子值大于帽子均值的2或3倍，即可以认定为高杠杆值点。  
 #8.6.4.3 -- 强影响点
  #方法1 --  Cook距离，或称D统计量
  cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
  plot(fit, which=4, cook.levels=cutoff)
  abline(h=cutoff, lty=2, col="red")  #图形可以判断Alaska、 Hawaii和Nevada是强影响点。若删除这些点，将会导致回归模型截距项和斜率发生显著变化。
  #方法2 -- 变量添加图
  library(car)
  avPlots(fit, ask=FALSE, id.method="identify") #图中鉴别出Alaska为强影响点
#若删除点Alaska，直线将往负向移动。事实上，删除Alaska， Income的回归系数将会从0.000 06变为???0.000 85。  
 #8.6.4.4 综合的判断，离群点、高杠杆值点、强影响点。
  library(car)
  influencePlot(fit, id.method="identify", main="Influence Plot", 
                sub="Circle size is proportial to Cook's Distance" )
#反映出Nevada和Rhode Island是离群点， New York、 California、 Hawaii和Washington有高杠杆值， Nevada、 Alaska和Hawaii为强影响点。
#纵坐标超过+2或小于???2的州可被认为是离群点，水平轴超过0.2或0.3的州有高杠杆值（通常为预测值的组合）。圆圈大小与影响成比例，圆圈很大的点可能是对模型参数的估计造成的不成比例影响的强影响点  
  
 #8.7 变量转换的-- 探索
  library(car)
  summary(powerTransform(states$Murder)) #当模型违反了正态假设时，通常可以对响应变量尝试某种变换。 
  # Box-Tidwell Transformations to linearity
  library(car)
  boxTidwell(Murder~Population+Illiteracy,data=states) #当违反了线性假设时，对预测变量进行变换常常会比较有用。 

 # 8.8 选择最优模型  ####
 # 8.8.1 比较模型 --  方法1
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
             data=states)
  fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
  anova(fit2, fit1)
# 由于检验不显著（p=0.994），因此我们可以得出结论：不需要将这两个变量添加到线性模型中，可以将它们从模型中删除。  
  
 # 8.8.2 比较模型 -- 方法2
  fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
             data=states)
  fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
  AIC(fit1,fit2)  #AIC最小，为最优模型
 # 8.8.3 向后逐步回归法 Backward stepwise
  library(MASS)
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
            data=states)
  stepAIC(fit, direction="backward")
#第一步， Frost被删除， AIC从97.75降低到95.75；第二步， Income被删除 AIC继续下降，成为93.76，然后再删除变量将会增加AIC，因此终止选择过程。  
  
  # 8.8.4 全子集回归 选择 All subsets regression
  library(leaps)
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +
                       Frost, data=states, nbest=4)
  plot(leaps, scale="adjr2")
#第一行中（图底部开始），可以看到含intercept（截距项）和Income的模型调整R平方为0.33，含intercept和Population的模型调整R平方为0.1。
  library(car)
  subsets(leaps, statistic="cp",main="Cp Plot for All Subsets Regression") # 需要交互
  abline(1,1,lty=2,col="red") 
#你会看到对于不同子集大小，基于Mallows Cp统计量的四个最佳模型。越好的模型离截距项和斜率均为1的直线越近。  
  
 # 8.9 深层次分析
  #8.9.1 k重交叉验证 --R方的验证
  #在k 重交叉验证中，样本被分为k个子样本，轮流将k???1个子样本组合作为训练集，另外1个子样本作为保留集。这样会获得k 个预测方程，记录k 个保留样本的预测表现结果，然后求其平均值。
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
  shrinkage(fit)  #可以看到，基于初始样本的R平方（0.567）过于乐观了。对新数据更好的方差解释率估计是交叉验证后的R平方（0.448）。
  fit2 <- lm(Murder~Population+Illiteracy,data=states)
  shrinkage(fit2)  #还可以用交叉验证来挑选变量。例如，含两个预测变量（Population和Illiteracy）的模型，比全变量模型R平方减少得更少
  
 #8.9.2 预测变量的重要性 --比较
 #8.9.2.1 方法1 -- 标准化回归系数
  states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
  zstates <- as.data.frame(scale(states))
  zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
  coef(zfit)  # 可认为Illiteracy是最重要的预测变量，而Frost是最不重要的。
  
 # 8.9.2.2 相对权重法  #和书上有不同的地方
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
# 9 方差分析  ####
  setwd("D:/Rworkspace/Rshizhan_code_@liurui/work_dir@mine")
  rm(list = ls())
  opar <- par(no.readonly=TRUE)
 #9.1 单因素方差分析 ANOVA ####
 # 9.1.1 正态性检验  Q-Q图
  library(car)
  qqPlot(lm(response ~ trt, data=cholesterol), 
         simulate=TRUE, main="Q-Q Plot", labels=FALSE)
 # 9.1.2 方差齐性检验
  bartlett.test(response ~ trt, data=cholesterol)
 # 9.1.3 - 单因素方差分析 ANOVA  
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
 # 9.1.3-1 方法1： 多重比较 两两比较
  TukeyHSD(fit)
# detach("package::HH") #如果TukeyHSD()受到冲突  
  par(las=2)
  par(mar=c(5,8,4,2)) 
  plot(TukeyHSD(fit))
  par(opar)
 # 9.1.3-2 方法2： 多重比较 两两比较Multiple comparisons
  library(multcomp)  #multcomp包中的glht()函数提供了多重均值比较更为全面的方法，既适用于线性模型（如本章各例），也适用于广义线性模型（见第13章）
  par(mar=c(5,4,6,2))
  tuk <- glht(fit, linfct=mcp(trt="Tukey"))
  plot(cld(tuk, level=.05),col="lightgrey")
  par(opar)  #有相同字母的组（用箱线图表示）说明均值差异不显著。
 # 9.1.4 离群点
  library(car)
  outlierTest(fit)
  
 #9.2 -单因素协方差分析 One-way ANCOVA ####
  #9.2.1 分析
  data(litter, package="multcomp")
  attach(litter)
  table(dose) 
  aggregate(weight, by=list(dose), FUN=mean)
  fit <- aov(weight ~ gesttime + dose)   #怀孕时间为协变量，写在前面                          
  summary(fit)
  # 
  library(effects)
  effect("dose", fit) #想要获取调整的组均值——即去除协变量效应后的组均值
  # 9.2.2 多重比较，用户定义[对照c(3, -1, -1, -1)设定第一组和其他三组的均值进行比较。]
  library(multcomp)
  contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1))
  summary(glht(fit, linfct=mcp(dose=contrast)))  #因此，可以得出未用药组比其他用药条件下的出生体重高的结论。
  # 9.2.3 检验回归斜率的同质性 - 
  library(multcomp)
  fit2 <- aov(weight ~ gesttime*dose, data=litter)
  summary(fit2) #可以看到交互效应不显著，支持了斜率相等的假设。若假设不成立，可以尝试变换协变量或因变量，或使用能对每个斜率独立解释的模型，
  # 9.2.4 结果可视化[可以绘制因变量、协变量和因子之间的关系图。]
  library(HH)
  ancova(weight ~ gesttime + dose, data=litter)
 #9.3 双因素方差分析 ####
  # - Two way ANOVA
  attach(ToothGrowth)
  table(supp,dose)
  aggregate(len, by=list(supp=supp,dose=dose), FUN=mean)
  aggregate(len, by=list(supp=supp,dose=dose), FUN=sd)
  dose <- factor(dose)
  fit <- aov(len ~ supp*dose)
  summary(fit)
  # 可视化
  library(HH)
  interaction2wt(len~supp*dose)
 #9.4 重复测量方差分析  ####
  CO2$conc <- factor(CO2$conc)
  w1b1 <- subset(CO2, Treatment=='chilled')
  fit <- aov(uptake ~ (conc*Type) + Error(Plant/(conc)), w1b1)
  summary(fit)
  # 可视化
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
  # 作图3
  attach(w1b1)
  interaction2wt(uptake ~ Type*conc)
  detach(w1b1)
 #9.5 多元方差分析  ####
 #9.5.1 单因素多元方差分析 
  library(MASS)
  attach(UScereal)
  shelf <- factor(shelf)
  y <- cbind(calories, fat, sugars)
  aggregate(y, by=list(shelf), FUN=mean)
  cov(y)
  fit <- manova(y ~ shelf)
  summary(fit)
  summary.aov(fit)
  # 检验 多元正态性
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
 # 9.5.2 稳健多元方差分析[当不满足多元正态和方差齐性时]
  library(rrcov)
  Wilks.test(y,shelf, method="mcd")  # this can take a while
#从结果来看，稳健检验对离群点和违反MANOVA假设的情况不敏感，而且再一次验证了存储在货架顶部、中部和底部的谷物营养成分含量不同。  
 # 9.6 #  随机区组设计  ====
  block <- c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)) ##生成区组
  treat <- c(rep(c("A", "B", "C"), 4)) ##生成组别
  alt <- c(76, 86, 115, 12, 38, 85, 40, 81, 103, 12, 33, 57) 
  alt <- data.frame(block= block, treat= treat, alt= alt) ##建立数据框
  alt$block <- as.factor(alt$block) ##因子化
  alt$treat <- as.factor(alt$treat) ##因子化
  model <- aov(alt ~ treat+ block, data= alt) ##随机区组方差分析
  summary(model)             ##查看模型
  
  TukeyHSD(model)  ##两两比较 
  res <- residuals(model) ##生成残差
  shapiro.test(res) ##评估残差正态性
  par(mfrow=c(2,2))
  plot(model)
  par(mfrow=c(1,1))
  
#10. logistics回归 ####
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
  
 #10.3建立模型1-- fit full model
  fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                    religiousness + education + occupation +rating,
                  data=Affairs,family=binomial())
  summary(fit.full)
  
 #10.4 调整变量后，建立模型2 fit reduced model
  fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + 
                       rating, data=Affairs, family=binomial())
  summary(fit.reduced)
 #10.5 compare models
  anova(fit.reduced, fit.full, test="Chisq")
 #10.6 参数 比值比 interpret coefficients
  coef(fit.reduced)
  exp(coef(fit.reduced))  #比值比
  exp(confint(fit.reduced)) #系数的95%置信区间
 #10.7[番外，调试数据，自定义数据来测试模型]calculate probability of extramariatal affair by marital ratings
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
  
 #10.8 过度离势 evaluate overdispersion
  fit <- glm(ynaffair ~ age + yearsmarried + religiousness +
               rating, family = binomial(), data = Affairs)
  fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness +
                  rating, family = quasibinomial(), data = Affairs)
  pchisq(summary(fit.od)$dispersion * fit$df.residual,  
         fit$df.residual, lower = F)
# 第2次模型 使用family = quasibinomial()，此处p值（0.34）显然不显著（p > 0.05），这更增强了我们认为不存在过度离势的信心。
#11. 泊松回归 ####
 #11.1 数据
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
 #11.3 计算参数 interpret model parameters
  coef(fit)
  exp(coef(fit))
 #11.4 过度离势 evaluate overdispersion
  deviance(fit)/df.residual(fit)
  library(qcc)
  qcc.overdispersion.test(breslow.dat$sumY, type="poisson")
#显著性检验的p值果然小于0.05，进一步表明确实存在过度离势。
 # fit model with quasipoisson
  fit.od <- glm(sumY ~ Base + Age + Trt, data=breslow.dat,
                family=quasipoisson())
  summary(fit.od) #通过用family = "quasipoisson"替换family = "poisson"，处理过度离势。
  
  
  
  
  
#12. 时间序列分析 ####
 # 12.1 数据整理
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
 # 12.2 简单移动平均 平滑处理
  library(forecast)
  opar <- par(no.readonly=TRUE)
  par(mfrow=c(2,2))
  ylim <- c(min(Nile), max(Nile))
  plot(Nile, main="Raw time series")
  plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
  plot(ma(Nile, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
  plot(ma(Nile, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
  par(opar)
 # 12.3 季节性分解，了解数据 using slt()
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
 # 12.4 指数平滑模型
  #单指数平滑[常数水平项和时间点i处随机项]
  library(forecast)
  fit <- ets(nhtemp,model = "ANN")
  fit
  forecast(fit,1)
  accuracy(fit)
  #双指数平滑，Holt指数平滑[水平项和趋势项]
  library(forecast)
  fit <- ets(nhtemp,model = "AAN")
  fit
  forecast(fit,1)
  accuracy(fit)
  #三指数平滑，Holt-Winters指数平滑[水平项、趋势项、季节效应]
  fit <- ets(log(AirPassengers),model="AAA")      
  fit #三个光滑参数，即水平项0.82、趋势项0.0004、季节项0.012
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
  # ets()自动选择预测
  library(forecast)
  fit <- ets(JohnsonJohnson)
  fit #所选中的模型同时有可乘趋势项、季节项和随机误差项。
  plot(forecast(fit), main="Johnson and Johnson Forecasts", 
       ylab="Quarterly Earnings (Dollars)", xlab="Time")
 #12.5. ARIMA模型 & SARIMA模型
  # 12.5.1 数据整理
  library(forecast)
  library(tseries)
  plot(Nile)  
#判断：从图上看，方差还算稳，如果不稳，用log(y)进行下一步分析
#序列的一次差分可以移除序列中的线性趋势，二次差分移除二次项趋势，三次差分移除三次项趋势。
  ndiffs(Nile) #找出最适合的d值，这里是1
  dNile <- diff(Nile)                                              
  plot(dNile)
  adf.test(dNile)  # 这里的log=?,可以自定义adf.test(dNile,k=12)
  
  par(mfrow=c(2,1))
  Acf(dNile)  #这里使用的是forecast()里的函数，也可以用基本的acf()pacf()
  Pacf(dNile)
  par(mfrow=c(1,1))
 # 12.5.2 建立拟合模型
  fit <- arima(Nile, order=c(0,1,1)) #根据acf图/pacf图来确定p/q                                 
  fit
  accuracy(fit)
 # 12.5.3 模型检验
  qqnorm(fit$residuals)     
  qqline(fit$residuals)
  Box.test(fit$residuals, type="Ljung-Box") #P值大于0.05，说明模型ok
#模型的残差没有通过显性著检验，即我们可以认为残差的自相关系数为零。 ARIMA模型能较好地拟合本数据。 
  
 # 12.5.4 模型预测
  forecast(fit, 3)
  plot(forecast(fit, 3), xlab="Year", ylab="Annual Flow")

 # 12.5.6 自动预测 - Automated ARIMA forecasting
  library(forecast)
  fit <- auto.arima(Nile,trace = T)
  fit
  forecast(fit, 3)
  accuracy(fit)
  plot(forecast(fit, 3), xlab="Year", ylab="Annual Flow")
 # 12.5.7 SARIMA模型 ####
  y<-read.csv(file.choose(),header = T)
  ty<-ts(y,start = c(2005,1),frequency = 12)
  ty
  plot(ty,ylab='incidence/10???',xlab='Time')
  #
  ndiffs(ty) #找出最适合的d值，这里是1
#  dty <- diff(ty) #一阶差分  
  sdty<-diff(diff(ty),lag=12)  # 1阶差分 + 1阶季节差分
  plot(sdty)
  adf.test(sdty)  # 这里的log=?,可以自定义
  #
  par(mfrow=c(2,1))
  Acf(sdty)  #这里使用的是forecast()里的函数，也可以用基本的acf()pacf()
  Pacf(sdty)
  par(mfrow=c(1,1))  #从图中判断p1 q1 P1 Q1
  #
  auto.arima(ty,trace=T)  #得最优模型 ARIMA(2,0,2)(0,1,1)[12] 
  fit0 <- auto.arima(ty,trace=T)
  accuracy(fit0)
  #
  fit<-arima(ty,order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12),method = 'ML')
  fit
  accuracy(fit)
  
  # fit<-fit0  #在比较fit0和fit后，选最优的模型
  qqnorm(fit$residuals)
  qqline(fit$residuals)
  Box.test(fit$residuals,type = "Ljung-Box")
  
  forecast(fit,12)
  plot(forecast(fit,12),xlab='Year',ylab='prevalence/10万')
  plot(forecast(fit,12),xlab='Year',type='o',ylab='prevalence/10万')
  
#### plot  ####

  
#12R
  options(digits = 2)
  cor(mtcars)
  library(corrgram)
  corrgram(mtcars,order = TRUE,lower.panel = panel.shade,upper.panel = panel.pie,text.panel = panel.txt,main="Correlogram of mtcars intercorrelations")
  
  
  
  
  
  
  
  
  