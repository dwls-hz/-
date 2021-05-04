##案例：英国国王死亡时间（非季节性模型）(Age of Death of Successive Kings of England)

#1.
#导入所需的包
install.packages()
library("TTR")
library("forecast")

#2.
#导入文件
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings

#3.
#将时间序列数据读入R后，下一步就是将数据存储在R中的时间序列对象中。
#这样就可以使用R的许多函数来分析时间序列数据，我们使用R中的ts（）函数。
#ts函数（time series）：通过一向量或者矩阵创建一个一元的或多元的时间序列
kingstimeseries <- ts(kings)
kingstimeseries

#4.
#绘制kingstimeseries的数据图
plot.ts(kingstimeseries)

#5.
#“TTR”包中的SMA（）函数可用于使用简单的移动平均值来平滑时间序列数据。
#要使用SMA（）函数，需要使用参数“n”指定简单移动平均值的顺序（跨度）。
#这个42位英国国王的去世年龄数据呈现出非季节性，并且由于其随机变动在整个时间段内是大致不变的，这个序列也可以被描述称为一个相加模型。
#使用3阶简单移动平均值平滑时间序列，并绘制平滑时间序列数据。
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)

#6.
#在使用3阶简单移动平均值平滑的时间序列中，似乎存在相当多的随机波动。
#因此，为了更准确地估计趋势分量，我们可能希望尝试使用简单的移动平均值来平滑数据。更高阶。
#使用8阶简单移动平均值平滑时间序列，并绘制平滑时间序列数据。
#使用8阶简单移动平均值进行平滑的数据可以更清晰地显示趋势分量，我们可以看到英国国王的死亡年龄似乎已经从大约55岁降至大约38岁。在最后的20位国王中，然后在第40位国王在时间序列的统治结束之后增加到大约73岁。
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

#7.
#从时间图（kingstimeseries图）中，我们可以看出时间序列不是平均值。要计算第一个差异的时间序列并绘制它
#第一个差异的时间序列在均值和方差上似乎是固定的，因此ARIMA（p，1，q）模型可能适合于英格兰国王的死亡年龄的时间序列。
#通过采用第一个差异的时间序列，我们删除了国王死亡时代的时间序列的趋势分量，并留下不规则的成分。
#我们现在可以检查这个不规则分量的连续项之间是否存在相关性; 如果是这样，这可以帮助我们为国王死亡的年龄做出预测模型。
kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingtimeseriesdiff1)

#8.
#要绘制相关图和部分相关图，我们可以分别使用R中的“acf（）”和“pacf（）”函数。为了获得自相关和部分自相关的实际值，我们在“acf（）”和“pacf（）”函数中设置“plot = FALSE”。
#绘制英国国王死亡时间的一次差异时间序列的滞后1-20的相关图，并获得自相关的值
acf(kingtimeseriesdiff1, lag.max=20)             # 绘制相关图
acf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # 得到自相关系数

#9.
#我们从相关图中看到，滞后1（-0.360）处的自相关超过了显着边界，但是滞后1-20之间的所有其他自相关都没有超过显著边界。
#为了绘制英语国王死亡时间的一次差异时间序列的滞后1-20的部分相关图，并获得部分自相关的值，我们使用“pacf（）”函数
pacf(kingtimeseriesdiff1, lag.max=20)             # 绘制偏相关图
pacf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # 得到偏相关系数
#部分相关图显示滞后1,2和3的部分自相关超过显着边界，为负，并且随着滞后的增加而在幅度上缓慢下降（滞后1：-0.360，滞后2：-0.335，滞后3：-0.321 ）。在滞后3之后，部分自相关变为零。
#故ARMA（3，0）、ARMA（0，1）、ARMA（p,q）均可行，但是根据参数最少的思路，选择ARMA（0，1）模型

#10.
#MA（移动平均）模型通常用于模拟时间序列，该时间序列显示连续观察之间的短期依赖性。
#很有意义的是，MA模型可以用来描述英国国王死亡时间序列中的不规则成分。
#我们可以预期特定英国国王的死亡年龄对后任一位或两位国王的死亡年龄年龄有一定影响。
#auto.arima（）函数可用于查找适当的ARIMA模型。在之前学习ARMA模型的过程中我们通过查看序列的ACF/PACF图来帮助定阶。其实实际应用中往往不是那么容易就能通过ACF/PACF图来识别出ARIMA模型的阶数。forecast包中的auto.arima()可以自动尝试不同的阶数组合并挑选出可能的最优模型。可以帮助我们进行定阶
auto.arima(kings)
#由于ARMA（0,1）模型（p = 0，q = 1）被认为是英国国王死亡年龄的第一个差异的时间序列的最佳候选模型，那么原始的时间序列死亡年龄可以使用ARIMA（0,1,1）模型建模（p = 0，d = 1，q = 1，其中d是所需差分的顺序）。

#11.
#将ARIMA（0,1,1）模型拟合到我们的时间序列，意味着将ARMA（0,1）模型拟合到第一个差分的时间序列。
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # 拟合 ARIMA(0,1,1) 模型
kingstimeseriesarima
#输出的ma1=-0.7218为时间序列的系数

#12.
#对ARIMA模型使用“forecast.Arima（）”函数对时间序列的未来值进行预测
kingstimeseriesforecasts <- forecast(kingstimeseriesarima, h=5)
kingstimeseriesforecasts
plot(kingstimeseriesforecasts)
#forecast.Arima（）函数给出了对接下来的五位英国国王（43-47）的死亡年龄的预测，包括80％到95％置信区间。第42位英国国王的死亡年龄为56岁，接下来五位国王死亡的预测年龄为67.8岁。

#13.
#预测后还需研究ARIMA模型的预测误差是否正态分布为均值为零，方差是否为常数。（是否符合白噪声过程）
#为国王死亡时的ARIMA（0,1,1）模型制作预测误差的相关图，并通过输入以下内容执行Ljung-Box测试
#白噪声检验-Ljung-Box Test：首先假设序列为白噪声，根据假设求得的P值如果小于阈值（一般为5%），那么假设不成立；反之，假设成立。
acf(kingstimeseriesforecasts$residuals, lag.max=20)
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(kingstimeseriesforecasts$residuals) # 绘制预测误差序列图

#14.
#定义plotForecastErrors函数
plotForecastErrors <- function(forecasterrors){
  #画预测误差的直方图
  hist(forecasterrors, col="red", freq = F)
  #画方差是预测误差数据的方差，平均值是0的正态分布数据的线
  mysd <- sd(forecasterrors)
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  myhist <- hist(mynorm, plot = F)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

#15.
#绘制直方图
plotForecastErrors(kingstimeseriesforecasts$residuals) #绘制直方图
