#Load Packages
library(quantmod)
library(rugarch)
library(fBasics)

####example1:ABOUT CPHI   ====================================================
###part1:get the data about CPHI, compute the price and log return, plot it
getSymbols("CPHI", from = "2018-01-01", to = "2020-10-27")
CPHI.rtn = diff(log(CPHI$CPHI.Adjusted))
CPHI.price = CPHI$CPHI.Close
chartSeries(CPHI)
chartSeries(CPHI.rtn,theme = "black", name = "The return of CPHI from 2018-01-03 to 2020-10-26")
chartSeries(CPHI.price,theme = "white", name = "The price of CPHI from 2018-01-02 to 2020-10-26")
basicStats(CPHI.price)#compute summary statistics about the price of CPHI
basicStats(CPHI.rtn)#compute summary statistics about the return of CPHI


###part2:distribution of financial data;plot the histogram, the fitted density and normal density in one figure
CPHI.rtn1 = na.omit(CPHI.rtn)
hist(CPHI.rtn1, nclass = 40, freq = F, main = "Histogram of CPHI's return", xlab = "CPHI_return")
d1 = density(CPHI.rtn1)
range(CPHI.rtn1)
x1 = seq(-0.4,0.4,0.001)
y1 = dnorm(x1, mean(CPHI.rtn1),stdev(CPHI.rtn1))
lines(d1$x, d1$y, xlab = "x", ylab = "density", lty = 1, col = "blue")
lines(x1, y1, lty = 2, col = "red")


### part3: normality test
t.test(CPHI.rtn1)  # Testing mean return = 0
s3=skewness(CPHI.rtn1)
T=length(CPHI.rtn1) # Sample size
t3=s3/sqrt(6/T) # Skewness test
pp=2*(1-pnorm(t3)) # Compute p-value
s4=kurtosis(CPHI.rtn1)
t4=s4/sqrt(24/T) # Kurtosis test
normalTest(CPHI.rtn1,method='jb') # JB-test



### part4:Save data
write.csv(CPHI, file = "CPHI.csv")
write.csv(CPHI.rtn,file = "CHPI_rtn.csv")







####example2:ABOUT VANKE
###part1:get the data about VANKE, compute the price and log return, plot it
setSymbolLookup(WK=list(name='000002.sz',from = "2018-01-01", to = "2020-10-27",src='yahoo'))
getSymbols("WK")
VANKE.price = WK[,4]
VANKE.rtn = diff(log(WK[,6]))
chartSeries(WK)
chartSeries(VANKE.rtn,theme = "black", name = "The return of VANKE from 2018-01-03 to 2020-10-26")
chartSeries(VANKE.price,theme = "white", name = "The price of VANKE from 2018-01-02 to 2020-10-26")
basicStats(VANKE.price)#compute summary statistics about the price of VANKE
basicStats(VANKE.rtn)#compute summary statistics about the return of VANKE


###part2:distribution of financial data;plot the histogram, the fitted density and normal density in one figure
VANKE.rtn1 = na.omit(VANKE.rtn)
hist(VANKE.rtn1, nclass = 40, freq = F, main = "Histogram of VANKE's return", xlab = "VANKE_return")
d2 = density(VANKE.rtn1)
range(VANKE.rtn1)
x2 = seq(-0.1,0.1,0.001)
y2 = dnorm(x2, mean(VANKE.rtn1),stdev(VANKE.rtn1))
lines(d2$x, d2$y, xlab = "x", ylab = "density", lty = 1, col = "blue")
lines(x2, y2, lty = 2, col = "red")


### part3: normality test
t.test(VANKE.rtn1)  # Testing mean return = 0
s3=skewness(VANKE.rtn1)
T=length(VANKE.rtn1) # Sample size
t3=s3/sqrt(6/T) # Skewness test
pp=2*(1-pnorm(t3)) # Compute p-value
s4=kurtosis(VANKE.rtn1)
t4=s4/sqrt(24/T) # Kurtosis test
normalTest(VANKE.rtn1,method='jb') # JB-test


### part3:Save data
write.csv(WK, file = "VANKE.csv")
write.csv(VANKE.rtn,file = "VANKE_rtn.csv")



####Analysis
#Put two histograms together
opar = par(no.readonly = TRUE)
par(mfrow = c(2,1))
hist(CPHI.rtn1, nclass = 40, freq = F,main = "Histogram of CPHI's return", xlab = "CPHI_return")
lines(d1$x, d1$y, xlab = "x", ylab = "density", lty = 1, col = "blue")
lines(x1, y1, lty = 2, col = "red")
hist(VANKE.rtn1, nclass = 40, freq = F, main = "Histogram of VANKE's return", xlab = "VANKE_return")
lines(d2$x, d2$y, xlab = "x", ylab = "density", lty = 1, col = "blue")
lines(x2, y2, lty = 2, col = "red")
par(opar)






###        CPHI      ====================================================================
## plot ACF and PACF
acf1 = acf(CPHI.rtn1, lag = 20, main = "Series CPHI")
pacf1 = pacf(CPHI.rtn1, lag = 20, main = "Series CPHI")

## using aic criterion
mm1 = ar(CPHI.rtn1,lag = 4, method = 'ols')
mm1$aic
plot(c(0:mm1$order.max), mm1$aic, type = "h",xlab = "order", ylab = "AIC", main = "AIC of CPHI")
lines(0:mm1$order.max, mm1$aic, lty = 2, col = 'red')

##Trying ARMA and choosing the lowest AIC
aic_mat = matrix(NA, 5,5)
for(i in 1:5){
  for(j in 1:5){
    tmp = arima(CPHI.rtn1,order = c(i,0,j))
    aic_mat[i,j] = tmp$aic
  }
}
aic_mat
which(aic_mat == min(aic_mat),arr.ind = T)
CPHI_AR = arima(CPHI.rtn1, order = c(4,0,0))
CPHI_AR$aic
CPHI_MA = arima(CPHI.rtn1, order = c(0,0,4))
CPHI_MA$aic

## Using ARMA, test and eatimate
m1 = arima(CPHI.rtn1, order = c(4,0,4))
m1
tvalue1 = m1$coef/sqrt(diag(m1$var.coef))
tvalue1
Box.test(m1$residuals, lag = 12, type = "Ljung")

## plot something about residuals
opar = par(no.readonly = T)
par(mfcol=c(3,1))
plot(m1$residuals,type="l",ylab="residuals")
acf(m1$residuals,lag=20,col="red",lwd=2,main="")
pacf(m1$residuals,lag=20,col="red",lwd=2,main="")
par(opar)
qqnorm(m1$residuals)
qqline(m1$residuals)





###       VANKE     ====================================================================
## plot ACF and PACF
acf2 = acf(VANKE.rtn1, lag = 20, main = "Series VANKE")
pacf2 = pacf(VANKE.rtn1, lag = 20, main = "Series VANKE")

## using aic criterion
mm2 = ar(VANKE.rtn1,lag = 4, method = 'ols')
mm2$aic
plot(c(0:mm2$order.max), mm2$aic, type = "h",xlab = "order", ylab = "AIC", main = "AIC of VANKE")
lines(0:mm2$order.max, mm2$aic, lty = 2, col = 'red')

##Trying ARMA and choosing the lowest AIC
aic_mat = matrix(NA, 5,5)
for(i in 1:5){
  for(j in 1:5){
    tmp = arima(VANKE.rtn1,order = c(i,0,j))
    aic_mat[i,j] = tmp$aic
  }
}
aic_mat
which(aic_mat == min(aic_mat),arr.ind = T)
CPHI_AR = arima(VANKE.rtn1, order = c(5,0,0))
CPHI_AR$aic
CPHI_MA = arima(VANKE.rtn1, order = c(0,0,2))
CPHI_MA$aic

## Using ARMA, test and eatimate
m2 = arima(VANKE.rtn1, order = c(5,0,2))
m2
tvalue2 = m2$coef/sqrt(diag(m2$var.coef))
tvalue2
Box.test(m2$residuals, lag = 12, type = "Ljung")

## plot something about residuals
opar = par(no.readonly = T)
par(mfcol=c(3,1))
plot(m2$residuals,type="l",ylab="residuals")
acf(m2$residuals,lag=20,col="red",lwd=2,main="")
pacf(m2$residuals,lag=20,col="red",lwd=2,main="")
par(opar)
qqnorm(m2$residuals)
qqline(m2$residuals)