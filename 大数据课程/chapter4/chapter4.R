### chapter 4  scripts ###
### tungbin, 2020 ###
library(quantmod)
library(fBasics)
#example 1, daily prices of Apple stock 
getSymbols("AAPL",from="2007-01-03",to="2011-12-31") #Specify period
AAPL.rtn=diff(log(AAPL$AAPL.Adjusted)) # Compute log returns
AAPL.price = AAPL$AAPL.Adjusted # price series
#chartSeries(AAPL.price,theme="white")
chartSeries(AAPL.rtn,theme="white")
# basicStats(AAPL.rtn)
abline(h= -0.05,lty=2,col="red")
abline(h= 0.05,lty=2,col="red")

#example 2, daily price of CSI 300 
# Set my working directory
setwd("C:/Users/17621/Documents/R studio project/大数据课程/chapter4") 
code = "000300.SH"
da = read.csv(paste(code,"_ret.csv",sep=""),sep=',',header=TRUE)
head(da)  #See the first 6 rows of the data
rtn=100*da[,3]  
##load package "zoo"
#通过zoo工具包得到带日期的数据类da1
library(zoo)
##数字转换成日期
csi300.time <- as.Date(da[,1])
csi300.data <- matrix(da[,3], ncol = 1)
colnames(csi300.data) <- "ret"
csi300<- zoo(csi300.data, csi300.time)
#plot(csi300,col="green")
chartSeries(csi300,theme="white")
#basicStats(csi300)
abline(h= -0.05,lty=2,col="red")
abline(h= 0.05,lty=2,col="red")

## correlations
# example 1, log-returns of ibm and spx
da=read.table("m-ibmsp-2611.txt",header=T)
head(da)
ibm=log(da$ibm+1) # Transform to log returns
sp=log(da$sp+1)
cor(ibm,sp)  # Obtain sample correlation
m1=lm(ibm~sp)  # Fit the Market Model (linear model)
summary(m1)
plot(sp,ibm,cex=0.8)  # Obtain scatter plot
abline(0.008,.807,col="red",lwd=2) # Add the linear regression line
# example 2, log-returns of csi300 and csi500
da = read.csv("000300.SH_ret.csv",sep=',',header=TRUE)
csi300 = 100*da[,3] 
da = read.csv("000905.SH_ret.csv",sep=',',header=TRUE)
csi500 = 100*da[,3] 
cor(csi300,csi500)  # Obtain sample correlation
m1=lm(csi500~csi300)  # Fit the Market Model (linear model)
summary(m1)
plot(csi300,csi500,cex=0.8)  # Obtain scatter plot
abline(0.01,1.02,col="red",lwd=2) # Add the linear regression line

## autocorrelation
# example 1, sample ACF
da=read.table("m-dec12910.txt",header=T)
head(da)
d10=da$dec10  # select the Decile 10 returns
dec10=ts(d10,frequency=12,start=c(1967,1))
par(mfcol=c(2,1))
plot(dec10,xlab='year',ylab='returns')
title(main='(a): Simple returns')
acf(d10,lag=24) # command to obtain sample ACF of the data

# test of single acf
f1=acf(d10,lag=24)
f1$acf
tt=f1$acf[13]*sqrt(516)
tt

# Ljung-Box Q statistics
da=read.table("m-ibmsp6709.txt",header=T)
ibm=da$ibm # simple returns
lnibm=log(ibm+1) # Transfer to log returns
# sample acf
f1 = acf(ibm,lag=100,main="",col="red")
title(main="simple return of ibm")
f2= acf(lnibm,lag=100,main="",col="red")
title(main="log return of ibm")
Box.test(ibm,lag=12,type='Ljung')
Box.test(lnibm,lag=12,type='Ljung')

## ACF of AR(1)
par(mfcol=c(2,1))
rho = 1
phi = 0.8
for(i in 2:16){
  rho = c(rho,rho[i-1]*phi)
}
plot(rho,type="h",xlab='order',ylab='ACF',lwd=2,col="red")
rho = 1
phi = -0.8
for(i in 2:16){
  rho = c(rho,rho[i-1]*phi)
}
plot(rho,type="h",xlab='order',ylab='ACF',lwd=2,col="red")
abline(h=0,lwd=1) 

## identification of AR orders
# example 1, GNP data
da=read.table("q-gnp4710.txt",header=T)
head(da)
G=da$VALUE
LG=log(G)
gnp=diff(LG)
dim(da)
tdx=c(1:253)/4+1947 # create the time index
par(mfcol=c(2,1))
plot(tdx,G,xlab='year',ylab='GNP',type='l')
plot(tdx[2:253],gnp,type='l',xlab='year',ylab='growth') 
acf(gnp,lag=12,col="red",lwd=2)
pacf(gnp,lag=12,col="red",lwd=2) # compute PACF
m1=arima(gnp,order=c(3,0,0))
m1

# using aic criterion 
mm1=ar(gnp,method='mle')
mm1$order # Find the identified order 
names(mm1)
print(mm1$aic,digits=3)
aic=mm1$aic  # For plotting below.
length(aic)
par(mfcol=c(1,1))
plot(c(0:12),aic,type='h',xlab='order',ylab='aic')
lines(0:12,aic,lty=2,col="red")

# example 2, csi 300 data
# using PACF
da = read.csv("000300.SH_ret.csv",sep=',',header=TRUE)
csi300 = da[,3] # log returns 
par(mfcol=c(2,1))
acf(csi300,lag=12,col="red",lwd=2)
pacf(csi300,lag=12,col="red",lwd=2) # compute PACF
# using aic
mm1=ar(csi300,method='mle')
mm1$order # Find the identified order 
names(mm1)
print(mm1$aic,digits=3)
aic=mm1$aic  # For plotting below.
length(aic)
par(mfcol=c(1,1))
plot(c(0:12),aic,type='h',xlab='order',ylab='aic')
lines(0:12,aic,type="b",lty=2,col="red")
title(main="AIC of CSI 300")

## estimation of AR(p) models
# example 1, GNP data
da=read.table("q-gnp4710.txt",header=T)
G=da$VALUE
gnp=diff(log(G))
m1=arima(gnp,order=c(3,0,0))
m1
# t-values of coef
tvalue = m1$coef/sqrt(diag(m1$var.coef))
tvalue
# example 2, csi 300 data
da = read.csv("000300.SH_ret.csv",sep=',',header=TRUE)
csi300 = da[,3] # log returns 
m2=arima(csi300,order=c(6,0,0))
m2
tvalue2 = m2$coef/sqrt(diag(m2$var.coef))
tvalue2

## model test
# example 1, ibm data, month return
da=read.table('m-ibm3dx2608.txt',header=T)
ibm = da$ibmrtn
m3 = arima(ibm,order=c(3,0,0))
# t-values
tvalue3 = m3$coef/sqrt(diag(m3$var.coef))
tvalue3
# Box-Ljung test
Box.test(m3$residuals,lag=12,type='Ljung')
# use adjusted degrees of freedom
pv=1-pchisq(11.658,9)
pv

# example 2, csi300, daily return
da = read.csv("000300.SH_ret.csv",sep=',',header=TRUE)
csi300 = da[,3] # log returns 
m4=arima(csi300,order=c(6,0,0))
m4
tvalue4 = m4$coef/sqrt(diag(m4$var.coef))
tvalue4
# Box-Ljung test
Box.test(m4$residuals,lag=12,type='Ljung')
# use the subcommand "fixed" to fix parameter values.
m5=arima(csi300,order=c(0,0,6),fixed=c(0,0,0,NA,0,NA,NA))
Box.test(m5$residuals,lag=12,type='Ljung')

## predict
# example 1, gnp data
da=read.table("q-gnp4710.txt",header=T)
G=da$VALUE
gnp=diff(log(G))
T1 = length(G)
T2 = length(gnp)
m1=arima(gnp[1:(T2-9)],order=c(3,0,0))
ar_predict = predict(m1,9)
# figure
par(mfcol=c(1,1))
plot((T2-16):T2,gnp[(T2-16):T2],type="l",ylim=c(-0.03,0.05),xlab="Time",ylab="s-return")
lines(244:252,gnp[(T2-8):T2],type="b")
lines(243:252,c(gnp[T2-9],ar_predict$pred),type="b",col="blue")
lines(243:252,c(gnp[T2-9],ar_predict$pred+2*ar_predict$se),lty=2, col="red",type="b")
lines(243:252,c(gnp[T2-9],ar_predict$pred-2*ar_predict$se),lty=2, col="green",type="b")
#title(main="multistep predict of GNP")

# example 2, csi300 return
da = read.csv("000300.SH_ret.csv",sep=',',header=TRUE)
csi300 = da[,3] # log returns 
T2=length(csi300)
predict_steps = 9
m2=arima(csi300[1:(T2-predict_steps)],order=c(3,0,0))
ar_predict = predict(m2,predict_steps)
# figure
par(mfcol=c(1,1))
plot((T2-16):T2,csi300[(T2-16):T2],type="l",ylim=c(-0.05,0.05),xlab="Time",ylab="s-return")
lines((T2-predict_steps+1):T2,csi300[(T2-predict_steps+1):T2],type="b")
lines((T2-predict_steps):T2,c(csi300[T2-predict_steps],ar_predict$pred),type="b",col="blue")
lines((T2-predict_steps):T2,c(csi300[T2-predict_steps],ar_predict$pred+2*ar_predict$se),lty=2, col="red",type="b")
lines((T2-predict_steps):T2,c(csi300[T2-predict_steps],ar_predict$pred-2*ar_predict$se),lty=2, col="green",type="b")


