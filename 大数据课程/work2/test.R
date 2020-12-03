### Loading Packages===========================
library(quantmod)
library(rugarch)
library(fBasics)

### Get Data and Plot price, log price and log return==========================
getSymbols("CPHI", from = "2015-01-01", to = "2020-12-02")
setSymbolLookup(VANKE=list(name='000002.sz',from = "2015-01-01", to = "2020-12-02",src='yahoo'))
getSymbols("VANKE")
CPHI.price <- CPHI$CPHI.Adjusted
VANKE.price <- VANKE$'000002.SZ.Adjusted'
CPHI.rtn <- na.omit(diff(log(CPHI$CPHI.Adjusted)))
VANKE.rtn <- na.omit(diff(log(VANKE$'000002.SZ.Adjusted')))
CPHI.tdx <- c(1:1490)/252 + 2015
VANKE.tdx <- c(1:1438)/252 + 2015
CPHI.adjtdx <- c(2:1490)/252 + 2015
VANKE.adjtdx <- c(2:1438)/252 + 2015

# Price 
opar <- par(no.readonly = TRUE)
par(mfcol = c(2,1))
plot(CPHI.tdx, CPHI.price, xlab = "Year", ylab = "Price", type = 'l', main = "The price of CPHI from 2015 to 2020")
plot(VANKE.tdx, VANKE.price, xlab = "Year", ylab = "Price", type = 'l', main = "The price of VANKE from 2015 to 2020")

# Log Price 
plot(CPHI.tdx, log(CPHI.price), xlab = "Year", ylab = "Log Price", type = 'l', main = "The log price of CPHI from 2015 to 2020")
plot(VANKE.tdx, log(VANKE.price), xlab = "Year", ylab = "Log Price", type = 'l', main = "The log price of VANKE from 2015 to 2020")

# Log return
plot(CPHI.adjtdx, CPHI.rtn, xlab = "Year", ylab = "Log Return", type = 'l', main = "The log return of CPHI from 2015 to 2020")
plot(VANKE.adjtdx, VANKE.rtn, xlab = "Year", ylab = "Log Return", type = 'l', main = "The log return of VANKE from 2015 to 2020")
par(opar)

### Part1: CPHI ===================
# ACF AND PACF
acf1 <- acf(CPHI.rtn, lag = 20, col = "red", main = "Series CPHI")
pacf1 <- pacf(CPHI.rtn, lag = 20, col = "red", main = "Series CPHI")

# AR(4) model
m1 <- arima(CPHI.rtn, order = c(4,0,0))
m1
tsdiag(m1, gof = 20)
t.test(CPHI.rtn)
mm1 <- arima(CPHI.rtn,order = c(4,0,0), include.mean = F)
mm1

# MA(4) model
m2 <- arima(CPHI.rtn, order = c(0,0,4))
m2
tsdiag(m2, gof = 20)
t.test(CPHI.rtn)
mm2 <- arima(CPHI.rtn, order = c(0,0,4), include.mean = F)
mm2

# ARMA(1,1) model
m3 <- arima(CPHI.rtn, order = c(1,0,1), include.mean = F)
m3
tsdiag(m3, gof = 20)

# choose the best arma model
aic_mat = matrix(NA, 5, 5)
for(i in 1:5){
  for(j in 1:5){
    tmp = arima(CPHI.rtn,order = c(i,0,j))
    aic_mat[i,j] = tmp$aic
  }
}
aic_mat
which(aic_mat == min(aic_mat),arr.ind = T)

m4 <- arima(CPHI.rtn, order = c(5,0,5))
m4
tsdiag(m4)
mm4 <- arima(CPHI.rtn, order = c(5,0,5), include.mean = F)
mm4

# comparison of out-of-sample prediction ============================
source("backtest.R")
pm1=backtest(mm1,CPHI.rtn,745,1,inc.mean=F)
pm2=backtest(mm2,CPHI.rtn,745,1,inc.mean=F)
pm3=backtest(m3,CPHI.rtn,745,1,inc.mean=F)
pm4=backtest(mm4,CPHI.rtn,745,1,inc.mean=F)

aic1 = c(mm1$aic,mm2$aic,m3$aic,mm4$aic)
rmse1 = c(pm1$rmse,pm2$rmse,pm3$rmse,pm4$rmse)
mabso1 = c(pm1$mabso,pm2$mabso,pm3$mabso,pm4$mabso)
perform_tab_CPHI = data.frame(aic1,rmse1,mabso1)
write.csv(perform_tab_CPHI,"outofsample_perform_CPHI.csv")

#plot the figure
pm3fit=CPHI.rtn[746:1489]-pm3$error
pm4fit=CPHI.rtn[746:1489]-pm4$error
plot(CPHI.adjtdx[746:1489],CPHI.rtn[746:1489],xlab='year',ylab='log return',type='l')
points(CPHI.adjtdx[746:1489],pm3fit,pch='*')
plot(CPHI.adjtdx[746:1489],CPHI.rtn[746:1489],xlab='year',ylab='log return',type='l')
points(CPHI.adjtdx[746:1489],pm4fit,pch='*')




### Part2: VANKE =======================
# ACF and PACF
acf2 <- acf(VANKE.rtn, lag = 20, col = "red", main = "Series VANKE")
pacf2 <- pacf(VANKE.rtn, lag = 20, col = "red", main = "Series VANKE")

# AR(2) model
m5 <- arima(VANKE.rtn, order = c(2,0,0))
m5
tsdiag(m5, gof = 20)
t.test(VANKE.rtn)
mm5 <- arima(VANKE.rtn, order = c(2,0,0), include.mean = F)
mm5

# MA(2) model
m6 <- arima(VANKE.rtn, order = c(0,0,2))
m6
tsdiag(m6, lag = 20)
t.test(VANKE.rtn)
mm6 <- arima(VANKE.rtn, order = c(0,0,2), include.mean = F)
mm6

# ARMA(1,1) model
m7 <- arima(VANKE.rtn, order = c(1,0,1), include.mean = F)
m7
tsdiag(m7,lag = 20)

# choose the best arma model
aic_mat = matrix(NA, 5, 5)
for(i in 1:5){
  for(j in 1:5){
    tmp = arima(VANKE.rtn,order = c(i,0,j))
    aic_mat[i,j] = tmp$aic
  }
}
aic_mat
which(aic_mat == min(aic_mat),arr.ind = T)

# ARMA(2,3) model
m8 <- arima(VANKE.rtn, order = c(2,0,3))
m8
tsdiag(m8, lag = 20)
mm8 <- arima(VANKE.rtn, order = c(2,0,3), include.mean = F, fixed = c(NA,NA,0,NA,NA))
mm8

# comparison of out-of-sample prediction ==========================
source("backtest.R")
pm5=backtest(mm5,VANKE.rtn,719,1,inc.mean=F)
pm6=backtest(mm6,VANKE.rtn,719,1,inc.mean=F)
pm7=backtest(m7,VANKE.rtn,719,1,inc.mean=F)
pm8=backtest(mm8,VANKE.rtn,719,1,fixed = c(NA,NA,0,NA,NA), inc.mean=F)

aic2 = c(mm5$aic,mm6$aic,m7$aic,mm8$aic)
rmse2 = c(pm5$rmse,pm6$rmse,pm7$rmse,pm8$rmse)
mabso2 = c(pm5$mabso,pm6$mabso,pm7$mabso,pm8$mabso)
perform_tab_VANKE = data.frame(aic2,rmse2,mabso2)
write.csv(perform_tab_VANKE,"outofsample_perform_VANKE.csv")

#plot the figure
pm7fit=VANKE.rtn[720:1437]-pm7$error
pm8fit=VANKE.rtn[720:1437]-pm8$error
plot(VANKE.adjtdx[720:1437],VANKE.rtn[720:1437],xlab='year',ylab='log return',type='l')
points(VANKE.adjtdx[720:1437],pm7fit,pch='*')
plot(VANKE.adjtdx[720:1437],VANKE.rtn[720:1437],xlab='year',ylab='log return',type='l')
points(VANKE.adjtdx[720:1437],pm8fit,pch='*')




write.csv(CPHI, file = "CPHI.csv")
write.csv(VANKE, file = "VANKE.csv")
