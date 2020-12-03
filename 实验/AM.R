library(quantmod)
library(fBasics)
getSymbols("AMZN")
AMZN.rtn <- na.omit(diff(log(AMZN$AMZN.Adjusted)))
acf <- acf(AMZN.rtn, lag = 20)
pacf <- pacf(AMZN.rtn, lag = 20)

mm1 <- ar(AMZN.rtn,lag = 4, method = 'ols')
mm1$aic
plot(c(0:mm1$order.max), mm1$aic, type = "h",xlab = "order", ylab = "AIC")
lines(0:mm1$order.max, mm1$aic, lty = 2, col = 'red')

aic_mat = matrix(NA, 3,3)
for(i in 1:3){
  for(j in 1:3){
    tmp = arima(AMZN.rtn,order = c(i,0,j))
    aic_mat[i,j] = tmp$aic
  }
}
aic_mat
which(aic_mat == min(aic_mat),arr.ind = T)


m1 = arima(AMZN.rtn, order = c(1,0,2))
m1
tvalue1 = m1$coef/sqrt(diag(m1$var.coef))
tvalue1
Box.test(m1$residuals, lag = 12, type = "Ljung")
