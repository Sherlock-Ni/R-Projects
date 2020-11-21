library(quantmod)
library(rugarch)
library(fBasics)

###part1:get the data about CPHI, compute the price and log return, plot it
getSymbols("CPHI")
CPHI.rtn = diff(log(CPHI$CPHI.Adjusted))
CPHI.price = CPHI$CPHI.Adjusted
chartSeries(CPHI.rtn,theme = "black")
chartSeries(CPHI.price,theme = "white")
basicStats(CPHI.price)#compute summary statistics about the price of CPHI
basicStats(CPHI.rtn)#compute summary statistics about the return of CPHI


###part2:distribution of financial data;plot the histogram, the fitted density and normal density in one figure
CPHI.rtn1 = na.omit(CPHI.rtn)
hist(CPHI.rtn1, nclass = 40, freq = F)
d1 = density(CPHI.rtn1)
range(CPHI.rtn1)
x1 = seq(-0.4,0.4,0.001)
y1 = dnorm(x1, mean(CPHI.rtn1),stdev(CPHI.rtn1))
lines(d1$x, d1$y, xlab = "x", ylab = "density", lty = 1, col = "blue")
lines(x1, y1, lty = 2, col = "red")


### part3:Save data
write.csv(CPHI, file = "CPHI.csv")
write.csv(CPHI.rtn,file = "CHPI_rtn.csv")




####example2:ABOUT VANKE
###part1:get the data about VANKE, compute the price and log return, plot it
setSymbolLookup(WK=list(name='000002.sz',from = "2018-01-01", to = "2020-10-27",src='yahoo'))
getSymbols("WK")
VANKE.price = WK[,6]
VANKE.rtn = diff(log(VANKE.price))
chartSeries(VANKE.rtn,theme = "black")
chartSeries(VANKE.price,theme = "white")
basicStats(VANKE.price)#compute summary statistics about the price of VANKE
basicStats(VANKE.rtn)#compute summary statistics about the return of VANKE


###part2:distribution of financial data;plot the histogram, the fitted density and normal density in one figure
VANKE.rtn1 = na.omit(VANKE.rtn)
hist(VANKE.rtn1, nclass = 40, freq = F)
d2 = density(VANKE.rtn1)
range(VANKE.rtn1)
x2 = seq(-0.1,0.1,0.001)
y2 = dnorm(x2, mean(VANKE.rtn1),stdev(VANKE.rtn1))
lines(d2$x, d2$y, xlab = "x", ylab = "density", lty = 1, col = "blue")
lines(x2, y2, lty = 2, col = "red")



### part3:Save data
write.csv(WK, file = "VANKE.csv")
write.csv(VANKE.rtn,file = "VANKE_rtn.csv")





#plot hist together
opar = par(no.readonly = TRUE)
par(mfrow = c(2,1))
hist(CPHI.rtn1, nclass = 40, freq = F,main = "Histogram of CPHI's return", xlab = "CPHI_return")
lines(d1$x, d1$y, xlab = "x", ylab = "density", lty = 1, col = "blue")
lines(x1, y1, lty = 2, col = "red")
hist(VANKE.rtn1, nclass = 40, freq = F, main = "Histogram of VANKE's return", xlab = "VANKE_return")
lines(d2$x, d2$y, xlab = "x", ylab = "density", lty = 1, col = "blue")
lines(x2, y2, lty = 2, col = "red")
par(opar)








####test
setSymbolLookup(WK=list(name='000002.sz',from = "2018-01-01", to = "2020-10-27",src='yahoo'))
getSymbols("WK")
VANKE.price = WK[,6]
VANKE.rtn = diff(log(VANKE.price))
chartSeries(VANKE.rtn,theme = "black")
chartSeries(CPHI.price,theme = "white")
basicStats(CPHI.price)#compute summary statistics about the price of CPHI
basicStats(CPHI.rtn)#compute summary statistics about the return of CPHI


###part2:distribution of financial data;plot the histogram, the fitted density and normal density in one figure
CPHI.rtn1 = na.omit(CPHI.rtn)
hist(CPHI.rtn1, nclass = 40)
d1 = density(CPHI.rtn1)
range(CPHI.rtn1)
x = seq(-0.8,0.8,0.001)
y1 = dnorm(x, mean(CPHI.rtn1),stdev(CPHI.rtn1))
lines(d1$x, d1$y, xlab = "x", ylab = "density", lty = 1, col = "blue")
lines(x, y1, lty = 2, col = "red")


### part3:Save data
write.csv(CPHI, file = "CPHI.csv")
write.csv(CPHI.rtn,file = "CHPI_rtn.csv")