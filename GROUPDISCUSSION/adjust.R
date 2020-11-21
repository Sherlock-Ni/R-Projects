library(fBasics)
library(rugarch)
library(quantmod)


getSymbols("TSLA",from="2018-01-01",to="2020-10-27")#Specify period
TSLA.rtn=diff(log(TSLA$TSLA.Adjusted))#Compute log returns
TSLA.close=TSLA$TSLA.Close
chartSeries(TSLA.close,theme = "white")#Get the plot of close price
chartSeries(TSLA.rtn,theme = "white")#Get the plot of log returns
TSLA.rtn = na.omit(TSLA.rtn)
write.csv(TSLA.rtn, file = "TSLA_return.csv")


basicStats(TSLA.rtn) #Compute summary statistics


hist(TSLA.rtn,nclass=150,freq = F,col="grey")#Histogram
d1=density(TSLA.rtn)#Obtain density estimate
range(TSLA.rtn)#Range of 3M returns
x=seq(-.1,.1,.001)#Create a sequence of x with increment 0.001.
y1=dnorm(x,mean(TSLA.rtn),stdev(TSLA.rtn))
lines(d1$x,d1$y,lty=1,col="blue")
lines(x,y1,lty=2,col="red")