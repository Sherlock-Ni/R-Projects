

library(fBasics)
library(rugarch)
library(quantmod)


setwd("D:/金融大数据课程")
getSymbols("TSLA",from="2011-01-03",to="2020-10-29")#Specify period
TSLA.rtn=diff(log(TSLA$TSLA.Adjusted))#Compute log returns
TSLA.close=TSLA$TSLA.Close
chartSeries(TSLA.close,theme = "white")#Get the plot of close price
chartSeries(TSLA.rtn,theme = "white")#Get the plot of log returns
TSLA.rtn = na.omit(TSLA.rtn)
write.csv(TSLA.rtn, file = "TSLA_return.csv")


getSymbols("^IXIC",from="2011-01-03",to="2020-10-29")#Specify period
IXIC.rtn=diff(log(IXIC$IXIC.Adjusted))#Compute log returns
IXIC.close=IXIC$IXIC.Close
chartSeries(IXIC.close,theme = "white")#Get the plot of close price
chartSeries(IXIC.rtn,theme = "white")#Get the plot of log returns
IXIC.rtn = na.omit(IXIC.rtn)
write.csv(IXIC.rtn, file = "IXIC_return.csv")

basicStats(IXIC.rtn) #Compute summary statistics
basicStats(TSLA.rtn) #Compute summary statistics


hist(TSLA.rtn,nclass=150,freq = F,col="grey")#Histogram
d1=density(TSLA.rtn)#Obtain density estimate
range(TSLA.rtn)#Range of 3M returns
x=seq(-.1,.1,.001)#Create a sequence of x with increment 0.001.
y1=dnorm(x,mean(TSLA.rtn),stdev(TSLA.rtn))
lines(d1$x,d1$y,lty=1,col="blue")
lines(x,y1,lty=2,col="red")



hist(IXIC.rtn,nclass=150,freq = F,col="grey")#Histogram
d2=density(IXIC.rtn)#Obtain density estimate
range(IXIC.rtn)#Range of 3M returns
x=seq(-.1,.1,.001)#Create a sequence of x with increment 0.001.
y2=dnorm(x,mean(IXIC.rtn),stdev(IXIC.rtn))
lines(d2$x,d2$y,lty=1,col="blue")
lines(x,y2,lty=2,col="red")