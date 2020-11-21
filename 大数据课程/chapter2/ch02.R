### chapter 2  scripts ###
### tungbin, 2020 ###
### part 1锛歊 intro and R studio intro



### part 2: install and load R packages
### example 1, Quantmod package
# install.packages(鈥淨uantmod鈥?)  # install the Quantmod package
library(quantmod)   # Load the package
getSymbols("AAPL")  # Download daily prices of Apple stock from Yahoo
dim(AAPL)  # (dimension): See the size of the downloaded data.
head(AAPL)  # See the first 6 rows of the data
tail(AAPL)   # See the last 6 rows of the data 
chartSeries(AAPL,theme="white")  # Plot the daily price and volume
chartSeries(AAPL)#Not shown giving the same plot with black background.
getSymbols("AAPL",from="2005-01-02", to="2010-12-31")
head(AAPL)
getSymbols("UNRATE",src="FRED")#Download unemployment rates from FRED.
head(UNRATE,8) # See the first  8 rows of the data  
tail(UNRATE,8) # See the last  8 rows of the data 
chartSeries(UNRATE,theme="white")  # Plot monthly unemployment rates
getSymbols("INTC",src="google")  # Download data from Google.
head(INTC)
getSymbols("^TNX") # Download CBOE 10-year Treasures Notes
head(TNX)
tail(TNX)  
chartSeries(TNX,theme="white",TA=NULL) # Obtain plot without volume.

### example 2, rugarch package
# install.packages("rugarch")
library(rugarch)
library(quantmod)
getSymbols("AAPL",from="2007-01-03",to="2020-9-28") #Specify period
AAPL.rtn=diff(log(AAPL$AAPL.Adjusted)) # Compute log returns
AAPL.close = AAPL$AAPL.Close
chartSeries(AAPL.close,theme="white") # Get the plot of close price
chartSeries(AAPL.rtn,theme="white") # Get the plot of log returns
spec = ugarchspec(mean.model=list(armaOrder=c(0,0),
                                  include.mean=T),distribution.model="norm") # model specification
aapl_ret = zoo(AAPL.rtn) # To get the zoo type data
gfit_norm = ugarchfit(spec=spec,data=aapl_ret[2:2189])
plot(gfit_norm) # Get the plots associated with the GARCH fitting


### part 3: use read.table and read.csv to get data
## 
setwd("C:/Users/17621/Documents/R studio project/大数据课程/chapter2") # Set my working directory
# install.packages("fBasics") # install package
library(fBasics) # Load package
da=read.table('d-ibm-0110.txt',header=TRUE) # Load text data with names.
## header=T means 1st row of the data file contains
## variable names. The default is header=F, i.e., no names.
head(da) # See the first 6 rows
dim(da)  # Dimension of the data object "da".
basicStats(da$return)
da$logret = log(da$return+1) # Get the log returns
write.csv(da, file = "ibm_return.csv")
da <- read.csv("ibm_return.csv",header=T) # Load csv data with names.
da = da[,-1]




#function example
f = function(x,y){
  c(x,y); a = x+y; a
}
f(1,2)

#编写函数
"normplot"=function(N,mu,sigma,plot="True"){
  zdata=rnorm(N,mu,sigma)
  zrange=range(zdata)
  x=seq(zrange[1],zrange[2])
  y=dnorm(x,mu,sigma)
  
  # plot the figure
  if(plot){
    hist(zdata,30,freq="False")
  limes(x,u,col="red",lwd=2)
  normplot=zdata
  }
  
}
zdata = rnorm(1000,0,1)
hist(zdata,30)
normplot(1000,0.1,0.1)

