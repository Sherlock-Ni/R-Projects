normplot1 <- function(N,mu,sigma,plot=TRUE){
  # N: sample length of normal random numbers
  # mu: mean 
  # sigma: standard deviation
  #
  zdata = rnorm(N,mu,sigma)
  zrange = range(zdata)
  x = seq(zrange[1],zrange[2],length.out = 100)
  y = dnorm(x,mu,sigma)
  
  if(plot){
    par(mfcol=c(1,1))
    hist(zdata,nclass=30,freq=F,col="grey", main ="") # Histogram
    lines(x,y,lty=2,col="red",lwd = 2)
    title(main='normal distribution')
  }
  normplot = zdata
}