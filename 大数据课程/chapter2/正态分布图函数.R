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